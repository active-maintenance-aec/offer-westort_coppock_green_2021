# offer-westort_coppock_green_2021/maintained/helpers.R
# Output: (none; sourced by every other script)
# Description: Packages, paths, and the simulation and estimation machinery the
#   deposit keeps in utils.R. The algorithms are reproduced here rather than
#   sourced because utils.R loads its own packages and resolves paths relative to
#   the working directory. Every random-number call is in the same order as the
#   deposit, so seeded results are comparable.

library(here)
library(tidyverse)
library(estimatr)
library(broom)
library(bandit)
library(randomizr)
library(ggrepel)
library(cowplot)
library(ggpubr)
library(knitr)
library(kableExtra)
library(modelsummary)

here::i_am("maintained/helpers.R")

data_dir <- here::here("original")
out_dir <- here::here("maintained", "output")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

options(modelsummary_format_numeric_latex = "plain")

# Long form of a matrix ----
# reshape2::melt() applied to an unnamed matrix, without reshape2: row index in
# Var1, column index in Var2, column-major order. The estimation code below
# depends on that ordering, since three melts are lined up side by side.
melt_matrix <- function(m) {
  tibble(
    Var1 = rep(seq_len(nrow(m)), times = ncol(m)),
    Var2 = rep(seq_len(ncol(m)), each = nrow(m)),
    value = as.vector(m)
  )
}

# Thompson sampling simulation ----
# Returns cumulative sample (nmat), cumulative successes (xmat), and the
# posterior probability that each arm is best (ppmat), one row per period.
sim_out <- function(n = 100, periods = 10, arms = 9, probs, static = FALSE,
                    first = NA) {
  if (is.na(first)) {
    first <- n
  } else {
    n <- (n * periods - first) / (periods - 1)
  }

  xmat <- nmat <- ppmat <- matrix(NA, ncol = arms, nrow = periods)

  i <- 1
  nmat[i, ] <- table(simple_ra(N = first, prob_each = rep(1 / arms, arms)))
  xmat[i, ] <- mapply(rbinom, n = 1, size = nmat[i, ], prob = probs)
  ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])

  while (i < periods) {
    i <- i + 1
    if (static) {
      newvals <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
    } else {
      newvals <- table(simple_ra(N = n, prob_each = ppmat[(i - 1), ]))
    }
    nmat[i, ] <- nmat[(i - 1), ] + newvals
    xmat[i, ] <- xmat[(i - 1), ] + mapply(rbinom, n = 1, size = newvals, prob = probs)
    ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])
  }

  list(nmat = nmat, xmat = xmat, ppmat = ppmat)
}

# Control-augmented sampling probabilities ----
# The control arm's share of the batch is whatever it needs to catch up with the
# current best arm, capped at 90 per cent; the rest is Thompson sampling.
ca_sampling_probs <- function(xvec, nvec, cn = 3, n) {
  pn <- best_binomial_bandit(xvec, nvec)
  sp <- rep(NA, length(xvec))
  cd <- nvec[which.max(pn)] - nvec[cn]
  pp <- min(max(cd / n, 0), .9)
  sp[cn] <- 1 / length(xvec) * (1 - pp) + pp
  sp[-cn] <- pn[-cn] / sum(pn[-cn]) * ((length(xvec) - 1) / length(xvec)) * (1 - pp)
  sp
}

# Control-augmented simulation ----
sim_out_te <- function(n = 100, periods = 10, arms = 9, probs, static = FALSE,
                       control = 3, first = NA) {
  if (is.na(first)) {
    first <- n
  } else {
    n <- (n * periods - first) / (periods - 1)
  }

  xmat <- nmat <- ppmat <- spmat <- matrix(NA, ncol = arms, nrow = periods)

  i <- 1
  nmat[i, ] <- table(simple_ra(N = first, prob_each = rep(1 / arms, arms)))
  xmat[i, ] <- mapply(rbinom, n = 1, size = nmat[i, ], prob = probs)
  ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])
  spmat[i, ] <- 1 / arms

  while (i < periods) {
    i <- i + 1
    if (static) {
      newvals <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
      spmat[i, ] <- 1 / arms
    } else {
      spmat[i, ] <- ca_sampling_probs(
        xvec = xmat[(i - 1), ], nvec = nmat[(i - 1), ], cn = control, n = n
      )
      newvals <- table(simple_ra(N = n, prob_each = spmat[i, ]))
    }
    nmat[i, ] <- nmat[(i - 1), ] + newvals
    xmat[i, ] <- xmat[(i - 1), ] + mapply(rbinom, n = 1, size = newvals, prob = probs)
    ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])
  }

  list(nmat = nmat, xmat = xmat, ppmat = ppmat, spmat = spmat, control = control)
}

# Randomization-inference simulation ----
# Reassigns the observed outcome vector to arms under the adaptive or static
# rule, which is how the null distribution of the F statistic is built.
sim_out_RI <- function(Y, Z, periods = 10, static = FALSE, control = FALSE) {
  ii <- cumsum(table(cut_interval(seq_along(Y), 10)))
  i <- 1
  n <- ii[i]
  index <- 1:ii[i]
  arms <- length(Z)
  xmat <- nmat <- ppmat <- spmat <- matrix(NA, ncol = arms, nrow = periods)

  nmat[i, ] <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
  zz <- sample(rep(Z, nmat[i, ]))
  xmat[i, ] <- unlist(lapply(split(Y[index], zz), sum))
  ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])
  spmat[i, ] <- 1 / arms

  while (i < periods) {
    i <- i + 1
    index <- (ii[i - 1] + 1):ii[i]
    n <- ii[i] - ii[i - 1]
    if (static) {
      newvals <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
      spmat[i, ] <- 1 / arms
    } else if (isFALSE(control)) {
      newvals <- table(simple_ra(N = n, prob_each = ppmat[(i - 1), ]))
    } else {
      spmat[i, ] <- ca_sampling_probs(
        xvec = xmat[(i - 1), ], nvec = nmat[(i - 1), ], cn = control, n = n
      )
      newvals <- table(simple_ra(N = n, prob_each = spmat[i, ]))
    }
    nmat[i, ] <- nmat[(i - 1), ] + newvals
    zz <- sample(rep(Z, newvals))
    xmat[i, ] <- xmat[(i - 1), ] + unlist(lapply(split(Y[index], zz), sum))
    ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ])
  }

  if (isFALSE(control)) {
    list(nmat = nmat, xmat = xmat, ppmat = ppmat)
  } else {
    list(nmat = nmat, xmat = xmat, ppmat = ppmat, spmat = spmat, control = control)
  }
}

# Clustered randomization-inference simulation ----
# Study 2 assigns one condition per respondent and asks three questions, so the
# unit of assignment is a cluster of three observations.
sim_out_RI_clustered <- function(Y, Z, periods = 10, static = FALSE,
                                 control = FALSE, cluster_size = 3) {
  ii <- cumsum(table(cut_interval(seq_along(Y), 10)))
  i <- 1
  n <- ii[i]
  index <- 1:ii[i]
  arms <- length(Z)
  xmat <- nmat <- ppmat <- spmat <- matrix(NA, ncol = arms, nrow = periods)

  nmat[i, ] <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
  zz <- sample(rep(Z, nmat[i, ]))
  xmat[i, ] <- unlist(lapply(split(Y[index], zz), sum))
  ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ] * cluster_size)
  spmat[i, ] <- 1 / arms

  while (i < periods) {
    i <- i + 1
    index <- (ii[i - 1] + 1):ii[i]
    n <- ii[i] - ii[i - 1]
    if (static) {
      newvals <- table(simple_ra(N = n, prob_each = rep(1 / arms, arms)))
      spmat[i, ] <- 1 / arms
    } else if (isFALSE(control)) {
      newvals <- table(simple_ra(N = n, prob_each = ppmat[(i - 1), ]))
    } else {
      spmat[i, ] <- ca_sampling_probs(
        xvec = xmat[(i - 1), ], nvec = nmat[(i - 1), ] * cluster_size,
        cn = control, n = n
      )
      newvals <- table(simple_ra(N = n, prob_each = spmat[i, ]))
    }
    nmat[i, ] <- nmat[(i - 1), ] + newvals
    zz <- sample(rep(Z, newvals))
    xmat[i, ] <- xmat[(i - 1), ] + unlist(lapply(split(Y[index], zz), sum))
    ppmat[i, ] <- best_binomial_bandit_sim(xmat[i, ], nmat[i, ] * cluster_size)
  }

  cluster <- rep(seq_along(Y), each = cluster_size)
  if (isFALSE(control)) {
    list(nmat = nmat * cluster_size, xmat = xmat, ppmat = ppmat, cluster = cluster)
  } else {
    list(nmat = nmat * cluster_size, xmat = xmat, ppmat = ppmat, spmat = spmat,
         control = control, cluster = cluster)
  }
}

# Expand a simulation into subject-level rows ----
# Every simulation is stored as batch totals; estimation needs one row per
# simulated subject, carrying the sampling probability that generated it.
expand_sim <- function(vals, weight_source) {
  bbmat <- melt_matrix(rbind(vals$nmat[1, ], diff(vals$nmat)))
  bbmat$success <- melt_matrix(rbind(vals$xmat[1, ], diff(vals$xmat)))$value
  bbmat$weight <- melt_matrix(weight_source)$value

  tibble(
    y = unlist(map2(
      bbmat$value - bbmat$success, bbmat$success,
      function(failures, successes) c(rep(0, failures), rep(1, successes))
    )),
    z = factor(rep(bbmat$Var2, bbmat$value)),
    w = rep(bbmat$weight, bbmat$value)
  )
}

# The weights an ipw estimate uses ----
# Static assignment is uniform; adaptive assignment without a control arm uses
# the previous batch's posterior; the control-augmented design uses its own
# sampling probability matrix.
sim_weights <- function(vals, static, has_control) {
  arms <- ncol(vals$ppmat)
  if (static) {
    matrix(1 / arms, nrow = nrow(vals$nmat), ncol = arms)
  } else if (!has_control) {
    rbind(rep(1 / arms, arms), vals$ppmat[1:(nrow(vals$ppmat) - 1), ])
  } else {
    vals$spmat
  }
}

# Inverse-probability-weighted estimation inside a simulation ----
ipw_est <- function(vals, static = FALSE, se_type = "HC2") {
  has_control <- length(is.finite(vals$control)) > 0
  d <- expand_sim(vals, sim_weights(vals, static, has_control))

  if (!has_control) {
    lm_robust(y ~ -1 + z, data = d, weights = 1 / w, se_type = se_type)
  } else {
    d$z <- relevel(d$z, ref = as.character(vals$control))
    lm_robust(y ~ z, data = d, weights = 1 / w, se_type = se_type)
  }
}

# Clustered version, used for the study 2 randomization inference ----
ipw_est_clustered <- function(vals, static = FALSE, se_type = "CR2") {
  has_control <- length(is.finite(vals$control)) > 0
  d <- expand_sim(vals, sim_weights(vals, static, has_control))

  if (!has_control) {
    lm_robust(y ~ -1 + z, data = d, weights = 1 / w)
  } else {
    d$z <- relevel(d$z, ref = as.character(vals$control))
    lm_robust(y ~ z, data = d, weights = 1 / w, se_type = se_type,
              cluster = vals$cluster)
  }
}

# Iterate a simulation and record its statistics ----
mysims <- function(probs = NA, control = FALSE, static = FALSE, periods = 10,
                   n = 100, iter = 1000, RI = FALSE, Y = NA, Z = NA, first = NA) {
  outmat <- matrix(NA_real_, ncol = 11 + periods, nrow = iter)
  colnames(outmat) <- c("correct", "posterior_best", "bias", "rmse_best",
                        "rmse_te", "cov_best", "cov_te", "ses", "F", "p",
                        0:periods)

  K <- if (RI) length(Z) else length(probs)
  if (RI) probs <- rep(1 / mean(Y), K)

  outmat[, "0"] <- 1 / K

  for (i in 1:iter) {
    if (isFALSE(control)) {
      if (RI) {
        vals <- sim_out_RI(Y, Z, periods = periods, static = static)
        true_val <- mean(Y)
        true_vali <- "z1"
      } else {
        vals <- sim_out(probs = probs, arms = K, static = static,
                        periods = periods, n = n, first = first)
        true_val <- max(probs)
        true_vali <- paste0("z", which.max(probs))
      }
      lm_r <- ipw_est(vals, static)
      outmat[i, "rmse_best"] <- sqrt((coef(lm_r)[true_vali] - true_val)^2)
      outmat[i, "cov_best"] <- 1 * ((lm_r[["conf.low"]][true_vali] < true_val) &
                                      (lm_r[["conf.high"]][true_vali] > true_val))
    } else {
      if (RI) {
        vals <- sim_out_RI(Y, Z, periods = periods, static = static, control = control)
        true_val <- 0
        true_vali <- "z1"
      } else {
        vals <- sim_out_te(probs = probs, arms = K, static = static,
                           control = control, periods = periods, n = n, first = first)
        true_val <- max(probs) - probs[control]
        true_vali <- paste0("z", which.max(probs))
      }
      lm_r <- ipw_est(vals, static)
      est <- sum(coef(lm_r)[c("(Intercept)", true_vali)])
      se <- sqrt(abs(sum(vcov(lm_r)[c("(Intercept)", true_vali),
                                    c("(Intercept)", true_vali)])))
      outmat[i, "rmse_best"] <- sqrt((est - max(probs))^2)
      cint <- est + se * qt(0.975, lm_r$N - lm_r$k) * c(-1, 1)
      outmat[i, "cov_best"] <- 1 * ((cint[1] < max(probs)) & (cint[2] > max(probs)))
    }

    outmat[i, "correct"] <- (which.max(vals$ppmat[nrow(vals$ppmat), ]) == which.max(probs)) * 1
    outmat[i, "posterior_best"] <- vals$ppmat[nrow(vals$ppmat), which.max(probs)]
    outmat[i, "bias"] <- coef(lm_r)[true_vali] - true_val
    outmat[i, "ses"] <- lm_r$std.error[true_vali]

    if (isFALSE(control)) {
      # Arm 3 stands in as the control arm so that treatment-effect statistics
      # are defined for the designs that have no control condition.
      vals$control <- 3
      vals$spmat <- rbind(rep(1 / K, K), vals$ppmat)[1:periods, ]
      lm_r <- ipw_est(vals, static)
      true_val <- max(probs) - probs[vals$control]
    }

    outmat[i, "rmse_te"] <- sqrt((coef(lm_r)[true_vali] - true_val)^2)
    outmat[i, "cov_te"] <- 1 * ((lm_r[["conf.low"]][true_vali] < true_val) &
                                  (lm_r[["conf.high"]][true_vali] > true_val))
    if (is.na(outmat[i, "cov_te"])) outmat[i, "cov_te"] <- 0

    outmat[i, "p"] <- lm_r$p.value[true_vali]

    lm_r <- ipw_est(vals, static, se_type = "classical")
    outmat[i, "F"] <- summary(lm_r)[["fstatistic"]]["value"]

    outmat[i, as.character(1:periods)] <- vals$ppmat[, which.max(probs)]
  }

  outmat
}

# Clustered iteration, for study 2 ----
mysims_RI_clustered <- function(control = FALSE, static = FALSE, periods = 10,
                                iter = 1000, Y, Z, cluster_size = 3) {
  outmat <- matrix(NA_real_, ncol = 10 + periods, nrow = iter)
  colnames(outmat) <- c("correct", "posterior_best", "bias", "rmse_best",
                        "rmse_te", "cov", "ses", "F", "p", 0:periods)

  outmat[, "0"] <- 1 / length(Z)

  for (i in 1:iter) {
    vals <- sim_out_RI_clustered(Y, Z, periods = periods, static = static,
                                 control = control, cluster_size = cluster_size)
    true_val <- if (isFALSE(control)) mean(Y) else 0
    true_vali <- "z1"

    lm_r <- ipw_est_clustered(vals, static)

    outmat[i, "rmse_best"] <- if (isFALSE(control)) {
      sqrt((coef(lm_r)[true_vali] - true_val)^2)
    } else {
      sqrt((sum(coef(lm_r)[c("(Intercept)", true_vali)]) - mean(Y / cluster_size))^2)
    }

    outmat[i, "correct"] <- (which.max(vals$ppmat[nrow(vals$ppmat), ]) == 1) * 1
    outmat[i, "posterior_best"] <- vals$ppmat[nrow(vals$ppmat), 1]
    outmat[i, "bias"] <- coef(lm_r)[true_vali] - true_val
    outmat[i, "cov"] <- 1 * ((lm_r[["conf.low"]][true_vali] < true_val) &
                               (lm_r[["conf.high"]][true_vali] > true_val))
    outmat[i, "ses"] <- lm_r$std.error[true_vali]

    if (isFALSE(control)) {
      vals$control <- 3
      vals$spmat <- rbind(rep(1 / length(Z), length(Z)), vals$ppmat)[1:periods, ]
      lm_r <- ipw_est_clustered(vals, static)
    }

    outmat[i, "rmse_te"] <- sqrt((coef(lm_r)[true_vali] - true_val)^2)
    outmat[i, "p"] <- lm_r$p.value[true_vali]
    outmat[i, "F"] <- summary(lm_r)[["fstatistic"]]["value"]
    outmat[i, as.character(1:periods)] <- vals$ppmat[, 1]
  }

  outmat
}

# Formatting ----
format_num <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

add_parens <- function(x, digits = 2) {
  paste0("(", format_num(x, digits = digits), ")")
}

make_entry <- function(estimate, std.error, p.value) {
  paste0(format_num(estimate, 3), " ", add_parens(std.error, 3),
         if_else(p.value < 0.05, "*", ""))
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
