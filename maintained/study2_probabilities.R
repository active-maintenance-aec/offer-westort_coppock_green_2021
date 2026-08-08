# offer-westort_coppock_green_2021/maintained/study2_probabilities.R
# Output: output/study_2_probabilities.rds
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Bayesian bootstrap posterior probabilities that each study 2 arm is
#   best, computed separately for Democrats, Republicans, and independents after
#   each batch. These are not the realised assignment probabilities: those came
#   from the control-augmented algorithm and are already in the data as inverse
#   probability weights.
#
#   This is the one intermediate object the deposit's own analysis script needs
#   and the deposit does not contain, so it is computed here rather than read.
#   Runtime is about two minutes at J = 10,000.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

J <- 1e4
Zn <- n_distinct(mturk_bandit$Z)
batches <- sort(unique(mturk_bandit$batch))

arm_names <- function(d) {
  d |>
    distinct(pid, Z) |>
    arrange(pid, Z) |>
    mutate(name = paste0(str_sub(pid, 1, 3), "_", Z)) |>
    pull(name)
}

# One batch at a time, on the data observed up to that batch ----
posteriors_by_batch <- map(batches, function(b) {
  ymat <- mturk_bandit |>
    filter(batch <= b) |>
    select(pid, Z, Y_sum)

  cells <- arm_names(ymat)

  draws <- map(1:J, function(j) {
    weighted <- ymat |>
      mutate(d = rexp(n = n()),
             dy1 = Y_sum * d,
             dy0 = (3 - Y_sum) * d) |>
      group_by(pid, Z) |>
      summarize(alpha = sum(dy1) + 1, beta = sum(dy0) + 1, .groups = "drop")
    weighted$alpha / (weighted$alpha + weighted$beta)
  })

  theta <- do.call(rbind, draws)
  colnames(theta) <- cells

  # Share of draws in which each arm has the highest success rate, within party.
  share_best <- function(cols) {
    block <- theta[, cols, drop = FALSE]
    colMeans(block == apply(block, 1, max))
  }

  tibble(
    batch = b,
    pid = rep(c("democrat", "independent", "republican"), each = Zn),
    arm = rep(levels(mturk_bandit$Z), times = 3),
    posterior_prob = c(share_best(1:Zn),
                       share_best((Zn + 1):(Zn * 2)),
                       share_best((Zn * 2 + 1):(Zn * 3))),
    posterior_mean = c(colMeans(theta[, 1:Zn]),
                       colMeans(theta[, (Zn + 1):(Zn * 2)]),
                       colMeans(theta[, (Zn * 2 + 1):(Zn * 3)]))
  )
})

# Batch 0 is the uniform prior ----
prior <- expand_grid(
  batch = 0,
  pid = c("democrat", "independent", "republican"),
  arm = levels(mturk_bandit$Z)
) |>
  mutate(posterior_prob = 1 / Zn, posterior_mean = NA_real_)

study_2_probabilities <-
  bind_rows(prior, list_rbind(posteriors_by_batch)) |>
  arrange(pid, arm, batch)

write_rds(study_2_probabilities, file.path(out_dir, "study_2_probabilities.rds"))
write_csv(study_2_probabilities, file.path(out_dir, "study_2_probabilities.csv"))

print(study_2_probabilities |> filter(batch == max(batch)), n = 20)
