# offer-westort_coppock_green_2021/maintained/text_study2_f_tests.R
# Output: output/text_study2_f_tests.csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: The two randomization-inference F-test p-values the study 2 results
#   section reports. Assignment is clustered by respondent, since each respondent
#   answers three questions under one condition, so the null distribution comes
#   from the clustered version of the assignment procedure.
#
#   Which arm the control-augmented algorithm protects is a parameter of that
#   procedure, and the deposit sets it by position. It renames the treatment
#   levels to lottery, accuracy, control, google, direction, extratime and then
#   passes control = 3, but the levels in the data are control, lottery, accuracy,
#   google, direction, extratime, so position 3 is the accuracy arm. The null
#   distribution is therefore built by augmenting the wrong arm. Both versions are
#   computed here: augmented_arm = "position 3 (deposit)" reproduces the published
#   p-values, and augmented_arm = "control (corrected)" is what the labelling was
#   evidently meant to do. Runtime is about four minutes for the pair.

source(here::here("maintained", "helpers.R"))

iter <- 1e3
seed <- 95126

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

mturk_long <-
  mturk_bandit |>
  pivot_longer(
    cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
    names_to = "out",
    values_to = "Y",
    values_transform = list(Y = as.double)
  )

fit_d <- lm_robust(Y ~ Z, data = filter(mturk_long, pid == "democrat"),
                   cluster = id, weights = weights)
fit_r <- lm_robust(Y ~ Z, data = filter(mturk_long, pid == "republican"),
                   cluster = id, weights = weights)

observed_f <- c(Democrats = unname(summary(fit_d)$fstatistic["value"]),
                Republicans = unname(summary(fit_r)$fstatistic["value"]))

outcomes <- list(
  Democrats = filter(mturk_bandit, pid == "democrat")$Y_sum,
  Republicans = filter(mturk_bandit, pid == "republican")$Y_sum
)

arms <- factor(levels(mturk_bandit$Z), levels = levels(mturk_bandit$Z))
control_index <- c(`position 3 (deposit)` = 3, `control (corrected)` = 1)

# Share of the null distribution at or above the observed F ----
ri_p_value <- function(null_draws, observed) {
  1 - max(which(sort(null_draws[, "F"]) < observed)) / length(null_draws[, "F"])
}

# Each variant is its own seeded sequence, drawing Democrats then Republicans,
# which is the order the deposit uses.
text_study2_f_tests <-
  imap(control_index, function(cn, arm_label) {
    set.seed(seed)
    map(names(outcomes), function(p) {
      tibble(
        party = p,
        augmented_arm = arm_label,
        f_statistic = observed_f[[p]],
        p_value = ri_p_value(
          mysims_RI_clustered(Y = outcomes[[p]], Z = arms, iter = iter, control = cn),
          observed_f[[p]]
        ),
        iterations = iter
      )
    }) |>
      list_rbind()
  }) |>
  list_rbind()

write_csv(text_study2_f_tests, file.path(out_dir, "text_study2_f_tests.csv"))

print(text_study2_f_tests, width = 200)
