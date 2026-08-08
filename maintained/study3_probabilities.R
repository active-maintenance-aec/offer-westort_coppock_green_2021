# offer-westort_coppock_green_2021/maintained/study3_probabilities.R
# Output: output/study_3_probabilities.csv
# Depends on: helpers.R, original/study_3_clean.rds, original/study_3_stat_clean.rds
# Description: Study 3 is a conjoint trial with 192 possible profiles, far more
#   than the sample can distinguish on its own, so the posterior probability that
#   each profile is best comes from an additive probit model rather than from arm
#   counts. This script fits that model after each batch, for the adaptive arm and
#   for the parallel static arm, and writes the 192 posterior probabilities per
#   batch that Figures G.9 and G.10 both draw on.
#
#   The deposit assigns each batch's 192 probabilities through as_tibble_row()
#   with a name-repair function, which current tibble rejects outright: the repair
#   function returns 192 names for a one-column input. Writing the vector into the
#   row directly does the same thing and does not depend on the name repair.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

MCMCprobit <- MCMCpack::MCMCprobit

bandit_factor <- read_rds(file.path(data_dir, "study_3_clean.rds"))
bandit_factor_stat <- read_rds(file.path(data_dir, "study_3_stat_clean.rds"))

outcome_model <- formula(Y_binary ~ Z_1 + Z_2 + Z_3 + Z_4)

# expand_grid varies its last argument fastest and expand.grid its first, so the
# arguments are given in reverse to put the profiles in the deposit's order, then
# the columns are put back.
profiles <-
  expand_grid(
    Z_4 = factor(unique(bandit_factor$Z_4),
                 labels = c("Disclose > $200", "Disclose > $500", "Eliminate", "Disclose all")),
    Z_3 = factor(unique(bandit_factor$Z_3),
                 labels = c("Prohibit all", "1:1 match", "5:1 match", "Maintain status quo")),
    Z_2 = factor(unique(bandit_factor$Z_2),
                 labels = c("Allow all", "Prohibit candidate;\n allow PAC", "Prohibit all")),
    Z_1 = factor(unique(bandit_factor$Z_1),
                 labels = c("No limits", "$10,000", "100,000", "$1,000,000"))
  ) |>
  select(Z_1, Z_2, Z_3, Z_4) |>
  mutate(Y_binary = 1, profile = row_number())

profile_matrix <- model.matrix(outcome_model, data = profiles)

posterior_after_batch <- function(d, batch_number) {
  batches_so_far <- unique(d$batch)[1:batch_number]
  fit <- MCMCprobit(outcome_model, mcmc = 1e5, data = filter(d, batch %in% batches_so_far))
  tibble(
    batch = batch_number,
    profile = profiles$profile,
    # bandit::prob_winner returns a one-dimensional table rather than a numeric
    # vector, which a tibble column keeps and bind_rows then refuses to combine
    # with the batch 0 prior below. The deposit passes the same value through
    # matrix(), which drops the class; as.numeric does the same thing here.
    posterior_prob = as.numeric(prob_winner(fit %*% t(profile_matrix)))
  )
}

study_3_probabilities <-
  bind_rows(
    Adaptive = map(1:10, function(b) posterior_after_batch(bandit_factor, b)) |> list_rbind(),
    Static = map(1:10, function(b) posterior_after_batch(bandit_factor_stat, b)) |> list_rbind(),
    .id = "design"
  ) |>
  bind_rows(
    expand_grid(design = c("Adaptive", "Static"), batch = 0, profile = profiles$profile) |>
      mutate(posterior_prob = 1 / nrow(profiles))
  ) |>
  left_join(select(profiles, profile, Z_1, Z_2, Z_3, Z_4), by = "profile") |>
  arrange(design, profile, batch)

write_csv(study_3_probabilities, file.path(out_dir, "study_3_probabilities.csv"))

print(
  study_3_probabilities |>
    filter(batch == 10) |>
    group_by(design) |>
    slice_max(posterior_prob, n = 3) |>
    select(design, posterior_prob, Z_1, Z_2, Z_3, Z_4),
  width = 200
)
