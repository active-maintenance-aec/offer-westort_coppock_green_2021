# offer-westort_coppock_green_2021/maintained/text_study2_control_responses.R
# Output: output/text_study2_control_responses.csv, output/text_study2_counts.csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: The two sets of in-text numbers for study 2 that are not on a
#   figure: the share of control-condition respondents giving the correct answer
#   to each question, by party, and the sample sizes the Republican results
#   paragraph cites.

source(here::here("maintained", "helpers.R"))

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

# Correct answers under the control condition ----
# Restricted to respondents who answered every question, which is what the
# deposit does; the denominator is the number of valid responses per question.
text_study2_control_responses <-
  mturk_bandit |>
  filter(Z == "control", Y_no_response_sum == 0) |>
  group_by(pid) |>
  summarize(
    deficit = sum(Y_deficit) / sum(Y_deficit_no_response == 0),
    unemp = sum(Y_unemp) / sum(Y_unemp_no_response == 0),
    nfi = sum(Y_nfi) / sum(Y_nfi_no_response == 0),
    .groups = "drop"
  )

write_csv(text_study2_control_responses,
          file.path(out_dir, "text_study2_control_responses.csv"))

# Sample sizes ----
text_study2_counts <-
  mturk_bandit |>
  count(pid, Z, name = "n") |>
  group_by(pid) |>
  mutate(n_total = sum(n)) |>
  ungroup()

write_csv(text_study2_counts, file.path(out_dir, "text_study2_counts.csv"))

print(text_study2_control_responses)
print(text_study2_counts, n = nrow(text_study2_counts))
