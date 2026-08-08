# offer-westort_coppock_green_2021/maintained/text_study1_static_simulations.R
# Output: output/text_study1_static_simulations.csv
# Depends on: helpers.R, original/study_1_sims.rds
# Description: How study 1 would have fared under a static design. The deposit
#   ships the 10,000-iteration simulations as study_1_sims.rds, treating the
#   observed success rates as the truth; the four configurations behind that file
#   take roughly forty minutes to regenerate, so it is read rather than re-run.
#   The published sentences compare the share of runs picking the best proposal
#   and the ratio of average standard errors.

source(here::here("maintained", "helpers.R"))

study_1_sims <- read_rds(file.path(data_dir, "study_1_sims.rds"))

by_design <-
  study_1_sims |>
  group_by(simulation) |>
  summarize(iterations = n(),
            correct = mean(correct),
            rmse_best = mean(rmse_best),
            ses = mean(ses),
            .groups = "drop") |>
  mutate(
    topic = if_else(str_detect(simulation, "rtw"), "Right-to-work", "Minimum wage"),
    design = if_else(str_detect(simulation, "_stat$"), "Static", "Adaptive")
  )

text_study1_static_simulations <-
  by_design |>
  select(topic, design, iterations, correct, rmse_best, ses) |>
  pivot_wider(names_from = design, values_from = c(iterations, correct, rmse_best, ses)) |>
  transmute(
    topic,
    iterations = iterations_Adaptive,
    correct_adaptive = correct_Adaptive,
    correct_static = correct_Static,
    se_adaptive = ses_Adaptive,
    se_static = ses_Static,
    se_ratio_static_over_adaptive = ses_Static / ses_Adaptive,
    se_ratio_adaptive_over_static = ses_Adaptive / ses_Static
  )

write_csv(text_study1_static_simulations,
          file.path(out_dir, "text_study1_static_simulations.csv"))

print(text_study1_static_simulations, width = 200)
