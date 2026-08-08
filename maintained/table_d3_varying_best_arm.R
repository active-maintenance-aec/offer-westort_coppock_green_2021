# offer-westort_coppock_green_2021/maintained/table_d3_varying_best_arm.R
# Output: output/table_d3_varying_best_arm.csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Table D.3. The same statistics with the best arm's success
#   rate varied from 0.10 to 0.20 while the other eight arms stay at 0.10.
#   Supplement 2 covers 0.10 through 0.19 with 0.11 omitted; the 0.11 and 0.20
#   rows are the scenario 2 and scenario 1 runs from supplement 1, which are the
#   same configurations.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

table_d3 <-
  simulation_statistics |>
  filter(
    source == "supplement_2" |
      (source == "supplement_1" & case %in% c(1, 2) & batches %in% c(1, 10) &
         first_batch == 100)
  ) |>
  mutate(algorithm = factor(algorithm, levels = c("TS", "Static", "CA"))) |>
  arrange(algorithm, best_arm_value) |>
  select(algorithm, best_arm_value, correct, rmse_best, rmse_te, cov_best, cov_te)

write_csv(table_d3, file.path(out_dir, "table_d3_varying_best_arm.csv"))

print(table_d3, n = nrow(table_d3), width = 200)
