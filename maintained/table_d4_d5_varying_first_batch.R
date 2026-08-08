# offer-westort_coppock_green_2021/maintained/table_d4_d5_varying_first_batch.R
# Output: output/table_d4_d5_varying_first_batch.csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Tables D.4 and D.5. The same statistics with the size of
#   the first batch varied from 100 to 910, total sample held at 1,000. The two
#   published tables are one table split by algorithm, so they are written as one
#   file with an algorithm column: TS is Table D.4, CA is Table D.5.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

table_d4_d5 <-
  simulation_statistics |>
  filter(source == "supplement_1", algorithm != "Static", batches == 10) |>
  mutate(
    algorithm = factor(algorithm, levels = c("TS", "CA")),
    published_table = if_else(algorithm == "TS", "D.4", "D.5")
  ) |>
  arrange(algorithm, case, first_batch) |>
  select(published_table, algorithm, case, first_batch,
         correct, rmse_best, rmse_te, cov_best, cov_te)

write_csv(table_d4_d5, file.path(out_dir, "table_d4_d5_varying_first_batch.csv"))

print(table_d4_d5, n = nrow(table_d4_d5), width = 200)
