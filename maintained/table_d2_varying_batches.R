# offer-westort_coppock_green_2021/maintained/table_d2_varying_batches.R
# Output: output/table_d2_varying_batches.csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Table D.2. The same statistics as Table 1, with the
#   number of batches varied from 1 to 100 while total sample is held at 1,000.
#   A one-batch adaptive design is a static design, so the static runs supply the
#   first row of each block.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

table_d2 <-
  simulation_statistics |>
  filter(source == "supplement_1", first_batch == 100) |>
  mutate(
    algorithm = if_else(algorithm == "Static", "TS", algorithm),
    algorithm = factor(algorithm, levels = c("TS", "CA"))
  ) |>
  arrange(algorithm, case, batches) |>
  select(algorithm, case, batches, correct, rmse_best, rmse_te, cov_best, cov_te)

# The control-augmented design has no one-batch run: with a single batch there is
# nothing for the control arm to catch up to.
table_d2 <- filter(table_d2, !(algorithm == "CA" & batches == 1))

write_csv(table_d2, file.path(out_dir, "table_d2_varying_batches.csv"))

print(table_d2, n = nrow(table_d2), width = 200)
