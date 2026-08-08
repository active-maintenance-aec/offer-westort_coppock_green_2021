# offer-westort_coppock_green_2021/maintained/table_1_simulation_statistics.R
# Output: output/table_1_simulation_statistics.csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Table 1. Arm selection, RMSE, and coverage across 10,000 simulated
#   experiments in each of the three scenarios, under Thompson sampling, a static
#   design, and the control-augmented design.
#
#   The published table leaves the treatment-effect columns blank for Thompson
#   sampling, on the stated grounds that the design has no explicit control
#   condition. The quantities are defined all the same, using one of the inferior
#   arms as the comparison, and the appendix prints them: the "10 batches" rows of
#   Table D.2 are exactly these cells. They are carried here in rmse_te_ts_only and
#   cov_te_ts_only so the table's blanks read as a presentation choice rather than
#   a missing computation.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

table_1 <-
  simulation_statistics |>
  filter(source == "supplement_1", batches %in% c(1, 10), first_batch == 100) |>
  mutate(algorithm = factor(algorithm, levels = c("TS", "Static", "CA"))) |>
  arrange(algorithm, case) |>
  transmute(
    algorithm,
    case,
    correct,
    rmse_best,
    rmse_te_published = if_else(algorithm == "TS", NA_real_, rmse_te),
    cov_best,
    cov_te_published = if_else(algorithm == "TS", NA_real_, cov_te),
    rmse_te_ts_only = if_else(algorithm == "TS", rmse_te, NA_real_),
    cov_te_ts_only = if_else(algorithm == "TS", cov_te, NA_real_)
  )

write_csv(table_1, file.path(out_dir, "table_1_simulation_statistics.csv"))

print(table_1, width = 200)
