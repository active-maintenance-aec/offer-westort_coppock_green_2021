# offer-westort_coppock_green_2021/maintained/simulation_statistics.R
# Output: output/simulation_statistics.csv
# Depends on: helpers.R, original/simulations_supplement1.rds,
#             original/simulations_supplement2.rds
# Description: Summarise the deposited iterated simulations into one tidy table,
#   one row per configuration. Table 1 and every table and figure in appendix D
#   are slices of this table, so they cannot disagree with each other.
#
#   The deposit ships these simulations as saved objects rather than re-running
#   them, and the README says they need significant computational resources. That
#   is a claim worth a measurement rather than a repetition: one configuration at
#   1,000 iterations takes about 56 seconds here, so the 10,000 iterations behind
#   a single row take about nine minutes, and the 132 configurations behind these
#   two files take roughly twenty hours. The claim holds, and re-running is the
#   one thing in this reproduction that a reader cannot do over a coffee.

source(here::here("maintained", "helpers.R"))

supplement1 <- read_rds(file.path(data_dir, "simulations_supplement1.rds"))
supplement2 <- read_rds(file.path(data_dir, "simulations_supplement2.rds"))

# Labels carry the configuration ----
# supplement 1: out<case>[_te][_<batches>|_b_<first batch>], plus out<case>_stat.
# A single-batch design is a static design, which is why the appendix reports the
# static runs in the "1 batch" row.
parse_supplement1 <- function(simulation) {
  tibble(simulation = simulation) |>
    mutate(
      case = as.integer(str_sub(simulation, 4, 4)),
      algorithm = case_when(
        str_detect(simulation, "_stat$") ~ "Static",
        str_detect(simulation, "^out\\d_te") ~ "CA",
        TRUE ~ "TS"
      ),
      first_batch = if_else(
        str_detect(simulation, "_b_\\d+$"),
        as.numeric(str_extract(simulation, "(?<=_b_)\\d+$")),
        100
      ),
      batches = case_when(
        str_detect(simulation, "_stat$") ~ 1,
        str_detect(simulation, "_b_\\d+$") ~ 10,
        str_detect(simulation, "_\\d+$") ~ as.numeric(str_extract(simulation, "\\d+$")),
        TRUE ~ 10
      ),
      best_arm_value = c(0.20, 0.11, 0.20)[case]
    )
}

# supplement 2: out[_stat|_te]_<value in hundredths>, always case 1's structure
# with the best arm's success rate varied.
parse_supplement2 <- function(simulation) {
  tibble(simulation = simulation) |>
    mutate(
      case = NA_integer_,
      algorithm = case_when(
        str_detect(simulation, "^out_stat_") ~ "Static",
        str_detect(simulation, "^out_te_") ~ "CA",
        TRUE ~ "TS"
      ),
      first_batch = 100,
      batches = 10,
      best_arm_value = as.numeric(str_extract(simulation, "\\d+$")) / 100
    )
}

summarise_runs <- function(runs) {
  runs |>
    group_by(simulation) |>
    summarize(
      iterations = n(),
      correct = mean(correct),
      rmse_best = mean(rmse_best),
      rmse_te = mean(rmse_te),
      cov_best = mean(cov_best),
      cov_te = mean(cov_te),
      ses = mean(ses),
      .groups = "drop"
    )
}

stats1 <- summarise_runs(supplement1) |> left_join(
  parse_supplement1(unique(supplement1$simulation)), by = "simulation"
)

stats2 <- summarise_runs(supplement2) |> left_join(
  parse_supplement2(unique(supplement2$simulation)), by = "simulation"
)

simulation_statistics <-
  bind_rows(supplement_1 = stats1, supplement_2 = stats2, .id = "source") |>
  select(source, simulation, algorithm, case, best_arm_value, batches, first_batch,
         iterations, correct, rmse_best, rmse_te, cov_best, cov_te, ses)

write_csv(simulation_statistics, file.path(out_dir, "simulation_statistics.csv"))

print(simulation_statistics, n = 12, width = 200)
print(str_glue("{nrow(simulation_statistics)} configurations, ",
               "{sum(simulation_statistics$iterations)} simulated experiments."))
