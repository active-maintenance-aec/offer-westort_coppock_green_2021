# offer-westort_coppock_green_2021/maintained/text_study2_batched_ols.R
# Output: output/text_study2_batched_ols.csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: The batched OLS test of the Lottery effect reported in footnote 15.
#   Each batch is estimated on its own, the batch-level t statistics are averaged,
#   and the p-value comes from a simulated null built by summing draws from the
#   per-batch t distributions. Runtime is about ten seconds at 1e7 draws.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

iter <- 1e7

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

mturk_long <-
  mturk_bandit |>
  pivot_longer(
    cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
    names_to = "out",
    values_to = "Y",
    values_transform = list(Y = as.double)
  )

# One regression per party per batch ----
by_batch <-
  expand_grid(party = c("democrat", "republican"), batch_number = 1:10) |>
  mutate(
    fit = map2(party, batch_number, function(p, b) {
      lm_robust(Y ~ Z, data = filter(mturk_long, pid == p, batch == b), cluster = id)
    }),
    estimate = map_dbl(fit, function(f) f$coefficients[["Zlottery"]]),
    std.error = map_dbl(fit, function(f) f$std.error[["Zlottery"]]),
    df = map_dbl(fit, function(f) f$df[["Zlottery"]])
  ) |>
  select(-fit)

t_statistics <-
  by_batch |>
  group_by(party) |>
  summarize(T_stat = sum(estimate / std.error) / sqrt(n()), .groups = "drop")

# Simulated null: the sum of ten t draws with the observed degrees of freedom ----
null_draw <- function(party) {
  degrees <- by_batch$df[by_batch$party == party]
  rowSums(sapply(degrees, function(x) rt(iter, x))) / sqrt(length(degrees))
}

null_d <- null_draw("democrat")
null_r <- null_draw("republican")

text_study2_batched_ols <- tibble(
  party = c("Democrats", "Republicans"),
  T_statistic = c(t_statistics$T_stat[t_statistics$party == "democrat"],
                  t_statistics$T_stat[t_statistics$party == "republican"]),
  p_value = c(
    (1 - sum(null_d < abs(t_statistics$T_stat[t_statistics$party == "democrat"])) / iter) * 2,
    (1 - sum(null_r > -abs(t_statistics$T_stat[t_statistics$party == "republican"])) / iter) * 2
  ),
  draws = iter
)

write_csv(text_study2_batched_ols, file.path(out_dir, "text_study2_batched_ols.csv"))

print(text_study2_batched_ols)
print(by_batch, n = nrow(by_batch))
