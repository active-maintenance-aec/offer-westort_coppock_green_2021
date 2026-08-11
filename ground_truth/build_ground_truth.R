# offer-westort_coppock_green_2021/ground_truth/build_ground_truth.R
# Output: ground_truth/offer-westort_coppock_green_2021_ground_truth.csv,
#         ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/published_appendix_values.csv,
#             ground_truth/archive_values.csv, maintained/output/, maintained/in_text_claims.R
# Description: Lay every published number against what the maintained pipeline
#   produces and, where the deposit's own console output reaches it, against what
#   the deposited code produces. Nothing here is typed except the published values,
#   which come from published_claims.csv and published_appendix_values.csv and are
#   read from the article and the supplement rather than from any run.
#
#   The last section is the coverage gate. It runs in_text_claims.R as a program in
#   its own environment, counts the claims it prints, and checks them against the
#   extraction in both directions and value by value.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

out_dir <- here::here("maintained", "output")
out <- function(file) read_csv(file.path(out_dir, file), show_col_types = FALSE)

published_claims <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
published_cells <- read_csv(
  here::here("ground_truth", "published_appendix_values.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)
archive_values <- read_csv(here::here("ground_truth", "archive_values.csv"),
                           show_col_types = FALSE)

# Comparison machinery ----
# The article's own precision is the unit of comparison, so both sides are
# rendered to the digit count the extraction records and the strings compared.
# The epsilon keeps a value sitting exactly on a rounding boundary from being
# rejected by floating-point noise alone.
epsilon <- 1e-9

# An unseeded published value is compared against the dispersion its own procedure
# generates rather than against one draw. Five seeds of the deposit's own static
# bootstrap move the Figure F.8 estimates by up to three units in the last
# published digit, which is the tolerance below.
unseeded_tolerance <- 3

render <- function(x, digits) {
  rendered <- sprintf(paste0("%.", digits, "f"), x)
  # A linear solve or a difference of two nearly equal numbers can return a
  # negative zero, which prints with a minus sign and fails string equality
  # against a published 0.
  if_else(rendered == str_c("-", sprintf(paste0("%.", digits, "f"), 0)),
          sprintf(paste0("%.", digits, "f"), 0), rendered)
}

normalise_paper <- function(x) {
  x |>
    str_replace_all("−", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10.")
}

# A value agrees when the rewrite, printed to the page's precision, gives the
# page's digits. Where the two disagree by less than half a unit in the last
# printed digit they are sitting on a rounding boundary, which two defensible
# rounding rules resolve differently, so the tolerance is applied as well.
agrees <- function(value, value_paper, digits, tolerance = 0.5) {
  paper_number <- as.numeric(normalise_paper(value_paper))
  same_string <- render(value, digits) == normalise_paper(value_paper)
  on_boundary <- abs(value - paper_number) <= tolerance * 10^(-digits) + epsilon
  as.integer(same_string | on_boundary)
}

# One helper for every join, asserting uniqueness on both sides and a one-to-one
# result, so a mistyped key cannot quietly find nothing.
join_one_to_one <- function(x, y, by) {
  stopifnot(!any(duplicated(x[by])), !any(duplicated(y[by])))
  joined <- left_join(x, y, by = by, relationship = "one-to-one")
  stopifnot(nrow(joined) == nrow(x))
  joined
}

# What the rewrite produces ----
simulations <- out("simulations_illustrative.csv")
simulation_statistics <- out("simulation_statistics.csv")
figure_3 <- out("figure_3_study1_posterior_probs.csv")
figure_4 <- out("figure_4_study1_mean_outcomes.csv")
figure_5 <- out("figure_5_study2_posterior_probs.csv")
figure_6 <- out("figure_6_study2_ate_estimates.csv")
figure_f6 <- out("figure_f6_response_rates.csv")
figure_f7 <- out("figure_f7_independents.csv")
figure_f8 <- out("figure_f8_simulated_comparisons.csv")
study_3 <- out("study_3_probabilities.csv")
table_f7 <- out("table_f7_downstream.csv")
text_s1_f <- out("text_study1_f_tests.csv")
text_s1_static <- out("text_study1_static_simulations.csv")
text_s2_control <- out("text_study2_control_responses.csv")
text_s2_counts <- out("text_study2_counts.csv")
text_s2_f <- out("text_study2_f_tests.csv")
text_s2_bols <- out("text_study2_batched_ols.csv")

posterior <- function(which_case, which_design, which_arm) {
  simulations |>
    filter(quantity == "posterior_prob", batch == 10,
           str_starts(case, which_case), design == which_design, arm == which_arm) |>
    pull(value)
}

s1_posterior <- function(which_topic, rank) {
  figure_3 |>
    filter(batch == 10, topic == which_topic) |>
    slice_max(posterior_prob, n = rank) |>
    slice_min(posterior_prob, n = 1)
}

s1_arm_row <- function(which_topic, which_arm) {
  filter(figure_4, topic == which_topic, arm == which_arm)
}

s2_arm <- function(which_type, which_spec, which_arm, column) {
  figure_6 |>
    filter(type == which_type, Estimates == which_spec, term == which_arm) |>
    pull({{ column }})
}

top_profiles <- function(which_design) {
  study_3 |>
    filter(batch == 10, design == which_design) |>
    arrange(desc(posterior_prob))
}

# The counts the RI simulations use are one row per respondent per question, so
# the study 2 arm sizes are respondent counts rather than observation counts.
s2_count <- function(which_pid, which_z) {
  text_s2_counts |> filter(pid == which_pid, Z == which_z) |> pull(n)
}

f6_value <- function(which_type, which_term, column) {
  figure_f6 |> filter(type == which_type, term == which_term) |> pull({{ column }})
}

prose_values <- tribble(
  ~claim_id, ~value_rewrite,
  "sim_obs_per_period", simulations |> filter(quantity == "cumulative_n", batch == 1, design == "Static", str_starts(case, "Case 1")) |> pull(value) |> sum(),
  "sim_periods", max(simulations$batch),
  "sim_arms", n_distinct(simulations$arm),
  "sim_iterations", simulation_statistics |> filter(source == "supplement_1", batches == 10) |> pull(iterations) |> unique(),
  "ci_level", {
    row <- s1_arm_row("Minimum wage", "Proposal 3 (Y)")
    2 * pt((row$conf.high - row$estimate) / row$std.error, row$df) - 1
  },
  "fig1_case1_ts_best", posterior("Case 1", "Thompson sampling", "best"),
  "fig1_case1_static_best", posterior("Case 1", "Static", "best"),
  "fig1_case2_ts_best", posterior("Case 2", "Thompson sampling", "best"),
  "fig1_case2_ts_inferior", simulations |> filter(quantity == "posterior_prob", batch == 10, str_starts(case, "Case 2"), design == "Thompson sampling", arm != "best") |> pull(value) |> max(),
  "fig1_case2_static_best", posterior("Case 2", "Static", "best"),
  "fig1_case2_static_inferior", simulations |> filter(quantity == "posterior_prob", batch == 10, str_starts(case, "Case 2"), design == "Static", arm != "best") |> pull(value) |> max(),
  "fig1_case3_ts_best", posterior("Case 3", "Thompson sampling", "best"),
  "fig1_case3_ts_second", posterior("Case 3", "Thompson sampling", "alt1"),
  "fig1_case3_static_second", posterior("Case 3", "Static", "alt1"),
  "fig1_case3_static_best", posterior("Case 3", "Static", "best"),
  "s1_n", figure_3 |> filter(topic == "Minimum wage", batch == 10) |> pull(cumulative_trials) |> sum(),
  "s1_mw_arms", figure_3 |> filter(topic == "Minimum wage") |> pull(arm) |> n_distinct(),
  "s1_rtw_arms", figure_3 |> filter(topic == "Right-to-work") |> pull(arm) |> n_distinct(),
  "s1_mw_top_posterior", s1_posterior("Minimum wage", 1)$posterior_prob,
  "s1_mw_top_mean", s1_arm_row("Minimum wage", s1_posterior("Minimum wage", 1)$arm)$estimate,
  "s1_mw_top_n", s1_posterior("Minimum wage", 1)$cumulative_trials,
  "s1_mw_f_p", text_s1_f |> filter(topic == "Minimum wage") |> pull(p_value),
  "s1_rtw_top_posterior", s1_posterior("Right-to-work", 1)$posterior_prob,
  "s1_rtw_second_posterior", s1_posterior("Right-to-work", 2)$posterior_prob,
  "s1_rtw_top_mean", s1_arm_row("Right-to-work", s1_posterior("Right-to-work", 1)$arm)$estimate,
  "s1_rtw_second_mean", s1_arm_row("Right-to-work", s1_posterior("Right-to-work", 2)$arm)$estimate,
  "s1_rtw_top_n", s1_posterior("Right-to-work", 1)$cumulative_trials,
  "s1_rtw_second_n", s1_posterior("Right-to-work", 2)$cumulative_trials,
  "s1_rtw_f_p", text_s1_f |> filter(topic == "Right-to-work") |> pull(p_value),
  "s1_static_mw_per_arm", sum(filter(figure_3, topic == "Minimum wage", batch == 10)$cumulative_trials) / n_distinct(filter(figure_3, topic == "Minimum wage")$arm),
  "s1_static_rtw_per_arm", sum(filter(figure_3, topic == "Right-to-work", batch == 10)$cumulative_trials) / n_distinct(filter(figure_3, topic == "Right-to-work")$arm),
  "s1_mw_correct_adaptive", 100 * text_s1_static$correct_adaptive[text_s1_static$topic == "Minimum wage"],
  "s1_mw_correct_static", 100 * text_s1_static$correct_static[text_s1_static$topic == "Minimum wage"],
  "s1_rtw_correct_adaptive", 100 * text_s1_static$correct_adaptive[text_s1_static$topic == "Right-to-work"],
  "s1_rtw_correct_static", 100 * text_s1_static$correct_static[text_s1_static$topic == "Right-to-work"],
  "s1_mw_se_ratio", 100 * text_s1_static$se_ratio_static_over_adaptive[text_s1_static$topic == "Minimum wage"],
  "s1_rtw_se_ratio", 100 * text_s1_static$se_ratio_static_over_adaptive[text_s1_static$topic == "Right-to-work"],
  "s1_rtw_se_ratio_inverse", 100 * text_s1_static$se_ratio_adaptive_over_static[text_s1_static$topic == "Right-to-work"],
  "s2_arms", n_distinct(figure_5$arm),
  "s2_treatment_arms", n_distinct(figure_5$arm) - 1,
  "s2_obs_per_day", sum(text_s2_counts$n) / max(figure_5$batch),
  "s2_days", max(figure_5$batch),
  "s2_control_deficit_rep", 100 * text_s2_control$deficit[text_s2_control$pid == "republican"],
  "s2_control_deficit_dem", 100 * text_s2_control$deficit[text_s2_control$pid == "democrat"],
  "s2_control_unemp_rep", 100 * text_s2_control$unemp[text_s2_control$pid == "republican"],
  "s2_control_unemp_dem", 100 * text_s2_control$unemp[text_s2_control$pid == "democrat"],
  "s2_control_nfi_rep", 100 * text_s2_control$nfi[text_s2_control$pid == "republican"],
  "s2_control_nfi_dem", 100 * text_s2_control$nfi[text_s2_control$pid == "democrat"],
  "s2_independents_n", text_s2_counts |> filter(pid == "independent") |> pull(n_total) |> unique(),
  "s2_rep_lottery_ate", 100 * s2_arm("Republicans", "IPW", "Lottery", estimate),
  "s2_rep_lottery_se", 100 * s2_arm("Republicans", "IPW", "Lottery", std.error),
  "s2_rep_lottery_n", s2_count("republican", "lottery"),
  "s2_rep_total_n", text_s2_counts |> filter(pid == "republican") |> pull(n_total) |> unique(),
  "s2_rep_control_n", s2_count("republican", "control"),
  "s2_rep_static_se", 100 * figure_f8$std.error[figure_f8$Estimates == "Static" & figure_f8$type == "Republicans" & figure_f8$term == "Lottery"],
  "s2_rep_lottery_p", s2_arm("Republicans", "IPW", "Lottery", p.value),
  "s2_rep_static_p", figure_f8$p.value[figure_f8$Estimates == "Static" & figure_f8$type == "Republicans" & figure_f8$term == "Lottery"],
  "s2_rep_ri_p", text_s2_f |> filter(party == "Republicans", augmented_arm == "position 3 (deposit)") |> pull(p_value),
  "s2_dem_lottery_ate", 100 * s2_arm("Democrats", "IPW", "Lottery", estimate),
  "s2_dem_lottery_p", s2_arm("Democrats", "IPW", "Lottery", p.value),
  "s2_dem_lottery_ate_adj", 100 * s2_arm("Democrats", "IPW-Adjusted", "Lottery", estimate),
  "s2_dem_lottery_p_adj", s2_arm("Democrats", "IPW-Adjusted", "Lottery", p.value),
  "s2_dem_ri_p", text_s2_f |> filter(party == "Democrats", augmented_arm == "position 3 (deposit)") |> pull(p_value),
  "s2_bols_p_rep", text_s2_bols$p_value[text_s2_bols$party == "Republicans"],
  "s2_bols_p_dem", text_s2_bols$p_value[text_s2_bols$party == "Democrats"],
  "appd1_total_obs", 100 * max(simulations$batch),
  "table_1_cells", sum(published_cells$table_figure == "table_1"),
  "table_d2_cells", sum(published_cells$table_figure == "table_d2"),
  "table_d3_cells", sum(published_cells$table_figure == "table_d3"),
  "table_d4_cells", sum(published_cells$table_figure == "table_d4"),
  "table_d5_cells", sum(published_cells$table_figure == "table_d5"),
  "appf1_control_mean_dem", f6_value("Democrats", "Control mean", estimate),
  "appf1_control_mean_rep", f6_value("Republicans", "Control mean", estimate),
  "appf1_google_drop", -100 * mean(c(f6_value("Democrats", "Google", estimate), f6_value("Republicans", "Google", estimate))),
  "appf1_extratime_drop_dem", -100 * f6_value("Democrats", "Extra Time", estimate),
  "appf1_extratime_drop_rep", -100 * f6_value("Republicans", "Extra Time", estimate),
  "table_f7_cells", 2 * nrow(table_f7) + 2 * n_distinct(str_c(table_f7$outcome, table_f7$party, table_f7$adjusted)),
  "appf8_bootstraps", unique(figure_f8$replicates[figure_f8$Estimates == "Static"]),
  "appg_profiles", n_distinct(study_3$profile),
  "appg_levels_z1", n_distinct(study_3$Z_1),
  "appg_levels_z2", n_distinct(study_3$Z_2),
  "appg_levels_z3", n_distinct(study_3$Z_3),
  "appg_levels_z4", n_distinct(study_3$Z_4),
  "appg_waves", max(study_3$batch),
  "appg_adaptive_p1", top_profiles("Adaptive")$posterior_prob[1],
  "appg_adaptive_p2", top_profiles("Adaptive")$posterior_prob[2],
  "appg_adaptive_p3", top_profiles("Adaptive")$posterior_prob[3],
  "appg_static_p1", top_profiles("Static")$posterior_prob[1],
  "appg_static_p2", top_profiles("Static")$posterior_prob[2],
  "appg_static_p3", top_profiles("Static")$posterior_prob[3]
)

# Descriptive claims carry a truth value rather than a number ----
adaptive_top <- top_profiles("Adaptive")[1, ]
static_top <- top_profiles("Static")[1, ]
selection_by_batches <- simulation_statistics |>
  filter(source == "supplement_1", first_batch == 100, case == 1) |>
  mutate(algorithm = if_else(algorithm == "Static", "TS", algorithm))

republican_ipw <- figure_6 |> filter(type == "Republicans", Estimates == "IPW")
four_point_arm <- republican_ipw$term[which.min(abs(100 * republican_ipw$estimate - 4.1))]

gain_at_five <- with(selection_by_batches,
                     correct[batches == 5 & algorithm == "TS"] - correct[batches == 1 & algorithm == "TS"])
gain_overall <- with(selection_by_batches,
                     correct[batches == 100 & algorithm == "TS"] - correct[batches == 1 & algorithm == "TS"])

descriptive_values <- tibble(
  claim_id = c("s1_mw_arms_below_08", "s2_rep_4pt_arm", "appd1_gains_by_five",
               "appg_adaptive_profile", "appg_static_profile"),
  value_rewrite = c(
    sum(figure_4$estimate[figure_4$topic == "Minimum wage"] < 0.8),
    as.numeric(four_point_arm == "Accuracy"),
    as.numeric(gain_at_five / gain_overall > 0.5),
    as.numeric(adaptive_top$Z_1 == "$10,000" & adaptive_top$Z_2 == "Prohibit all" &
                 adaptive_top$Z_3 == "Prohibit all" & adaptive_top$Z_4 == "Disclose all"),
    as.numeric(static_top$Z_1 == "$10,000" & static_top$Z_2 == "Prohibit all" &
                 static_top$Z_3 == "1:1 match" & static_top$Z_4 == "Disclose > $500")
  )
)

# Float faces ----
slug <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "_")

float_values <- bind_rows(
  figure_4 |>
    transmute(claim_id = str_c("figure_4_", slug(str_c(topic, "_", arm))),
              estimate, se = std.error),
  figure_6 |>
    transmute(claim_id = str_c("figure_6_", slug(str_c(type, "_", Estimates, "_", term))),
              estimate, se = std.error),
  figure_f6 |>
    filter(term != "Control mean") |>
    transmute(claim_id = str_c("figure_f6_", slug(str_c(type, "_", term))),
              estimate, se = std.error),
  figure_f7 |>
    transmute(claim_id = str_c("figure_f7_", slug(str_c(Estimates, "_", term))),
              estimate, se = std.error),
  figure_f8 |>
    transmute(claim_id = str_c("figure_f8_", slug(str_c(type, "_", Estimates, "_", term))),
              estimate, se = std.error)
) |>
  pivot_longer(c(estimate, se), names_to = "statistic", values_to = "value_rewrite") |>
  transmute(claim_id = str_c(claim_id, "_", statistic), value_rewrite)

value_rewrite <- bind_rows(prose_values, descriptive_values, float_values)
stopifnot(!any(duplicated(value_rewrite$claim_id)))

# A published value with no rewrite counterpart stops the build ----
# A mistyped label would otherwise become value_rewrite = NA and land in the
# unverifiable bucket beside a quantity the pipeline genuinely cannot reach.
expected <- published_claims |> filter(needs_block)
missing <- setdiff(expected$claim_id, value_rewrite$claim_id)
if (length(missing) > 0) {
  stop("No rewrite counterpart for: ", str_c(missing, collapse = ", "))
}
stray <- setdiff(value_rewrite$claim_id, published_claims$claim_id)
if (length(stray) > 0) {
  stop("Rewrite values with no published claim: ", str_c(stray, collapse = ", "))
}

# The published table cells, one row each ----
simulation_cells <-
  bind_rows(
    simulation_statistics |>
      filter(source == "supplement_1", batches %in% c(1, 10), first_batch == 100) |>
      mutate(algorithm = if_else(batches == 1, "Static", algorithm), table_figure = "table_1") |>
      select(table_figure, algorithm, case, correct, rmse_best, rmse_te, cov_best, cov_te),
    simulation_statistics |>
      filter(source == "supplement_1", first_batch == 100) |>
      mutate(algorithm = if_else(algorithm == "Static", "TS", algorithm), table_figure = "table_d2") |>
      select(table_figure, algorithm, case, batches, correct, rmse_best, rmse_te, cov_best, cov_te),
    simulation_statistics |>
      filter(source == "supplement_2" |
               (source == "supplement_1" & case %in% c(1, 2) & batches %in% c(1, 10) & first_batch == 100)) |>
      mutate(table_figure = "table_d3") |>
      select(table_figure, algorithm, best_arm_value, correct, rmse_best, rmse_te, cov_best, cov_te),
    simulation_statistics |>
      filter(source == "supplement_1", algorithm == "TS", batches == 10) |>
      mutate(table_figure = "table_d4") |>
      select(table_figure, algorithm, case, first_batch, correct, rmse_best, rmse_te, cov_best, cov_te),
    simulation_statistics |>
      filter(source == "supplement_1", algorithm == "CA", batches == 10) |>
      mutate(table_figure = "table_d5") |>
      select(table_figure, algorithm, case, first_batch, correct, rmse_best, rmse_te, cov_best, cov_te)
  ) |>
  pivot_longer(c(correct, rmse_best, rmse_te, cov_best, cov_te),
               names_to = "statistic", values_to = "value_rewrite") |>
  mutate(best_arm_value = if_else(table_figure == "table_d3",
                                  round(if_else(is.na(best_arm_value), c(0.20, 0.11, 0.20)[case], best_arm_value), 2),
                                  NA_real_))

cell_keys <- c("table_figure", "algorithm", "case", "batches", "best_arm_value",
               "first_batch", "statistic")

table_cells <-
  published_cells |>
  mutate(best_arm_value = as.numeric(best_arm_value)) |>
  join_one_to_one(
    simulation_cells |> select(all_of(cell_keys), value_rewrite),
    by = cell_keys
  ) |>
  mutate(
    claim_id = str_c(table_figure, "_cell_",
                     str_pad(ave(row_number(), table_figure, FUN = seq_along), 3, pad = "0")),
    claim = str_c(str_to_upper(str_replace(table_figure, "_", " ")), ", ",
                  algorithm, ", ", statistic,
                  coalesce(str_c(", case ", case), ""),
                  coalesce(str_c(", ", batches, " batches"), ""),
                  coalesce(str_c(", best arm ", best_arm_value), ""),
                  coalesce(str_c(", first batch ", first_batch), "")),
    digits = 3L
  )
stopifnot(!any(is.na(table_cells$value_rewrite)))

# What the deposited code produces ----
# Six rows of each simulation table, the study 1 arm counts and the four
# randomization inference p-values are everything the deposit prints.
archive_table_cells <-
  archive_values |>
  filter(table_figure %in% c("table_1", "table_d2", "table_d3", "table_d4")) |>
  mutate(
    case = if_else(table_figure == "table_d3", NA_real_, case),
    best_arm_value = if_else(table_figure == "table_d3",
                             best_arm_value_hundredths / 100, NA_real_),
    batches = if_else(table_figure == "table_d2", batches, NA_real_),
    first_batch = if_else(table_figure == "table_d4", first_batch, NA_real_)
  ) |>
  select(all_of(cell_keys), value_script)

table_cells <- table_cells |>
  left_join(archive_table_cells, by = cell_keys, relationship = "one-to-one")

# Assemble the ground truth ----
claims_rows <-
  published_claims |>
  left_join(value_rewrite, by = "claim_id", relationship = "one-to-one") |>
  mutate(
    table_figure = case_when(
      str_starts(claim_id, "fig1_") ~ "figure_1",
      str_starts(claim_id, "figure_") ~ str_extract(claim_id, "^figure_[a-z0-9]+"),
      str_starts(claim_id, "table_") ~ str_extract(claim_id, "^table_[a-z0-9]+"),
      TRUE ~ "text"
    ),
    claim = quantity
  )

archive_prose <- tribble(
  ~claim_id, ~value_script,
  "s1_mw_f_p", archive_values |> filter(table_figure == "text_study1_f_tests", study == "mw", statistic == "p_value") |> pull(value_script),
  "s1_rtw_f_p", archive_values |> filter(table_figure == "text_study1_f_tests", study == "rtw", statistic == "p_value") |> pull(value_script),
  "s2_dem_ri_p", archive_values |> filter(table_figure == "text_study2_f_tests", study == "d", statistic == "p_value") |> pull(value_script),
  "s2_rep_ri_p", archive_values |> filter(table_figure == "text_study2_f_tests", study == "r", statistic == "p_value") |> pull(value_script),
  "s1_mw_top_n", archive_values |> filter(table_figure == "figure_3", topic == "Minimum wage", arm == "Proposal 3 (N)") |> pull(value_script),
  "s1_rtw_top_n", archive_values |> filter(table_figure == "figure_3", topic == "Right-to-work", arm == "Proposal 4 (BM)") |> pull(value_script),
  "s1_rtw_second_n", archive_values |> filter(table_figure == "figure_3", topic == "Right-to-work", arm == "Proposal 4 (CA)") |> pull(value_script)
)

ground_truth <-
  bind_rows(
    claims_rows |>
      left_join(archive_prose, by = "claim_id", relationship = "one-to-one") |>
      select(claim_id, table_figure, claim, claim_type, mode, operator, digits,
             value_paper, value_rewrite, value_script, needs_block),
    table_cells |>
      select(claim_id, table_figure, claim, digits, value_paper, value_rewrite,
             value_script) |>
      mutate(claim_type = "pipeline", mode = "", operator = "", needs_block = FALSE)
  ) |>
  mutate(paper_id = "offer-westort_coppock_green_2021", .before = 1)

# Verdicts ----
# match compares the deposit's own output against the article, match_rewrite the
# rewrite's. A descriptive claim has no number to compare, so its verdict lives in
# holds; an approximate claim has no verdict at all by design.
ground_truth <-
  ground_truth |>
  mutate(
    mode = coalesce(mode, ""),
    value_script = as.numeric(value_script),
    # The deposit prints its tables through a tibble, which shows three
    # significant digits, so value_script is already rounded when it is read. Two
    # separately rounded numbers can legitimately differ by a full unit in the
    # last printed digit, which is the tolerance the archive comparison takes.
    match = if_else(is.na(value_script) | mode != "", NA_integer_,
                    agrees(value_script, value_paper, digits, tolerance = 1)),
    match_rewrite = case_when(
      claim_type == "descriptive" ~ NA_integer_,
      mode != "" ~ NA_integer_,
      is.na(value_rewrite) ~ NA_integer_,
      TRUE ~ agrees(value_rewrite, value_paper, digits)
    ),
    holds = case_when(
      claim_type == "descriptive" ~ as.integer(value_rewrite == as.numeric(value_paper)),
      # An approximation is a claim about a rough magnitude, so it is judged on a
      # relative tolerance rather than at the page's printed precision; an unseeded
      # value is judged against the dispersion its own procedure generates.
      mode == "approximate" & !is.na(value_rewrite) ~
        as.integer(abs(value_rewrite - as.numeric(normalise_paper(value_paper))) <=
                     0.1 * abs(as.numeric(normalise_paper(value_paper))) + epsilon),
      mode == "unseeded" & !is.na(value_rewrite) ~
        as.integer(abs(value_rewrite - as.numeric(normalise_paper(value_paper))) <=
                     unseeded_tolerance * 10^(-digits) + epsilon),
      TRUE ~ NA_integer_
    )
  )

# The locus of every adverse row ----
locus <- tribble(
  ~claim_id, ~defect_locus, ~locus_note,
  "s1_mw_correct_static", "paper_internal",
  "The simulations pick the best minimum wage proposal in 36.46 per cent of runs, which is 36 per cent at the precision the sentence uses. Rounding to one decimal first gives 36.5 and then 37.",
  "s1_mw_top_mean", "paper_internal",
  "The sentence's posterior probability and respondent count belong to Proposal 3 (N), whose estimated mean is 0.865; 0.895 is Proposal 3 (Y)'s, over 121 respondents.",
  "s2_rep_4pt_arm", "paper_internal",
  "The 4.1-point Republican effect is the Lottery arm's, as the preceding sentence and Figure 6 both say; the Accuracy arm's estimate is -0.053.",
  "s2_rep_static_p", "archive",
  "The deposit's own script for this figure stops before reaching it, on a saved object no deposited file contains. Over five seeds of the bootstrap the p-value runs 0.076 to 0.117, and the estimate and standard error printed beside it on Figure F.8 both reproduce.",
  "s2_dem_ri_p", "paper_internal",
  "The article gives the Democrats 0.013, which is the value the deposited script returns for the Republicans; the value it returns for the Democrats is 0.028, which the article gives the Republicans. The two are transposed, and the observed F statistics settle which way round: the Republican F is the larger of the two, so the Republican p-value is the smaller. The pipeline returns 0.027 here, against the deposit's 0.028.",
  "s2_rep_ri_p", "paper_internal",
  "The article gives the Republicans 0.028, which is the value the deposited script returns for the Democrats; the value it returns for the Republicans is 0.013, which the article gives the Democrats. The pipeline returns 0.011 here, against the deposit's 0.013."
)

ground_truth <- ground_truth |>
  left_join(locus, by = "claim_id", relationship = "one-to-one") |>
  mutate(
    adverse = coalesce(match, 1L) == 0 | coalesce(match_rewrite, 1L) == 0 |
      coalesce(holds, 1L) == 0,
    notes = case_when(
      !is.na(locus_note) ~ locus_note,
      claim_type == "descriptive" & holds == 1 ~ "The estimates support the claim.",
      mode == "approximate" ~ str_c("The article hedges this figure; the pipeline gives ",
                                    render(value_rewrite, digits + 1), "."),
      mode == "unseeded" ~ str_c("A bootstrap the deposit does not seed reproducibly; the pipeline gives ",
                                 render(value_rewrite, digits), "."),
      is.na(value_rewrite) ~ "No counterpart in the pipeline; verified at the point of use.",
      coalesce(match_rewrite, 0L) == 1 ~ str_c("The rewrite gives ", render(value_rewrite, digits),
                                               ", which is the published value."),
      TRUE ~ ""
    )
  )

# THE LOCUS RULE, three states. An adverse row must carry a locus, a clean match
# must not, and a row with no verdict may.
clean_match <- with(ground_truth, coalesce(match_rewrite, 0L) == 1 & !adverse)
if (any(ground_truth$adverse & is.na(ground_truth$defect_locus))) {
  print(ground_truth |> filter(adverse, is.na(defect_locus)) |>
          select(claim_id, claim, digits, value_paper, value_rewrite, value_script,
                 match, match_rewrite, holds), n = 60, width = 220)
}
stopifnot(
  all(!ground_truth$adverse | !is.na(ground_truth$defect_locus)),
  all(!clean_match | is.na(ground_truth$defect_locus))
)

# The committed file stores the string the article prints ----
# Comparing needs a number and the record needs the page's own digits, so the two
# happen in sequence: compare numerically above, render back to a string here.
ground_truth <- ground_truth |>
  mutate(value_paper = render(as.numeric(normalise_paper(value_paper)), digits)) |>
  select(paper_id, claim_id, table_figure, claim, claim_type, mode, digits,
         value_script, value_paper, match, value_rewrite, match_rewrite, holds,
         defect_locus, notes)

# Errata spine gate ----
# errata.qmd names, for each published entry, the ground truth rows that entry corrects. An
# id that no longer exists is a typo or a renamed claim, and a dangling reference in a
# document whose whole purpose is correcting the record is worse than a build that refuses.
errata_path <- here::here("errata_entries.csv")
if (file.exists(errata_path)) {
  errata_ids <- read_csv(errata_path, show_col_types = FALSE) |>
    pull(claim_ids) |>
    str_split(";") |>
    unlist() |>
    str_trim()
  errata_ids <- errata_ids[!is.na(errata_ids) & errata_ids != ""]
  dangling_errata_ids <- setdiff(errata_ids, ground_truth$claim_id)
  if (length(dangling_errata_ids) > 0) {
    stop("errata_entries.csv lists claim ids absent from the ground truth: ",
         paste(dangling_errata_ids, collapse = ", "))
  }
  print(str_glue("Errata spine: {length(unique(errata_ids))} distinct claim ids listed, ",
                 "all present in the ground truth."))
}

write_csv(ground_truth,
          here::here("ground_truth", "offer-westort_coppock_green_2021_ground_truth.csv"))

# Float coverage, stated rather than merely computed ----
float_inventory <- tribble(
  ~table_figure, ~published_values, ~uncoverable_reason,
  "table_1", 39, NA_character_,
  "table_2", 0, "Study 1 treatment wording. Nothing in the deposit computes it.",
  "table_3", 0, "Study 2 question wording and the official statistics quoted in it. Nothing in the deposit computes them.",
  "table_4", 0, "Study 2 treatment wording. Nothing in the deposit computes it.",
  "figure_1", 10, NA_character_,
  "figure_2", 0, "Two views of the same nine simulations as Figure 1; the article states no number from it that Figure 1 does not.",
  "figure_3", 0, "A trajectory plot with no numbers on its face; its endpoints are the posterior probabilities the results section states.",
  "figure_4", 36, NA_character_,
  "figure_5", 0, "A trajectory plot with no numbers on its face.",
  "figure_6", 40, NA_character_,
  "table_c1", 24, "An analytic enumeration in the supplement's worked example. The deposit ships no code for supplement A, B or C.",
  "figure_d1", 0, "Plots the cells of Table D.2, which are covered.",
  "figure_d2", 0, "Plots the cells of Table D.2, which are covered.",
  "table_d2", 255, NA_character_,
  "figure_d3", 0, "Plots the cells of Table D.3, which are covered.",
  "table_d3", 165, NA_character_,
  "figure_d4", 0, "Plots the cells of Table D.4 and D.5, which are covered, plus the static run at a first batch of 1,000.",
  "figure_d5", 0, "Plots the cells of Table D.4 and D.5, which are covered, plus the static run at a first batch of 1,000.",
  "table_d4", 150, NA_character_,
  "table_d5", 150, NA_character_,
  "table_e6", 50, "Fifty state minimum wage rates transcribed from an external source. The deposited data carry no wage field.",
  "figure_f6", 20, NA_character_,
  "figure_f7", 20, NA_character_,
  "figure_f8", 40, NA_character_,
  "table_f7", 64, NA_character_,
  "table_g8", 0, "Conjoint attribute wording. The level counts it implies are covered separately.",
  "figure_g9", 0, "A trajectory plot with no numbers on its face.",
  "figure_g10", 0, "A trajectory plot with no numbers on its face; its endpoints are the six probabilities the supplement's results state."
)

# Table F.7 is reprinted from the fitted models rather than covered cell by cell,
# so its coverage is counted from the models the rewrite writes.
table_f7_covered <-
  table_f7 |>
  summarize(n = 2 * n() + 2 * n_distinct(str_c(outcome, party, adjusted))) |>
  pull(n)

covered <- ground_truth |>
  filter(!is.na(value_rewrite), !str_ends(claim_id, "_cells")) |>
  count(table_figure, name = "covered_values")

float_coverage <-
  float_inventory |>
  left_join(covered, by = "table_figure") |>
  mutate(
    covered_values = case_when(
      table_figure == "table_f7" ~ as.integer(table_f7_covered),
      TRUE ~ coalesce(covered_values, 0L)
    ),
    covered_fraction = if_else(published_values > 0,
                               covered_values / published_values, NA_real_)
  ) |>
  select(table_figure, published_values, covered_values, covered_fraction,
         uncoverable_reason)

stopifnot(all(float_coverage$published_values == 0 |
                !is.na(float_coverage$uncoverable_reason) |
                float_coverage$covered_values >= float_coverage$published_values))

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))

print(ground_truth |> count(claim_type, match, match_rewrite, holds))
print(ground_truth |> filter(coalesce(match_rewrite, 1L) == 0 | coalesce(holds, 1L) == 0 | coalesce(match, 1L) == 0) |>
        select(claim_id, value_paper, value_rewrite, value_script, match, match_rewrite, holds, defect_locus),
      n = 40, width = 220)
print(float_coverage, n = nrow(float_coverage), width = 220)

# The coverage gate ----
# in_text_claims.R is read as a program, not as text: a block that errors or
# prints nothing satisfies a textual check completely. It is sourced into its own
# environment, because the two files necessarily name the same outputs and a bare
# source() would replace this script's objects with the claims file's.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed <- tibble(line = claims_output) |>
  filter(str_starts(line, "CLAIM ")) |>
  mutate(
    claim_id = str_match(line, "^CLAIM ([^ ]+) = ")[, 2],
    printed_value = str_match(line, "^CLAIM [^ ]+ = ([^|]+) \\|\\|")[, 2] |> str_trim()
  )

required <- published_claims |> filter(needs_block)

stopifnot(
  nrow(printed) == nrow(required),
  !any(duplicated(printed$claim_id)),
  setequal(printed$claim_id, required$claim_id)
)

# Value by value, against the other instrument. A shared digits override is what
# makes this a check rather than an identity: both files look the precision up in
# published_claims.csv, so neither can name a different one.
cross <- required |>
  select(claim_id, value_paper, digits, mode) |>
  join_one_to_one(printed |> select(claim_id, printed_value), by = "claim_id") |>
  join_one_to_one(ground_truth |> select(claim_id, value_rewrite), by = "claim_id") |>
  mutate(
    build_value = render(value_rewrite, digits),
    exempt = coalesce(mode, "") == "approximate"
  )

mismatched <- cross |> filter(!exempt, printed_value != build_value)
if (nrow(mismatched) > 0) {
  print(mismatched, n = 40, width = 220)
  stop("The two instruments disagree on ", nrow(mismatched), " claims.")
}

# The re-rendered published string must equal the string the extraction stores,
# which catches a digits entry that is right about the value and wrong about the
# precision. Numeric equality passes that case; this does not.
rendered_paper <- required |>
  join_one_to_one(ground_truth |> select(claim_id, stored = value_paper), by = "claim_id") |>
  filter(normalise_paper(value_paper) != stored)
if (nrow(rendered_paper) > 0) {
  print(rendered_paper |> select(claim_id, value_paper, stored, digits), n = 40)
  stop("The stored published string differs from the extraction's on ",
       nrow(rendered_paper), " claims.")
}

print(str_glue(
  "Ground truth: {nrow(ground_truth)} rows, of which {sum(!is.na(ground_truth$match_rewrite))} ",
  "carry a value comparison and {sum(ground_truth$match_rewrite == 1, na.rm = TRUE)} match. ",
  "Coverage gate: {nrow(printed)} claims printed against {nrow(required)} required, ",
  "all ids present in both directions, {sum(!cross$exempt)} compared value by value."
))
