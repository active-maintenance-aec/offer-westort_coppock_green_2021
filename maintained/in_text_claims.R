# offer-westort_coppock_green_2021/maintained/in_text_claims.R
# Output: (printed; the coverage gate in build_ground_truth.R reads what it prints)
# Depends on: helpers.R, maintained/output/, ground_truth/published_claims.csv
# Description: The second instrument. One block per numeric claim the article or
#   the supplement makes, each recomputing the number from maintained/output/ by
#   its own path and printing it beside the sentence it belongs to. Nothing here
#   refits a model or reads raw data, and nothing here reads the ground truth: the
#   two files reach the same claimed numbers separately, and where they disagree
#   one of them is wrong.
#
#   Every line is CLAIM <id> = <value> || <label>. That printed line is the only
#   load-bearing link to the extraction, so a block that errors or prints nothing
#   fails the gate rather than passing it silently.

source(here::here("maintained", "helpers.R"))

options(width = 200)

extraction <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), .default = col_guess())
)

# Both instruments look the precision up here, so neither can name a different one.
claim <- function(id, value) {
  row <- extraction[extraction$claim_id == id, ]
  stopifnot(nrow(row) == 1, length(value) == 1, !is.na(value))
  zero <- sprintf(paste0("%.", row$digits, "f"), 0)
  rendered <- sprintf(paste0("%.", row$digits, "f"), value)
  # A difference of two nearly equal numbers can arrive as a negative zero, which
  # prints with a minus sign the other instrument will not have.
  if (rendered == str_c("-", zero)) rendered <- zero
  cat("CLAIM ", id, " = ", rendered, " || ", row$quantity,
      " [article: ", row$value_paper, "]\n", sep = "")
}

read_out <- function(file) read_csv(file.path(out_dir, file), show_col_types = FALSE)

simulations <- read_out("simulations_illustrative.csv")
simulation_statistics <- read_out("simulation_statistics.csv")
figure_3 <- read_out("figure_3_study1_posterior_probs.csv")
figure_4 <- read_out("figure_4_study1_mean_outcomes.csv")
figure_5 <- read_out("figure_5_study2_posterior_probs.csv")
figure_6 <- read_out("figure_6_study2_ate_estimates.csv")
figure_f6 <- read_out("figure_f6_response_rates.csv")
figure_f7 <- read_out("figure_f7_independents.csv")
figure_f8 <- read_out("figure_f8_simulated_comparisons.csv")
study_3 <- read_out("study_3_probabilities.csv")
table_f7 <- read_out("table_f7_downstream.csv")
s1_f <- read_out("text_study1_f_tests.csv")
s1_static <- read_out("text_study1_static_simulations.csv")
s2_control <- read_out("text_study2_control_responses.csv")
s2_counts <- read_out("text_study2_counts.csv")
s2_f <- read_out("text_study2_f_tests.csv")
s2_bols <- read_out("text_study2_batched_ols.csv")

# Simulations illustrating how adaptive designs work ----

# "In these scenarios, we simulate experiments sampling 100 observations for each
#  of 10 periods, assigning treatment to nine arms according to the adaptive
#  algorithms or a standard static design."
first_batch_total <- simulations |>
  filter(quantity == "cumulative_n", batch == 1, design == "Static",
         case == "Case 1: Clear winner")
claim("sim_obs_per_period", sum(first_batch_total$value))
claim("sim_periods", max(simulations$batch))
claim("sim_arms", length(unique(simulations$arm)))

# "We then present averages across 10,000 simulations (Table 1)."
claim("sim_iterations", unique(simulation_statistics$iterations[
  simulation_statistics$source == "supplement_1" & simulation_statistics$batches == 10]))

# "Here and throughout, we use HC2 robust standard errors for confidence
#  intervals, as implemented by the estimatr package"
# The level itself is never stated in words; it is recoverable from the width of
# any published interval relative to its own standard error.
half_widths <- (figure_4$conf.high - figure_4$estimate) / figure_4$std.error
claim("ci_level", mean(2 * pt(half_widths, figure_4$df) - 1))

# "By the end of the 10-period experiment, the true best arm is assigned a 0.90
#  probability of being best in the adaptive design and a .61 probability of
#  being best in the static trial."
last_batch <- simulations |> filter(quantity == "posterior_prob", batch == 10)
final_prob <- function(which_case, which_design, which_arm) {
  last_batch$value[last_batch$case == which_case & last_batch$design == which_design &
                     last_batch$arm == which_arm]
}
highest_inferior <- function(which_case, which_design) {
  rows <- last_batch[last_batch$case == which_case & last_batch$design == which_design &
                       last_batch$arm != "best", ]
  max(rows$value)
}
claim("fig1_case1_ts_best", final_prob("Case 1: Clear winner", "Thompson sampling", "best"))
claim("fig1_case1_static_best", final_prob("Case 1: Clear winner", "Static", "best"))

# "Indeed, we assign the best arm only 0.13 probability of being best, whereas we
#  assign an inferior arm 0.24 probability of being best. For the static
#  experiment, we assign the best arm 0.06 probability of being best, and an
#  inferior arm a 0.36 probability of being best"
claim("fig1_case2_ts_best", final_prob("Case 2: No clear winner", "Thompson sampling", "best"))
claim("fig1_case2_ts_inferior", highest_inferior("Case 2: No clear winner", "Thompson sampling"))
claim("fig1_case2_static_best", final_prob("Case 2: No clear winner", "Static", "best"))
claim("fig1_case2_static_inferior", highest_inferior("Case 2: No clear winner", "Static"))

# "In the third case (bottom panels of Figure 1), the adaptive design assigns the
#  best arm a 0.88 probability of being best and the second best arm a 0.04
#  probability of being best. The static design accords the second-best arm a
#  0.66 probability of being best, but gives the true best arm only a 0.24
#  probability of being best."
claim("fig1_case3_ts_best", final_prob("Case 3: Competing second best", "Thompson sampling", "best"))
claim("fig1_case3_ts_second", final_prob("Case 3: Competing second best", "Thompson sampling", "alt1"))
claim("fig1_case3_static_second", final_prob("Case 3: Competing second best", "Static", "alt1"))
claim("fig1_case3_static_best", final_prob("Case 3: Competing second best", "Static", "best"))

# Table 1 ----
# "TABLE 1 Iterated Simulation Statistics"
# Cells reproduced of cells published; the per-cell rows are in the ground truth.
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            col_types = cols(value_paper = col_character(),
                                             .default = col_guess()))
for (which_table in c("table_1", "table_d2", "table_d3", "table_d4", "table_d5")) {
  claim(str_c(which_table, "_cells"), sum(published_cells$table_figure == which_table))
}

# Empirical application: finding the best performing treatment arm ----

# "For this study, we recruited 1,000 subjects from the Amazon Mechanical Turk
#  (MTurk) marketplace."
final_counts <- figure_3 |> filter(batch == 10)
claim("s1_n", sum(final_counts$cumulative_trials[final_counts$topic == "Minimum wage"]))

# "We generated two versions of each of these five proposals, varying whether the
#  current value of the minimum wage was displayed, resulting in 10 unique
#  minimum wage treatments." and "resulting in eight unique right-to-work
#  treatments"
claim("s1_mw_arms", length(unique(figure_3$arm[figure_3$topic == "Minimum wage"])))
claim("s1_rtw_arms", length(unique(figure_3$arm[figure_3$topic == "Right-to-work"])))

# "The winning arm, by a hair, was Proposal 3 (B, without current minimum wage),
#  with a probability of being best of 0.219 and an estimated mean of 0.895 over
#  183 respondents."
mw_final <- final_counts |> filter(topic == "Minimum wage") |> arrange(desc(posterior_prob))
rtw_final <- final_counts |> filter(topic == "Right-to-work") |> arrange(desc(posterior_prob))
mean_of <- function(which_topic, which_arm) {
  figure_4$estimate[figure_4$topic == which_topic & figure_4$arm == which_arm]
}
claim("s1_mw_top_posterior", mw_final$posterior_prob[1])
claim("s1_mw_top_mean", mean_of("Minimum wage", mw_final$arm[1]))
claim("s1_mw_top_n", mw_final$cumulative_trials[1])

# "Of 10 arms, only two had success rates under 0.8"
claim("s1_mw_arms_below_08", sum(figure_4$estimate[figure_4$topic == "Minimum wage"] < 0.8))

# "The randomization inference F-test yielded a p-value of 0.429"
claim("s1_mw_f_p", s1_f$p_value[s1_f$topic == "Minimum wage"])

# "Proposal 4 (framed as a ballot measure) ended with a 0.906 probability of
#  being best. The second-best arm was also Proposal 4 (framed as a
#  constitutional amendment) with a probability of being best of 0.085. ... the
#  probability weighted estimates are 0.926 and 0.934 over 721 and 82
#  respondents, respectively"
claim("s1_rtw_top_posterior", rtw_final$posterior_prob[1])
claim("s1_rtw_second_posterior", rtw_final$posterior_prob[2])
claim("s1_rtw_top_mean", mean_of("Right-to-work", rtw_final$arm[1]))
claim("s1_rtw_second_mean", mean_of("Right-to-work", rtw_final$arm[2]))
claim("s1_rtw_top_n", rtw_final$cumulative_trials[1])
claim("s1_rtw_second_n", rtw_final$cumulative_trials[2])

# "The randomization inference F-test yielded a p-value of 0.199"
claim("s1_rtw_f_p", s1_f$p_value[s1_f$topic == "Right-to-work"])

# "The static design would have sampled each of the 10 arms in the minimum wage
#  experiment in expectation 100 times each and each of the eight arms in the
#  right-to-work experiment in expectation 125 times each."
per_arm <- function(which_topic) {
  rows <- final_counts[final_counts$topic == which_topic, ]
  sum(rows$cumulative_trials) / nrow(rows)
}
claim("s1_static_mw_per_arm", per_arm("Minimum wage"))
claim("s1_static_rtw_per_arm", per_arm("Right-to-work"))

# "Treating observed success rates as the truth in simulations of the minimum
#  wage experiment, we picked the best proposal 47% of the time in adaptive
#  experiments and 37% of the time in static experiments. In the right-to-work
#  experiment ... 65% of the time in adaptive experiments and 58% of the time in
#  static experiments."
static_row <- function(which_topic) s1_static[s1_static$topic == which_topic, ]
claim("s1_mw_correct_adaptive", 100 * static_row("Minimum wage")$correct_adaptive)
claim("s1_mw_correct_static", 100 * static_row("Minimum wage")$correct_static)
claim("s1_rtw_correct_adaptive", 100 * static_row("Right-to-work")$correct_adaptive)
claim("s1_rtw_correct_static", 100 * static_row("Right-to-work")$correct_static)

# "For the minimum wage experiment, standard errors around our estimate of the
#  success rate for the best arm in a simulated static design would have been, on
#  average, 90% as large as those under a comparable adaptive design. ... For the
#  right-to-work experiment ... 133% as large"
claim("s1_mw_se_ratio", 100 * static_row("Minimum wage")$se_static / static_row("Minimum wage")$se_adaptive)
claim("s1_rtw_se_ratio", 100 * static_row("Right-to-work")$se_static / static_row("Right-to-work")$se_adaptive)

# "The level of public support for the winning right-to-work ballot measure was
#  estimated with a standard error that was 75% as large as would have been the
#  case under a static design."
claim("s1_rtw_se_ratio_inverse",
      100 * static_row("Right-to-work")$se_adaptive / static_row("Right-to-work")$se_static)

# Empirical application: adaptive trials with a control condition ----

# "Drawing on the recent literature, we implement a six-arm trial, with a control
#  condition and each of five treatment arms representing a somewhat different
#  theoretical approach."
claim("s2_arms", length(unique(figure_5$arm)))
claim("s2_treatment_arms", sum(unique(figure_5$arm) != "Control"))

# "Using the Lucid platform, we gathered approximately 300 observations per day
#  for a total of 10 days"
claim("s2_days", max(figure_5$batch))
claim("s2_obs_per_day", sum(s2_counts$n) / max(figure_5$batch))

# "restricting our attention to those asked the control version of each question,
#  we see that 24% of Republicans and 62% of Democrats reported that the deficit
#  had grown under Trump; 75% and 34% reported that black unemployment had
#  decreased; and 22% and 59% said that farm income had declined."
control_share <- function(which_pid, question) {
  s2_control[[question]][s2_control$pid == which_pid]
}
claim("s2_control_deficit_rep", 100 * control_share("republican", "deficit"))
claim("s2_control_deficit_dem", 100 * control_share("democrat", "deficit"))
claim("s2_control_unemp_rep", 100 * control_share("republican", "unemp"))
claim("s2_control_unemp_dem", 100 * control_share("democrat", "unemp"))
claim("s2_control_nfi_rep", 100 * control_share("republican", "nfi"))
claim("s2_control_nfi_dem", 100 * control_share("democrat", "nfi"))

# "'Pure' independents were allocated separately as part of their own adaptive
#  trial, but their numbers are small (n = 568)"
claim("s2_independents_n", sum(s2_counts$n[s2_counts$pid == "independent"]))

# "the unadjusted estimate indicates a 4.1 percentage point increase in accuracy
#  with a 1.7 percentage point standard error, as shown in Figure 6."
ate <- function(which_type, which_spec, which_term, column) {
  rows <- figure_6[figure_6$type == which_type & figure_6$Estimates == which_spec &
                     figure_6$term == which_term, ]
  rows[[column]]
}
claim("s2_rep_lottery_ate", 100 * ate("Republicans", "IPW", "Lottery", "estimate"))
claim("s2_rep_lottery_se", 100 * ate("Republicans", "IPW", "Lottery", "std.error"))

# "the adaptive design allocated 552 of 1,258 Republicans to the Lottery
#  condition, with another 493 assigned to control."
arm_n <- function(which_pid, which_z) s2_counts$n[s2_counts$pid == which_pid & s2_counts$Z == which_z]
claim("s2_rep_lottery_n", arm_n("republican", "lottery"))
claim("s2_rep_total_n", sum(s2_counts$n[s2_counts$pid == "republican"]))
claim("s2_rep_control_n", arm_n("republican", "control"))

# "Had we implemented a static design with equal allocation to all six
#  conditions, based on simulations the standard error would have been on average
#  2.5 percentage points"
static_f8 <- function(which_type, which_term, column) {
  rows <- figure_f8[figure_f8$Estimates == "Static" & figure_f8$type == which_type &
                      figure_f8$term == which_term, ]
  rows[[column]]
}
claim("s2_rep_static_se", 100 * static_f8("Republicans", "Lottery", "std.error"))

# "the adaptive design with the smaller standard error allows us to declare the
#  4.1-point average effect of the accuracy treatment statistically significant
#  (p = 0.014), whereas under that static design the effect would not have
#  attained statistical significance (p = 0.152)."
claim("s2_rep_lottery_p", ate("Republicans", "IPW", "Lottery", "p.value"))
claim("s2_rep_static_p", static_f8("Republicans", "Lottery", "p.value"))

# The sentence names the accuracy treatment. Which arm carries a 4.1-point
# Republican effect is a question the estimates answer.
republican_ipw <- figure_6[figure_6$type == "Republicans" & figure_6$Estimates == "IPW", ]
four_point_arm <- republican_ipw$term[which.min(abs(100 * republican_ipw$estimate - 4.1))]
claim("s2_rep_4pt_arm", as.numeric(four_point_arm == "Accuracy"))

# "Using randomization inference, we reject the null of no difference across
#  treatments (p = 0.028)."
ri_p <- function(which_party) {
  s2_f$p_value[s2_f$party == which_party & s2_f$augmented_arm == "position 3 (deposit)"]
}
claim("s2_rep_ri_p", ri_p("Republicans"))

# "In the end, Lottery was significantly better than the control by 7.1
#  percentage points (unadjusted, p = 0.007) or 5.3 percentage points (adjusted,
#  p = 0.03). Using randomization inference, we again reject the null (p = 0.013)."
claim("s2_dem_lottery_ate", 100 * ate("Democrats", "IPW", "Lottery", "estimate"))
claim("s2_dem_lottery_p", ate("Democrats", "IPW", "Lottery", "p.value"))
claim("s2_dem_lottery_ate_adj", 100 * ate("Democrats", "IPW-Adjusted", "Lottery", "estimate"))
claim("s2_dem_lottery_p_adj", ate("Democrats", "IPW-Adjusted", "Lottery", "p.value"))
claim("s2_dem_ri_p", ri_p("Democrats"))

# "Had we used the batched OLS approach proposed by Zhang, Janson, and Murphy
#  (2020) ... the p-values for the ATE of the Lottery condition rise slightly,
#  with p = 0.026 for Republicans and p = 0.018 for Democrats."
claim("s2_bols_p_rep", s2_bols$p_value[s2_bols$party == "Republicans"])
claim("s2_bols_p_dem", s2_bols$p_value[s2_bols$party == "Democrats"])

# Supplement D: additional simulations ----

# "We replicate simulations presented in Table 1 with 1,000 total observations
#  and varying number of batches."
claim("appd1_total_obs", 100 * max(simulations$batch))

# "Most of the gains are realized by five total batches, with diminishing
#  marginal returns thereafter."
# A one-batch adaptive design is a static design, which is the row the appendix
# reports as "1 batch".
clear_winner <- simulation_statistics |>
  filter(source == "supplement_1", first_batch == 100, case == 1, algorithm != "CA") |>
  arrange(batches)
gain_at_five <- clear_winner$correct[clear_winner$batches == 5] - clear_winner$correct[clear_winner$batches == 1]
gain_total <- clear_winner$correct[clear_winner$batches == 100] - clear_winner$correct[clear_winner$batches == 1]
claim("appd1_gains_by_five", as.numeric(gain_at_five / gain_total > 0.5))

# Supplement F: additional analyses, study two ----

# "Control means are 0.956 and 0.977 for Democrats and Republicans,
#  respectively."
f6 <- function(which_type, which_term, column) {
  figure_f6[[column]][figure_f6$type == which_type & figure_f6$term == which_term]
}
claim("appf1_control_mean_dem", f6("Democrats", "Control mean", "estimate"))
claim("appf1_control_mean_rep", f6("Republicans", "Control mean", "estimate"))

# "Response rates under the Google condition are about 5 percentage points lower
#  for both Democrats and Republicans; response rates under the Extra Time
#  condition are 32 and 23 percentage points lower for Democrats and Republicans
#  respectively."
claim("appf1_google_drop",
      -100 * mean(c(f6("Democrats", "Google", "estimate"), f6("Republicans", "Google", "estimate"))))
claim("appf1_extratime_drop_dem", -100 * f6("Democrats", "Extra Time", "estimate"))
claim("appf1_extratime_drop_rep", -100 * f6("Republicans", "Extra Time", "estimate"))

# "Table F.7: Study Two, Downstream Effects of Treatment"
# Three coefficients and three standard errors in each of eight columns, plus the
# observation count and the R-squared under each.
claim("table_f7_cells", 2 * nrow(table_f7) + 2 * nrow(distinct(table_f7, outcome, party, adjusted)))

# "These estimates are bootstrapped 1,000 times"
claim("appf8_bootstraps", unique(figure_f8$replicates[figure_f8$Estimates == "Static"]))

# Supplement G: study three ----

# "the combination of factors with 4 x 3 x 4 x 4 levels results in 192 unique
#  experimental arms"
claim("appg_levels_z1", length(unique(study_3$Z_1)))
claim("appg_levels_z2", length(unique(study_3$Z_2)))
claim("appg_levels_z3", length(unique(study_3$Z_3)))
claim("appg_levels_z4", length(unique(study_3$Z_4)))
claim("appg_profiles", length(unique(study_3$profile)))

# "collected as a target of 100 responses for each of 10 waves"
# The two sample sizes in the same passage have no counterpart: the deposit ships
# study 3 as batch-level fitted data rather than a respondent file, so nothing in
# the pipeline counts subjects. The wave count it does carry.
claim("appg_waves", max(study_3$batch))

# "the most preferred profile includes each of the top attribute levels presented
#  in the left panel of Figure G.9, with a final probability of being best of
#  0.338; the second-most preferred profile ... 0.213; the third-most preferred
#  profile ... 0.076. For the static design ... 0.194 ... 0.161 ... 0.140."
top_three <- function(which_design) {
  study_3 |>
    filter(design == which_design, batch == max(study_3$batch)) |>
    slice_max(posterior_prob, n = 3) |>
    arrange(desc(posterior_prob))
}
adaptive_top <- top_three("Adaptive")
static_top <- top_three("Static")
claim("appg_adaptive_p1", adaptive_top$posterior_prob[1])
claim("appg_adaptive_p2", adaptive_top$posterior_prob[2])
claim("appg_adaptive_p3", adaptive_top$posterior_prob[3])
claim("appg_static_p1", static_top$posterior_prob[1])
claim("appg_static_p2", static_top$posterior_prob[2])
claim("appg_static_p3", static_top$posterior_prob[3])

# "the most preferred measure profile would propose personal contribution limits
#  of only $10,000, would prohibit all corporate contributions as well as public
#  funding, and would institute required disclosures of all contributions."
claim("appg_adaptive_profile", as.numeric(
  adaptive_top$Z_1[1] == "$10,000" & adaptive_top$Z_2[1] == "Prohibit all" &
    adaptive_top$Z_3[1] == "Prohibit all" & adaptive_top$Z_4[1] == "Disclose all"))

# "under the static design ... would also propose personal contribution limits of
#  only $10,000 and would prohibit all corporate contributions, but would support
#  $1 to $1 in matching funds, and would require disclosures above $500."
claim("appg_static_profile", as.numeric(
  static_top$Z_1[1] == "$10,000" & static_top$Z_2[1] == "Prohibit all" &
    static_top$Z_3[1] == "1:1 match" & static_top$Z_4[1] == "Disclose > $500"))

# Float faces ----
# Figures 4, 6, F.6, F.7 and F.8 print an estimate and a standard error beside
# every point, which makes each of them a published table in disguise. One line
# per printed number.
# covers: figure_4_*
# covers: figure_6_*
# covers: figure_f6_*
# covers: figure_f7_*
# covers: figure_f8_*
slugify <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "_")

print_face <- function(d, prefix, key_columns) {
  keys <- apply(d[key_columns], 1, function(row) slugify(str_c(row, collapse = "_")))
  walk2(keys, seq_len(nrow(d)), function(key, i) {
    claim(str_c(prefix, key, "_estimate"), d$estimate[i])
    claim(str_c(prefix, key, "_se"), d$std.error[i])
  })
}

print_face(figure_4, "figure_4_", c("topic", "arm"))
print_face(figure_6, "figure_6_", c("type", "Estimates", "term"))
print_face(filter(figure_f6, term != "Control mean"), "figure_f6_", c("type", "term"))
print_face(figure_f7, "figure_f7_", c("Estimates", "term"))
print_face(figure_f8, "figure_f8_", c("type", "Estimates", "term"))
