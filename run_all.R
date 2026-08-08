# offer-westort_coppock_green_2021/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# then the simulation summaries, then every published table and figure, then the
# in-text quantities. Every script is self-contained and can also be run on its
# own, except that the three scripts producing shared intermediates
# (simulations_illustrative.R, simulation_statistics.R, study2_probabilities.R,
# study3_probabilities.R) have to have run at least once first.
#
# Most of the running time is in five places: study2_probabilities.R (a Bayesian
# bootstrap at J = 10,000), study3_probabilities.R (twenty probit fits at 100,000
# draws each), figure_f8_simulated_comparisons.R (1,000 static counterfactual
# resamples), text_study2_f_tests.R (four randomization inference runs of 1,000),
# and extract_archive_values.R, which runs four of the deposited scripts. Every
# script's duration is written to run_timings.csv, so the report can state the
# measured cost rather than an estimate of it.

library(here)
here::i_am("run_all.R")

run_timings <- list()

# Each script is timed rather than merely sourced. The timings are properties of
# the run rather than of the paper, which is why they live outside output/.
timed <- function(...) {
  path <- here::here(...)
  started <- Sys.time()
  source(path)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  run_timings[[length(run_timings) + 1]] <<- tibble::tibble(
    script = paste(c(...), collapse = "/"), seconds = round(elapsed, 1)
  )
  invisible(NULL)
}

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
timed("download_original.R")

# Shared simulation intermediates ----
# Figures 1 and 2 are two views of one set of nine simulated experiments, and
# Table 1 with the whole of appendix D are slices of one summary of the deposited
# iterated simulations, so each is computed once and read back.
timed("maintained", "simulations_illustrative.R")
timed("maintained", "simulation_statistics.R")

# Tables ----
timed("maintained", "table_1_simulation_statistics.R")
timed("maintained", "table_d2_varying_batches.R")
timed("maintained", "table_d3_varying_best_arm.R")
timed("maintained", "table_d4_d5_varying_first_batch.R")
timed("maintained", "table_f7_downstream.R")

# Figures, main text ----
timed("maintained", "figure_1_simulated_posterior_probs.R")
timed("maintained", "figure_2_control_augmented_probs.R")
timed("maintained", "figure_3_study1_posterior_probs.R")
timed("maintained", "figure_4_study1_mean_outcomes.R")

# Study 2 posterior probabilities ----
# The one intermediate the deposit's own analysis script needs and the deposit
# does not ship. About two minutes at J = 10,000.
timed("maintained", "study2_probabilities.R")

timed("maintained", "figure_5_study2_posterior_probs.R")
timed("maintained", "figure_6_study2_ate_estimates.R")

# Figures, appendix ----
# Figure D.3 reads Table D.3's output, so it runs after it.
timed("maintained", "figure_d1_selection_varying_batches.R")
timed("maintained", "figure_d2_rmse_varying_batches.R")
timed("maintained", "figure_d3_varying_best_arm.R")
timed("maintained", "figure_d4_selection_first_batch.R")
timed("maintained", "figure_d5_rmse_first_batch.R")
timed("maintained", "figure_f6_response_rates.R")
timed("maintained", "figure_f7_independents.R")
timed("maintained", "figure_f8_simulated_comparisons.R")

# Study 3 posterior probabilities, then its two figures ----
timed("maintained", "study3_probabilities.R")
timed("maintained", "figure_g9_study3_by_attribute.R")
timed("maintained", "figure_g10_study3_joint.R")

# In-text quantities ----
timed("maintained", "text_study1_f_tests.R")
timed("maintained", "text_study1_static_simulations.R")
timed("maintained", "text_study2_control_responses.R")
timed("maintained", "text_study2_f_tests.R")
timed("maintained", "text_study2_batched_ols.R")

# The deposit's own output, then the ground truth ----
# extract_archive_values.R runs four of the deposited scripts and parses what they
# print, which is where value_script comes from; build_ground_truth.R reads every
# value_rewrite back out of maintained/output/, so both have to run late.
# It also runs in_text_claims.R under capture.output as its coverage gate, so that
# file necessarily runs twice per pipeline: once silently for the gate, and once
# below for the readable log.
timed("ground_truth", "extract_archive_values.R")
timed("ground_truth", "build_ground_truth.R")

# In-text claims, printed ----
# The second instrument. One block per numeric claim the article makes, each
# recomputing the number from maintained/output/ by its own path.
source(here::here("maintained", "in_text_claims.R"), local = new.env())

# Deposited archive, again ----
# The check at the top of this file is a precondition: it says original/ was intact
# before anything ran. Nothing above writes to original/, and this second pass is what
# demonstrates it rather than assuming it. Nothing is downloaded; the files are already
# present and are re-checked against the manifest on checksum, byte size and membership.
timed("download_original.R")

# Timings ----
# A property of this run and of the machine it ran on, so it is written outside
# maintained/output/ and never compared against a previously recorded figure.
timings <- purrr::list_rbind(run_timings)
readr::write_csv(timings, here::here("run_timings.csv"))
print(stringr::str_glue("Pipeline complete in {round(sum(timings$seconds) / 60, 1)} minutes."))
