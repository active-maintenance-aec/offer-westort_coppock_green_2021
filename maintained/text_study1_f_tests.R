# offer-westort_coppock_green_2021/maintained/text_study1_f_tests.R
# Output: output/text_study1_f_tests.csv
# Depends on: helpers.R, original/study_1_clean.rds
# Description: The two randomization-inference F-test p-values the study 1 results
#   section reports. The observed F comes from a classical weighted regression;
#   the null distribution comes from 1,000 re-runs of the adaptive assignment
#   procedure on the observed outcomes. Runtime is about a minute.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

iter <- 1e3

mturk_bandit <- read_rds(file.path(data_dir, "study_1_clean.rds"))

fit_mw <- lm_robust(Y_mw ~ Z_mw, weights = weights_mw, data = mturk_bandit,
                    se_type = "classical")
fit_rtw <- lm_robust(Y_rtw ~ Z_rtw, weights = weights_rtw, data = mturk_bandit,
                     se_type = "classical")

null_mw <- mysims(Y = mturk_bandit$Y_mw, Z = sort(unique(mturk_bandit$Z_mw)),
                  iter = iter, RI = TRUE)
null_rtw <- mysims(Y = mturk_bandit$Y_rtw, Z = sort(unique(mturk_bandit$Z_rtw)),
                   iter = iter, RI = TRUE)

# Share of the null distribution at or above the observed F ----
ri_p_value <- function(null_draws, observed) {
  1 - max(which(sort(null_draws[, "F"]) < observed)) / length(null_draws[, "F"])
}

text_study1_f_tests <- tibble(
  topic = c("Minimum wage", "Right-to-work"),
  f_statistic = c(summary(fit_mw)$fstatistic["value"],
                  summary(fit_rtw)$fstatistic["value"]),
  p_value = c(ri_p_value(null_mw, summary(fit_mw)$fstatistic["value"]),
              ri_p_value(null_rtw, summary(fit_rtw)$fstatistic["value"])),
  iterations = iter
)

write_csv(text_study1_f_tests, file.path(out_dir, "text_study1_f_tests.csv"))

print(text_study1_f_tests)
