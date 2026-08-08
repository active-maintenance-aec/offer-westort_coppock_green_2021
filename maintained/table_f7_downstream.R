# offer-westort_coppock_green_2021/maintained/table_f7_downstream.R
# Output: output/table_f7_downstream.tex, output/table_f7_downstream.csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Appendix Table F.7. Downstream effects of the encouragements on two
#   follow-up economic assessments. Google and Extra Time are excluded because
#   they change who answers, which is what Figure F.6 shows.
#
#   The deposit builds this table with stargazer, which has not been updated since
#   2022 and now fails outright on this call, and writes it through a commented-out
#   out= argument. modelsummary writes the same eight columns straight to a file.

source(here::here("maintained", "helpers.R"))

mturk_bandit <-
  read_rds(file.path(data_dir, "study_2_clean.rds")) |>
  mutate(
    econ1 = case_when(
      econ_followup_1 %in% c("Good", "Very good") ~ 1,
      is.na(econ_followup_1) ~ NA_real_,
      TRUE ~ 0
    ),
    econ2 = case_when(
      econ_followup_2 %in% c("get worse", "stay about the same") ~ 0,
      econ_followup_2 == "get better" ~ 1
    )
  ) |>
  filter(Z != "extratime", Z != "google") |>
  mutate(Z = droplevels(Z))

covariates <- "follow_pol_pre + race_pre + educ_5_pre + female_pre + age_pre + pid_7_pre + ideo_7_pre"

# The deposit fits with lm() and takes its standard errors from starprep(), whose
# default is HC2 on that weighted fit. lm_robust with the same weights and
# se_type = "HC2" returns the identical coefficients and the identical standard
# errors, and it carries them on the fit object, so the table and the CSV read the
# same numbers rather than two separately constructed ones.
fit_one <- function(outcome, party, adjusted) {
  formula <- as.formula(
    if (adjusted) paste(outcome, "~ Z +", covariates) else paste(outcome, "~ Z")
  )
  lm_robust(formula, weights = weights, se_type = "HC2",
            data = filter(mturk_bandit, pid == party))
}

specifications <-
  expand_grid(
    outcome = c("econ1", "econ2"),
    party = c("democrat", "republican"),
    adjusted = c(FALSE, TRUE)
  ) |>
  mutate(
    label = paste0(str_to_title(party), if_else(adjusted, ", adjusted", "")),
    fit = pmap(list(outcome, party, adjusted), fit_one)
  )

models <- set_names(specifications$fit,
                    paste0(specifications$outcome, ": ", specifications$label))

modelsummary(
  models,
  coef_map = c(Zlottery = "Lottery", Zaccuracy = "Accuracy", Zdirection = "Direction"),
  gof_map = c("nobs", "r.squared"),
  stars = c("*" = 0.05),
  output = file.path(out_dir, "table_f7_downstream.tex")
)

table_f7 <-
  specifications |>
  mutate(
    estimates = map(fit, function(m) {
      as_tibble(tidy(m)) |>
        select(term, estimate, std.error, p.value, conf.low, conf.high)
    }),
    nobs = map_dbl(fit, function(m) m$nobs),
    r_squared = map_dbl(fit, function(m) m$r.squared)
  ) |>
  select(outcome, party, adjusted, label, nobs, r_squared, estimates) |>
  unnest(estimates) |>
  filter(term %in% c("Zlottery", "Zaccuracy", "Zdirection")) |>
  mutate(term = str_to_title(str_remove(term, "^Z"))) |>
  select(outcome, party, adjusted, term, estimate, std.error, p.value, conf.low,
         conf.high, nobs, r_squared)

write_csv(table_f7, file.path(out_dir, "table_f7_downstream.csv"))

print(table_f7, n = nrow(table_f7), width = 200)
