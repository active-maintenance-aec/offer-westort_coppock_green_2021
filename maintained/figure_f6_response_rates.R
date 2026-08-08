# offer-westort_coppock_green_2021/maintained/figure_f6_response_rates.R
# Output: output/figure_f6_response_rates.pdf, .png, .csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Appendix Figure F.6. Effect of each encouragement on whether a
#   respondent answered every outcome question, by party. This is the attrition
#   check that motivates dropping Google and Extra Time from the downstream table.

source(here::here("maintained", "helpers.R"))

mturk_bandit <-
  read_rds(file.path(data_dir, "study_2_clean.rds")) |>
  mutate(always_present = 1 * (Y_no_response_sum == 0))

fit_party <- function(party) {
  lm_robust(always_present ~ Z, weights = weights,
            data = filter(mturk_bandit, pid == party))
}

estimates <-
  bind_rows(
    Democrats = as_tibble(tidy(fit_party("democrat"))),
    Republicans = as_tibble(tidy(fit_party("republican"))),
    .id = "type"
  )

# The intercept is the control condition's own response rate, which the appendix
# text states and the figure does not draw, so it is kept in the CSV and dropped
# from the plot.
gg_df <-
  estimates |>
  filter(str_starts(term, "Z")) |>
  mutate(
    term = str_to_title(str_remove(term, "^Z")),
    term = if_else(term == "Extratime", "Extra Time", term),
    label = make_entry(estimate, std.error, p.value)
  )

g <-
  ggplot(gg_df, aes(x = estimate, y = term)) +
  geom_point() +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~type) +
  theme_bw() +
  coord_cartesian(xlim = c(-.5, .1)) +
  geom_text(aes(label = label), size = 2.5, show.legend = FALSE, nudge_y = .15) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

ggsave(file.path(out_dir, "figure_f6_response_rates.pdf"), plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_f6_response_rates.png"), plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  estimates |>
    mutate(
      term = if_else(term == "(Intercept)", "Control mean",
                     str_to_title(str_remove(term, "^Z"))),
      term = if_else(term == "Extratime", "Extra Time", term)
    ) |>
    select(type, term, estimate, std.error, conf.low, conf.high, p.value),
  file.path(out_dir, "figure_f6_response_rates.csv")
)

print(gg_df |> select(type, term, estimate, std.error, p.value), n = 20)
