# offer-westort_coppock_green_2021/maintained/figure_4_study1_mean_outcomes.R
# Output: output/figure_4_study1_mean_outcomes.pdf, .png, .csv
# Depends on: helpers.R, original/study_1_clean.rds
# Description: Figure 4. Inverse-probability-weighted mean support for each ballot
#   measure wording, with HC2 confidence intervals. The deposit draws the interval
#   with geom_errorbarh(height = 0), which is geom_linerange.

source(here::here("maintained", "helpers.R"))

mturk_bandit <- read_rds(file.path(data_dir, "study_1_clean.rds"))

fit_mw <- lm_robust(Y_mw ~ Z_mw - 1, weights = weights_mw, data = mturk_bandit)
fit_rtw <- lm_robust(Y_rtw ~ Z_rtw - 1, weights = weights_rtw, data = mturk_bandit)

gg_df <-
  bind_rows(
    `Minimum wage` = as_tibble(tidy(fit_mw)),
    `Right-to-work` = as_tibble(tidy(fit_rtw)),
    .id = "topic"
  ) |>
  mutate(
    arm = str_remove(term, "Z_mw|Z_rtw"),
    label = fct_reorder(factor(arm), estimate)
  )

g <-
  ggplot(gg_df, aes(x = estimate, y = label, xmin = conf.low, xmax = conf.high)) +
  geom_point() +
  geom_linerange() +
  facet_wrap(~topic, scales = "free") +
  theme_bw() +
  geom_text(
    aes(label = paste0(format_num(estimate, 3), " ", add_parens(std.error, 3))),
    size = 2.5, show.legend = FALSE, nudge_y = .3, nudge_x = -.05
  ) +
  theme(strip.background = element_blank(), axis.title.y = element_blank()) +
  xlim(0, 1) +
  xlab("Average proportion of respondents supporting the measure")

ggsave(file.path(out_dir, "figure_4_study1_mean_outcomes.pdf"),
       plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_4_study1_mean_outcomes.png"),
       plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |> select(topic, arm, estimate, std.error, conf.low, conf.high, p.value, df),
  file.path(out_dir, "figure_4_study1_mean_outcomes.csv")
)

print(gg_df |> select(topic, arm, estimate, std.error), n = 20)
