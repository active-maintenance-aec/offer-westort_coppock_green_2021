# offer-westort_coppock_green_2021/maintained/figure_6_study2_ate_estimates.R
# Output: output/figure_6_study2_ate_estimates.pdf, .png, .csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Figure 6. Average treatment effects of each encouragement relative
#   to the control, inverse probability weighted, with CR2 standard errors
#   clustered by respondent, with and without covariate adjustment.
#   position_dodgev() from ggstance becomes position_dodge(), which dodges along
#   the discrete axis without help in current ggplot2; geom_errorbarh(height = 0)
#   becomes geom_linerange.

source(here::here("maintained", "helpers.R"))

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

mturk_long <-
  mturk_bandit |>
  pivot_longer(
    cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
    names_to = "out",
    values_to = "Y",
    values_transform = list(Y = as.double)
  )

covariates <- "follow_pol_pre + race_pre + educ_5_pre + female_pre + age_pre + pid_7_pre + ideo_7_pre"

fit_party <- function(party, adjusted) {
  formula <- if (adjusted) as.formula(paste("Y ~ Z +", covariates)) else Y ~ Z
  lm_robust(formula, data = filter(mturk_long, pid == party),
            cluster = id, weights = weights)
}

estimates <-
  expand_grid(party = c("democrat", "republican"), adjusted = c(TRUE, FALSE)) |>
  mutate(fit = map2(party, adjusted, fit_party)) |>
  mutate(tidied = map(fit, function(f) as_tibble(tidy(f)))) |>
  select(-fit) |>
  unnest(tidied)

gg_df <-
  estimates |>
  filter(str_starts(term, "Z")) |>
  mutate(
    type = if_else(party == "democrat", "Democrats", "Republicans"),
    Estimates = if_else(adjusted, "IPW-Adjusted", "IPW"),
    term = str_to_title(str_remove(term, "^Z")),
    term = if_else(term == "Extratime", "Extra Time", term),
    label = make_entry(estimate, std.error, p.value)
  )

label_df <-
  gg_df |>
  filter(type == "Democrats", term == "Lottery") |>
  mutate(x = conf.low - 0.03)

g <-
  ggplot(gg_df, aes(x = estimate, y = term, color = Estimates, shape = Estimates)) +
  geom_point(position = position_dodge(width = -0.4)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = -0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = label_df, size = 2.5, aes(label = Estimates, x = x),
            position = position_dodge(width = -0.4), hjust = 1) +
  facet_wrap(~type) +
  theme_bw() +
  coord_cartesian(xlim = c(-.15, .15)) +
  geom_text(aes(label = label), position = position_dodge(width = 0.4),
            size = 2.5, nudge_y = -0.25, show.legend = FALSE) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  scale_colour_manual(values = gray(seq(.1, .6, len = n_distinct(gg_df$Estimates))))

ggsave(file.path(out_dir, "figure_6_study2_ate_estimates.pdf"),
       plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_6_study2_ate_estimates.png"),
       plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |> select(type, Estimates, term, estimate, std.error, conf.low, conf.high, p.value),
  file.path(out_dir, "figure_6_study2_ate_estimates.csv")
)

print(gg_df |> select(type, Estimates, term, estimate, std.error, p.value), n = 20)
