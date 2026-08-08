# offer-westort_coppock_green_2021/maintained/figure_f7_independents.R
# Output: output/figure_f7_independents.pdf, .png, .csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Appendix Figure F.7. The study 2 treatment effects for pure
#   independents, who were allocated in their own adaptive trial and are too few
#   to support the main analysis.

source(here::here("maintained", "helpers.R"))

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

mturk_long <-
  mturk_bandit |>
  pivot_longer(
    cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
    names_to = "out",
    values_to = "Y",
    values_transform = list(Y = as.double)
  ) |>
  filter(pid == "independent")

covariates <- "follow_pol_pre + race_pre + educ_5_pre + female_pre + age_pre + pid_7_pre + ideo_7_pre"

fit_unadjusted <- lm_robust(Y ~ Z, data = mturk_long, cluster = id, weights = weights)
fit_adjusted <- lm_robust(as.formula(paste("Y ~ Z +", covariates)),
                          data = mturk_long, cluster = id, weights = weights)

gg_df <-
  bind_rows(
    `IPW-Adjusted` = as_tibble(tidy(fit_adjusted)),
    IPW = as_tibble(tidy(fit_unadjusted)),
    .id = "Estimates"
  ) |>
  filter(str_starts(term, "Z")) |>
  mutate(
    term = str_to_title(str_remove(term, "^Z")),
    term = if_else(term == "Extratime", "Extra Time", term),
    label = make_entry(estimate, std.error, p.value)
  )

label_df <-
  gg_df |>
  filter(term == "Lottery") |>
  mutate(x = conf.low - 0.03)

g <-
  ggplot(gg_df, aes(x = estimate, y = term, color = Estimates)) +
  geom_point(position = position_dodge(width = -0.4)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = -0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = label_df, size = 2.5, aes(label = Estimates, x = x),
            position = position_dodge(width = -0.4), hjust = 1) +
  theme_bw() +
  coord_cartesian(xlim = c(-.4, .4)) +
  geom_text(aes(label = label), position = position_dodge(width = 0.4),
            size = 2.5, nudge_y = -0.25, show.legend = FALSE) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  scale_colour_manual(values = gray(seq(.1, .6, len = n_distinct(gg_df$Estimates))))

ggsave(file.path(out_dir, "figure_f7_independents.pdf"), plot = g, width = 4, height = 4)
ggsave(file.path(out_dir, "figure_f7_independents.png"), plot = g, width = 4, height = 4, dpi = 300)

write_csv(
  gg_df |> select(Estimates, term, estimate, std.error, conf.low, conf.high, p.value),
  file.path(out_dir, "figure_f7_independents.csv")
)

print(gg_df |> select(Estimates, term, estimate, std.error, p.value), n = 20)
print(str_glue("Independents: n = {n_distinct(mturk_long$id)}"))
