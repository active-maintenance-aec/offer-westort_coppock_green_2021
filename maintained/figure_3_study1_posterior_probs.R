# offer-westort_coppock_green_2021/maintained/figure_3_study1_posterior_probs.R
# Output: output/figure_3_study1_posterior_probs.pdf, .png, .csv
# Depends on: helpers.R, original/study_1_clean.rds
# Description: Figure 3. Posterior probability that each ballot-measure wording is
#   the best arm, batch by batch, for the minimum wage and right-to-work
#   experiments. The CSV also carries the cumulative successes and trials the
#   posteriors are computed from, which are the treatment frequency counts the
#   text cites.

source(here::here("maintained", "helpers.R"))

mturk_bandit <- read_rds(file.path(data_dir, "study_1_clean.rds"))

# Posterior probabilities are built on cumulative successes and trials ----
posterior_by_batch <- function(d, arm_col, outcome_col, n_arms) {
  arms <- tibble(arm = d[[arm_col]], y = d[[outcome_col]], batch = d$batch)

  arms |>
    group_by(arm, batch) |>
    summarize(successes = sum(y), trials = n(), .groups = "drop") |>
    complete(arm, batch, fill = list(successes = 0, trials = 0)) |>
    group_by(arm) |>
    mutate(cumulative_successes = cumsum(successes),
           cumulative_trials = cumsum(trials)) |>
    group_by(batch) |>
    mutate(posterior_prob = best_binomial_bandit(cumulative_successes, cumulative_trials)) |>
    ungroup() |>
    bind_rows(tibble(arm = factor(levels(d[[arm_col]]), levels = levels(d[[arm_col]])),
                     batch = 0, posterior_prob = 1 / n_arms))
}

gg_df <-
  bind_rows(
    `Minimum wage` = posterior_by_batch(mturk_bandit, "Z_mw", "Y_mw", 10),
    `Right-to-work` = posterior_by_batch(mturk_bandit, "Z_rtw", "Y_rtw", 8),
    .id = "topic"
  ) |>
  mutate(arm = as.character(arm)) |>
  group_by(batch, topic) |>
  mutate(p.cat = rank(-posterior_prob)) |>
  ungroup() |>
  group_by(arm) |>
  mutate(p.cat = p.cat[batch == 10]) |>
  ungroup() |>
  mutate(p.cat = factor(p.cat))

text_df <- filter(gg_df, batch == 10)

g <-
  ggplot(gg_df, aes(x = batch, y = posterior_prob, group = arm, color = p.cat)) +
  geom_line() +
  ylim(0, 1) +
  facet_wrap(~topic) +
  geom_text_repel(
    data = text_df,
    aes(label = arm),
    hjust = -1, vjust = .5, force = 10, seed = 343,
    direction = "y", segment.alpha = .25
  ) +
  theme_bw() +
  scale_colour_manual(values = gray(seq(.1, .6, len = nlevels(gg_df$p.cat)))) +
  coord_cartesian(xlim = c(0, 20)) +
  scale_x_continuous(breaks = 1:10) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Posterior probability of being the best arm") +
  xlab("Batch number")

ggsave(file.path(out_dir, "figure_3_study1_posterior_probs.pdf"),
       plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_3_study1_posterior_probs.png"),
       plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |>
    select(topic, arm, batch, posterior_prob, successes, trials,
           cumulative_successes, cumulative_trials) |>
    arrange(topic, arm, batch),
  file.path(out_dir, "figure_3_study1_posterior_probs.csv")
)

print(text_df |> arrange(topic, desc(posterior_prob)) |>
        select(topic, arm, posterior_prob, cumulative_trials), n = 20)
