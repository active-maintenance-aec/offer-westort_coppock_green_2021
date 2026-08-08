# offer-westort_coppock_green_2021/maintained/figure_5_study2_posterior_probs.R
# Output: output/figure_5_study2_posterior_probs.pdf, .png, .csv
# Depends on: helpers.R, output/study_2_probabilities.rds
# Description: Figure 5. Posterior probability that each encouragement is the best
#   arm, batch by batch, separately for Democrats and Republicans.

source(here::here("maintained", "helpers.R"))

study_2_probabilities <- read_rds(file.path(out_dir, "study_2_probabilities.rds"))

gg_df <-
  study_2_probabilities |>
  filter(pid %in% c("democrat", "republican")) |>
  mutate(
    type = if_else(pid == "democrat", "Democrats", "Republicans"),
    arm = str_to_title(arm),
    arm = if_else(arm == "Extratime", "Extra Time", arm)
  ) |>
  group_by(batch, type) |>
  mutate(p.cat = rank(-posterior_prob)) |>
  ungroup() |>
  group_by(arm, type) |>
  mutate(p.cat = p.cat[batch == 10]) |>
  ungroup() |>
  mutate(p.cat = factor(p.cat))

text_df <- filter(gg_df, batch == max(batch))

g <-
  ggplot(gg_df, aes(x = batch, y = posterior_prob, group = arm, color = p.cat)) +
  geom_line() +
  theme_bw() +
  facet_grid(~type) +
  coord_cartesian(xlim = c(0, 15),
                  ylim = c(0, max(gg_df$posterior_prob) + .1),
                  clip = "off") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  scale_colour_manual(values = gray(seq(.1, .6, len = nlevels(gg_df$p.cat)))) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylab("Posterior probability of being the best arm") +
  xlab("Batch number") +
  geom_text_repel(
    data = text_df,
    aes(label = arm),
    nudge_x = 5, nudge_y = .05, hjust = 1, segment.size = .2,
    seed = 343, direction = "y", size = 3
  )

ggsave(file.path(out_dir, "figure_5_study2_posterior_probs.pdf"),
       plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_5_study2_posterior_probs.png"),
       plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |> select(type, arm, batch, posterior_prob, posterior_mean),
  file.path(out_dir, "figure_5_study2_posterior_probs.csv")
)

print(text_df |> arrange(type, desc(posterior_prob)) |> select(type, arm, posterior_prob), n = 12)
