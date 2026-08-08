# offer-westort_coppock_green_2021/maintained/figure_g9_study3_by_attribute.R
# Output: output/figure_g9_study3_by_attribute.pdf, .png, .csv
# Depends on: helpers.R, output/study_3_probabilities.csv
# Description: Appendix Figure G.9. The 192 joint posterior probabilities summed
#   within each level of each conjoint attribute, batch by batch, for the adaptive
#   and static arms of study 3.

source(here::here("maintained", "helpers.R"))

study_3_probabilities <- read_csv(file.path(out_dir, "study_3_probabilities.csv"),
                                  show_col_types = FALSE)

attributes <- c(Z_1 = "Personal limits", Z_2 = "Corporate limits",
                Z_3 = "Public funding", Z_4 = "Disclosures")

gg_df <-
  study_3_probabilities |>
  pivot_longer(all_of(names(attributes)), names_to = "attribute", values_to = "level") |>
  group_by(design, batch, attribute, level) |>
  summarize(posterior_prob = sum(posterior_prob), .groups = "drop") |>
  mutate(
    attribute = factor(attributes[attribute], levels = unname(attributes)),
    design = factor(design, levels = c("Adaptive", "Static"))
  ) |>
  group_by(batch, attribute, design) |>
  mutate(p.cat = rank(-posterior_prob, ties.method = "first")) |>
  ungroup() |>
  group_by(attribute, level, design) |>
  mutate(p.cat = p.cat[batch == 10]) |>
  ungroup() |>
  mutate(p.cat = factor(p.cat))

text_df <- filter(gg_df, batch == max(batch))

g <-
  ggplot(gg_df, aes(x = batch, y = posterior_prob, group = level, color = p.cat)) +
  geom_line() +
  theme_bw() +
  facet_grid(attribute ~ design) +
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
    aes(label = level),
    nudge_x = 5, nudge_y = .05, hjust = 1, segment.size = .2,
    seed = 343, direction = "y", size = 3
  )

ggsave(file.path(out_dir, "figure_g9_study3_by_attribute.pdf"), plot = g, width = 6.5, height = 6.5)
ggsave(file.path(out_dir, "figure_g9_study3_by_attribute.png"), plot = g, width = 6.5, height = 6.5, dpi = 300)

write_csv(
  gg_df |> select(design, attribute, level, batch, posterior_prob),
  file.path(out_dir, "figure_g9_study3_by_attribute.csv")
)

print(text_df |> arrange(design, attribute, desc(posterior_prob)) |>
        select(design, attribute, level, posterior_prob), n = 30)
