# offer-westort_coppock_green_2021/maintained/figure_1_simulated_posterior_probs.R
# Output: output/figure_1_simulated_posterior_probs.pdf, .png, .csv
# Depends on: helpers.R, output/simulations_illustrative.csv
# Description: Figure 1. Posterior probability that each arm is best over ten
#   batches, under Thompson sampling and under a static design, in each of the
#   three scenarios.

source(here::here("maintained", "helpers.R"))

sims <- read_csv(file.path(out_dir, "simulations_illustrative.csv"), show_col_types = FALSE)

gg_df <-
  sims |>
  filter(quantity == "posterior_prob", design %in% c("Thompson sampling", "Static")) |>
  mutate(
    design = factor(design, levels = c("Thompson sampling", "Static")),
    panel = paste0(case, ", ", if_else(design == "Thompson sampling", "TS", "static")),
    p.cat = factor(case_when(
      arm == "best" ~ 0,
      arm == "alt1" & case == "Case 3: Competing second best" ~ 1,
      TRUE ~ 2
    ))
  )

text_df <-
  gg_df |>
  filter(batch == max(batch)) |>
  mutate(profile = case_when(
    arm == "best" ~ "True best arm",
    arm == "alt1" & case == "Case 3: Competing second best" ~ "Second best arm"
  ))

g <-
  ggplot(gg_df, aes(x = batch, y = value, group = arm, color = p.cat, linetype = p.cat)) +
  geom_line() +
  theme_bw() +
  facet_wrap(~ case + design, nrow = 3,
             labeller = label_wrap_gen(width = 40, multi_line = FALSE)) +
  coord_cartesian(xlim = c(0, 14.5), ylim = c(0, 1), clip = "off") +
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
    aes(label = profile),
    nudge_x = 5, hjust = 1, segment.size = .2, seed = 343,
    direction = "y", size = 3
  )

ggsave(file.path(out_dir, "figure_1_simulated_posterior_probs.pdf"),
       plot = g, width = 6.5, height = 6)
ggsave(file.path(out_dir, "figure_1_simulated_posterior_probs.png"),
       plot = g, width = 6.5, height = 6, dpi = 300)

write_csv(
  gg_df |> select(case, design, arm, batch, posterior_prob = value),
  file.path(out_dir, "figure_1_simulated_posterior_probs.csv")
)

print(gg_df |> filter(batch == 10, arm == "best") |> select(case, design, value))
