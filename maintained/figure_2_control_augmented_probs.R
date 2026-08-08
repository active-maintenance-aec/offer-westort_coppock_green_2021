# offer-westort_coppock_green_2021/maintained/figure_2_control_augmented_probs.R
# Output: output/figure_2_control_augmented_probs.pdf, .png, .csv
# Depends on: helpers.R, output/simulations_illustrative.csv
# Description: Figure 2. Under the control-augmented design, the posterior
#   probability of being best (left) and the cumulative sample assigned to each
#   arm (right), in each of the three scenarios.

source(here::here("maintained", "helpers.R"))

sims <- read_csv(file.path(out_dir, "simulations_illustrative.csv"), show_col_types = FALSE)

gg_df <-
  sims |>
  filter(design == "Control-augmented") |>
  mutate(
    panel = paste0(case, ", TS, control-augmented"),
    p.cat = factor(case_when(
      arm == "best" ~ 0,
      arm == "alt2" ~ 3,
      arm == "alt1" & case == "Case 3: Competing second best" ~ 1,
      TRUE ~ 2
    ))
  )

text_df <-
  gg_df |>
  filter(batch == max(batch)) |>
  mutate(profile = case_when(
    arm == "best" ~ "True best arm",
    arm == "alt2" ~ "Control arm",
    arm == "alt1" & case == "Case 3: Competing second best" ~ "Second best arm"
  ))

panel_plot <- function(which_quantity, y_label, y_limits) {
  ggplot(
    filter(gg_df, quantity == which_quantity),
    aes(x = batch, y = value, group = arm, color = p.cat, linetype = p.cat)
  ) +
    geom_line() +
    theme_bw() +
    facet_wrap(~panel, nrow = 3,
               labeller = label_wrap_gen(width = 80, multi_line = TRUE),
               scales = "free_y") +
    coord_cartesian(xlim = c(0, 14.5), ylim = y_limits, clip = "off") +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    scale_colour_manual(values = gray(seq(.1, .6, len = nlevels(gg_df$p.cat)))) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),
      strip.text = element_text(colour = "white")
    ) +
    ylab(y_label) +
    geom_text_repel(
      data = filter(text_df, quantity == which_quantity),
      aes(label = profile),
      nudge_x = 5, hjust = 1, segment.size = .2, seed = 343,
      direction = "y", size = 3
    )
}

g1 <- panel_plot("posterior_prob", "Posterior probability of being the best arm", c(0, 1))
g2 <- panel_plot("cumulative_n", "Cumulative sample", c(0, 350))

# ggarrange converts both panels to grobs, which opens a graphics device; pdf(NULL)
# keeps it from leaving an Rplots.pdf behind when the script is run with Rscript.
pdf(NULL)
g <- annotate_figure(ggarrange(g1, g2), bottom = " ") +
  annotate("text", x = .5, y = .97, label = "Case 1: Clear winner, TS, control augmented") +
  annotate("text", x = .5, y = .66, label = "Case 2: No clear winner, TS, control augmented") +
  annotate("text", x = .5, y = .355, label = "Case 3: Competing second best, TS, control augmented") +
  annotate("text", x = .5, y = .03, label = "Batch number")
dev.off()

ggsave(file.path(out_dir, "figure_2_control_augmented_probs.pdf"),
       plot = g, width = 6.5, height = 6)
ggsave(file.path(out_dir, "figure_2_control_augmented_probs.png"),
       plot = g, width = 6.5, height = 6, dpi = 300)

write_csv(
  gg_df |> select(case, arm, batch, quantity, value),
  file.path(out_dir, "figure_2_control_augmented_probs.csv")
)

print(gg_df |> filter(batch == 10, arm %in% c("best", "alt2")) |> select(case, arm, quantity, value))
