# offer-westort_coppock_green_2021/maintained/figure_d5_rmse_first_batch.R
# Output: output/figure_d5_rmse_first_batch.pdf, .png, .csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Figure D.5. Root mean squared error of the best arm's
#   success rate and of its treatment effect as the first batch grows from 100 to
#   910 of the 1,000 observations.
#
#   The published figure carries an eleventh point at 1,000, where the whole
#   sample sits in the first batch and there is nothing left to adapt on. That is
#   the static run, which the deposit binds into both algorithms' series, so it is
#   the same value under either label and the two curves meet there.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

scenario_labels <- c("1: Clear winner", "2: No clear winner", "3: Competing second-best")

adaptive_runs <-
  simulation_statistics |>
  filter(source == "supplement_1", algorithm != "Static", batches == 10)

whole_sample_first_batch <-
  simulation_statistics |>
  filter(source == "supplement_1", algorithm == "Static", batches == 1, first_batch == 100) |>
  select(-algorithm) |>
  expand_grid(algorithm = c("TS", "CA")) |>
  mutate(first_batch = 1000)

gg_df <-
  bind_rows(adaptive_runs, whole_sample_first_batch) |>
  mutate(
    Algorithm = if_else(algorithm == "CA", "Control-Augmented", "Thompson Sampling"),
    Scenario = scenario_labels[case]
  ) |>
  select(Scenario, Algorithm, first_batch,
         `Success rate of best arm` = rmse_best,
         `ATE of best arm` = rmse_te) |>
  pivot_longer(c("Success rate of best arm", "ATE of best arm"),
               names_to = "statistic", values_to = "value")

g <-
  ggplot(gg_df, aes(x = first_batch, y = value, group = Scenario, color = Scenario,
                    linetype = Scenario, shape = Scenario)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_grid(statistic ~ Algorithm, scales = "free_y") +
  scale_x_continuous(breaks = seq(250, 1000, 250)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(3, "lines"),
    strip.background = element_blank(),
    panel.spacing = unit(.5, "lines")
  ) +
  labs(x = "Size of the first batch", y = "Root mean squared error")

ggsave(file.path(out_dir, "figure_d5_rmse_first_batch.pdf"), plot = g, width = 6.5, height = 6.5)
ggsave(file.path(out_dir, "figure_d5_rmse_first_batch.png"), plot = g, width = 6.5, height = 6.5, dpi = 300)

write_csv(gg_df, file.path(out_dir, "figure_d5_rmse_first_batch.csv"))

print(gg_df |> filter(first_batch == 550), n = 20)
