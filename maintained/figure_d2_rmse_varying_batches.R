# offer-westort_coppock_green_2021/maintained/figure_d2_rmse_varying_batches.R
# Output: output/figure_d2_rmse_varying_batches.pdf, .png, .csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Figure D.2. Root mean squared error of the best arm's
#   success rate and of its treatment effect, as the number of batches goes from
#   1 to 100.

source(here::here("maintained", "helpers.R"))

simulation_statistics <- read_csv(file.path(out_dir, "simulation_statistics.csv"),
                                  show_col_types = FALSE)

scenario_labels <- c("1: Clear winner", "2: No clear winner", "3: Competing second-best")

gg_df <-
  simulation_statistics |>
  filter(source == "supplement_1", first_batch == 100) |>
  mutate(
    Algorithm = if_else(algorithm == "CA", "Control-Augmented", "Thompson Sampling"),
    Scenario = scenario_labels[case]
  ) |>
  select(Scenario, Algorithm, batches,
         `Success rate of best arm` = rmse_best,
         `ATE of best arm` = rmse_te) |>
  pivot_longer(c("Success rate of best arm", "ATE of best arm"),
               names_to = "statistic", values_to = "value")

g <-
  ggplot(gg_df, aes(x = batches, y = value, group = Scenario, color = Scenario,
                    linetype = Scenario, shape = Scenario)) +
  geom_point(size = 2) +
  geom_line() +
  scale_color_grey() +
  facet_grid(statistic ~ Algorithm, scales = "free_y") +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(3, "lines"),
    strip.background = element_blank(),
    panel.spacing = unit(.5, "lines")
  ) +
  labs(x = "Number of batches (log scale)", y = "Root mean squared error")

ggsave(file.path(out_dir, "figure_d2_rmse_varying_batches.pdf"), plot = g, width = 6.5, height = 6.5)
ggsave(file.path(out_dir, "figure_d2_rmse_varying_batches.png"), plot = g, width = 6.5, height = 6.5, dpi = 300)

write_csv(gg_df, file.path(out_dir, "figure_d2_rmse_varying_batches.csv"))

print(gg_df |> filter(batches == 10), n = 20)
