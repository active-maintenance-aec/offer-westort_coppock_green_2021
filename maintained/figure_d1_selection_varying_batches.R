# offer-westort_coppock_green_2021/maintained/figure_d1_selection_varying_batches.R
# Output: output/figure_d1_selection_varying_batches.pdf, .png, .csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Figure D.1. Best-arm selection and the coverage of both
#   confidence intervals as the number of batches goes from 1 to 100, by scenario
#   and algorithm. The published figure is set in two parts on one page; the parts
#   share an x axis and a legend and are drawn here as one grid.

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
         `Best arm selection` = correct,
         `Coverage of best arm` = cov_best,
         `Coverage of ATE` = cov_te) |>
  pivot_longer(c("Best arm selection", "Coverage of best arm", "Coverage of ATE"),
               names_to = "statistic", values_to = "value") |>
  mutate(statistic = factor(statistic, levels = c("Best arm selection",
                                                  "Coverage of best arm",
                                                  "Coverage of ATE")))

nominal <- tibble(statistic = factor(c("Coverage of best arm", "Coverage of ATE"),
                                     levels = levels(gg_df$statistic)),
                  Y = 0.95)

g <-
  ggplot(gg_df, aes(x = batches, y = value, group = Algorithm, color = Algorithm,
                    linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(data = nominal, aes(yintercept = Y), linetype = "dashed") +
  scale_color_grey() +
  facet_grid(statistic ~ Scenario, scales = "free_y") +
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
  labs(x = "Number of batches (log scale)", y = element_blank())

ggsave(file.path(out_dir, "figure_d1_selection_varying_batches.pdf"), plot = g, width = 6.5, height = 7)
ggsave(file.path(out_dir, "figure_d1_selection_varying_batches.png"), plot = g, width = 6.5, height = 7, dpi = 300)

write_csv(gg_df, file.path(out_dir, "figure_d1_selection_varying_batches.csv"))

print(gg_df |> filter(batches == 10), n = 20)
