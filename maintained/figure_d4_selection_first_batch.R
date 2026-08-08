# offer-westort_coppock_green_2021/maintained/figure_d4_selection_first_batch.R
# Output: output/figure_d4_selection_first_batch.pdf, .png, .csv
# Depends on: helpers.R, output/simulation_statistics.csv
# Description: Appendix Figure D.4. Best-arm selection and coverage as the first
#   batch grows from 100 to 910 of the 1,000 observations, which is how much of
#   the sample is committed before the algorithm learns anything.
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
  ggplot(gg_df, aes(x = first_batch, y = value, group = Algorithm, color = Algorithm,
                    linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(data = nominal, aes(yintercept = Y), linetype = "dashed") +
  scale_color_grey() +
  facet_grid(statistic ~ Scenario, scales = "free_y") +
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
  labs(x = "Size of the first batch", y = element_blank())

ggsave(file.path(out_dir, "figure_d4_selection_first_batch.pdf"), plot = g, width = 6.5, height = 7)
ggsave(file.path(out_dir, "figure_d4_selection_first_batch.png"), plot = g, width = 6.5, height = 7, dpi = 300)

write_csv(gg_df, file.path(out_dir, "figure_d4_selection_first_batch.csv"))

print(gg_df |> filter(first_batch == 550), n = 20)
