# offer-westort_coppock_green_2021/maintained/figure_d3_varying_best_arm.R
# Output: output/figure_d3_varying_best_arm.pdf, .png, .csv
# Depends on: helpers.R, output/table_d3_varying_best_arm.csv
# Description: Appendix Figure D.3. Best-arm selection, coverage, and RMSE as the
#   best arm's success rate rises from 0.10 to 0.20 with the other eight arms
#   fixed at 0.10. It plots the same numbers as Table D.3, and reads them from
#   that table so the two cannot disagree.

source(here::here("maintained", "helpers.R"))

table_d3 <- read_csv(file.path(out_dir, "table_d3_varying_best_arm.csv"),
                     show_col_types = FALSE)

algorithm_labels <- c(TS = "Thompson Sampling", Static = "Static",
                      CA = "Control-Augmented")

gg_df <-
  table_d3 |>
  mutate(Algorithm = factor(algorithm_labels[algorithm], levels = unname(algorithm_labels))) |>
  select(Algorithm, best_arm_value,
         `Best arm selection` = correct,
         `Coverage of best arm` = cov_best,
         `Coverage of ATE` = cov_te,
         `RMSE of best arm` = rmse_best,
         `RMSE of ATE` = rmse_te) |>
  pivot_longer(-c(Algorithm, best_arm_value), names_to = "statistic", values_to = "value") |>
  mutate(statistic = factor(statistic, levels = c("Best arm selection",
                                                  "Coverage of best arm",
                                                  "Coverage of ATE",
                                                  "RMSE of best arm",
                                                  "RMSE of ATE")))

nominal <- tibble(statistic = factor(c("Coverage of best arm", "Coverage of ATE"),
                                     levels = levels(gg_df$statistic)),
                  Y = 0.95)

g <-
  ggplot(gg_df, aes(x = best_arm_value, y = value, group = Algorithm, color = Algorithm,
                    linetype = Algorithm, shape = Algorithm)) +
  geom_point(size = 2) +
  geom_line() +
  geom_hline(data = nominal, aes(yintercept = Y), linetype = "dashed") +
  scale_color_grey() +
  facet_wrap(~statistic, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = seq(0.10, 0.20, 0.02)) +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(3, "lines"),
    strip.background = element_blank(),
    panel.spacing = unit(.5, "lines")
  ) +
  labs(x = "Success rate of the best arm", y = element_blank())

ggsave(file.path(out_dir, "figure_d3_varying_best_arm.pdf"), plot = g, width = 6.5, height = 7)
ggsave(file.path(out_dir, "figure_d3_varying_best_arm.png"), plot = g, width = 6.5, height = 7, dpi = 300)

write_csv(gg_df, file.path(out_dir, "figure_d3_varying_best_arm.csv"))

print(gg_df |> filter(best_arm_value == 0.15), n = 20)
