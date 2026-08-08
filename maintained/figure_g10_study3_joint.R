# offer-westort_coppock_green_2021/maintained/figure_g10_study3_joint.R
# Output: output/figure_g10_study3_joint.pdf, .png, .csv
# Depends on: helpers.R, output/study_3_probabilities.csv
# Description: Appendix Figure G.10. The joint posterior probability that each of
#   the 192 conjoint profiles is best, batch by batch, under the adaptive and the
#   static design. Profiles are grouped by where they finish.

source(here::here("maintained", "helpers.R"))

study_3_probabilities <- read_csv(file.path(out_dir, "study_3_probabilities.csv"),
                                  show_col_types = FALSE)

breaks <- c(0, 10, 50, 100, 192)

gg_df <-
  study_3_probabilities |>
  mutate(design = factor(design, levels = c("Adaptive", "Static"))) |>
  group_by(design, batch) |>
  mutate(p.order = rank(-posterior_prob, ties.method = "first")) |>
  group_by(design, profile) |>
  mutate(p.last = p.order[batch == max(batch)],
         p.cat = cut(p.last, breaks = breaks)) |>
  ungroup()

levels(gg_df$p.cat) <- paste0("[", breaks[1:(length(breaks) - 1)] + 1, ",",
                              breaks[2:length(breaks)], "]")

g <-
  ggplot(gg_df, aes(x = batch, y = posterior_prob, group = profile,
                    color = p.cat, linetype = p.cat)) +
  geom_line() +
  facet_wrap(~design) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  coord_cartesian(xlim = c(0, 10),
                  ylim = c(0, max(gg_df$posterior_prob) + .01),
                  clip = "off") +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(breaks = seq(0, .5, .1)) +
  scale_colour_manual(name = "Position in last period",
                      values = gray(seq(.1, .6, len = nlevels(gg_df$p.cat)))) +
  scale_linetype_manual(name = "Position in last period",
                        values = 1:nlevels(gg_df$p.cat)) +
  ylab("Posterior probability of being the best arm") +
  xlab("Batch number")

ggsave(file.path(out_dir, "figure_g10_study3_joint.pdf"), plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_g10_study3_joint.png"), plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |> select(design, profile, batch, posterior_prob, Z_1, Z_2, Z_3, Z_4),
  file.path(out_dir, "figure_g10_study3_joint.csv")
)

print(gg_df |> filter(batch == 10) |> group_by(design) |> slice_max(posterior_prob, n = 3) |>
        select(design, profile, posterior_prob), width = 200)
