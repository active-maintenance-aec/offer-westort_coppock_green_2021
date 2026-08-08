# offer-westort_coppock_green_2021/maintained/figure_f8_simulated_comparisons.R
# Output: output/figure_f8_simulated_comparisons.pdf, .png, .csv
# Depends on: helpers.R, original/study_2_clean.rds
# Description: Appendix Figure F.8. What study 2 would have looked like under a
#   static design: 1,000 resamples that reassign the observed respondents to arms
#   with equal probability, weighted by the inverse of their realised assignment
#   probability, compared against the adaptive design's own estimates. Runtime is
#   about a minute and a quarter.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

iter <- 1e3

mturk_bandit <- read_rds(file.path(data_dir, "study_2_clean.rds"))

mturk_long <-
  mturk_bandit |>
  pivot_longer(
    cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
    names_to = "out",
    values_to = "Y",
    values_transform = list(Y = as.double)
  )

fit_d <- lm_robust(Y ~ Z, data = filter(mturk_long, pid == "democrat"),
                   cluster = id, weights = weights)
fit_r <- lm_robust(Y ~ Z, data = filter(mturk_long, pid == "republican"),
                   cluster = id, weights = weights)

lm_ipw <-
  bind_rows(
    Democrats = as_tibble(tidy(fit_d)),
    Republicans = as_tibble(tidy(fit_r)),
    .id = "type"
  )

arms <- levels(mturk_bandit$Z)
parties <- c("democrat", "republican")

# One static counterfactual ----
# Arm sizes are drawn afresh each iteration, because complete random assignment
# would not have split a party's respondents into exactly equal arms either.
one_replicate <- function() {
  arm_n <- suppressWarnings(
    sapply(
      mturk_bandit |> group_by(pid) |> summarise(n = n(), .groups = "keep") |> pull(n),
      function(x) table(complete_ra(x, num_arms = length(arms)))
    )
  )
  colnames(arm_n) <- sort(unique(mturk_bandit$pid))
  rownames(arm_n) <- arms

  resampled <-
    expand_grid(party = parties, arm = arms) |>
    mutate(draw = map2(party, arm, function(p, z) {
      mturk_bandit |>
        filter(pid == p, Z == z) |>
        slice_sample(n = arm_n[z, p], weight_by = weights, replace = TRUE)
    })) |>
    pull(draw) |>
    list_rbind() |>
    mutate(id = row_number())

  resampled_long <-
    resampled |>
    pivot_longer(
      cols = c("Y_deficit", "Y_unemp", "Y_nfi"),
      names_to = "out",
      values_to = "Y",
      values_transform = list(Y = as.double)
    ) |>
    mutate(Z = relevel(Z, "control"))

  bind_rows(
    Democrats = as_tibble(tidy(lm_robust(
      Y ~ Z, data = filter(resampled_long, pid == "democrat"), cluster = id))),
    Republicans = as_tibble(tidy(lm_robust(
      Y ~ Z, data = filter(resampled_long, pid == "republican"), cluster = id))),
    .id = "type"
  )
}

replicates <- map(1:iter, function(i) one_replicate()) |> list_rbind()

lm_static <-
  replicates |>
  group_by(type, term) |>
  summarize(
    std.mean = mean(std.error),
    std.error = sd(estimate),
    estimate = mean(estimate),
    .groups = "drop"
  ) |>
  mutate(
    statistic = estimate / std.error,
    p.value = 2 * pt(-abs(statistic), iter),
    conf.low = estimate - qt(.975, iter) * std.error,
    conf.high = estimate + qt(.975, iter) * std.error
  )

gg_df <-
  bind_rows(Static = lm_static, IPW = lm_ipw, .id = "Estimates") |>
  filter(str_starts(term, "Z")) |>
  mutate(
    term = str_to_title(str_remove(term, "^Z")),
    term = if_else(term == "Extratime", "Extra Time", term),
    label = make_entry(estimate, std.error, p.value)
  )

label_df <-
  gg_df |>
  filter(type == "Democrats", term == "Lottery") |>
  mutate(x = conf.low - 0.03)

g <-
  ggplot(gg_df, aes(x = estimate, y = term, color = Estimates)) +
  geom_point(position = position_dodge(width = -0.4)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = -0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = label_df, size = 2.5, aes(label = Estimates, x = x),
            position = position_dodge(width = -0.4), hjust = 1) +
  facet_wrap(~type) +
  theme_bw() +
  coord_cartesian(xlim = c(-.15, .15)) +
  geom_text(aes(label = label), position = position_dodge(width = 0.4),
            size = 2.5, nudge_y = -0.25, show.legend = FALSE) +
  theme(
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  scale_colour_manual(values = gray(seq(.1, .6, len = n_distinct(gg_df$Estimates))))

ggsave(file.path(out_dir, "figure_f8_simulated_comparisons.pdf"), plot = g, width = 6.5, height = 4)
ggsave(file.path(out_dir, "figure_f8_simulated_comparisons.png"), plot = g, width = 6.5, height = 4, dpi = 300)

write_csv(
  gg_df |>
    mutate(replicates = if_else(Estimates == "Static", iter, NA_real_)) |>
    select(Estimates, type, term, estimate, std.error, std.mean, conf.low,
           conf.high, p.value, replicates),
  file.path(out_dir, "figure_f8_simulated_comparisons.csv")
)

print(gg_df |> select(Estimates, type, term, estimate, std.error, p.value), n = 20)
