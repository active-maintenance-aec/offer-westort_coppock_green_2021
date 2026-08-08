# offer-westort_coppock_green_2021/maintained/simulations_illustrative.R
# Output: output/simulations_illustrative.rds, output/simulations_illustrative.csv
# Depends on: helpers.R
# Description: One simulated experiment per scenario under Thompson sampling, a
#   static design, and the control-augmented design. Figures 1 and 2 are both
#   drawn from these nine simulations, so they are generated once here under a
#   single seed rather than twice, which is also how the deposit does it.

source(here::here("maintained", "helpers.R"))
set.seed(95126)

periods <- 10
arms <- 9
arm_names <- c("best", paste0("alt", 1:(arms - 1)))

# Three scenarios ----
cases <- list(
  `Case 1: Clear winner` = c(.2, rep(.1, arms - 1)),
  `Case 2: No clear winner` = c(.11, rep(.1, arms - 1)),
  `Case 3: Competing second best` = c(.2, .18, rep(.1, arms - 2))
)

# The deposit draws in this order: adaptive, control-augmented, static for case
# 1, then adaptive, static, control-augmented for cases 2 and 3. Matching it
# matters, because a shared random stream makes the order part of the result.
draw_order <- list(
  c("adapt", "adapt_te", "stat"),
  c("adapt", "stat", "adapt_te"),
  c("adapt", "stat", "adapt_te")
)

sims <- list()
for (k in seq_along(cases)) {
  probs <- cases[[k]]
  for (which_sim in draw_order[[k]]) {
    sims[[paste(names(cases)[k], which_sim)]] <- switch(
      which_sim,
      adapt = sim_out(probs = probs),
      stat = sim_out(probs = probs, static = TRUE),
      adapt_te = sim_out_te(probs = probs)
    )
  }
}

write_rds(sims, file.path(out_dir, "simulations_illustrative.rds"))

# Tidy form, one row per case, design, arm and batch ----
# Batch 0 is the uniform prior, which the figures show as the common starting
# point of every arm.
as_long <- function(mat, quantity) {
  full <- rbind(if (quantity == "posterior_prob") rep(1 / arms, arms) else rep(0, arms), mat)
  colnames(full) <- arm_names
  as_tibble(full) |>
    mutate(batch = 0:periods) |>
    pivot_longer(all_of(arm_names), names_to = "arm", values_to = "value") |>
    mutate(quantity = quantity)
}

gg_df <-
  imap(
    sims,
    function(sim, label) {
      design <- str_extract(label, "adapt_te$|adapt$|stat$")
      bind_rows(
        as_long(sim$ppmat, "posterior_prob"),
        as_long(sim$nmat, "cumulative_n")
      ) |>
        mutate(
          case = str_remove(label, " (adapt_te|adapt|stat)$"),
          design = recode(design,
                          adapt = "Thompson sampling",
                          stat = "Static",
                          adapt_te = "Control-augmented")
        )
    }
  ) |>
  list_rbind() |>
  select(case, design, arm, batch, quantity, value)

write_csv(gg_df, file.path(out_dir, "simulations_illustrative.csv"))

print(gg_df |> filter(batch == periods, quantity == "posterior_prob", arm == "best"))
