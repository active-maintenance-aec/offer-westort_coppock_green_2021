# offer-westort_coppock_green_2021/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: the deposited archive (original/, or ARCHIVE_RUN_DIR)
# Description: Recover what the deposited code itself computes, by running it and
#   parsing what it prints. The deposit writes almost nothing to disk: of the 48
#   calls in it that touch a file, 44 are commented out, so its tables and figures
#   exist only as console output and on-screen plots. Reconstructing that output
#   from the deposit's data would mean reimplementing its formatting, and a
#   reimplementation is a guess about the deposit rather than a reading of it.
#   Running the scripts and parsing their stdout is a reading.
#
#   Two details make the parse possible at all. R does not auto-print under
#   source() unless print.eval is set, so a plain source() of these scripts is
#   silent and a harness that only records exit status sees nothing; every run
#   here passes print.eval = TRUE. And the deposit prints its four simulation
#   tables through head(), so six rows of each reach the console and the rest
#   never exist anywhere. Those 24 rows are what the deposit can support.
#
#   Everything runs in a scratch copy, never in original/. Three of the deposit's
#   four live write calls name deposited files.

library(tidyverse)
library(here)

here::i_am("ground_truth/extract_archive_values.R")

archive_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = here::here("original"))
stopifnot(file.exists(file.path(archive_dir, "utils.R")))

run_dir <- file.path(tempdir(), "owcg_archive_values")
unlink(run_dir, recursive = TRUE)
dir.create(run_dir, recursive = TRUE)
file.copy(list.files(archive_dir, full.names = TRUE), run_dir)

run_script <- function(script) {
  command <- str_glue(
    "cd {shQuote(run_dir)} && exec Rscript -e ",
    "{shQuote(str_glue('source(\"{script}\", print.eval = TRUE)'))} 2>&1"
  )
  started <- Sys.time()
  output <- suppressWarnings(system(command, intern = TRUE, timeout = 1800))
  print(str_glue("{script}: {round(as.numeric(difftime(Sys.time(), started, units = 'secs')), 1)} seconds"))
  output
}

# A printed tibble is a header line, a types line, then one line per row, each
# beginning with its row number. Values are the numeric tokens after that number.
parse_tibble <- function(lines, after, columns) {
  start <- which(str_detect(lines, fixed(after)))[1]
  stopifnot(!is.na(start))
  body <- lines[(start + 2):(start + 2 + 40)]
  # The body ends at the first line that is not a numbered row, so the window is
  # cut there rather than filtered: a later tibble in the same log would otherwise
  # be swept into this one.
  numbered <- str_detect(body, "^\\s*[0-9]+\\s")
  body <- body[seq_len(which(!numbered)[1] - 1)]
  rows <- map(body, function(ln) {
    tokens <- str_extract_all(str_remove(ln, "^\\s*[0-9]+\\s"), "-?[0-9]+\\.?[0-9]*|NA")[[1]]
    if (length(tokens) < length(columns)) return(NULL)
    set_names(as.list(head(tokens, length(columns))), columns)
  })
  rows |> compact() |> map(as_tibble) |> list_rbind()
}

archive_values <- list()

# simulations_iterated.R: Table 1 and appendix Tables D.2 to D.5 ----
# The script reads the two deposited simulation objects rather than re-running
# them, builds each table, prints head() of it, and then dies on a defunct
# facet_grid(facets =) argument in the figure code below the tables. Everything
# above that point has already printed, which is why the tables survive the
# failure.
sim_log <- run_script("simulations_iterated.R")

table_heads <- list(
  table_1 = list(after = "algorithm  case correct rmse_best rmse_te cov_best cov_te",
                 columns = c("case", "correct", "rmse_best", "rmse_te", "cov_best", "cov_te")),
  table_d2 = list(after = "algorithm  case batches correct rmse_best rmse_te cov_best cov_te",
                  columns = c("case", "batches", "correct", "rmse_best", "rmse_te",
                              "cov_best", "cov_te")),
  table_d4 = list(after = "algorithm  case first_batch correct rmse_best rmse_te cov_best cov_te",
                  columns = c("case", "first_batch", "correct", "rmse_best", "rmse_te",
                              "cov_best", "cov_te"))
)

# Table D.3's printed head repeats Table 1's column names, so it is taken as the
# second occurrence rather than the first; its "case" column holds the best arm's
# success rate in hundredths.
d3_starts <- which(str_detect(sim_log, fixed(table_heads$table_1$after)))
stopifnot(length(d3_starts) == 2)

# Table 1's printed head is three Thompson sampling rows then three static ones;
# the other two heads are the first six rows of a single algorithm's block.
head_algorithms <- list(table_1 = rep(c("TS", "Static"), each = 3),
                        table_d2 = rep("TS", 6),
                        table_d4 = rep("TS", 6))

for (nm in names(table_heads)) {
  parsed <- parse_tibble(sim_log, table_heads[[nm]]$after, table_heads[[nm]]$columns)
  stopifnot(nrow(parsed) == 6)
  archive_values[[nm]] <- parsed |>
    mutate(algorithm = head_algorithms[[nm]], table_figure = nm, .before = 1)
}

d3 <- parse_tibble(sim_log[d3_starts[2]:length(sim_log)],
                   table_heads$table_1$after, table_heads$table_1$columns) |>
  rename(best_arm_value_hundredths = case) |>
  mutate(algorithm = "TS", table_figure = "table_d3", .before = 1)
stopifnot(nrow(d3) == 6)
archive_values$table_d3 <- d3

# study_1_rtw-mw_analysis.R: the arm counts behind Figures 3 and 4 ----
counts_log <- run_script("study_1_rtw-mw_analysis.R")
arm_counts <- function(lines, header) {
  start <- which(str_detect(lines, fixed(header)))[1]
  stopifnot(!is.na(start))
  body <- lines[(start + 2):(start + 13)]
  body <- body[str_detect(body, "Proposal")]
  tibble(
    arm = str_extract(body, "Proposal [0-9] \\([A-Z]+\\)"),
    n = str_extract(body, "[0-9]+\\s*$") |> str_trim()
  )
}
archive_values$study_1_counts <- bind_rows(
  `Right-to-work` = arm_counts(counts_log, "Z_rtw               n"),
  `Minimum wage` = arm_counts(counts_log, "Z_mw               n"),
  .id = "topic"
) |>
  mutate(table_figure = "figure_3", .before = 1)

# study_1_rtw-mw_F.R and study_2_misperceptions_F.R: the four RI p-values ----
# Both print a base data frame rather than a tibble, so the F statistic and the
# p-value sit on the row line with no types line above them.
parse_f_frame <- function(lines) {
  body <- lines[str_detect(lines, "^[0-9]+\\s+[0-9]+\\.[0-9]+\\s+[0-9.]+\\s+\\w+\\s*$")]
  tibble(
    f_statistic = str_extract(body, "(?<=^[0-9]{1,2} )\\s*[0-9]+\\.[0-9]+") |> str_trim(),
    p_value = str_split_i(str_squish(body), " ", 3),
    study = str_split_i(str_squish(body), " ", 4)
  )
}

archive_values$study_1_f <- parse_f_frame(run_script("study_1_rtw-mw_F.R")) |>
  mutate(table_figure = "text_study1_f_tests", .before = 1)
archive_values$study_2_f <- parse_f_frame(run_script("study_2_misperceptions_F.R")) |>
  mutate(table_figure = "text_study2_f_tests", .before = 1)

out <- archive_values |>
  list_rbind(names_to = "source_block") |>
  pivot_longer(
    -c(source_block, table_figure, algorithm, topic, study, arm, case,
       batches, first_batch, best_arm_value_hundredths),
    names_to = "statistic", values_to = "value_script"
  ) |>
  filter(!is.na(value_script), value_script != "NA") |>
  select(table_figure, source_block, algorithm, topic, study, arm, case, batches,
         first_batch, best_arm_value_hundredths, statistic, value_script)

write_csv(out, here::here("ground_truth", "archive_values.csv"))

print(out |> count(table_figure))
print(str_glue("{nrow(out)} values recovered from the deposit's own console output."))
