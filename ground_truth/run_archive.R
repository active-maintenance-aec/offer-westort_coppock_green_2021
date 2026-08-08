# offer-westort_coppock_green_2021/ground_truth/run_archive.R
# Output: ground_truth/archive_run_status.csv, ground_truth/archive_write_calls.csv
# Depends on: the deposited archive (original/, or ARCHIVE_RUN_DIR)
# Description: Run every script the deposit ships, twice: once against the deposit
#   as it was downloaded, and once against a copy stripped to data plus code.
#   Record for each script whether it completed and, if not, where it stopped.
#   Also record every call in the deposit that writes to disk, commented or not,
#   and which deposited files a run overwrites.
#
#   Both passes run in scratch copies under tempdir() and never in original/ itself.
#   That matters here rather than in principle. Three of the deposit's four
#   uncommented write calls name deposited files: simulations_iterated.R writes
#   simulations_supplement1.rds and simulations_supplement2.rds, and
#   study_1_rtw-mw_simulations.R writes study_1_sims.rds. Each sits behind an
#   if (file.exists(...)) guard, so a run in place overwrites part of the deposit
#   only once the file has been removed, which is exactly what the stripped pass
#   does.
#
#   The stripped pass removes those three saved simulation objects, which are the
#   deposit's only derived files: the other four .rds files are the cleaned survey
#   data. A script that loads a shipped simulation rather than computing one can
#   finish either way, and the stripped pass is what separates those two outcomes.
#
#   Scripts run one at a time in the order the deposit's README gives, each in its
#   own R process. Sequential rather than parallel because the deposit is not a set
#   of independent scripts: study_2_misperceptions_analysis.R reads
#   study_2_probabilities.rds, which no deposited file contains and which
#   study_2_misperceptions_probabilities.R writes. Running them in isolation would
#   record that dependency as a failure when the README documents it.

library(tidyverse)
library(here)

here::i_am("ground_truth/run_archive.R")

archive_dir <- Sys.getenv("ARCHIVE_RUN_DIR", unset = here::here("original"))

stopifnot(file.exists(file.path(archive_dir, "utils.R")))

# README order. utils.R is the deposit's library file: it defines functions, loads
# packages and produces nothing, so it is sourced by the others rather than run.
script_names <- c(
  "simulations.R",
  "simulations_iterated.R",
  "study_1_rtw-mw_analysis.R",
  "study_1_rtw-mw_F.R",
  "study_1_rtw-mw_simulations.R",
  "study_2_misperceptions_probabilities.R",
  "study_2_misperceptions_analysis.R",
  "study_2_misperceptions_additional_analysis.R",
  "study_2_misperceptions_bols.R",
  "study_2_misperceptions_F.R",
  "study_3_conjoint_analysis.R"
)

stopifnot(all(file.exists(file.path(archive_dir, script_names))))

# The deposit's own derived objects ----
# Everything the deposit's code can regenerate. Removing them is what makes the
# stripped pass a test rather than a repetition of the first one.
derived_files <- c(
  "simulations_supplement1.rds",
  "simulations_supplement2.rds",
  "study_1_sims.rds"
)

# Write calls in the deposited code ----
# Commented calls are recorded too, and here that is the point rather than a
# nicety: the deposit's README lists 27 output files and the code writes four,
# because almost every write is commented out. An archive that writes nothing
# produces no artifact anyone could diff against.
write_call_pattern <- str_c(
  "write_rds|saveRDS|write\\.csv|write_csv|write\\.table|ggsave|pdf\\(|png\\(",
  "|sink\\(|writeLines|write_lines|file\\.copy|unlink|stargazer\\(|save\\("
)

write_calls <-
  file.path(archive_dir, c(script_names, "utils.R")) |>
  set_names(c(script_names, "utils.R")) |>
  map(function(path) tibble(line = seq_along(read_lines(path)), text = read_lines(path))) |>
  list_rbind(names_to = "script") |>
  filter(str_detect(text, write_call_pattern)) |>
  mutate(text = str_trim(text), commented = as.integer(str_starts(text, "#"))) |>
  select(script, line, commented, text)

write_csv(write_calls, here::here("ground_truth", "archive_write_calls.csv"))

# Two passes ----
prepare_copy <- function(label, strip) {
  destination <- file.path(tempdir(), str_c("owcg_archive_", label))
  unlink(destination, recursive = TRUE)
  dir.create(destination, recursive = TRUE)
  file.copy(
    list.files(archive_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    destination, recursive = TRUE
  )
  if (strip) unlink(file.path(destination, derived_files))
  destination
}

# exec matters. Without it the shell stays alive as the parent, the timeout kills
# the shell, and the R process it launched carries on as an orphan for as long as
# it likes. With it the shell is replaced by R, so the process the timeout can
# reach is the one doing the work.
script_timeout_seconds <- 600

run_one <- function(directory, script) {
  command <- str_glue(
    "cd {shQuote(directory)} && exec Rscript -e {shQuote(str_glue('source(\"{script}\", echo = FALSE)'))} 2>&1"
  )
  started <- Sys.time()
  output <- suppressWarnings(system(command, intern = TRUE, timeout = script_timeout_seconds))
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  status <- attr(output, "status")
  first_error <- output[str_detect(output, "^Error")][1]
  timed_out <- !is.null(status) && status == 124
  print(str_glue("{script}: {round(elapsed, 1)} seconds, ",
                 "{if (is.null(status) || status == 0) 'completed' else 'stopped'}"))
  tibble(
    script = script,
    completed = as.integer(is.null(status) || status == 0),
    # The timeout is tested before the error text. A script killed part way through
    # prints an error of its own on the way down, so taking the first error line
    # would record an interrupted worker as the reason it stopped and hide the fact
    # that it was still running when it was killed.
    stopped_at = case_when(
      timed_out ~ str_glue("Did not finish within {script_timeout_seconds} seconds."),
      !is.na(first_error) ~ str_trunc(first_error, 200),
      .default = NA_character_
    )
  )
}

run_pass <- function(label, strip) {
  directory <- prepare_copy(label, strip)
  before <- file.info(list.files(directory, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  status <- map(script_names, function(s) run_one(directory, s)) |> list_rbind()
  after <- file.info(list.files(directory, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  touched <- rownames(before)[
    !is.na(after[rownames(before), "mtime"]) &
      before$mtime != after[rownames(before), "mtime"]
  ]
  added <- setdiff(rownames(after), rownames(before))
  list(
    status = status |> mutate(pass = label, .before = 1),
    overwritten = basename(touched),
    added = basename(added)
  )
}

as_shipped <- run_pass("as_shipped", strip = FALSE)
stripped <- run_pass("stripped", strip = TRUE)

archive_run_status <- bind_rows(as_shipped$status, stripped$status)

write_csv(archive_run_status, here::here("ground_truth", "archive_run_status.csv"))

# A measured zero needs its explanation. "Nothing was overwritten" and "every
# script failed before reaching a write" are the same number and opposite
# findings, so the completion counts are printed beside the overwrite counts.
print(str_glue(
  "Deposited scripts run: {length(script_names)}. ",
  "Completed as shipped: {sum(as_shipped$status$completed)}. ",
  "Completed stripped: {sum(stripped$status$completed)}. ",
  "Write calls in the deposit: {nrow(write_calls)}, of which ",
  "{sum(write_calls$commented == 0)} are uncommented. ",
  "Deposited files a run as shipped overwrites: ",
  "{length(as_shipped$overwritten)} ({str_c(as_shipped$overwritten, collapse = ', ')}). ",
  "Deposited files a stripped run overwrites: ",
  "{length(stripped$overwritten)} ({str_c(stripped$overwritten, collapse = ', ')}). ",
  "Files a run as shipped adds: ",
  "{length(as_shipped$added)} ({str_c(as_shipped$added, collapse = ', ')})."
))
