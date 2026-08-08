# offer-westort_coppock_green_2021/download_original.R
# Output: original/ (the deposited replication archive, not redistributed in this repo)
# Depends on: original_manifest.csv
# Description: Fetch the deposited archive from Harvard Dataverse and verify every
#   file. Run this once before running anything in maintained/. Re-running is free:
#   files already present with the right checksum are not downloaded again.
#
#   The deposit is 132 MB, almost all of it two saved simulation objects, so the
#   first run takes a few minutes.
#
#   The manifest carries two checksums per file. md5_served is the MD5 of the bytes
#   Dataverse returns for `?format=original`, which is what this code was written
#   against. md5_published is the checksum Dataverse displays. Here all 24 agree,
#   but they do not always: another deposit in this program carries three published
#   checksums that verify neither the original nor the derived tabular file, so
#   verification runs against md5_served and any disagreement is reported.
#
#   Nothing in this deposit was ingested into a tabular representation, so no file
#   carries a UNF and `?format=original` and the plain download return the same
#   bytes. The deposit is flat: no file record carries a directory label. Dataverse
#   returns an empty persistent id for every file in this dataset, so that column
#   is present but blank.

library(tidyverse)
library(here)

here::i_am("download_original.R")

dataset_doi <- "doi:10.7910/DVN/CMUHBU"
base_url <- "https://dataverse.harvard.edu/api/access/datafile"

# Manifest ----
manifest <- read_csv(here::here("original_manifest.csv"), show_col_types = FALSE)

dir.create(here::here("original"), showWarnings = FALSE)

# Download what is missing or wrong ----
# format=original asks for the deposited bytes rather than any representation
# Dataverse derives.
planned <- manifest |>
  mutate(
    path = here::here("original", file),
    url = str_glue("{base_url}/{dataverse_file_id}?format=original"),
    md5_local = unname(tools::md5sum(path)),
    needs_download = is.na(md5_local) | md5_local != md5_served
  )

walk2(
  planned$url[planned$needs_download],
  planned$path[planned$needs_download],
  function(url, path) download.file(url, destfile = path, mode = "wb", quiet = TRUE)
)

print(str_glue("Downloaded {sum(planned$needs_download)} of {nrow(planned)} files; ",
               "{sum(!planned$needs_download)} already present and verified."))

# Verify ----
# md5_served is the MD5 of the bytes `?format=original` returns, and it is the gate.
# md5_published is what Dataverse displays for those same bytes: where the two agree it
# adds nothing a check could fail on, and where they disagree the archive's own metadata
# is wrong and no local copy could satisfy both, so it is reported rather than enforced.
verified <- planned |>
  mutate(
    md5_downloaded = unname(tools::md5sum(path)),
    bytes_on_disk = file.size(path),
    md5_ok = md5_downloaded == md5_served,
    bytes_ok = bytes_on_disk == bytes,
    published_agrees = md5_served == md5_published
  ) |>
  select(file, bytes, bytes_on_disk, bytes_ok, md5_served, md5_downloaded, md5_ok,
         published_agrees)

print(verified |> select(file, bytes, bytes_ok, md5_ok, published_agrees), n = nrow(verified))

if (!all(verified$md5_ok)) {
  stop("Checksum mismatch in original/: ",
       paste(verified$file[!verified$md5_ok], collapse = ", "),
       ". Delete the offending files and re-run to refetch them from Dataverse.")
}

if (!all(verified$bytes_ok)) {
  stop("Byte size mismatch in original/: ",
       paste(verified$file[!verified$bytes_ok], collapse = ", "), ".")
}

# The deposit and nothing else ----
# A name check alone would pass a renamed file, so the check above is by checksum
# and this one catches anything extra sitting in original/. Running the deposited
# scripts writes files into the archive directory, which is why they are run from a
# copy and why this check exists.
# all.files = TRUE is not optional: without it a stray dotfile passes unseen, and a
# deposit can ship dotfiles of its own, which the manifest then has to list.
extra <- setdiff(
  list.files(here::here("original"), recursive = TRUE, all.files = TRUE, no.. = TRUE),
  manifest$file
)
if (length(extra) > 0) {
  stop("original/ holds files the manifest does not list: ",
       paste(extra, collapse = ", "),
       ". Move them elsewhere; original/ is the deposit and only the deposit.")
}

print(str_glue("All {nrow(verified)} files match on MD5 and byte size, and original/ holds ",
               "nothing else. {sum(!verified$published_agrees)} carry a published checksum ",
               "that disagrees with what Dataverse serves."))
print(str_glue("Archive: {dataset_doi}"))
