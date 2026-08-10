# Adaptive Experimental Design: a maintained reproduction


- [What this repository is](#what-this-repository-is)
  - [Layout](#layout)
  - [How to reproduce](#how-to-reproduce)
- [The paper](#the-paper)
- [The deposited archive](#the-deposited-archive)
  - [What it contains, and what it
    writes](#what-it-contains-and-what-it-writes)
  - [Whether it runs](#whether-it-runs)
  - [Whether the numbers reproduce](#whether-the-numbers-reproduce)
- [Errata](#errata)
  - [Two findings that do not belong in an
    errata](#two-findings-that-do-not-belong-in-an-errata)
- [The ground truth](#the-ground-truth)
  - [Float coverage](#float-coverage)
- [The extraction and the two
  instruments](#the-extraction-and-the-two-instruments)
- [The maintained rewrite](#the-maintained-rewrite)
  - [Porting the deposit’s profile
    grid](#porting-the-deposits-profile-grid)
  - [A labelling error in the deposit that changes
    nothing](#a-labelling-error-in-the-deposit-that-changes-nothing)
- [Figure verification](#figure-verification)
- [Rewrite verification](#rewrite-verification)
- [R environment](#r-environment)
- [Licence](#licence)

# What this repository is

A maintained reproduction of Molly Offer-Westort, Alexander Coppock, and
Donald P. Green, “Adaptive Experimental Design: Prospects and
Applications in Political Science,” *American Journal of Political
Science* 65(4): 826–844.

The article’s replication archive is on the Harvard Dataverse. This
repository does not redistribute it. Instead it carries a manifest of
the deposit’s 24 files with their published checksums, a script that
fetches and verifies them, a rewrite of the analysis in current R, a
ground truth laying every published number against what the rewrite and
the deposited code produce, and this report.

*Drafted by Claude Opus 5 under the supervision of Alex Coppock.*

| Resource                   | Link                                 |
|----------------------------|--------------------------------------|
| Article                    | <https://doi.org/10.1111/ajps.12597> |
| Replication archive        | <https://doi.org/10.7910/DVN/CMUHBU> |
| Pre-analysis plan, study 2 | <https://osf.io/6p7ry>               |
| Pre-analysis plan, study 3 | <https://osf.io/xp8mc>               |

## Layout

    download_original.R   fetch the deposit and verify it, on every run
    run_all.R             the whole pipeline, in order
    original/             the deposit (not redistributed; fetched, not committed)
    original_manifest.csv file names, sizes and published checksums
    maintained/           the rewrite: one script per figure, table or in-text quantity
    maintained/output/    everything the rewrite writes, committed so a fresh run can be diffed
    maintained/in_text_claims.R  the second instrument: one block per published number
    ground_truth/         the extraction, the comparison, and the deposit's own output
    README.qmd            this report
    errata.qmd            the sentences in the article the data do not support
    errata_entries.csv    the errata, as data; errata.qmd writes it, this report reads it

## How to reproduce

Download this repository, open `offer-westort_coppock_green_2021.Rproj`,
and run

``` r
source("run_all.R")
```

The first run downloads about 132 MB from the Dataverse, almost all of
it two saved simulation objects. Later runs verify what is already on
disk and download nothing. The whole pipeline took 13.6 minutes on the
machine that produced the committed output,
maintained/text_study2_f_tests.R alone accounting for 3.7 of them.
Timings are not comparable across machines.

# The paper

Response-adaptive designs shift assignment probabilities toward
whichever arm is performing best, on the argument that a multiarm trial
finds its winner faster that way. The article introduces batch-wise
Thompson sampling to political science, proposes a control-augmented
variant that also protects the control arm so the best arm’s treatment
effect can be estimated precisely, and evaluates both against a balanced
static design.

The evidence comes in three layers. Simulations put nine arms through
ten batches of a hundred subjects each under three scenarios: a clear
winner, no clear winner, and a competing second best. Study 1 runs
Thompson sampling on the wording of minimum wage and right-to-work
ballot measures with 1,000 MTurk subjects. Study 2 runs the
control-augmented algorithm on six ways of encouraging survey
respondents to answer factual questions about the economy accurately,
separately among Democrats and Republicans. A third study in the
supplement applies a model-based version to a 192-arm conjoint on
campaign finance.

# The deposited archive

## What it contains, and what it writes

The deposit is 24 files: eleven analysis scripts, a library of shared
functions, four cleaned data files, two saved simulation objects, four
questionnaire codebooks, and a README. It is flat, with no directory
structure, and nothing in it was ingested into a derived tabular
representation, so `?format=original` and the plain download return the
same bytes.

The first thing worth knowing about it is that it writes almost nothing.
Of the 46 calls in the deposited code that touch a file, 40 are
commented out and 6 are live. The README lists 27 output files; the code
produces 6, and three of those four write files the deposit itself
ships. Every table and figure in the article therefore exists in the
deposit only as console output or as a plot drawn to the active device.
That decides how a comparison against it is possible at all, and it is
why `ground_truth/extract_archive_values.R` runs the deposited scripts
and parses what they print rather than reimplementing their formatting.

## Whether it runs

`ground_truth/run_archive.R` runs every deposited script twice: once
against the deposit as downloaded, and once against a copy stripped of
the three objects the deposit’s own code can regenerate. Both passes run
in scratch copies, never in `original/`, which matters here rather than
in principle: `simulations_iterated.R` writes
`simulations_supplement1.rds` and `simulations_supplement2.rds` and
`study_1_rtw-mw_simulations.R` writes `study_1_sims.rds`, and all three
are deposited members.

| Pass | Script | Completed | Stopped at |
|:---|:---|:---|:---|
| As shipped | simulations.R | yes |  |
| As shipped | simulations_iterated.R | no | Error: |
| As shipped | study_1_rtw-mw_analysis.R | yes |  |
| As shipped | study_1_rtw-mw_F.R | yes |  |
| As shipped | study_1_rtw-mw_simulations.R | yes |  |
| As shipped | study_2_misperceptions_probabilities.R | no | Error in file(file, …) : argument “file” is missing, with no default |
| As shipped | study_2_misperceptions_analysis.R | no | Error in readRDS(con, refhook = refhook) : cannot open the connection |
| As shipped | study_2_misperceptions_additional_analysis.R | no | Error in if (is.na(s)) { : the condition has length \> 1 |
| As shipped | study_2_misperceptions_bols.R | yes |  |
| As shipped | study_2_misperceptions_F.R | yes |  |
| As shipped | study_3_conjoint_analysis.R | no | Error in `repaired_names()`: |
| Stripped | simulations.R | yes |  |
| Stripped | simulations_iterated.R | no | Did not finish within 600 seconds. |
| Stripped | study_1_rtw-mw_analysis.R | yes |  |
| Stripped | study_1_rtw-mw_F.R | yes |  |
| Stripped | study_1_rtw-mw_simulations.R | no | Did not finish within 600 seconds. |
| Stripped | study_2_misperceptions_probabilities.R | no | Error in file(file, …) : argument “file” is missing, with no default |
| Stripped | study_2_misperceptions_analysis.R | no | Error in readRDS(con, refhook = refhook) : cannot open the connection |
| Stripped | study_2_misperceptions_additional_analysis.R | no | Error in if (is.na(s)) { : the condition has length \> 1 |
| Stripped | study_2_misperceptions_bols.R | yes |  |
| Stripped | study_2_misperceptions_F.R | yes |  |
| Stripped | study_3_conjoint_analysis.R | no | Error in `repaired_names()`: |

Every deposited script, run as shipped and run from a copy stripped to
data plus code.

6 of 11 scripts complete as shipped. Stripping the saved simulations
costs two more: `simulations_iterated.R` and
`study_1_rtw-mw_simulations.R` both stop trying to regenerate objects
that take hours. The four failures common to both passes are worth
separating, because they are different kinds of problem.

- `study_2_misperceptions_probabilities.R` calls
  `write_rds(pmat, path = 'study_2_probabilities.rds')`. The `path`
  argument was deprecated in readr 1.4.0 and is now an error, so the
  script dies at its only write.
- `study_2_misperceptions_analysis.R` then fails on the first line that
  reads that file. The object it needs exists in no deposited file and
  nothing else creates it, so the analysis behind Figures 5, 6 and F.8
  cannot run from the deposit as it stands.
- `study_2_misperceptions_additional_analysis.R` reaches its
  `stargazer()` call and dies inside it. `stargazer` has not been
  updated since 2022 and its internals no longer work on current R.
- `study_3_conjoint_analysis.R` fails in `as_tibble_row()` with a
  name-repair function that returns 192 names for a one-column input,
  which current tibble rejects.

`simulations_iterated.R` fails in a fifth way that is easy to miss: it
builds and prints all four of its tables and only then dies, in the
figure code below them, on a `facet_grid(facets = )` argument that was
deprecated in ggplot2 2.2.0 and is now defunct. Everything above that
point has already printed, which is why the deposit’s own Table 1 and
appendix table values survive its failure and can be recovered.

## Whether the numbers reproduce

Running is not reproducing, and the two questions have different answers
here. Where the deposited code reaches a published number, it returns
it. `study_1_rtw-mw_F.R` prints the two randomization inference p-values
the study 1 results section reports, 0.429 and 0.199, exactly.
`study_1_rtw-mw_analysis.R` prints the eighteen arm counts behind
Figures 3 and 4, exactly. `study_2_misperceptions_F.R` prints two
p-values that are exactly the two the article reports, but attached to
the opposite parties; that finding is below. The four simulation tables
print six rows each, and every one of those cells agrees with the
article.

# Errata

Three sentences in the article state a number or a treatment label the
data do not support, and one reference-list entry is wrong in two ways
the deposit cannot speak to. Every figure and table is correct as
printed, and no conclusion changes. All four are set out in
`errata.qmd`, which renders to
`offer-westort_coppock_green_2021_errata.pdf` with its corrected values
computed at render time and writes `errata_entries.csv`, the spine this
section counts and numbers from.

1.  **The estimated mean of the leading minimum wage arm.** The article
    gives the winning arm “a probability of being best of 0.219 and an
    estimated mean of 0.895 over 183 respondents”. The arm with a 0.219
    probability and 183 respondents is Proposal 3 (N), whose mean is
    0.865. The 0.895 belongs to Proposal 3 (Y), a different arm. The
    same sentence labels the arm “(B” where Figures 3 and 4 use “(N)”.
2.  **The arm carrying the 4.1-point Republican effect.** The article
    declares “the 4.1-point average effect of the accuracy treatment
    statistically significant (p = 0.014)”. The 4.1-point effect is the
    Lottery arm’s, as the two preceding sentences and Figure 6 both say.
    The Accuracy arm’s Republican estimate is in the opposite direction.
3.  **A share rounded twice.** The article reports picking the best
    minimum wage proposal “37% of the time in static experiments”. The
    simulations give 36.46 per cent, which is 36 at whole-percentage
    precision; rounding to one decimal first and then again produces 37.
4.  **Pallmann et al. (2018) skips its ninth author and runs two words
    of its title together.** A sweep of all 49 printed references
    against Crossref drew three flags; two are Crossref matching an
    entirely different work and this one is real. The entry’s ninth and
    tenth names are the article’s tenth and eleventh authors, Lang’o
    Odondi having been dropped, and the title prints “Why UseThem”
    without the space.

## Two findings that do not belong in an errata

**The two study 2 randomization inference p-values are transposed.** The
article reports p = 0.028 for the Republican trial and p = 0.013 for the
Democratic one. The deposited script `study_2_misperceptions_F.R`
returns 0.028 for the Democrats and 0.013 for the Republicans. The
article’s own F statistics settle which way round is right without
appealing to the deposit at all: the Republican F is the larger of the
two, so the Republican p-value must be the smaller. The values are
correct and the labels are swapped. This stays out of the errata because
a corrected sentence has to print a number, and the corrected numbers
are simulation estimates of 1,000 draws each that the deposit and this
reproduction do not agree on to the third decimal: the reproduction
returns 0.027 where the deposit returns 0.028, and 0.011 where the
deposit returns 0.013. Both readings reject the null at conventional
levels, so no conclusion turns on it.

**The static-design p-value of 0.152 is not reproducible.** The article
compares the Republican Lottery effect under the adaptive design against
a simulated static one and gives the latter a p-value of 0.152. That
figure comes from a bootstrap of 1,000 resamples which the deposit does
not seed reproducibly, and the deposited script that would produce it
stops earlier, on the missing saved object described above. Over five
seeds the p-value runs from 0.076 to 0.117. The estimate and standard
error printed beside it on Figure F.8, 0.040 and 0.025, both reproduce.

# The ground truth

`ground_truth/offer-westort_coppock_green_2021_ground_truth.csv` has
1052 rows. 981 carry a value comparison against the article and 977 of
those agree at the precision the page prints. 29 more carry a truth
value rather than a number, of which 27 hold. Nothing in the file is
typed except the published values, which are read from the article and
the supplement; every other column is computed by
`ground_truth/build_ground_truth.R`, which `run_all.R` runs.

| Claim | Article | Reproduction | Deposit | Locus |
|:---|:---|:---|:---|:---|
| Minimum wage leading arm’s estimated mean | 0.895 | 0.865 |  | paper_internal |
| Share of simulated static minimum wage trials picking the best proposal, per cent | 37 | 36.460 |  | paper_internal |
| Republican Lottery p-value, simulated static design | 0.152 | 0.117 |  | archive |
| The article names the 4.1-point Republican effect as the accuracy treatment’s | 1 | 0.000 |  | paper_internal |
| Republican randomization inference F-test p-value | 0.028 | 0.011 | 0.013 | paper_internal |
| Democratic randomization inference F-test p-value | 0.013 | 0.027 | 0.028 | paper_internal |

Every row on which the article, the deposit and the reproduction do not
agree.

`defect_locus` records where the fault lies, because a disagreement
reads as a failure of the reproduction and here never is. Four of the
six rows are the article disagreeing with its own materials, one is a
bootstrap the deposit cannot support, and one is the transposed pair
described above.

## Float coverage

Every published float gets rows in the ground truth or a stated reason
it cannot. The reason is itself a claim, so “prints no numbers” is
recorded only where the published page’s text layer confirms it.

| Float | Published values | Covered | Share | If uncovered, why |
|:---|---:|---:|:---|:---|
| TABLE 1 | 39 | 39 | 100% |  |
| TABLE 2 | 0 | 0 |  | Study 1 treatment wording. Nothing in the deposit computes it. |
| TABLE 3 | 0 | 0 |  | Study 2 question wording and the official statistics quoted in it. Nothing in the deposit computes them. |
| TABLE 4 | 0 | 0 |  | Study 2 treatment wording. Nothing in the deposit computes it. |
| FIGURE 1 | 10 | 10 | 100% |  |
| FIGURE 2 | 0 | 0 |  | Two views of the same nine simulations as Figure 1; the article states no number from it that Figure 1 does not. |
| FIGURE 3 | 0 | 0 |  | A trajectory plot with no numbers on its face; its endpoints are the posterior probabilities the results section states. |
| FIGURE 4 | 36 | 36 | 100% |  |
| FIGURE 5 | 0 | 0 |  | A trajectory plot with no numbers on its face. |
| FIGURE 6 | 40 | 40 | 100% |  |
| TABLE C1 | 24 | 0 | 0% | An analytic enumeration in the supplement’s worked example. The deposit ships no code for supplement A, B or C. |
| FIGURE D1 | 0 | 0 |  | Plots the cells of Table D.2, which are covered. |
| FIGURE D2 | 0 | 0 |  | Plots the cells of Table D.2, which are covered. |
| TABLE D2 | 255 | 255 | 100% |  |
| FIGURE D3 | 0 | 0 |  | Plots the cells of Table D.3, which are covered. |
| TABLE D3 | 165 | 165 | 100% |  |
| FIGURE D4 | 0 | 0 |  | Plots the cells of Table D.4 and D.5, which are covered, plus the static run at a first batch of 1,000. |
| FIGURE D5 | 0 | 0 |  | Plots the cells of Table D.4 and D.5, which are covered, plus the static run at a first batch of 1,000. |
| TABLE D4 | 150 | 150 | 100% |  |
| TABLE D5 | 150 | 150 | 100% |  |
| TABLE E6 | 50 | 0 | 0% | Fifty state minimum wage rates transcribed from an external source. The deposited data carry no wage field. |
| FIGURE F6 | 20 | 20 | 100% |  |
| FIGURE F7 | 20 | 20 | 100% |  |
| FIGURE F8 | 40 | 40 | 100% |  |
| TABLE F7 | 64 | 64 | 100% |  |
| TABLE G8 | 0 | 0 |  | Conjoint attribute wording. The level counts it implies are covered separately. |
| FIGURE G9 | 0 | 0 |  | A trajectory plot with no numbers on its face. |
| FIGURE G10 | 0 | 0 |  | A trajectory plot with no numbers on its face; its endpoints are the six probabilities the supplement’s results state. |

Published values per float and the share the ground truth reaches.

989 of the 1063 numbers the article and supplement print inside floats
are covered, across 12 of 14 floats that print any. The two exceptions
are Table C.1, an analytic enumeration in a worked example the deposit
ships no code for, and Table E.6, fifty state minimum wage rates
transcribed from a source the table names.

Five of the covered floats are figures that print an estimate and a
standard error beside every point, which makes them published tables in
disguise: Figures 4 and 6 in the article, whose published pages have no
text layer at all and whose 76 values were read off rendered pages, and
Figures F.6, F.7 and F.8 in the supplement.

# The extraction and the two instruments

`ground_truth/published_claims.csv` is the extraction: 293 numeric
claims the article and supplement make, each classified by hand after an
exhaustive scan of both documents for numeric tokens and a separate pass
for numbers written as words. 251 of them require a claim block.

| Claim type   | Needs a block | Claims |
|:-------------|:--------------|-------:|
| definitional | FALSE         |     33 |
| definitional | TRUE          |     21 |
| descriptive  | TRUE          |      5 |
| pipeline     | TRUE          |    224 |
| structural   | FALSE         |      4 |
| structural   | TRUE          |      1 |
| transcribed  | FALSE         |      5 |

The extraction, by claim type.

The claims that need no block are those no part of the pipeline can
reach: payments, field dates, treatment wording, the analytic worked
examples in supplement C, and the two conjoint sample sizes, since the
deposit ships study 3 as batch-level fitted data rather than a
respondent file. Each carries its reason in the file.

Two instruments read the same pipeline output by separate paths.
`ground_truth/build_ground_truth.R` assembles the comparison.
`maintained/in_text_claims.R` prints one line per claim beside the
sentence it belongs to, with the article’s own words in a block comment
above the code. Neither reads the other’s result, and where they
disagree one of them is wrong.

The coverage gate at the end of the build runs `in_text_claims.R` as a
program in its own environment, captures what it prints, and asserts
four things: that the number of claims printed equals the number the
extraction requires, that the two sets of identifiers are equal in both
directions, that every printed value matches the build’s independently
derived one, and that the published string re-rendered at each claim’s
recorded precision is the string the extraction stores. Both files look
that precision up in the extraction, so neither can name a different
one, which is what makes the value comparison a check rather than an
identity.

# The maintained rewrite

`maintained/` is 32 scripts: a shared `helpers.R`, one per published
figure and table, and one per group of in-text quantities. The deposit’s
`utils.R` is reproduced in `helpers.R` rather than sourced, because it
loads its own packages and resolves paths relative to the working
directory, and every random-number call is kept in the deposit’s order
so seeded results stay comparable.

The substitutions the rewrite makes:

| Deposited | Maintained | Why |
|----|----|----|
| `%>%` | `\|>` | Native pipe |
| `gather()`, `reshape2::melt()` | `pivot_longer()`, an explicit long-form helper | Superseded |
| `geom_errorbarh(height = 0)` | `geom_linerange()` | Deprecated in ggplot2 4.0.0 |
| `position_dodgev()` | `position_dodge()` | Current ggplot2 dodges along a discrete axis without help |
| `write_rds(path = )` | `write_rds(file = )` | `path` is an error under readr 2.2.0 |
| `stargazer()` | `modelsummary()` | `stargazer` fails outright on current R |
| `facet_grid(facets = )` | `facet_grid(rows ~ cols)` | Defunct since ggplot2 2.2.0 |
| `as_tibble_row(.name_repair = )` | assignment into the row | The repair function no longer matches a one-column input |

Two of these are not cosmetic. `study_2_misperceptions_probabilities.R`
cannot run at all under current readr, so
`maintained/study2_probabilities.R` is what makes the study 2 analysis
reachable; it takes about 2 minutes at 10,000 Bayesian bootstrap draws.
And `study_3_conjoint_analysis.R` cannot run at all under current
tibble, so `maintained/study3_probabilities.R` is what makes Figures G.9
and G.10 reachable.

## Porting the deposit’s profile grid

Study 3’s 192 conjoint profiles are built in the deposit with
`expand.grid()`, and the model matrix that indexes them decides which
posterior probability belongs to which profile. `expand.grid()` varies
its first argument fastest and `tidyr::expand_grid()` varies its last,
so a naive port transposes the grid: the numbers come out identical and
land on the wrong profiles, which no comparison against the estimates
can see. `maintained/study3_probabilities.R` supplies the four
attributes in reverse and puts the columns back, and the resulting
profile order and model matrix are identical to the deposit’s, row for
row.

What confirms it is the supplement’s own results. The top three adaptive
profiles reproduce at 0.338, 0.213 and 0.076, and the top three static
profiles at 0.194, 0.161 and 0.140, each carrying the attribute levels
the supplement describes. A transposition would have moved every one of
them.

## A labelling error in the deposit that changes nothing

`study_2_misperceptions_F.R` renames the treatment levels to
`lottery, accuracy, control, google, direction, extratime` and then asks
the control-augmented algorithm to protect position 3. The levels in the
data are `control, lottery, accuracy, google, direction, extratime`, so
position 3 is the accuracy arm rather than the control. Under the sharp
null the test is built against, every arm is exchangeable, so which
index the algorithm protects changes the random stream and nothing else.
`maintained/text_study2_f_tests.R` computes both and writes both.

# Figure verification

Every figure script writes a CSV of the estimates it plots, so a fresh
run can be diffed against the committed one without comparing images.
Each rendered figure was also laid beside the published page, which is
the only check that catches a plot drawing the right numbers in the
wrong places.

<div id="fig-3">

![](maintained/output/figure_3_study1_posterior_probs.png)

Figure 1: Study 1, over time posterior probabilities (Figure 3).

</div>

<div id="fig-g9">

![](maintained/output/figure_g9_study3_by_attribute.png)

Figure 2: Study 3, over time posterior probabilities by attribute
(Figure G.9). This is the figure a transposed profile grid would have
scrambled.

</div>

Two differences from the published figures are deliberate. Appendix
Figures D.4 and D.5 carry an eleventh point at a first batch of 1,000,
where the whole sample sits in the first batch and there is nothing left
to adapt on; the deposit binds its static run in at that position under
both algorithm labels, and the rewrite does the same. Figure 6’s
covariate-adjusted series is drawn with a distinct shape as well as a
distinct shade, which the published version does not do.

# Rewrite verification

`run_all.R` runs the pipeline end to end, ending with a second
verification of `original/`. The deposited archive is checked on every
run, before anything else and again afterwards, against the published
MD5 and the recorded byte size of all 24 files, and `original/` is
asserted to hold nothing the manifest does not list. All 24 published
checksums agree with the bytes the Dataverse serves, so the two
checksums the archive records are one claim rather than two.

| Script                                       | Minutes |
|:---------------------------------------------|:--------|
| maintained/text_study2_f_tests.R             | 3.7     |
| ground_truth/extract_archive_values.R        | 3.1     |
| maintained/study3_probabilities.R            | 2.2     |
| maintained/study2_probabilities.R            | 2.1     |
| maintained/figure_f8_simulated_comparisons.R | 1.2     |
| maintained/text_study1_f_tests.R             | 1.0     |
| maintained/text_study2_batched_ols.R         | 0.2     |
| maintained/simulations_illustrative.R        | 0.0     |

The eight slowest steps of one full run.

`maintained/output/` is committed, so a reader can run the pipeline and
diff the result. Everything except the figure PDFs comes back
byte-identical; a PDF records the time it was written, so its bytes
change on every run and its checksum is not a stable fact about the
analysis.

# R environment

| Component    | Version |
|:-------------|:--------|
| R            | 4.6.0   |
| tidyverse    | 2.0.0   |
| estimatr     | 1.0.6   |
| ggplot2      | 4.0.3   |
| MCMCpack     | 1.7.1   |
| bandit       | 0.5.1   |
| modelsummary | 2.6.0   |

The environment the committed output was produced in.

The analysis is seeded throughout at the deposit’s own seed, 95126.
Three quantities are nonetheless not bit-reproducible against the
deposit and are reported as such above rather than presented as
agreement.

# Licence

CC0 1.0, matching the deposit.
