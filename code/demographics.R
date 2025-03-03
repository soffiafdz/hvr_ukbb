#!/usr/bin/env Rscript

library(here)
library(data.table)
#library(lubridate)
#library(readr)
library(gtsummary)

## Recreate tables
redo_tables <- TRUE


### INPUT
paths        <- list(
  rds = c("hc-hvr_adj.rds", "cog_mcfa_domains.rds"),
  scripts = c("adjust_hc-hvr.R", "mcfa_cog.R")
) |> Map(
  f = function(Files, Dir) here(Dir, Files),
  c("data/rds", "code")
)

if (file.exists(paths[["rds"]][1])) {
  hc_hvr.lst <- readRDS(paths[["rds"]][1])
} else {
  source(paths[["scripts"]][1])
}

if (file.exists(paths[["rds"]][2])) {
  lat_cog.dt <- readRDS(paths[["rds"]][2])
} else {
  source(paths[["scripts"]][1])
}

rm(paths)

### Data CLEANING
## Labels
#rois          <- c(
  #ICC = "Intracranial volume",
  #HC  = "Hippocampus",
  #VC  = "Ventricles"
#)
#setnames(
  #c("MEMORY", "PROCSPEED", "REAS_EXFN"),
  #c("Memory", "Proc. speed", "Reasoning & Exec. Fn.")
#)

## Average sides for HC/VC (Unadjusted) & Merge with Cognitive factors
DT.lst <- hc_hvr.lst |>
  lapply(function(crs_lvl) {
    lapply(crs_lvl$CRS, function(mtch_lvl) {
      group_by_cols <- if ("MATCH" %in% names(mtch_lvl)) {
        c("EID", "MATCH", "SEX", "INST", "AGE", "ICC")
      } else {
        c("EID", "SEX", "INST", "AGE", "ICC")
      }

      mtch_lvl[
        "NON",
        on = "ADJ",
        # Average sides for HC & VC
        lapply(.SD, mean),
        by = group_by_cols,
        .SDcols = names(rois)[2:3]
      ] |>
      # Aggregate ROIs
      setcolorder(c("ICC", "HC", "VC"), after = "AGE") |>
      # Key resulting DT
      setkey(EID, INST) |>
      # Merge with COG factors
      merge(lat_cog.dt, all.x = TRUE)
    })
  })


# Table 1: Cross-sectional demographics
DT.lst[[1]][[1]][, .(SEX, AGE, ICC, HC, VC, MEMORY, PROCSPEED, REAS_EXFN)] |>
  tbl_summary(
    by = SEX,
    label = list(
      AGE ~ "Age (years)",
      ICC ~ "Head-size (TIV)",
      HC ~ "Hippocampus (cc)",
      VC ~ "Ventricules (cc)",
      MEMORY ~ "Memory",
      PROCSPEED ~ "Processing speed",
      REAS_EXFN ~ "Reasoning & Executive function"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing_text = "Missing"
  ) |>
  add_p() |>
  as_gt() |>
  gt::gtsave(filename = here("tables/demog1.html"))


DT.lst[[1]][[2]][, .(SEX, AGE, ICC, HC, VC)] |>
  tbl_summary(
    by = SEX,
    label = list(
      AGE ~ "Age (years)",
      ICC ~ "Head-size (TIV)",
      HC ~ "Hippocampal vol",
      VC ~ "Ventricular vol"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing_text = "Missing"
  ) |>
  add_p() |>
  as_gt() |>
  gt::gtsave(filename = here("tables/demog2.html"))

excl_matches <- DT[
  is.na(MEMORY) | is.na(PROCSPEED) | is.na(REAS_EXFN),
  unique(MATCH)
]


DT.lst[[1]][[2]][
  !MATCH %in% excl_matches,
  .(SEX, AGE, ICC, MEMORY, PROCSPEED, REAS_EXFN)
  ] |>
  tbl_summary(
    by = SEX,
    label = list(
      AGE ~ "Age (years)",
      ICC ~ "Head-size (TIV)",
      MEMORY ~ "Memory",
      PROCSPEED ~ "Processing speed",
      REAS_EXFN ~ "Reasoning & Executive function"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    missing_text = "Missing"
  ) |>
  add_p() |>
  as_gt() |>
  gt::gtsave(filename = here("tables/demog3.html"))
