#!/usr/bin/env Rscript

library(here)
library(data.table)
#library(lubridate)
#library(readr)
library(gt)
library(gtsummary)

## Recreate tables
REDO_TABLES <- TRUE

## Network
#NETWORK <- "ABLYNET"
NETWORK <- "LPP_CNN"

### INPUT
paths        <- list(
  rds = c(
    "hc-hvr_adj.rds", "cfa_cog-doms.rds", "../arrow/UKBB_covars_all.arrow"
  ),
  scripts = c("adjust_hc-hvr.R", "cfa_cog.R", "parse_covars.R")
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

if (file.exists(paths[["rds"]][3])) {
  covars.dt <- arrow::read_feather(paths[["rds"]][3])
} else {
  source(paths[["scripts"]][3])
}

rm(paths)

### Data CLEANING
edu_years_map <- c(
  "None of the above"                                       = 7,
  "CSEs or equivalent"                                      = 10,
  "O levels/GCSEs or equivalent"                            = 11,
  "NVQ or HND or HNC or equivalent"                         = 12.5,
  "A levels/AS levels or equivalent"                        = 13,
  "Other professional qualifications eg: nursing, teaching" = 14.5,
  "College or University degree"                            = 16,
  "Prefer not to answer"                                    = NA_real_
)

covars.dt <- covars.dt[
  , .(
    EDUC = edu_years_map[EDUC_lv_ses2_0]
  ),
  keyby = "EID"
]

## Labels
rois          <- c(
  ICC = "Intracranial volume",
  HC  = "Hippocampus",
  VC  = "Ventricles"
)
#setnames(
  #c("MEMORY", "PROCSPEED", "REAS_EXFN"),
  #c("Memory", "Proc. speed", "Reasoning & Exec. Fn.")
#)

## Average sides for HC/VC (Unadjusted) & Merge with Cognitive factors
DT.lst <- lapply(
  hc_hvr.lst[[NETWORK]],
  \(crs_lvl) lapply(
    crs_lvl,
    \(mtch_lvl) {
      if ("MATCH" %in% names(mtch_lvl)) {
        group_by_cols <- c("EID", "MATCH", "SEX", "INST", "AGE", "ICC")
      } else {
        group_by_cols <- c("EID", "SEX", "INST", "AGE", "ICC")
      }

      mtch_lvl[
        "NON", on = "ADJ",
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
        merge(lat_cog.dt, all.x = TRUE) |>
        merge(covars.dt, all.x = TRUE) |>
        setcolorder("EDUC", after = "AGE")
    }
  )
)

# No need for Matches in longitudinal sample
DT.lst$LNG <- DT.lst$LNG$ALL

### Group comparisons
ttests.lst <- list()
## Cross-sectional sample
for (s in c("ALL", "MTCH")) {
  ttests.lst[["CRS"]][[s]] <- data.table()
  for (v in names(DT.lst$CRS[[s]][, .SD, .SDcols = AGE:REAS_EXFN])) {
    tt <- DT.lst$CRS[[s]][, t.test(get(v) ~ SEX, na.rm = TRUE)]
    ttests.lst$CRS[[s]] <- rbind(
      ttests.lst$CRS[[s]],
      data.table(
        X = v,
        Tstat = tt$statistic,
        DF = tt$parameter,
        Pval = tt$p.value
      )
    )
  }
}
rm(s, v, tt)

ttests.lst$CRS$ALL
#            X       Tstat       DF         Pval
#       <char>       <num>    <num>        <num>
# 1:       AGE  -14.430151 26368.44 5.052458e-47
# 2:      EDUC   -5.017454 24441.45 5.272765e-07
# 3:       ICC -153.762374 24835.77 0.000000e+00
# 4:        HC  -69.796979 25469.49 0.000000e+00
# 5:        VC  -65.127870 21888.42 0.000000e+00
# 6:    MEMORY    2.797161 25230.50 5.159281e-03
# 7: PROCSPEED    4.818396 25649.45 1.455438e-06
# 8: REAS_EXFN  -11.244064 25011.39 2.911992e-29

ttests.lst$CRS$MTCH
#            X      Tstat       DF         Pval
#       <char>      <num>    <num>        <num>
# 1:       AGE -0.0943091 9835.719 9.248655e-01
# 2:      EDUC -7.5310534 8744.738 5.533626e-14
# 3:       ICC -1.3247331 9832.682 1.852905e-01
# 4:        HC -2.1699098 9823.256 3.003757e-02
# 5:        VC -9.3594985 9739.290 9.787949e-21
# 6:    MEMORY  1.6956856 9171.294 8.997937e-02
# 7: PROCSPEED  4.4054004 9252.927 1.067769e-05
# 8: REAS_EXFN  5.7799520 9182.319 7.715998e-09

## Longitudinal sample
# Calculate time difference (Months)
DT.lst$LNG[
  , TIME_diff := floor((AGE[INST == "ses-3"] - AGE[INST == "ses-2"]) * 12), EID
]
setcolorder(DT.lst$LNG, "TIME_diff", after = "AGE")
# T-tests (Sex differences, baseline)
ttests.lst[["LNG"]] <- data.table()
for (v in names(DT.lst$LNG[, .SD, .SDcols = AGE:REAS_EXFN])) {
  tt <- DT.lst$LNG[!duplicated(EID), t.test(get(v) ~ SEX, na.rm = TRUE)]
  ttests.lst$LNG <- rbind(
    ttests.lst$LNG,
    data.table(
      X = v,
      Tstat = tt$statistic,
      DF = tt$parameter,
      Pval = tt$p.value
    )
  )
}
rm(v, tt)

ttests.lst$LNG
#            X        Tstat       DF          Pval
#       <char>        <num>    <num>         <num>
# 1:       AGE  -4.26555641 2706.902  2.062442e-05
# 2: TIME_diff   0.02015781 2738.359  9.839190e-01
# 3:      EDUC  -2.59035406 2501.240  9.643395e-03
# 4:       ICC -47.99301765 2463.431  0.000000e+00
# 5:        HC -23.47529516 2661.530 6.467080e-111
# 6:        VC -19.39316647 2243.817  1.390400e-77
# 7:    MEMORY   1.23538774 2381.986  2.168081e-01
# 8: PROCSPEED   3.18740987 2505.928  1.453209e-03
# 9: REAS_EXFN  -3.43982952 2473.515  5.917250e-04


### TABLES
tables.lst <- list()
outdir <- here("tables")
if (!file.exists(outdir)) dir.create(outdir)
exts <- c("html", "tex")

## Table 1: Cross-sectional demographics
fpaths <- sprintf("table-1_demog.%s", exts)
if (any(!file.exists(here(outdir, fpaths)), REDO_TABLES)) {
  tables.lst[["TABLE1"]] <- DT.lst$CRS$ALL[
    , .SD, .SDcols = SEX:REAS_EXFN
  ] |>
    melt(id = "SEX", variable = "VAR") |>
    na.omit() |>
    (\(DT) DT[
      , sprintf(
        fifelse(VAR %like% "MEM|PRO|REA", "%.3f (%.2f)", "%.1f (%.1f)"),
        mean(value),
        sd(value)
      ),
      .(SEX, VAR)
    ])() |>
    rbind(
      DT.lst$CRS$ALL[
        , lapply(.SD, \(V) sum(is.na(V))), SEX, .SDcols = AGE:REAS_EXFN
      ] |>
        melt(id = "SEX", variable = "VAR", value = "V1") |>
        (
          \(DT)
          DT[
            V1 > 0,
            .(VAR = sprintf("%s_NA", VAR), V1 = format(V1, big.mark = ",")),
            SEX
          ]
        )()
    ) |>
    dcast(VAR ~ SEX, value.var = "V1") |>
    merge(
      ttests.lst$CRS$ALL[
        , .(
          VAR = X,
          Pval = fifelse(Pval < 0.001, "<0.001", sprintf("%.3f", Pval))
        )
      ],
      by = "VAR",
      all.x = T
    ) |>
    (\(DT) DT[
      ,
      VAR := factor(
        VAR,
        levels = c(
          "AGE",
          "EDUC",
          "EDUC_NA",
          "ICC",
          "HC",
          "VC",
          "MEMORY",
          "MEMORY_NA",
          "PROCSPEED",
          "PROCSPEED_NA",
          "REAS_EXFN",
          "REAS_EXFN_NA"
        ),
        labels = c(
          "Age (years)",
          "Education (years)",
          "NA_EDUC",
          "Head-size (TIV)",
          "Hippocampus (cc)",
          "Lat. ventricles (cc)",
          "Memory",
          "NA_MEM",
          "Processing speed",
          "NA_PRSP",
          "Reasoning & Ex. func.",
          "NA_REXFN"
        )
      )][order(VAR)]
    )() |>
    gt(rowname_col = "VAR", process_md = TRUE) |>
    tab_spanner(label = "Sex", columns = c("Female", "Male")) |>
    tab_options(footnotes.multiline = F, latex.tbl.pos = "h") |>
    cols_align("left", columns = "VAR") |>
    cols_align("center", columns = c("Female", "Male")) |>
    cols_align("right", columns = "Pval") |>
    cols_label(
      Female = sprintf(
        "**Females**, N: %s",
        DT.lst$CRS$ALL["Female", on = "SEX", format(.N, big.mark = ",")]
      ) |> md(),
      Male = sprintf(
        "**Males**, N: %s",
        DT.lst$CRS$ALL["Male", on = "SEX", format(.N, big.mark = ",")]
      ) |> md(),
      Pval = md("$p$")
    ) |>
    tab_stub_indent(starts_with("NA"), indent = 5) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Pval")) |>
    sub_missing(missing_text = "") |>
    sub_values(
      values = c("NA_EDUC", "NA_MEM", "NA_PRSP", "NA_REXFN"),
      replacement = "Missing"
    ) |>
    tab_footnote(
      footnote = "Mean (SD); Independent samples T-test.",
    )

  for (f in fpaths) {
    if (any(!file.exists(here(outdir, f)), REDO_TABLES)) {
      gtsave(tables.lst[["TABLE1"]], f, outdir)
    }
  }
}

## Table 2: Cross-sectional demographics (Matched sample)
fpaths <- sprintf("table-2_demog-mtch.%s", exts)
if (any(!file.exists(here(outdir, fpaths)), REDO_TABLES)) {
  tables.lst[["TABLE2"]] <- DT.lst$CRS$MTCH[
    , .SD, .SDcols = SEX:REAS_EXFN
  ] |>
    melt(id = "SEX", variable = "VAR") |>
    na.omit() |>
    (\(DT) DT[
      , sprintf(
        fifelse(VAR %like% "MEM|PRO|REA", "%.3f (%.2f)", "%.1f (%.1f)"),
        mean(value),
        sd(value)
      ),
      .(SEX, VAR)
    ])() |>
    rbind(
      DT.lst$CRS$ALL[
        , lapply(.SD, \(V) sum(is.na(V))), SEX, .SDcols = AGE:REAS_EXFN
      ] |>
        melt(id = "SEX", variable = "VAR", value = "V1") |>
        (
          \(DT)
          DT[
            V1 > 0,
            .(VAR = sprintf("%s_NA", VAR), V1 = format(V1, big.mark = ",")),
            SEX
          ]
        )()
    ) |>
    dcast(VAR ~ SEX, value.var = "V1") |>
    merge(
      ttests.lst$CRS$MTCH[
        , .(
          VAR = X,
          Pval = fifelse(Pval < 0.001, "<0.001", sprintf("%.3f", Pval)),
          SIGN = Pval < 0.05
        )
      ],
      by = "VAR",
      all.x = T
    ) |>
    (\(DT) DT[
      ,
      VAR := factor(
        VAR,
        levels = c(
          "AGE",
          "EDUC",
          "EDUC_NA",
          "ICC",
          "HC",
          "VC",
          "MEMORY",
          "MEMORY_NA",
          "PROCSPEED",
          "PROCSPEED_NA",
          "REAS_EXFN",
          "REAS_EXFN_NA"
        ),
        labels = c(
          "Age (years)",
          "Education (years)",
          "NA_EDUC",
          "Head-size (TIV)",
          "Hippocampus (cc)",
          "Lat. ventricles (cc)",
          "Memory",
          "NA_MEM",
          "Processing speed",
          "NA_PRSP",
          "Reasoning & Ex. func.",
          "NA_REXFN"
        )
      )][order(VAR)]
    )() |>
    gt(rowname_col = "VAR", process_md = TRUE) |>
    tab_spanner(label = "Sex", columns = c("Female", "Male")) |>
    tab_options(footnotes.multiline = F, latex.tbl.pos = "h") |>
    cols_align("left", columns = "VAR") |>
    cols_align("center", columns = c("Female", "Male")) |>
    cols_align("right", columns = "Pval") |>
    cols_label(
      Female = sprintf(
        "**Females**, N: %s",
        DT.lst$CRS$MTCH["Female", on = "SEX", format(.N, big.mark = ",")]
      ) |> md(),
      Male = sprintf(
        "**Males**, N: %s",
        DT.lst$CRS$MTCH["Male", on = "SEX", format(.N, big.mark = ",")]
      ) |> md(),
      Pval = md("$p$")
    ) |>
    tab_stub_indent(starts_with("NA"), indent = 5) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Pval", rows = SIGN == TRUE)
    ) |>
    cols_hide(columns = contains("SIGN")) |>
    sub_missing(missing_text = "") |>
    sub_values(
      values = c("NA_EDUC", "NA_MEM", "NA_PRSP", "NA_REXFN"),
      replacement = "Missing"
    ) |>
    tab_footnote(
      footnote = "Mean (SD); Independent samples T-test.",
    )

  for (f in fpaths) {
    if (any(!file.exists(here(outdir, f)), REDO_TABLES)) {
      gtsave(tables.lst[["TABLE2"]], f, outdir)
    }
  }
}

## Table 3: Matched demographics
fpaths <- sprintf("table-3_demog-lng.%s", exts)
if (any(!file.exists(here(outdir, fpaths)), REDO_TABLES)) {
  tables.lst[["TABLE3"]] <- DT.lst$LNG[
    !duplicated(EID), .SD, .SDcols = SEX:REAS_EXFN
  ] |>
    melt(id = "SEX", variable = "VAR") |>
    na.omit() |>
    (\(DT) DT[
      , sprintf(
        fifelse(VAR %like% "MEM|PRO|REA", "%.3f (%.2f)", "%.1f (%.1f)"),
        mean(value),
        sd(value)
      ),
      .(SEX, VAR)
    ])() |>
    rbind(
      DT.lst$LNG[
        !duplicated(EID),
        lapply(.SD, \(V) sum(is.na(V))),
        SEX,
        .SDcols = AGE:REAS_EXFN
      ] |>
        melt(id = "SEX", variable = "VAR", value = "V1") |>
        (
          \(DT)
          DT[
            V1 > 0,
            .(VAR = sprintf("%s_NA", VAR), V1),
            SEX
          ]
        )()
    ) |>
    dcast(VAR ~ SEX, value.var = "V1") |>
    merge(
      ttests.lst$LNG[
        , .(
          VAR = X,
          Pval = fifelse(Pval < 0.001, "<0.001", sprintf("%.3f", Pval)),
          SIGN = Pval < 0.05
        )
      ],
      by = "VAR",
      all.x = T
    ) |>
    (\(DT) DT[
      ,
      VAR := factor(
        VAR,
        levels = c(
          "AGE",
          "TIME_diff",
          "EDUC",
          "EDUC_NA",
          "ICC",
          "HC",
          "VC",
          "MEMORY",
          "MEMORY_NA",
          "PROCSPEED",
          "PROCSPEED_NA",
          "REAS_EXFN",
          "REAS_EXFN_NA"
        ),
        labels = c(
          "Age (years)",
          "Follow-up (months)",
          "Education (years)",
          "NA_EDUC",
          "Head-size (TIV)",
          "Hippocampus (cc)",
          "Lat. ventricles (cc)",
          "Memory",
          "NA_MEM",
          "Processing speed",
          "NA_PRSP",
          "Reasoning & Ex. func.",
          "NA_REXFN"
        )
      )][order(VAR)]
    )() |>
    gt(rowname_col = "VAR", process_md = TRUE) |>
    tab_spanner(label = "Sex", columns = c("Female", "Male")) |>
    tab_options(footnotes.multiline = T, latex.tbl.pos = "h") |>
    cols_align("left", columns = "VAR") |>
    cols_align("center", columns = c("Female", "Male")) |>
    cols_align("right", columns = "Pval") |>
    cols_label(
      Female = sprintf(
        "**Females**, N: %s",
        DT.lst$LNG["Female", on = "SEX", format(.N / 2, big.mark = ",")]
      ) |> md(),
      Male = sprintf(
        "**Males**, N: %s",
        DT.lst$LNG["Male", on = "SEX", format(.N / 2, big.mark = ",")]
      ) |> md(),
      Pval = md("$p$")
    ) |>
    tab_stub_indent(starts_with("NA"), indent = 5) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = "Pval", rows = SIGN == TRUE)
    ) |>
    cols_hide(columns = contains("SIGN")) |>
    sub_missing(missing_text = "") |>
    sub_values(
      values = c("NA_EDUC", "NA_MEM", "NA_PRSP", "NA_REXFN"),
      replacement = "Missing"
    ) |>
    tab_footnote(
      footnote = "Showing data for first imaging session.",
    ) |>
    tab_footnote(
      footnote = "Mean (SD); Independent samples T-test.",
    )

  for (f in fpaths) {
    if (any(!file.exists(here(outdir, f)), REDO_TABLES)) {
      gtsave(tables.lst[["TABLE3"]], f, outdir)
    }
  }
}
