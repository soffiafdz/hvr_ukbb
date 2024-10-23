#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)
library(lubridate)
library(progress)

### Rerun matching algorithm
REMATCH     <- FALSE


### IN
fpaths      <- here("data", c("ukbb_lng_icc_scale.csv",
                              "hcvcag_segmentations.csv",
                              "arrow/UKBB_assemblynet_20240521.arrow",
                              "arrow/UKBB_covars_all.arrow",
                              "rds/qc_darq-assemblynet.rds"))

for (fpath in fpaths[1:3]) {
  if (!file.exists(fpath)) {
    sprintf("File: %s is required but could not be found.", fpath) |> stop()
  }
}

## ICC volume and ScaleFactors
icc_scl.dt  <- fread(fpaths[1], col.names = c("EID", "INST", "ICC", "SCALE"))

## Segmentations
vols.dt     <- fread(fpaths[2])

## AssemblyNet
asblynet.dt <- arrow::read_feather(fpaths[3]) |> setDT()

## Covariates
if (file.exists(fpaths[4])) {
  covars.dt <- arrow::read_feather(fpaths[4]) |> setDT()
} else {
  here("code/parse_covars.R") |> source()
}

## QC data
if (file.exists(fpaths[5])) {
  qc.dt     <- readRDS(fpaths[5])
} else {
  here("code/filter_qc.R") |> source()
}
rm(fpaths)


### Data CLEANING
## AssemblyNet
# Keep only: IC, HC l/r &
# Cols: subject - 1, visit - 2, scale - 6, icc - 23,
# hcr - 101, hcl - 103, inf-lat-vcr - 521, inf_lat-vcl - 523
# SCALE_asbly-net & SCALE_pipeline: r = -0.85
asbly.cols  <- names(asblynet.dt)[c(1:2, 23, 103, 101, 523, 521)]
asblynet.dt <- asblynet.dt[, ..asbly.cols] |>
setnames(asbly.cols, c("EID", "INST", "ICC", "HC_l", "HC_r", "VC_l", "VC_r"))
asblynet.dt[, EID := as.integer(str_remove(EID, "sub-"))]
asblynet.dt[, SEGM := "ABLYNET"]
setkey(asblynet.dt, EID, INST)
roi.cols    <- grep("(r|l)$", names(asblynet.dt), value = TRUE)
rm(asbly.cols)


## CNN volumes
# Parse HC/VC subfields into whole ROI
vols.dt     <- vols.dt[,
                       .(EID  = as.integer(str_extract(id, "(?<=sub-)\\d*")),
                         INST = str_extract(id, "ses-\\d"),
                         HC_l = (l_hc_t + l_hc_b + l_hc_h) / 1000,
                         HC_r = (r_hc_t + r_hc_b + r_hc_h) / 1000,
                         VC_l = (l_vc_t + l_vc_b + l_vc_h) / 1000,
                         VC_r = (r_vc_t + r_vc_b + r_vc_h) / 1000)] |>
setkey(EID, INST)

# Merge ICC/Scale & volumes; bring back to subject space
icc_scl.dt[, EID := as.integer(str_remove(EID, "sub-"))]
icc_scl.dt[, ICC := ICC / 1000]
setkey(icc_scl.dt, EID, INST)
cnn.dt      <- icc_scl.dt[vols.dt, nomatch = NULL]
cnn.dt[, (roi.cols) := .SD / SCALE, .SDcols = roi.cols]
cnn.dt[, SCALE := NULL]
cnn.dt[, SEGM := "LPP_CNN"]
rm(roi.cols, vols.dt, icc_scl.dt)

## Clinical history
# Filter out people with F, G or Q0 dx in ICD10 and bad QC
icd_10.dt   <- covars.dt[ICD_10 %like% "F|G|Q0", .(EID)]
cnn.dt      <- cnn.dt[!icd_10.dt, on = "EID"]
asblynet.dt <- asblynet.dt[!icd_10.dt, on = "EID"]
rm(icd_10.dt)

## Quality control
# One subject slipped through
cnn.dt      <- cnn.dt[!qc.dt[DARQ < .25]][EID != 4873046]
asblynet.dt <- asblynet.dt[!qc.dt[ASBLYNET != "A"]]
rm(qc.dt)

# TODO: Perform visual inspection on CNN
# Meanwhile, exclude outliers (HC|VC < 3 Z-scores)
# 3 SD seems to be enough to take out catastrophic cases
# Two-directions? I am only excluding *small* HC or VC, not larger
cnn.l.dt    <- melt(cnn.dt, measure = patterns("HC|VC"))
cnn.l.dt[, Z := scale(value), variable]
cnn.l.dt[Z < -3, value := NA]
cnn.dt      <- cnn.l.dt[, -"Z"] |>
dcast(... ~ variable, value.var = "value") |>
na.omit() |>
setkey(EID, INST)
rm(cnn.l.dt)

## Keep UNION of segmentation methods
hcvc.dt     <- rbind(cnn.dt[asblynet.dt[, 1:2], nomatch = NULL],
                     asblynet.dt[cnn.dt[, 1:2], nomatch = NULL],
                     use.names = TRUE) |>
setcolorder("SEGM", after = "INST") |>
setkey(EID, INST)
rm(cnn.dt, asblynet.dt)

## AGE & SEX for group matching
# Obtain age (years measured at imaging session) and sex data
age_sex.dt  <-
  #covars.dt[!is.na(DATE_mri_ses2) | !is.na(DATE_mri_ses3),
            #.(BIRTH_my = my(sprintf("%i-%i", as.numeric(BIRTH_m), BIRTH_y)),
              #DATE_mri_ses2, DATE_mri_ses3),
            #.(EID, SEX = SEX_r)] |>
  covars.dt[!is.na(DATE_mri_ses2) | !is.na(DATE_mri_ses3),
            .(BIRTH_m, BIRTH_y, DATE_mri_ses2, DATE_mri_ses3),
            .(EID, SEX = SEX_r)
            ][, BIRTH_my := my(sprintf("%i-%i", as.numeric(BIRTH_m), BIRTH_y))
            ] |>
melt(measure = patterns("DATE"), variable = "INST") |>
{function(DT)
  DT[, INST := fifelse(INST %like% 2, "ses-2", "ses-3")
     ][, value := str_extract(value, "^[^ ]+")
     ][, AGE := as.duration(ymd(value) - BIRTH_my) / dyears(1)
     ][, c("BIRTH_m", "BIRTH_y", "BIRTH_my", "value") := NULL]}() |>
na.omit() |>
setkey(EID, INST)
rm(covars.dt)

### SUBCOHORTS
# Which ICC measure to use: Longitudinal Pipeline or Assemblynet?
# They are highly correlated: r = .986, should make no difference
icc.measure <- "LPP_CNN"

## Cross-sectional cohort
# When subjects have both sessions, use the first one.
crs.dt      <- hcvc.dt[icc.measure, on = "SEGM", .(EID, INST, ICC)
                       ][!duplicated(EID)
                       ][age_sex.dt, nomatch = NULL] |>
setcolorder("SEX", after = "EID")

# Explore SDs to get an idea for distances in algorithm
crs.dt[, lapply(.SD, sd), .SDcols = AGE:ICC, by = SEX]

## Longitudinal cohort
# Subjects with both imaging sessions
lng.dt      <- hcvc.dt[icc.measure, on = "SEGM", .(EID, INST, ICC)
                       ][, if (.N == 2) .SD, EID
                       ][age_sex.dt, on = .(EID, INST), nomatch = NULL] |>
setcolorder("SEX", after = "EID") |>
setkey(EID, INST)

## Save EIDs for easier parsing later?
#lng.subs    <- lng.dt[!duplicated(EID), .(EID)]
#setkey(lng.subs, EID)

# Explore SDs to get an idea for distances in algorithm
lng.dt[, lapply(.SD, sd), .SDcols = AGE:ICC, by = SEX]

## Matching algorithm
# Matching women -> men
# Fewer men :: CS 12K < 15K & LNG 2.5K < 3.4K
# TODO: Use between sessions time for matching longitudinal data??
fpath       <- here("data/rds/sex-age-icc_matching.rds")
if (all(!REMATCH, file.exists(fpath))) {
  match.lst <- readRDS(fpath)
} else {
  # Random seed
  set.seed(1618)

  # Distances
  dist.lst  <-
    list(OFFSET  = 1,   # Time between sessions: M & F: 1.2 (1-8)
         AGE     = 1,   # Age: F:7 (45-82) & M:7.8 (44-81) (CS)
         ICC     = 25)  # ICC: F:107 (1018-2524) & M:126 (1289-2392) (CS)

  # Data setup
  # Sex-stratified input
  # Cross-sectional data
  crs_f.dt  <- crs.dt[SEX %like% "F", .(AGE, ICC, MATCHED = F), EID]
  crs_m.dt  <- crs.dt[SEX %like% "M", .(AGE, ICC, MATCHED = F), EID]

  # Longitudinal data
  # Use first instance data and time offset (years) between instances
  lng_b.dt  <- lng.dt[, .(OFF = diff(AGE), MATCHED = FALSE), EID
                      ][lng.dt["ses-2", on = "INST"]]
  lng_f.dt  <- lng_b.dt[SEX %like% "F", .(OFF, AGE, ICC, MATCHED), EID]
  lng_m.dt  <- lng_b.dt[SEX %like% "M", .(OFF, AGE, ICC, MATCHED), EID]


  # Matched output
  match.lst <- list(CRS = data.table(ID        = integer(),
                                     EID_m     = integer(),
                                     EID_f     = integer(),
                                     DIFF_age  = numeric(),
                                     DIFF_icc  = numeric()),
                    LNG = data.table(ID        = integer(),
                                     EID_m     = integer(),
                                     EID_f     = integer(),
                                     DIFF_off  = numeric(),
                                     DIFF_age  = numeric(),
                                     DIFF_icc  = numeric()))

  # Progress bar
  pb <- progress_bar$new(format = "Matching | :what [:bar] :current/:total",
                         total  = sum(crs_m.dt[, .N], lng_m.dt[, .N]),
                         clear  = FALSE,
                         width  = 75)

  # MAIN: CrossSectional
  for (m.i in crs_m.dt[, .I]) {
    pb$tick(tokens = list(what = "Cross-Sectional"))
    # Find females within specified distances for Age & ICC
    match.i <-
      crs_f.dt[(!MATCHED) &
               abs(crs_m.dt[m.i, AGE] - AGE) < dist.lst$AGE &
               abs(crs_m.dt[m.i, ICC] - ICC) < dist.lst$ICC,
               which = TRUE]

    if (length(match.i) > 0) {
      # From DT of potentials, pick female sub with closest ICC
      match.row <-
        crs_f.dt[match.i, .(EID_f = EID, AGE, ICC, ICC_f = ICC)
                 ][crs_m.dt[m.i], on = "ICC",
                 .(ID = m.i, EID_m = EID, EID_f,
                   DIFF_age = i.AGE - AGE,
                   DIFF_icc = ICC - ICC_f),
                 roll = "nearest"]

      # Bind match.row to previous matches
      match.lst$CRS <- rbind(match.lst$CRS, match.row, use.names = TRUE)

      # Mark matches
      crs_m.dt[m.i, MATCHED := TRUE]
      crs_f.dt[.(match.row$EID_f), on = "EID", MATCHED := TRUE]
      rm(match.row)
    }
    rm(m.i, match.i)
  }

  # MAIN: Longitudinal
  for (m.i in lng_m.dt[, .I]) {
    pb$tick(tokens = list(what = "Longitudinal"))
    # Find females within specified distances for Offset & Age & ICC
    match.i <-
      lng_f.dt[(!MATCHED) &
               abs(lng_m.dt[m.i, OFF] - OFF) < dist.lst$OFF &
               abs(lng_m.dt[m.i, AGE] - AGE) < dist.lst$AGE &
               abs(lng_m.dt[m.i, ICC] - ICC) < dist.lst$ICC,
               which = TRUE]

    if (length(match.i) > 0) {
      # From DT of potentials, pick female sub with closest ICC
      match.row <-
        lng_f.dt[match.i, .(EID_f = EID, AGE, OFF, ICC, ICC_f = ICC)
                 ][lng_m.dt[m.i], on = "ICC",
                 .(ID = m.i, EID_m = EID, EID_f,
                   DIFF_age = i.AGE - AGE,
                   DIFF_off = i.OFF - OFF,
                   DIFF_icc = ICC - ICC_f),
                 roll = "nearest"]

      match.lst$LNG <- rbind(match.lst$LNG, match.row, fill = TRUE)

      # Mark matches
      lng_m.dt[m.i, MATCHED := TRUE]
      lng_f.dt[.(match.row$EID_f), on = "EID", MATCHED := TRUE]
      rm(match.row)
    }
    rm(m.i, match.i)
  }
  rm(pb, crs_f.dt, crs_m.dt, lng_b.dt, lng_f.dt, lng_m.dt)
}
rm(age_sex.dt, icc.measure, fpath)

### Head-size adjustment
# Make hcvc.dt long-format for easier adjustment
hcvc.dt     <- hcvc.dt |>
melt(measure = patterns("HC|VC"), value = "CC") |>
{function(DT) DT[, c("ROI", "SIDE") := tstrsplit(variable, split = "_")
                 ][, SIDE := toupper(SIDE)
                 ][, variable := NULL]}() |>
setcolorder("CC", after = "SIDE") |>
setkey(EID, INST)

## Concile subcohorts in a nested list
# 1st lvl: CNN vs ASSEMBLYNET
hcvc.lst    <- list(LPP_CNN = list(), ABLYNET = list())
for (i in seq_along(hcvc.lst)) {
  hcvc.lst[[i]] <- list(CRS = NULL, LNG = NULL)
  # 2nd lvl: CrossSecc vs Longitudinal
  for (j in seq_along(hcvc.lst[[i]])) {
    # 3rd lvl: All vs Sex/Age/ICC Pairing
    hcvc.lst[[i]][[j]]$ALL <-
      merge(list(crs.dt, lng.dt)[[j]][, -"ICC"],
            hcvc.dt[names(hcvc.lst)[i], on = "SEGM", -"SEGM"],
            all = FALSE)
    hcvc.lst[[i]][[j]]$MTCH <-
      match.lst[[j]][, .(MATCH = ID, EID_m, EID_f)] |>
      melt(id = "MATCH", value = "EID") |>
      merge(hcvc.lst[[i]][[j]]$ALL , by = "EID") |>
      setkey(EID, INST)
    # Remove unnecesary column
    hcvc.lst[[i]][[j]]$MTCH[, variable := NULL]
    rm(j)
  }
  rm(i)
}
rm(crs.dt, lng.dt, hcvc.dt, REMATCH)

## Data preprocessing
# Mean ICC
icc.lst     <- hcvc.lst |>
lapply(function(segm_lvl)
       lapply(segm_lvl,
              function(dsgn_lvl)
              lapply(dsgn_lvl,
                     function(match_lvl)
                     match_lvl[, .(ICC_mean = mean(ICC)), SEX])))

hcvc.lst    <-
  Map(function(list1, list2)
      Map(function(sublist1, sublist2)
          Map(function(DT1, DT2)
              merge(DT1, DT2, by = "SEX"),
              sublist1, sublist2),
          list1, list2),
      icc.lst, hcvc.lst)
rm(icc.lst)

# Regression slopes
slope.lst   <- hcvc.lst |>
lapply(function(segm_lvl)
       lapply(segm_lvl,
              function(dsgn_lvl)
              lapply(dsgn_lvl,
                     function(match_lvl)
                     match_lvl[,
                               .(B = summary(lm(CC ~ ICC))[[4]][2]),
                               .(SEX, ROI)])))

hcvc.lst    <-
  Map(function(list1, list2)
      Map(function(sublist1, sublist2)
          Map(function(DT1, DT2)
              merge(DT1, DT2, by = c("SEX", "ROI")),
              sublist1, sublist2),
          list1, list2),
      slope.lst, hcvc.lst)
rm(slope.lst)


## Apply adjustment methods
hc_hvr.lst  <-   hcvc.lst |>
lapply(function(segm_lvl)
       lapply(segm_lvl,
              function(dsgn_lvl)
              lapply(dsgn_lvl,
                     function(match_lvl)
                       match_lvl[, let(RAW_cc = CC,
                                       # TIV proportion method: VOL/TIV
                                       PRP_cc = CC / ICC,
                                       # Residual normalizations
                                       RES_cc = CC - B * (ICC - ICC_mean))
                                 # Clean columns
                                 ][, c("CC", "B", "ICC_mean") := NULL] |>
                        # Create a column for Head-size adjustment
                        melt(measure = patterns("_cc$"), variable = "ADJ") |>
                        # Disaggregate ROIs
                        dcast(... ~ ROI, value = "value") |>
                        # Calculate HVR
                        {function(DT)
                         DT[, ADJ := fifelse(ADJ == "RAW_cc",
                                             NA, str_remove(ADJ, "_cc$"))
                            ][!is.na(ADJ), HVR := (HC / (HC + VC))]}() |>
                        # Set keys for easier joining
                        setkey(EID, INST))))
rm(hcvc.lst)

### OUT
outrds      <- c("sex-age-icc_matching", "hc-hvr_adj") |>
sprintf(fmt = "data/rds/%s.rds") |>
here()

match.lst |> saveRDS(outrds[1])
hc_hvr.lst |> saveRDS(outrds[2])

rm(outrds)
