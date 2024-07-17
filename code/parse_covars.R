#!/usr/bin/env Rscript

library(here)
library(data.table)
library(arrow)
library(stringr)
library(lubridate)

### Columns of interest:
## Dates
# Brain MRI timestamp: 21862-2
# Touchscreen cognitive timestamp - scan: 21825-2
## Demographics
# Sex (reported): 31-0.0; 0-F,1-M
# Age (imaging): 21003-[12].0
# Month of birth: 52-0.0
# Year of birth: 22200-0.0
# Education (years): 845-2.0; Lots of NAs keep??
# Education scores (Need to scale)
# England: 26414-0.0
# Scotland: 26431-0.0
# Wales: 26421-0.0
## Alzheimer's Family
# Illnesses of father: 20107-[0-3].[0-9]; AD: 10
# Illnesses of mother: 20110-[0-3].[0-9]; AD: 10
## ICD10 dx
# F: Mental disorders; F00: dementia due to AD
# G: Diseases of the NS; G30: AD
# Q0: congenital malformations of NS
# ALL diagnoses of ICD10: 41270-0.[0-225]
## Alzheimer's report date
# F00 first reported: 130836-0.0
# G30 first reported: 131036-0.0
## FreeSurfer Hippocampus
# HCvol left: 26562-[23].0
# HCvol right: 26593-[23].0
## Memory scores
# Numeric memory; max digits remembered: 4282-0, 4282-2; higher better
# Prospective memory; result: 20018-0, 20018-2
# 0-skipped,incorrect; 1-first attempt; 2-second attempt; NEED REORDERING
# Paired associate learning; score: 20197-2 # Not found
# Pairs matching; n incorrect: 399-[0123].[123]; lower better

### IN
covars.dt <- read_feather("data/arrow/UKBB_covars.arrow") |> as.data.table()

cols_orig <- c("eid",                                 # ID
               "31-0.0",                              # Sex: 0-F, 1-M
               sprintf("264%i-0.0", c(14, 31, 21)),   # Educ: Eng/Scot/Wales
               sprintf("20107-%i.%i",                 # Illnesses of father
                       expand.grid(0:3, 0:9)[[1]],    # AD : 10
                       expand.grid(0:3, 0:9)[[2]]),
               sprintf("20110-%i.%i",                 # Illnesses of mother
                       expand.grid(0:3, 0:9)[[1]],    # AD: 10
                       expand.grid(0:3, 0:9)[[2]]),
               sprintf("41270-0.%i", 0:225),          # ICD10 (F / G / Q0)
               sprintf("21003-%i.0", 2:3),            # Age (on scanning)
               sprintf("21862-%i.0", 2:3),            # Timestamps of scan
               sprintf("265%i-%i.0", c(62, 93),       # FreeSurfer volumes
                       rep(2:3, each = 2)),           # HC (L / R)
               sprintf("4282-%i.0", 2:3),             # Numeric memory
               sprintf("20018-%i.0", 2:3),            # Prospective memory
               sprintf("399-%i.%i",                   # Pairs matching
                       expand.grid(2:3, 1:3)[[1]],    # three instances p/visit
                       expand.grid(2:3, 1:3)[[2]])
               )

col_names <- c("ID",
               "SEX",
               sprintf("EDUC.%i", 1:3),
               sprintf("AD_pater.%i.%i",
                       expand.grid(0:3, 0:9)[[1]],
                       expand.grid(0:3, 0:9)[[2]]),
               sprintf("AD_mater.%i.%i",
                       expand.grid(0:3, 0:9)[[1]],
                       expand.grid(0:3, 0:9)[[2]]),
               sprintf("ICD10.%i", 0:225),
               sprintf("AGE_%s", c("bl", "fu")),
               sprintf("SCAN_%s", c("bl", "fu")),
               sprintf("FS_HC_%s_%s", c("l", "r"),
                       rep(c("bl", "fu"), each = 2)),
               sprintf("MEM_num_%s", c("bl", "fu")),
               sprintf("MEM_prosp_%s", c("bl", "fu")),
               sprintf("MEM_pairs.%i_%s",
                       expand.grid(c("bl", "fu"), 1:3)[[2]],
                       expand.grid(c("bl", "fu"), 1:3)[[1]])
               )

### Data cleaning
## Keep only specified columns and rename them
covars.dt <- covars.dt[, ..cols_orig]
setnames(covars.dt, col_names)
setkey(covars.dt, ID)
rm(cols_orig)

## Parse AD history columns
# AD in paternal side
p_cols    <- str_subset(names(covars.dt), "AD_pater")
pater.dt  <- covars.dt[, .(ID, DX = do.call(paste, c(.SD, sep = ","))),
                       .SDcols = p_cols
                       ][, .(ID, AD_pat = str_detect(DX, "10"))]

# AD in maternal side
m_cols    <- str_subset(names(covars.dt), "AD_mater")
mater.dt  <- covars.dt[, .(ID, DX = do.call(paste, c(.SD, sep = ","))),
                       .SDcols = m_cols
                       ][, .(ID, AD_mat = str_detect(DX, "10"))]

# Join & remove columns
covars.dt <- pater.dt[mater.dt][covars.dt]
covars.dt[, (p_cols) := NULL]
covars.dt[, (m_cols) := NULL]
#rm(p_cols, pater.dt, m_cols, mater.dt)

## Find subjects with significant Diagnoses
# FXX: Mental & behavioural disorders
# GXX: Diseases of the nervous system
# Q0X: Congenital malformations of the nervous system
icd_cols  <- str_subset(names(covars.dt), "ICD10")
icd_10.dt <- covars.dt[, .(ID, DX = do.call(paste, c(.SD, sep = ","))),
                       .SDcols = icd_cols
                       ][, .(ID,
                             ICD10_f = str_detect(DX, "F"),
                             ICD10_g = str_detect(DX, "G"),
                             ICD10_q = str_detect(DX, "Q0"))]

# Join & remove columns
covars.dt <- icd_10.dt[covars.dt]
covars.dt[, (icd_cols) := NULL]
#rm(icd_cols, icd_10.dt)

## Sex, factorize
covars.dt[, SEX := factor(SEX, labels = c("F", "M"))]

## Calculate intervale between MRI scans
# Parse dates
covars.dt[, `:=`(SCAN_bl = ymd_hms(SCAN_bl), SCAN_fu = ymd_hms(SCAN_fu))]

# Infer follow-up time from age
covars.dt[is.na(SCAN_fu) & !is.na(AGE_fu),
          SCAN_fu := SCAN_bl + years(AGE_fu - AGE_bl)]

# Calculate time intervals
covars.dt[, SCAN_diff := time_length(interval(SCAN_bl, SCAN_fu), "years")]
covars.dt[, c("SCAN_bl", "SCAN_fu") := NULL]

## Education standardize for equivalence between countries
# All subjects
covars.dt[!is.na(EDUC.1),
          EDUC_all := scale(EDUC.1, center = TRUE, scale = TRUE)]
covars.dt[!is.na(EDUC.2),
          EDUC_all := scale(EDUC.2, center = TRUE, scale = TRUE)]
covars.dt[!is.na(EDUC.3),
          EDUC_all := scale(EDUC.3, center = TRUE, scale = TRUE)]

# Healthy subjects
covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.1),
          EDUC_hty := scale(EDUC.1, center = TRUE, scale = TRUE)]
covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.2),
          EDUC_hty := scale(EDUC.2, center = TRUE, scale = TRUE)]
covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.3),
          EDUC_hty := scale(EDUC.3, center = TRUE, scale = TRUE)]

# Remove old covariates
covars.dt[, c("EDUC.1", "EDUC.2", "EDUC.3") := NULL]

# Long format regarding Scan visit
covars.dt <- melt(covars.dt, measure = patterns("_bl$|_fu$"))

covars.dt[grepl("_bl$", variable), SCAN_visit := 1]
covars.dt[grepl("_fu$", variable), SCAN_visit := 2]

covars.dt[, variable := str_sub(variable, end = -4)]

covars.dt <- dcast(covars.dt, ... + SCAN_visit ~ variable, value = "value")

covars.dt[, SCAN_visit := factor(SCAN_visit, levels = 1:2,
                                 labels = c("Baseline", "Follow-up"))]

## Prospective memory: reorder values
covars.dt[, MEM_prosp_ := MEM_prosp]
covars.dt[MEM_prosp_ == 1, MEM_prosp := 2]
covars.dt[MEM_prosp_ == 2, MEM_prosp := 1]
covars.dt[, MEM_prosp_ := NULL]

### OUT
setkey(covars.dt, ID, SCAN_visit)
covars.dt |> write_feather(here("data/arrow/UKBB_covars_clean.arrow"))
