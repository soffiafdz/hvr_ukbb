#!/usr/bin/env Rscript

library(here)
library(data.table)
library(readr)
library(stringr)
#library(progress)
library(lavaan)
library(lavaanPlot)
#library(ggplot2)
#library(patchwork)
#library(semTable)

## Rerun (?)
# TODO: Needed?

## Import files
# Covariates
fpath       <- here("data/rds/covars.rds")
if (file.exists(fpath)) {
  covars.dt <- read_rds(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

# ICC volume and ScaleFactors
fpath         <- here("data/icc_scale.csv")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

icc.dt        <- fread(fpath, select = 1:2, col.names = c("ID", "ICC"))
icc.dt[, ID := as.numeric(stringr::str_extract(ID, "(?<=sub-)\\d*"))]
icc.dt[, ICC := ICC / 100000]
rm(fpath)

# Adjusted HC/HVR
fpath       <- here("data/rds/hc-hvr_icv-adjusted.rds")
if (file.exists(fpath)) {
  hc_hvr_adj.dt <- read_rds(fpath)
} else {
  here("code/adjust_hc-hvr.R") |> source()
}

# Memory scores
fpath       <- here("data/rds/memory_scores.rds")
if (file.exists(fpath)) {
  memory.dt <- read_rds(fpath)
} else {
  here("code/factor_memory.R") |> source()
}

rm(fpath)

## Merge
DT          <- covars.dt[, .(ID, SEX, AGE, EDUC)
                         ][icc.dt, on = "ID"
                         ][memory.dt[, .(ID, MEMORY)], on = "ID"
                         ][hc_hvr_adj.dt, on = "ID"]

# Remove NAs
#DT          <- DT[!is.na(SEX) & !is.na(MEMORY) & !is.na(VALUE)]
DT          <- DT[!is.na(SEX) & !is.na(EDUC) & !is.na(MEMORY) & !is.na(VALUE)]

# Extract ratio and average sides
DT          <- dcast(DT, ... ~ SIDE, value.var = "VALUE")
DT[, AVG := (L + R) / 2]
#DT[, RATIO := L/R]

# Adjust scaling
DT[METHOD == "prop", AVG := AVG * 1000]
DT[METHOD == "pcp" & MEASURE != "HVR", AVG := AVG * 10]

## Mediation model
model       <- "
# Regressions
#ICC     ~ SEX + AGE
AVG     ~ a * ICC + SEX + AGE + EDUC
MEMORY  ~ c * ICC + b * AVG + SEX + AGE + EDUC

# Direct effect
dICC    := c
# Indirect effect
iHC     := a * b
# Total effect
Total   := dICC + iHC

# Proportions:
pICC    := dICC / Total
pHC     := iHC / Total
"

mtds        <- DT[, levels(METHOD)]
fits_cnn <- fits_fs <- fits_hvr <- vector("list", length(mtds))
names(fits_cnn) <- names(fits_fs) <- names(fits_hvr) <- mtds
fits_hvr[2:3] <- NULL
for (i in seq_along(mtds)) {
  #print(mtds[i])
  fits_cnn[[i]]  <- sem(model, DT[METHOD == mtds[i] & MEASURE == "HC_dl"])
  fits_fs[[i]]   <- sem(model, DT[METHOD == mtds[i] & MEASURE == "HC_fs"])
  if (i %in% 2:3) next
  fits_hvr[[i]]  <- sem(model, DT[METHOD == mtds[i] & MEASURE == "HVR"])
}
