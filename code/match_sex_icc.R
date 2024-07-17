#!/usr/bin/env Rscript

library(here)
library(data.table)

## Import files
# ICC volume and ScaleFactors
fpath         <- here("data/icc_scale.csv")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

icc_scale.dt  <- fread(fpath)
icc.dt        <-
  icc_scale.dt[, .(ID = as.numeric(stringr::str_extract(PTID, "(?<=sub-)\\d*")),
                   ICC)]

rm(fpath, icc_scale.dt)

# Covariates
fpath         <- here("data/rds/covars.rds")
if (file.exists(fpath)) {
  covars.dt   <- readr::read_rds(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

rm(fpath)

icc_sex.dt    <- covars.dt[, .(ID, AGE, SEX)][icc.dt, on = "ID"]
icc_sex.dt    <- icc_sex.dt[!is.na(SEX)]

match_model   <- matchit(SEX ~ ICC + AGE, data = icc_sex.dt,
                         method = "nearest", ratio = 1)

