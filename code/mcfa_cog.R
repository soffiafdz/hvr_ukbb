#!/usr/bin/env Rscript

library(here)
library(data.table)
library(lavaan)
library(stringr)

#### IN
#fpaths <- here(
  #"data",
  #c("rds/metadata.dt", "arrow/UKBB_covars_all.arrow")
#)

#if (all(file.exists(fpaths))) {
  #metadata.dt <- readRDS(fpaths[1])
  #covars.dt <- arrow::read_feather(fpaths[2])
#} else {
  #here("code/parse_covars.R") |> source()
#}

#rm(fpaths)

### Clinical history
## Filter out people with F, G or Q0 dx in ICD10 and bad QC
#icd_10.dt   <- covars.dt[ICD_10 %like% "F|G|Q0", .(EID)]
#cnn.dt      <- cnn.dt[!icd_10.dt, on = "EID"]
#rm(icd_10.dt)

### FUNCTIONS for parsing cognitive data
## Extract columns according to a regex pattern
ext_cols <- function(
  DT,
  pattern,
  include.eid = TRUE,
  ignore.case = FALSE
) {
  if (include.eid) pattern <- sprintf("EID|%s", pattern)
  DT[, .SD, .SDcols = patterns(pattern, ignore.case = ignore.case)]
}

## Convert to long format by Session from column name
ext_sess <- function(
  DT,
  id.vars = "EID",
  ses.pattern = "ses\\d",
  ses.name = "SESSION"
) {
  DT <- melt(DT, id.vars = id.vars) |> suppressWarnings()
  DT[, (ses.name) := str_extract(variable, ses.pattern)]
  DT[, variable := str_remove(variable, paste0("_", ses.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

## Convert to long format by Array from column name
ext_arrays <- function(
  DT,
  id.vars = "EID",
  include.sess = TRUE,
  sess.name = "SESSION",
  array.name = "ARRAY",
  array.pattern = "\\d*$"
) {
  if (include.sess) id.vars <- c(id.vars, sess.name)
  DT <- melt(DT, id.vars = id.vars)
  DT[, (array.name) := str_extract(variable, array.pattern)]
  #TODO: This is commented in code/clean_cog.R; Verify it is needed.
  DT["", on = array.name, (array.name) := NA]
  DT[, variable := str_remove(variable, paste0("_", array.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

## Convert to long format by whether it was performed online/onsite
sep_online <- function(
  DT,
  id.vars = "EID",
  online.pattern = "onl",
  online.name = "ONLINE",
  include.sess = TRUE,
  sess.name = "SESSION",
  include.array = TRUE,
  array.name = "ARRAY"
) {
  if (include.sess) id.vars <- c(id.vars, sess.name)
  if (include.array) id.vars <- c(id.vars, array.name)
  DT <- melt(DT, id.vars = id.vars)
  DT[, (online.name) := fifelse(variable %like% online.pattern, TRUE, FALSE)]
  DT[, variable := str_remove(variable, paste0("_", online.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

### Data CLEANING
## Pairs matching
pairs.dt <- covars.dt |>
  ext_cols("PRS") |>
  ext_sess() |>
  ext_arrays() |>
  sep_online()
