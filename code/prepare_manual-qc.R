#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)

## DARQ is not good enough.
## Need to do further manual QC.

## IN
fpaths      <- here(c("lists/aqc_jpgs/paths.lst",
                      "data/rds/qc_darq-assemblynet.rds"))

if (file.exists(fpaths[1])) {
  paths.dt  <- fread(fpaths[1], header = FALSE, col.names = "PATH")
} else {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

if (file.exists(fpaths[2])) {
  qc.dt     <- readRDS(fpaths[2])
} else {
  here("code/filter_qc.R") |> source()
}

# Parse data.table of paths
paths.dt[, EID := as.integer(str_extract(PATH, "(?<=-)\\d{7}"))]
paths.dt[, INSTANCE := str_extract(PATH, "ses-\\d")]
setkey(paths.dt, EID, INSTANCE)

# Visually inspect the subjects who are below DARQ level
# TODO: Define this threshold, .9 seems enough
qc.thresh   <- .9
manual.dt   <- qc.dt[DARQ < qc.thresh,
                     .(DARQ = sprintf("%.3f", DARQ)),
                     keyby = .(EID, INSTANCE)]

manual.dt   <- paths.dt[manual.dt, .(PATH, DARQ)]

## OUT
outcsv      <- here("lists/qc_manual_post-darq.csv")
manual.dt |> fwrite(outcsv, col.names = FALSE)
