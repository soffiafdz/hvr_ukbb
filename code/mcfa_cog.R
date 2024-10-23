#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)

### IN
fpaths      <- here("data", c("rds/metadata.dt",
                              "arrow/UKBB_covars_all.arrow"))
if (all(file.exists(fpaths))) {
  metadata.dt <- readRDS(fpaths[1])
  DT <- arrow::read_feather(fpaths[2])
} else {
  here("code/parse_covars.R") |> source()
}
rm(fpaths)


