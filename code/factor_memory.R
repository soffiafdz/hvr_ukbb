#!/usr/bin/env Rscript

library(here)
library(data.table)
library(lavaan)
library(readr)

## Import files
# Subjects with segmentation
fpath       <- here("data/hcvcag_segmentations.csv")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

ids.dt      <- fread(fpath, select = 1) |>
  {\(x) x[, .(ID = as.numeric(stringr::str_extract(id, "(?<=sub-)\\d*")))]} ()

rm(fpath)

# Covariates
fpath       <- here("data/rds/covars.rds")
if (file.exists(fpath)) {
  covars.dt <- read_rds(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

rm(fpath)

DT          <- covars.dt[ids.dt, on = "ID"]
rm(covars.dt, ids.dt)

## CFA: Pairs
mod_pairs   <- "
  MEM_pairs =~ MEM_pairs1 + MEM_pairs2 + MEM_pairs3
"

fit_pairs   <- sem(mod_pairs, data = DT[!is.na(MEM_pairs1)], missing = "ml")
rm(mod_pairs)

fit_pairs |> write_rds(here('data/rds/cfa_fit_mem-pairs.rds'))

DT          <- DT[!is.na(MEM_pairs1), MEM_pairs := lavPredict(fit_pairs)]

## CFA: Memory
mod_mem     <- "
  MEMORY =~ MEM_prosp + MEM_pairs + MEM_num
"

fit_mem     <- sem(mod_mem, data = DT[!is.na(MEM_pairs)], missing = "ml")
rm(mod_mem)

fit_mem |> write_rds(here('data/rds/cfa_fit_mem-global.rds'))

DT          <- DT[!is.na(MEM_pairs), MEMORY := lavPredict(fit_mem)]

## Export memory scores
memory.dt   <- DT[,
                  .(ID, MEM_num, MEM_prosp, MEM_pairs, MEMORY)]
rm(DT)

memory.dt |> write_rds(here('data/rds/memory_scores.rds'))
