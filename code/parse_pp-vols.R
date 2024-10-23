#!/usr/bin/env Rscript

library(here)
library(data.table)

COI         <- c("SubjectID", "VisitID", "ICC_vol", "ScaleFactor")

DT          <- here("data/pp_vols") |>
              list.files(full.names = TRUE) |>
              lapply(fread, select = COI) |>
              rbindlist() |>
              fwrite(here("data/ukbb_lng_icc_scale.csv"))
