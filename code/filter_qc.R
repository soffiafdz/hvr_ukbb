#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)

### IN
fpaths      <- c("lists/darq_out_20240408_lng_r152.csv",
                 "data/arrow/UKBB_assemblynet_20240521.arrow") |> here()

for (fpath in fpaths) {
  if (!file.exists(fpath)) {
    sprintf("File: %s is required but could not be found.", fpath) |> stop()
  }
}

darq.dt     <- fread(fpaths[1])
darq.dt[, EID := as.integer(str_extract(fn, "(?<=-)\\d{7}"))]
darq.dt[, INSTANCE := str_extract(fn, "ses-\\d")]
darq.dt[, fn := NULL]
darq.dt |> setcolorder(2:3) |> setnames("darq", "DARQ")

asnet.dt    <- arrow::read_feather(fpaths[2]) |>
setDT() |>
{\(DT) DT[, c(1:2, 8)]}() |>
setnames(c("EID", "INSTANCE", "ASBLYNET"))
asnet.dt[, EID := as.integer(str_remove(EID, "sub-"))]

qc.dt       <- asnet.dt[darq.dt, on = .(EID, INSTANCE)]
rm(fpaths, darq.dt, asnet.dt)

## OUT
outrds      <- here("data/rds/qc_darq-assemblynet.rds")
qc.dt |> saveRDS(outrds)
rm(outrds)
