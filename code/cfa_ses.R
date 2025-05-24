#!/usr/bin/env Rscript

library(here)
library(data.table)
library(lavaan)

### IN
fpath <- here("data/arrow/UKBB_covars_all.arrow")

if (file.exists(fpath)) {
  covars.dt <- arrow::read_feather(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

rm(fpath)


### Data CLEANING
cols <- covars.dt |> names() |> grep(pattern = "EID|*_score_*")

DT <- covars.dt[, ..cols] |>
  melt(id = 1, value = "SCORE") |>
  suppressWarnings() |>
  na.omit() |>
  (\(DT) {
    DT[
        , c("ITEM", "COUNTRY") := tstrsplit(variable, "_score_", fixed = T)
      ][
        , let(variable = NULL, COUNTRY = toupper(COUNTRY))
      ]
  })() |>
  dcast(EID + COUNTRY ~ ITEM, value.var = "SCORE")


## Wrong coding:
# Ranges: Education
# - Scotland: -2.89 -> 2.73
# - Wales:    0 -> 100
# - England:  0 -> 98
wrong_educ_eid <- DT[is.na(EMPL), .(EID)]
really_sct <- DT[
  wrong_educ_eid, on = "EID"
][
  !is.na(EDUC)
][
  "WLS", on = "COUNTRY"
][
  , .(EID, COUNTRY = "SCT", EDUC)
]

really_wls <- DT[
  wrong_educ_eid, on = "EID"
][
  !is.na(EDUC)
][
  "SCT", on = "COUNTRY"
][
  , .(EID, COUNTRY = "WLS", EDUC)
]

DT_ <- DT[wrong_educ_eid, on = "EID"][is.na(EDUC)][, EDUC := NULL]
DT_sct <- really_sct[DT_["SCT", on = "COUNTRY"], on = .(EID, COUNTRY)]
DT_wls <- really_wls[DT_["WLS", on = "COUNTRY"], on = .(EID, COUNTRY)]

ses.dt <- DT |>
  na.omit() |>
  rbind(DT_sct, DT_wls, use.names = TRUE)

rm(cols, DT, DT_, DT_sct, DT_wls, really_sct, really_wls, wrong_educ_eid)

### Model
ses_model <- '
  SES =~ EMPL + EDUC + INCM + HLTH + HOUS
'

fits <- vector("list", 3)
names(fits) <- ses.dt[, unique(COUNTRY)]
for (country in ses.dt[, unique(COUNTRY)]) {
  cat("=== ", country, " ===\n")
  fits[[country]] <- cfa(
    ses_model,
    ses.dt[country, on = "COUNTRY"],
    estimator = "WLSMV"
  )
  print(fitMeasures(fits[[country]], c("cfi", "rmsea")))
  print(modificationIndices(fits[[country]], sort=TRUE)[1:5,])
  ses.dt[country, on = "COUNTRY", LAT_SES := lavPredict(fits[[country]])]
}

ses.dt <- copy(DT)
ses.dt[, IMDP := NULL]

rm(country)

### OUT
here("data/rds/cfa_ses_fits.rds") |> saveRDS(object = fits)
here("data/rds/cfa_ses_data.rds") |> saveRDS(object = ses.dt)
