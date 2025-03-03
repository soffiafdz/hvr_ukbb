#!/usr/bin/env Rscript

library(here)
library(data.table)
#library(effsize)
#library(ggplot2)
#library(GGally)
#library(ggtext)
library(gt)

### REMAKE plots
#REMAKE_PLOTS  <- TRUE

### INPUT
paths        <- list(
  rds = c("hc-hvr_adj.rds", "cog_mcfa_domains.rds"),
  scripts = c("adjust_hc-hvr.R", "mcfa_cog.R")
) |> Map(
  f = function(Files, Dir) here(Dir, Files),
  c("data/rds", "code")
)

if (file.exists(paths[["rds"]][1])) {
  hc_hvr.lst <- readRDS(paths[["rds"]][1])
} else {
  source(paths[["scripts"]][1])
}

if (file.exists(paths[["rds"]][2])) {
  lat_cog.dt <- readRDS(paths[["rds"]][2])
} else {
  source(paths[["scripts"]][2])
}

rm(paths)


### Data CLEANING
## Labels
adjs          <- c("Unadjusted", "Proportions", "Residuals")
rois          <- c(
  ICC = "Intracranial volume",
  HC  = "Hippocampus",
  VC  = "Ventricles",
  HVR = "HC-to-VC ratio"
)

## Average sides for HC/HVR & Make long format
hc_hvr.lst <- hc_hvr.lst |>
  lapply(function(crs_lvl) {
    # Keep only Cross-Sectional cohort & complete samples
    crs_lvl$CRS$ALL[
      ,
      # Average sides for all ROIs, except ICC
      lapply(.SD, mean),
      by = .(EID, SEX, INST, ICC, ADJ),
      .SDcols = names(rois)[-1]
    ][
      # Rename ADJ for plotting
      , ADJ := factor(ADJ, labels = adjs)
    ][
      # ICC is always unadjusted, so delete duplicate values
      !adjs[1], on = "ADJ", ICC := NA
    ] |>
    # Rename ROIs for plotting
    setnames(names(rois), rois) |>
    # Aggregate ROIs
    melt(measure = rois, variable = "ROI", value = "VAL", na.rm = TRUE) |>
    # Key resulting DT
    setkey(EID, INST)
  })

## Make lat_cog long format
lat_cog.dt  <- lat_cog.dt |>
  melt(id = 1:2, variable = "FACTOR", value = "SCORE") |>
  na.omit() |>
  setkey(EID, INST)
cog_factors <- levels(lat_cog.dt$FACTOR)

### Data PROCESSING
cors.dt <- list()
cor.cols <- list(
  R = "estimate.cor",
  CONF_l = "conf.int1",
  CONF_h = "conf.int2",
  STAT = "statistic.t",
  P_val = "p.value"
)

for (seg in names(hc_hvr.lst)) {
  for (fct in cog_factors) {
    for (roi in rois) {
      for (adj in adjs) {
        subDT <- hc_hvr.lst[[seg]][
          adj, on = "ADJ"
        ][
          roi, on = "ROI",
          VAL,
          .(EID, INST)
        ][
          lat_cog.dt[fct, on = "FACTOR"],
          nomatch = NULL
        ]

        # N of subsample
        n <- subDT[, .N]
        if (n > 0) {
          cor.res <- subDT[, cor.test(VAL, SCORE)] |> unlist()
          sublist <- list(SEG = seg, COG = fct, ADJ = adj, ROI = roi, N = n)
          for (i in seq_along(cor.cols)) {
            sublist[[names(cor.cols[i])]] <- cor.res[[cor.cols[[i]]]]
          }
          setDT(sublist)
          cors.dt <- rbind(cors.dt, sublist)
          rm(i, cor.res, sublist)
        }
      }
    }
  }
}
cors.dt[
  , (names(cor.cols)) := lapply(.SD, as.numeric),
  .SDcols = names(cor.cols)
][
  , P_adj := p.adjust(P_val, method = "bonferroni")
]
rm(seg, fct, roi, adj, subDT, n, cor.cols)

### Print tables
# TODO: Put this in a list to not repeat; maybe lapply

## LPP_CNN
# Memory
cors.dt[
  "LPP_CNN", on = "SEG"
][
  order(-abs(R))
][
  "MEMORY",
  on = "COG",
  -c("SEG", "COG", "N", "P_val")
] |>
gt() |>
tab_header(
  title = "Correlation: Cognition ~ {HC, HVR, VC, ICC}",
) |>
fmt_number(decimals = 4) |>
cols_merge(columns = c("ROI", "ADJ"), pattern = "{1} ({2})") |>
cols_merge(columns = c("R", "CONF_l", "CONF_h"), pattern = "{1} ({2}, {3})") |>
cols_label(
  ROI = md("**ROI (adj.)**"),
  R = md("***r***"),
  STAT = md("**T**"),
  P_adj = md("***p* adj.**")
) |>
tab_row_group(
  label = sprintf(
    "Memory (%s)",
    cors.dt["MEMORY", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["LPP_CNN", on = "SEG"]["MEMORY", on = "COG", which(COG == "MEMORY")]
) |>
gtsave("tables/cors_cnn_mem.html")

# Processing speed
cors.dt[
  "LPP_CNN", on = "SEG"
][
  order(-abs(R))
][
  "PROCSPEED",
  on = "COG",
  -c("SEG", "COG", "N", "P_val")
] |>
gt() |>
tab_header(
  title = "Correlation: Cognition ~ {HC, HVR, VC, ICC}",
) |>
fmt_number(decimals = 4) |>
cols_merge(columns = c("ROI", "ADJ"), pattern = "{1} ({2})") |>
cols_merge(columns = c("R", "CONF_l", "CONF_h"), pattern = "{1} ({2}, {3})") |>
cols_label(
  ROI = md("**ROI (adj.)**"),
  R = md("***r***"),
  STAT = md("**T**"),
  P_adj = md("***p* adj.**")
) |>
tab_row_group(
  label = sprintf(
    "Processing speed (%s)",
    cors.dt["PROCSPEED", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["LPP_CNN", on = "SEG"]["PROCSPEED", on = "COG", which(COG == "PROCSPEED")]
) |>
gtsave("tables/cors_cnn_procsp.html")


#Reasoning & Exec function
cors.dt[
  "LPP_CNN", on = "SEG"
][
  order(-abs(R))
][
  "REAS_EXFN",
  on = "COG",
  -c("SEG", "COG", "N", "P_val")
] |>
gt() |>
tab_header(
  title = "Correlation: Cognition ~ {HC, HVR, VC, ICC}",
) |>
fmt_number(decimals = 4) |>
cols_merge(columns = c("ROI", "ADJ"), pattern = "{1} ({2})") |>
cols_merge(columns = c("R", "CONF_l", "CONF_h"), pattern = "{1} ({2}, {3})") |>
cols_label(
  ROI = md("**ROI (adj.)**"),
  R = md("***r***"),
  STAT = md("**T**"),
  P_adj = md("***p* adj.**")
) |>
tab_row_group(
  label = sprintf(
    "Reasoning & Executive function (%s)",
    cors.dt["REAS_EXFN", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["LPP_CNN", on = "SEG"]["REAS_EXFN", on = "COG", which(COG == "REAS_EXFN")]
) |>
gtsave("tables/cors_cnn_exfn.html")

## ABLYNET
cors.dt[
  "ABLYNET", on = "SEG"
][
  order(-abs(R))
][
  order(COG),
  -c("SEG", "COG", "N", "P_val")
] |>
gt() |>
tab_header(
  title = "Correlation: Cognition ~ {HC, HVR, VC, ICC}",
  subtitle = "Replication with AssemblyNet"
) |>
fmt_number(decimals = 4) |>
cols_merge(columns = c("ROI", "ADJ"), pattern = "{1} ({2})") |>
cols_merge(columns = c("R", "CONF_l", "CONF_h"), pattern = "{1} ({2}, {3})") |>
cols_label(
  ROI = md("**ROI (adj.)**"),
  R = md("***r***"),
  STAT = md("**T**"),
  P_adj = md("***p* adj.**")
) |>
tab_row_group(
  label = sprintf(
    "Memory (%s)",
    cors.dt["MEMORY", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["ABLYNET", on = "SEG"][order(COG), which(COG == "MEMORY")]
) |>
tab_row_group(
  label = sprintf(
    "Processing speed (%s)",
    cors.dt["PROCSPEED", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["ABLYNET", on = "SEG"][order(COG), which(COG == "PROCSPEED")]
) |>
tab_row_group(
  label = sprintf(
    "Reasoning & Executive function (%s)",
    cors.dt["REAS_EXFN", on = "COG", N] |> unique() |> format(big.mark = ",")
  ),
  rows = cors.dt["ABLYNET", on = "SEG"][order(COG), which(COG == "REAS_EXFN")]
) |>
gtsave("tables/cors_rep.html")

### OUT
#TODO: fill this
