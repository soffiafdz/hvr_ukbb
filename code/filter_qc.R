#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)
library(ggplot2)

### IN
fpaths      <- c("lists/darq_out_20240408_lng_r152.csv",
                 "data/arrow/UKBB_assemblynet_20240521.arrow",
                 "data/xfm_displacements_reza_vlad-lng.csv",
                 "data/xfm_displacements_vlad_v1-v2.csv") |> here()

for (fpath in fpaths) {
  if (!file.exists(fpath)) {
    sprintf("File: %s is required but could not be found.", fpath) |> stop()
  }
}
rm(fpath)

## DARQ (Automatic Reg QC)
## This is useless
darq.dt     <- fread(fpaths[1])
darq.dt[, EID := as.integer(str_extract(fn, "(?<=-)\\d{7}"))]
darq.dt[, INSTANCE := str_extract(fn, "ses-\\d")]
darq.dt[, fn := NULL]
darq.dt |> setcolorder(2:3) |> setnames("darq", "DARQ")

## AssemblyNet (Automatic Reg QC)
## Specific to ASSBLYNET data
asnet.dt    <- arrow::read_feather(fpaths[2]) |>
setDT() |>
{\(DT) DT[, c(1:2, 8)]}() |>
setnames(c("EID", "INSTANCE", "ASBLYNET"))
asnet.dt[, EID := as.integer(str_remove(EID, "sub-"))]

### Comparison of xfms
xfm_rv.dt   <- fread(fpaths[3], key = "EID")
xfm_vv.dt   <- fread(fpaths[4], key = "EID")

#xfm.dt      <- xfm_vv.dt[xfm_rv.dt] |>
#melt(id = "EID", variable = "COMP", value = "DIST") |>
#{function(DT)
  #DT[, COMP := factor(COMP, labels = c("Vlad's pipeline: v1-v2",
                                       #"Pipelines: Reza's-Vlad's"))]}()

xfm_rv.dt[, let(RV_Mean = rowMeans(.SD),
                RV_Max  = do.call(pmax, .SD)),
          .SDcols = Corner1:Corner8]
xfm_vv.dt[, let(VV_Mean = rowMeans(.SD),
                VV_Max  = do.call(pmax, .SD)),
          .SDcols = Corner1:Corner8]

xfm.dt      <- xfm_rv.dt[, .SD, EID, .SDcols = 10:11
                         ][xfm_vv.dt[, .SD, EID, .SDcols = 10:11]
                         ][, BTWN_Mean := rowMeans(.SD),
                           .SDcols = patterns("Mean")
                         ][, BTWN_Max  := rowMeans(.SD),
                           .SDcols = patterns("Max")
                         ] |>
melt(id = "EID", value = "DIST") |>
{function(DT) DT[, c("COMP", "MSR") := tstrsplit(variable, "_")
                 ][, variable := NULL
                 ][, MSR := factor(MSR, labels = c("Max of 8 corners",
                                                   "Mean of 8 corners"))
                 ][, COMP := factor(COMP,
                                    levels = c("RV", "VV", "BTWN"),
                                    labels = c("Reza-Vlad",
                                               "V1-V2",
                                               "Average of both"))
                 ]}()

# Plot histograms to compare distributions of distances
xfm_q.dt    <- xfm.dt[,
                      quantile(DIST, probs = c(.25, .5, .75), na.rm = T),
                      .(COMP, MSR)]

ommit.dt    <- xfm.dt[DIST > 25, .N, .(COMP, MSR)]

p1          <- ggplot(xfm.dt[DIST < 25], aes(x = DIST)) +
  theme_classic(base_size = 12) +
  geom_histogram(binwidth = .5, fill = "transparent", colour = "black") +
  geom_vline(data = xfm_q.dt, colour = "red", lty = "dashed",
             aes(xintercept = V1)) +
  facet_grid(rows = vars(COMP), cols = vars(MSR)) +
  labs(title = "Comparison of transformations to stx space:",
       x = "Distance (mm)", y = "Count",
       caption = paste("Histograms are capped to 25.", #ommit.str,
                       "Dashed red lines indicate 25th, 50th & 75th quartiles",
                       "on the complete data."))
#rm(ommit.dt, ommit.str)

here("plots/comps_xfm.png") |>
png(width = 12, height = 7, units = "in", res = 600)
print(p1)
dev.off()

#qc.dt       <- asnet.dt[darq.dt, on = .(EID, INSTANCE)]
##rm(fpaths, darq.dt, asnet.dt)

#### OUT
#outrds      <- here("data/rds/qc_darq-assemblynet.rds")
#qc.dt |> saveRDS(outrds)
#rm(outrds)
