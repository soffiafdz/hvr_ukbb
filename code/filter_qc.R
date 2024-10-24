#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)

### IN
fpaths      <- c("lists/darq_out_20240408_lng_r152.csv",
                 "data/arrow/UKBB_assemblynet_20240521.arrow",
                 "data/xfm_distances_reza_vlad-lng.csv",
                 "data/xfm_distances_vlad_v1-v2.csv") |> here()

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

## Comparison of xfms
xfm_rv.dt   <- fread(fpaths[3], col.names = c("EID", "DIST_rv"), key = "EID")
xfm_vv.dt   <- fread(fpaths[4], col.names = c("EID", "DIST_vv"), key = "EID")

xfm.dt      <- xfm_vv.dt[xfm_rv.dt] |>
melt(id = "EID", variable = "COMP", value = "DIST") |>
{function(DT)
  DT[, COMP := factor(COMP, labels = c("Vlad's pipeline: v1-v2",
                                       "Pipelines: Reza's-Vlad's"))]}()

# Plot histograms to compare distributions of distances
xfm_q.dt    <- xfm.dt[,
                      quantile(DIST, probs = c(.25, .5, .75), na.rm = T),
                      COMP]
p1          <- ggplot(xfm.dt[DIST < 10], aes(x = DIST)) +
  theme_classic(base_size = 12) +
  geom_histogram(binwidth = 0.05, fill = "transparent", colour = "black") +
  geom_vline(data = xfm_q.dt, colour = "red", lty = "dashed",
             aes(xintercept = V1)) +
  facet_grid(rows = vars(COMP)) +
  labs(title = "Comparison of transformations to stx space:",
       x = "Frobenius norm", y = "Count",
       caption = paste("Histograms are capped to 10.",
                       "Dashed red lines indicate 25th, 50th & 75th quartiles",
                       "on the complete data."))

here("plots/comps_xfm.png") |>
png(width = 12, height = 7, units = "in", res = 600)
print(p1)
dev.off()

qc.dt       <- asnet.dt[darq.dt, on = .(EID, INSTANCE)]
#rm(fpaths, darq.dt, asnet.dt)

### OUT
outrds      <- here("data/rds/qc_darq-assemblynet.rds")
qc.dt |> saveRDS(outrds)
rm(outrds)
