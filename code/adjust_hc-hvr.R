#!/usr/bin/env Rscript

library(here)
library(arrow)
library(data.table)
library(stringr)

### IN
## ICC volume and ScaleFactors
fpath         <- here("data/icc_scale.csv")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

icc_scale.dt  <- fread(fpath)
rm(fpath)

## Segmentations
fpath         <- here("data/hcvcag_segmentations.csv")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

vols.dt       <- fread(fpath)
rm(fpath)

## AssemblyNet
fpath         <- here("data/arrow/UKBB_assemblynet_20240521.arrow")
if (!file.exists(fpath)) {
  sprintf("File: %s is required but could not be found.", fpath) |> stop()
}

asbly_net.dt  <- read_feather(fpath) |> as.data.table()
rm(fpath)

## Covariates
fpath         <- here("data/arrow/UKBB_covars_clean.arrow")
if (file.exists(fpath)) {
  covars.dt   <- read_feather(fpath) |> as.data.table()
} else {
  here("code/parse_covars.R") |> source()
}


### Data processing
## Parse HC/VC subfields into whole ROI
vols.dt[, `:=`(ID = as.numeric(str_extract(id, "(?<=sub-)\\d*")),
               HC_l_dl = (l_hc_t + l_hc_b + l_hc_h),
               HC_r_dl = (r_hc_t + r_hc_b + r_hc_h),
               VC_l_dl = (l_vc_t + l_vc_b + l_vc_h),
               VC_r_dl = (r_vc_t + r_vc_b + r_vc_h))]


## Merge ICC and volumes
icc_scale.dt[, ID := as.numeric(str_extract(PTID, "(?<=sub-)\\d*"))]

#DT            <- covars.dt[!is.na(HC_l_fs) & !is.na(HC_r_fs),
                           #.(ID = as.character(ID), HC_l_fs, HC_r_fs)
                           #][vols.dt, on = "ID"
                           #][icc_scale.dt, on = "ID",
                           #.(ID, ICC, SCALEFACTOR,
                             #HC_l_dl, HC_l_fs,
                             #HC_r_dl, HC_r_fs,
                             #VC_l_dl, VC_r_dl)]

#rm(icc_scale.dt, covars.dt)

#DT            <- melt(DT, measure = patterns(DL = "_dl$", FS = "_fs$"))

#DT[, variable := factor(variable,
                             #labels = c("HC_l", "HC_r", "VC_l", "VC_r"))]

#DT[, `:=`(ROI = str_split_i(variable, "_", 1),
               #SIDE = str_split_i(variable, "_", 2))]

#DT[, variable := NULL]

##volumes[, ICC := ICC / 1000]

### Bring back to native scale
### To convert stx volumes to native space DIVIDE by SCALEFACTOR
### Scale everything to cm^3
#DT[, `:=`(ICC    = ICC / 1000,
               #DL_stx = DL  / 1000,
               #DL_nat = DL  / (SCALEFACTOR * 1000),
               #FS_stx = FS  * SCALEFACTOR / 1000,
               #FS_nat = FS  / 1000)]

#### Regression slopes for PCP & Residual normalizations
### Use only Controls for the models
##volumes_lng <- volumes[QC == "Pass" & PTID %in% controls,
                       ##.(METHOD, ICC,
                         ##HC_l_nat, HC_r_nat,
                         ##CSF_l_nat, CSF_r_nat)] |>
  ##melt(id.vars = c("METHOD", "ICC"),
       ##variable.name = "ROI",
       ##value.name = "VAL")

##rm(controls)

##volumes_lng[, ROI := stringr::str_remove(ROI, "_nat")]
###volumes_lng[, ROI := stringr::str_to_lower(ROI)]

### Power-corrected proportion:
### VOL_adj = VOL / ICC ** b
### b: slope of log(VOL) ~ log(ICC)
#b_pcp_dl.dt   <- DT[, summary(lm(log(DL_nat) ~ log(ICC)))$coefficients[2],
                    #.(ROI, SIDE)]
#b_pcp_fs.dt   <- DT[!is.na(FS_nat),
                    #summary(lm(log(FS_nat) ~ log(ICC)))$coefficients[2],
                    #.(ROI, SIDE)]

#setnames(b_pcp_dl.dt, "V1", "DL_b_pcp")
#setnames(b_pcp_fs.dt, "V1", "FS_b_pcp")

#DT             <- b_pcp_fs.dt[b_pcp_dl.dt, on = .(ROI, SIDE)
                          #][DT, on = .(ROI, SIDE)]

#rm(b_pcp_dl.dt, b_pcp_fs.dt)

### Residuals
### Remove the residuals from VOL ~ ICC regression
### VOL_adj = VOL - b(ICC - ICC_cn)

#b_res_dl.dt   <- DT[, summary(lm(DL_nat ~ ICC))$coefficients[2],
                    #.(ROI, SIDE)]
#b_res_fs.dt   <- DT[!is.na(FS_nat),
                    #summary(lm(FS_nat ~ ICC))$coefficients[2],
                    #.(ROI, SIDE)]

#setnames(b_res_dl.dt, "V1", "DL_b_res")
#setnames(b_res_fs.dt, "V1", "FS_b_res")

#DT            <- b_res_fs.dt[b_res_dl.dt, on = .(ROI, SIDE)
                             #][DT, on = .(ROI, SIDE)]

#rm(b_res_dl.dt, b_res_fs.dt)

#### Apply adjustment methods
#icc_mean      <- DT[, mean(ICC)]
#DT            <- DT[, .(ID, ROI, SIDE,
                        #DL_nat, FS_nat, DL_stx, FS_stx,
                        #DL_prop = DL_nat / ICC,
                        #FS_prop = FS_nat / ICC,
                        #DL_pcp  = DL_nat / ICC ** DL_b_pcp,
                        #FS_pcp  = FS_nat / ICC ** FS_b_pcp,
                        #DL_res  = DL_nat - DL_b_res * (ICC - icc_mean),
                        #FS_res  = FS_nat - FS_b_res * (ICC - icc_mean))]

#DT             <- melt(DT,
                      #measure = patterns(DL = "^DL", FS = "^FS"),
                      #variable = "METHOD")

#DT[, `:=`(SIDE = str_to_upper(SIDE),
          #METHOD = factor(METHOD,
                          #labels = c("raw", "stx", "prop", "pcp", "res")))]
### HVR
#hvr_adj.dt    <- DT[! METHOD %in% c("stx", "prop"), -"FS"] |>
                  #dcast(... ~ ROI) |>
                  #{\(x) x[, .(HVR = HC / (HC + VC)),
                          #.(ID, SIDE, METHOD)]} ()

### HC
#hc_adj.dt     <- DT[ROI != "VC", .(ID, SIDE, METHOD, HC_dl = DL, HC_fs = FS)]

#hc_hvr_adj.dt <- hvr_adj.dt[hc_adj.dt, on = .(ID, SIDE, METHOD)] |>
                  #melt(id = 1:3, variable = "MEASURE", value = "VALUE")

#rm(icc_mean, DT, hc_adj.dt, hvr_adj.dt)

#### Export RDS
#hc_hvr_adj.dt[, ID := as.numeric(ID)]
#hc_hvr_adj.dt |>
  #readr::write_rds(here("data/rds/hc-hvr_icv-adjusted.rds"))
