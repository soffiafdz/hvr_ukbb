#!/usr/bin/env Rscript

library(here)
library(data.table)
library(progress)
library(gamlss)
library(gamlss.add)
library(gt)
library(ggplot2)
library(ggtext)
library(ggnewscale)

### REDO plots
REDO_PLOTS  <- TRUE

### REDO plots
REDO_TABLES <- TRUE

## Change between CNN and AssemblyNet
#NETWORK <- "ABLYNET"
NETWORK <- "LPP_CNN"

### INPUT
paths        <- list(
  rds = c("hc-hvr_adj.rds", "../arrow/UKBB_covars_all.arrow"),
  scripts = c("adjust_hc-hvr.R", "parse_covars.R")
) |> Map(
  f = function(Files, Dir) here(Dir, Files),
  c("data/rds", "code")
)

if (file.exists(paths$rds[1])) {
  hc_hvr.lst <- readRDS(paths$rds[1])
} else {
  source(paths$scripts[1])
}

if (file.exists(paths$rds[2])) {
  covars.dt <- arrow::read_feather(paths$rds[2])
} else {
  source(paths$scripts[2])
}
rm(paths)

### Data CLEANING
## Labels
eth_map <- c(
  "White" = "White",
  "British" = "White",
  "Irish" = "White",
  "Any other white background" = "White",
  "Mixed" = "Mixed",
  "White and Black Caribbean" = "Mixed",
  "White and Black African" = "Mixed",
  "White and Asian" = "Mixed",
  "Any other mixed background" = "Mixed",
  "Black or Black British" = "Black",
  "Caribbean" = "Black",
  "African" = "Black",
  "Any other Black background" = "Black",
  "Asian or Asian British" = "Asian",
  "Indian" = "Asian",
  "Bangladeshi" = "Asian",
  "Pakistani" = "Asian",
  "Any other Asian background" = "Asian",
  "Chinese" = "Chinese",
  "Other ethnic group" = "Other",
  "Do not know" = NA,
  "Prefer not to answer" = NA
)

edu_years_map <- c(
  "None of the above"                                       = 7,
  "CSEs or equivalent"                                      = 10,
  "O levels/GCSEs or equivalent"                            = 11,
  "NVQ or HND or HNC or equivalent"                         = 12.5,
  "A levels/AS levels or equivalent"                        = 13,
  "Other professional qualifications eg: nursing, teaching" = 14.5,
  "College or University degree"                            = 16,
  "Prefer not to answer"                                    = NA_real_
)

covars.dt <- covars.dt[
  , .(
    ETH  = eth_map[ETHNIC_ses0],
    EDUC = edu_years_map[EDUC_lv_ses2_0]
  ),
  keyby = "EID"
]

rm(eth_map, edu_years_map)

rois          <- c(
  ICC = "Intracranial volume",
  HC  = "Hippocampus",
  VC  = "Ventricles",
  HVR = "HC-to-VC ratio"
)

adjs  <- c(
  NON = "Unadjusted",
  PRP = "Proportions",
  RES = "Residuals"
)

## Average sides for HC/HVR & Make long format
hc_hvr.lst <- hc_hvr.lst |>
  lapply(function(crs_lvl) {
    # Average of sides
    avg.dt <- crs_lvl$CRS$ALL[
      ,
      # Average sides for all ROIs, except ICC
      lapply(.SD, mean),
      by = .(EID, SEX, AGE, INST, ICC, ADJ),
      .SDcols = names(rois)[-1]
    ]
    avg.dt[, SIDE := "AVG"]

    # Keep only Cross-Sectional cohort & complete samples
    crs_lvl$CRS$ALL |>
    rbind(avg.dt, use.names = T, fill = T) |>
    merge(covars.dt[, -"ETH"], by = "EID") |>
    melt(
      measure = names(rois)[-1],
      na.rm = TRUE,
      value = "VAL",
      variable = "ROI"
    ) |>
    ( \(DT) DT[, let(
        SEX = factor(as.character(SEX)),
        #ETH = factor(ETH),
        SIDE = factor(SIDE),
        ADJ = factor(ADJ)
      )]
    )() |>
  # Key resulting DT
  #setcolorder(c("ETH", "EDUC"), after = "SEX") |>
  setcolorder("EDUC", after = "SEX") |>
  setkey(EID, INST)
  })

### GAMLSS
## Fitting
sides <- c("L", "R", "AVG")

fpaths <- c("s", "-data") |>
  sprintf(fmt = "gamlss_fit%s") |>
  paste(NETWORK, sep = "_") |>
  tolower() |>
  sub(pattern = "lpp_", replacement = "") |>
  sprintf(fmt = "data/rds/%s.rds") |>
  here()

if (all(file.exists(fpaths))) {
  gamlss.fits.lst <- readRDS(fpaths[1])
  gamlss.fits.dt <- readRDS(fpaths[2])
} else {
  pb <- progress_bar$new(
    format = "GAMLSS (fit) | :what [:bar] :current/:total\n",
    total  = 48,
    clear  = FALSE,
    width  = 75
  )

  gamlss.fits.lst <- list()
  gamlss.fits.dt <- hc_hvr.lst[[NETWORK]]
  for (roi in names(rois)[-1]) {
    gamlss.fits.lst[[roi]] <- list()
    for (adj in names(adjs)) {
      if (roi == "HVR" && adj == "NON") next
      gamlss.fits.lst[[roi]][[adj]] <- list()
      for (side in sides) {
        gamlss.fits.lst[[roi]][[adj]][[side]] <- list()

        gamlss.sub.dt <- gamlss.fits.dt[
          roi, on = "ROI"
        ][
          adj, on = "ADJ"
        ][
          side, on = "SIDE"
        ][
          # IDK why some residual ventricles are negative
          VAL > 0
        ]

        if (adj == "NON") {
          pb$tick(tokens = list(
            what = sprintf("%s — %s — %s: NO", roi, adj, side)
          ))

          gamlss.fits.lst[[roi]][[adj]][[side]][["NO"]] <- gamlss(
            formula = VAL ~ cs(AGE), + ICC + SEX + EDUC,
            sigma.formula = ~ cs(AGE) + SEX,
            data = na.omit(gamlss.sub.dt),
            family = NO()
          )

          pb$tick(tokens = list(
            what = sprintf("%s — %s — %s: BCCG", roi, adj, side)
          ))

          gamlss.fits.lst[[roi]][[adj]][[side]][["BCCG"]] <- gamlss(
            formula = VAL ~ cs(AGE), + ICC + SEX + EDUC,
            sigma.formula = ~ cs(AGE) + SEX,
            nu.formula = ~ cs(AGE) + SEX,
            data = na.omit(gamlss.sub.dt),
            family = BCCG()
          )
        } else {
          pb$tick(tokens = list(
            what = sprintf("%s — %s — %s: NO", roi, adj, side)
          ))

          gamlss.fits.lst[[roi]][[adj]][[side]][["NO"]] <- gamlss(
            formula = VAL ~ cs(AGE), + SEX + EDUC,
            sigma.formula = ~ cs(AGE) + SEX,
            data = na.omit(gamlss.sub.dt),
            family = NO()
          )

          pb$tick(tokens = list(
            what = sprintf("%s — %s — %s: BCCG", roi, adj, side)
          ))

          gamlss.fits.lst[[roi]][[adj]][[side]][["BCCG"]] <- gamlss(
            formula = VAL ~ cs(AGE), + SEX + EDUC,
            sigma.formula = ~ cs(AGE) + SEX,
            nu.formula = ~ cs(AGE) + SEX,
            data = na.omit(gamlss.sub.dt),
            family = BCCG()
          )
        }
      }
    }
  }
  saveRDS(gamlss.fits.lst, fpaths[1])
  saveRDS(gamlss.fits.dt, fpaths[2])
  rm(roi, adj, side, gamlss.sub.dt)
}
rm(fpaths)

## AIC & BIC
aic_bic.lst <- list()
for (roi in names(rois)[-1]) {
  for (adj in names(adjs)) {
    if (roi == "HVR" && adj == "NON") next
    for (side in sides) {
      for (mod in c("NO", "BCCG")) {
        f <- gamlss.fits.lst[[roi]][[adj]][[side]][[mod]]
        aic_bic.lst[[length(aic_bic.lst) + 1]] <- data.table(
          ROI = roi, ADJ = adj, SIDE = side, FAM = mod,
          AIC = tryCatch(AIC(f), error = \(e) NA),
          BIC = tryCatch(BIC(f), error = \(e) NA)
        )
      }
    }
  }
}
aic_bic.dt <- rbindlist(aic_bic.lst)
rm(roi, adj, side, mod, f, aic_bic.lst)

### Tables & Z-scores
sexes  <- hc_hvr.lst[[NETWORK]][, levels(SEX)]
# Range: 44.6 -> 82.8
ages    <- hc_hvr.lst[[NETWORK]][, seq(min(ceiling(AGE)), max(floor(AGE)), 1)]
# Median covariates
ed_med  <- hc_hvr.lst[[NETWORK]][, median(EDUC, na.rm = T)]
icc_med <- hc_hvr.lst[[NETWORK]][, median(ICC)]


fpaths <- c("norm_tables", "z-scores") |>
  paste(NETWORK, sep = "_") |>
  tolower() |>
  sub(pattern = "lpp_", replacement = "") |>
  sprintf(fmt = "data/rds/%s.rds") |>
  here()

if (all(file.exists(fpaths), !REDO_TABLES)) {
  cent_tbls.lst <- readRDS(fpaths[1])
  z_scores.dt   <- readRDS(fpaths[2])
} else {
  if (any(!file.exists(fpaths[1]), REDO_TABLES)) cent_tbls.lst <- list()
  if (any(!file.exists(fpaths[2]), REDO_TABLES)) z_scores.dt  <- data.table()
  for (roi in names(rois)[-1]) {
    for (adj in names(adjs)) {
      if (roi == "HVR" && adj == "NON") next
      for (side in sides) {
        # predict.gamlss Needs the original data to exist in env
        gamlss.sub.dt <- gamlss.fits.dt[
          roi, on = "ROI"
        ][
          adj, on = "ADJ"
        ][
          side, on = "SIDE"
        ][
          # IDK why some residual ventricles are negative
          VAL > 0
        ]

        for (mod in c("NO", "BCCG")) {
          # Model fit
          f <- gamlss.fits.lst[[roi]][[adj]][[side]][[mod]]

          # Tables
          if (any(!file.exists(fpaths[1]), REDO_TABLES)) {
            tab.lst <- list()
            for (sex in sexes) {
              # Parameters
              mu  <- predict(
                f,
                newdata = data.table(
                  SEX = sex, AGE = ages, ICC = icc_med, EDUC = ed_med
                ),
                what = "mu",
                type = "response"
              )
              sig <- predict(
                f,
                newdata = data.table(
                  SEX = sex, AGE = ages, ICC = icc_med, EDUC = ed_med
                ),
                what = "sigma",
                type = "response"
              )
              if (mod == "BCCG") {
                nu <- predict(
                  f,
                  newdata = data.table(
                    SEX = sex, AGE = ages, ICC = icc_med, EDUC = ed_med
                  ),
                  what = "nu",
                  type = "response"
                )
              }
              # Centiles (Deciles)
              cent <- seq(10, 90, 5)
              if (mod == "NO") {
                cent_vals <- sapply(
                  cent, \(p) qNO(p / 100, mu = mu, sigma = sig)
                )
                #rm(mu, sig)
              } else {
                cent_vals <- sapply(
                  cent, \(p) qBCCG(p / 100, mu = mu, sigma = sig, nu = nu)
                )
                #rm(mu, sig, nu)
              }
              cent.dt <- data.table(SEX = sex, AGE = ages, MU = mu, cent_vals)
              setnames(cent.dt, paste0("V", 1:length(cent)), paste0("p", cent))
              tab.lst[[sex]] <- cent.dt
            }
            cent_tbls.lst[[roi]][[adj]][[side]][[mod]] <- rbindlist(tab.lst)
            rm(cent, cent_vals, cent.dt, sex, tab.lst)
          }

          # Z-scores
          if (any(!file.exists(fpaths[2]), REDO_TABLES)) {
            subDT <- copy(gamlss.sub.dt)
            # Parameters
            mu  <- predict(
              f, newdata = subDT, what = "mu", type = "response"
            )
            sig <- predict(
              f, newdata = subDT, what = "sigma", type = "response"
            )
            if (mod == "BCCG") {
              nu <- predict(
                f, newdata = subDT, what = "nu", type = "response"
              )
            }
            # Prediction
            if (mod == "NO") {
              subDT[, let(FAMILY = mod, Z = (VAL - mu) / sig)]
            } else {
              subDT[, let(FAMILY = mod, Z = (((VAL / mu)^nu - 1)/ (nu * sig)))]
            }
            z_scores.dt <- rbind(z_scores.dt, subDT)
            rm(subDT)
          }
        }
      }
    }
  }
  # Roi
  if (any(!file.exists(fpaths[1]), REDO_TABLES)) {
    saveRDS(cent_tbls.lst, fpaths[1])
  }
  if (any(!file.exists(fpaths[2]), REDO_TABLES)) {
    saveRDS(z_scores.dt, fpaths[2])
  }
  rm(gamlss.sub.dt)
}
rm(fpaths)

## Normative tables
tables.lst <- list()
outdir <- here("tables")
if (!file.exists(outdir)) dir.create(outdir)
exts <- c("html", "tex")
mod_names <- c(
  NO = "Normal distribution",
  BCCG = "Box-Cox Cole & Green distribution"
)
summ_cents <- c("p10", "p25", "p50", "p75", "p90")
cols <- c("SEX", "AGE", "SIDE", "FAMILY", summ_cents)
for (roi in names(rois)[-1]) {
  for (adj in names(adjs)) {
    if (roi == "HVR" && adj == "NON") next
    cent_all.dt <- cent_summ.dt <- data.table()
    # Extract sides. Tables will be: L-R & AVG
    for (side in sides) {
      # Extract two models
      for (mod in c("NO", "BCCG")) {
        subDT <- cent_tbls.lst[[roi]][[adj]][[side]][[mod]]
        subDT[, let(SIDE = side, FAMILY = mod)]
        # All percentiles (CSV)
        cent_all.dt <- subDT[, -"MU"] |> copy() |> rbind(cent_all.dt)
        # Summary (LaTeX)
        cent_summ.dt <- subDT[, ..cols] |> copy() |> rbind(cent_summ.dt)
      }
    }

    # Minimize Sex
    cent_summ.dt[, SEX := factor(SEX, labels = c("F", "M"))]

    # Age Bands
    cent_summ.dt[
      ,
      AGE_band := cut(
        AGE,
        breaks = seq(45, 90, 5),
        right = FALSE,
        labels = paste(seq(45, 85, 5), seq(49, 89, 5), sep = "-")
      )
    ][
      , AGE := NULL
    ]

    # Median by 5y bands
    cent_summ.dt <- cent_summ.dt[
      ,
      lapply(.SD, median, na.rm = TRUE),
      by = .(SEX, AGE_band, SIDE, FAMILY),
      .SDcols = summ_cents
    ]

    ## Tables
    for (mod in c("NO", "BCCG")) {
      ## Left vs Right
      fpaths <- "gamlss-%s_%s-%s_lr.%s" |>
        sprintf(mod, roi, adj, exts) |>
        tolower()

      tbl <- cent_summ.dt[
        mod, on = "FAMILY", -"FAMILY"
      ][
        !"AVG", on = "SIDE"
      ] |>
      dcast(... ~ SIDE, value.var = summ_cents) |>
      gt(
        rowname_col = "SEX",
        groupname_col = "AGE_band",
        row_group_as_column = TRUE
      ) |>
      tab_options(footnotes.multiline = T, latex.tbl.pos = "h") |>
      fmt_number(decimals = 3) |>
      tab_stubhead(label = "Age bands") |>
      cols_align("center", columns = everything()) |>
      tab_spanner(label = "10th", columns = starts_with("p10")) |>
      tab_spanner(label = "25th", columns = starts_with("p25")) |>
      tab_spanner(label = "50th", columns = starts_with("p50")) |>
      tab_spanner(label = "75th", columns = starts_with("p75")) |>
      tab_spanner(label = "90th", columns = starts_with("p90")) |>
      cols_label(
        ends_with("_L") ~ "Left",
        ends_with("_R") ~ "Right"
      ) |>
      tab_footnote(footnote = sprintf("GAMLSS (%s);", mod_names[[mod]]))

      if (roi == "HVR") {
        tbl <- tbl |>
        tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
        )
      } else {
        if (adj == "NON") {
          tbl <- tbl |>
          tab_footnote(
            footnote = "Cubic spline: Age; Covariates: TIV, sex, and education."
          )
        }
        if (adj == "PRP") {
          tbl <- tbl |>
          tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
          ) |>
          tab_footnote(footnote = "Head-size adjustment: Proportions method.")
        }
        if (adj == "RES") {
          tbl <- tbl |>
          tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
          ) |>
          tab_footnote(footnote = "Head-size adjustment: Residuals method.")
        }
      }

      # Table saving
      tables.lst[[roi]][[adj]][[mod]][["LR"]] <- tbl
      for (f in fpaths) {
        if (any(!file.exists(here(outdir, f)), REDO_TABLES)) {
          tables.lst[[roi]][[adj]][[mod]]$LR |> gtsave(f, outdir)
        }
      }

      ## Average
      fpaths <- "gamlss-%s_%s-%s_avg.%s" |>
        sprintf(mod, roi, adj, exts) |>
        tolower()

      tbl <- cent_summ.dt[
        mod, on = "FAMILY", -"FAMILY"
      ][
        "AVG", on = "SIDE", -"SIDE"
      ] |>
      gt(
        rowname_col = "SEX",
        groupname_col = "AGE_band",
        row_group_as_column = TRUE
      ) |>
      tab_options(latex.tbl.pos = "h") |>
      fmt_number(decimals = 3) |>
      tab_stubhead(label = "Age bands") |>
      cols_align("center", columns = everything()) |>
      cols_label(
        p10 = "10th",
        p25 = "25th",
        p50 = "50th",
        p75 = "75th",
        p90 = "90th"
      ) |>
      tab_footnote(footnote = sprintf("GAMLSS (%s);", mod_names[[mod]]))

      if (roi == "HVR") {
        tbl <- tbl |>
        tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
        )
      } else {
        if (adj == "NON") {
          tbl <- tbl |>
          tab_footnote(
            footnote = "Cubic spline: Age; Covariates: TIV, sex, and education."
          )
        }
        if (adj == "PRP") {
          tbl <- tbl |>
          tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
          ) |>
          tab_footnote(footnote = "Head-size adjustment: Proportions method.")
        }
        if (adj == "RES") {
          tbl <- tbl |>
          tab_footnote(
          footnote = "Cubic spline: Age; Covariates: sex & education."
          ) |>
          tab_footnote(footnote = "Head-size adjustment: Residuals method.")
        }
      }

      # Table saving
      tables.lst[[roi]][[adj]][[mod]][["AVG"]] <- tbl
      for (f in fpaths) {
        if (any(!file.exists(here(outdir, f)), REDO_TABLES)) {
          tables.lst[[roi]][[adj]][[mod]]$AVG |> gtsave(f, outdir)
        }
      }
    }
  }
}


### Plots
## GAMLSS Fit vs observed
plots.lst <- plot_params.lst <- list()
plot_params.lst[["CENTS"]] <- c(10, 25, 50, 75, 90) |> sprintf(fmt = "p%i")
#plot_params.lst[["LINECOLS"]] <- c(
  #"p10" = "#333399",     # indigo
  #"p25" = "#0072b2",     # blue
  #"p50" = "#222222",     # black/charcoal for median
  #"p75" = "#e69f00",     # orange
  #"p90" = "#d55e00"      # red-orange
#)
plot_params.lst[["LINETYPES"]] <- c(
  "p10" = "dashed",
  "p25" = "dotdash",
  "p50" = "solid",
  "p75" = "dotdash",
  "p90" = "dashed"
)
plot_params.lst[["SEX"]][["COLS"]] <- c(
  Female = "darkred",
  Male = "midnightblue"
)
plot_params.lst[["SEX"]][["MD"]] <- c(
  Female = "<span style='color: darkred;'>Females</span>",
  Male = "<span style='color: midnightblue;'>Males</span>"
)
plot_params.lst[["Y_LABS"]] <- lapply(
  rois[-1],
  \(roi) lapply(
    adjs, \(adj) sprintf(
      "%s (%s)",
      roi,
      fcase(
        roi %like% "ratio", "[0-1]",
        adj %like% "Prop", "CC/ICV",
        default = "CC"
      )
    )
  )
)
plot_params.lst[["NOTES"]] <- list(
  NON = paste(
    "Controlling for head-size adding ICC as a covariate in the model.",
    "Centiles are calculated with medians for ICC and years of Education.",
    sep = "\n"
  ),
  PRP = paste(
    "Controlling for head-size with the proportions method.",
    "Centiles are calculated with median of years of Education.",
    sep = "\n"
  ),
  RES = paste(
    "Controlling for head-size with the residuals method.",
    "Centiles are calculated with median of years of Education.",
    sep = "\n"
  ),
  HVR = "Centiles are calculated with median of years of Education."
)

outdir <- here("plots")
if (!file.exists(outdir)) dir.create(outdir, recursive = TRUE)

# Plotting loop
for (roi in names(rois)[-1]) {
  for (adj in names(adjs)) {
    if (roi == "HVR" && adj == "NON") next
    cent.dt <- data.table()
    # Extract sides. Plotting will be: L-R & AVG
    for (side in sides) {
      # Extract two models
      for (mod in c("NO", "BCCG")) {
        cols <- c("SEX", "AGE", "MU", plot_params.lst$CENTS)
        subDT <- cent_tbls.lst[[roi]][[adj]][[side]][[mod]][, ..cols]
        subDT[, let(SIDE = side, FAMILY = mod)]
        cent.dt <- rbind(cent.dt, subDT)
      }
    }
    # Observed data
    obs.dt <- gamlss.fits.dt[
        roi, on = "ROI"
      ][
        adj, on = "ADJ"
      ][
        # IDK why some residual ventricles are negative
        VAL > 0
      ] |> copy()

    # Centile data
    cent.dt <- melt(
      cent.dt,
      measure = plot_params.lst$CENTS,
      variable = "Centiles",
      value = "VAL"
    )

    # Males & Females:
    setnames(obs.dt, "SEX", "Sex")
    obs.dt[, SEX := factor(Sex, labels = plot_params.lst$SEX$MD)]

    cent.dt[
      , let(
        Sex = factor(SEX),
        SEX = factor(SEX, labels = plot_params.lst$SEX$MD)
      )
    ]

    ## Plotting
    for (mod in c("NO", "BCCG")) {
      fpaths <- "%s/gamlss-%s_%s-%s_%s_%s.png" |>
        sprintf(outdir, mod, roi, adj, c("lr", "avg"), NETWORK) |>
        tolower()

      ## Left vs Right
      plots.lst[[roi]][[adj]][[mod]][["LR"]] <- ggplot() +
      theme_classic(base_size = 11) +
      theme(
        text = element_text(size = 11),
        axis.text = element_text(size = 10),
        axis.text.x = element_markdown(),
        axis.text.y = element_markdown(),
        strip.text = element_markdown(),
        plot.caption = element_text(size = 8),
        legend.position = "bottom"
      ) +
      geom_point(
        aes(x = AGE, y = VAL, colour = Sex),
        obs.dt[!"AVG", on = "SIDE"],
        alpha = 0.2,
        size = 0.5,
        shape = 21
      ) +
      scale_colour_manual(values = plot_params.lst$SEX$COLS) +
      geom_smooth(
        aes(x = AGE, y = VAL, linetype = Centiles),
        cent.dt[!"AVG", on = "SIDE"],
        se = FALSE,
        colour = "#222222",
        method = "loess",
        span = 0.35,
        linewidth = .5
        #linetype = "dotdashed",
      ) +
      scale_linetype_manual(values = plot_params.lst$LINETYPES) +
      facet_grid(cols = vars(SIDE), rows = vars(SEX)) +
      labs(
        title = "GAMLSS Normative Centiles (by sex and hemispheres)",
        caption = fifelse(
          roi == "HVR",
          plot_params.lst$NOTES$HVR,
          plot_params.lst$NOTES[[adj]]
        ),
        x = "Age (years)",
        y = plot_params.lst$Y_LABS[[roi]][[adj]],
      )

      # Plot saving
      if (any(!file.exists(fpaths[1]), REDO_PLOTS)) {
        plots.lst[[roi]][[adj]][[mod]][["LR"]] |>
          ggsave(
            filename = fpaths[1],
            width = 7,
            height = 7,
            units = "in",
            dpi = 600
          )
      }

      # Averaged by hemispheres
      plots.lst[[roi]][[adj]][[mod]][["AVG"]] <- ggplot() +
      theme_classic(base_size = 11) +
      theme(
        text = element_text(size = 11),
        axis.text = element_text(size = 10),
        plot.title = element_markdown(),
        strip.text = element_markdown(),
        plot.caption = element_text(size = 9),
        legend.position = "bottom"
      ) +
      geom_point(
        aes(x = AGE, y = VAL, colour = Sex),
        obs.dt["AVG", on = "SIDE"],
        alpha = 0.2,
        size = 0.5,
        shape = 21
      ) +
      scale_colour_manual(values = plot_params.lst$SEX$COLS) +
      geom_smooth(
        aes(x = AGE, y = VAL, linetype = Centiles),
        cent.dt["AVG", on = "SIDE"],
        se = FALSE,
        colour = "#222222",
        method = "loess",
        span = 0.35,
        linewidth = .5
      ) +
      scale_linetype_manual(values = plot_params.lst$LINETYPES) +
      facet_grid(cols = vars(SEX)) +
      labs(
        title = "GAMLSS Normative Centiles (by sex and hemispheres)",
        x = "Age (years)",
        y = plot_params.lst$Y_LABS[[roi]][[adj]],
        caption = fifelse(
            roi == "HVR",
            plot_params.lst$NOTES$HVR,
            plot_params.lst$NOTES[[adj]]
        )
      )

      # Plot saving
      if (any(!file.exists(fpaths[2]), REDO_PLOTS)) {
        plots.lst[[roi]][[adj]][[mod]][["AVG"]] |>
          ggsave(
            filename = fpaths[2],
            width = 7,
            height = 4,
            units = "in",
            dpi = 600
          )
      }
    }
  }
}
