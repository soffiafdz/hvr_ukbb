#!/usr/bin/env Rscript

library(here)
library(data.table)
library(readr)
#library(bootES)
library(effsize)
library(ggplot2)
library(ggtext)

## Remake plots
ReDoPlots     <- TRUE
ReRunSims     <- FALSE

## Import files
# Covariates
fpath       <- here("data/rds/covars.rds")
if (file.exists(fpath)) {
  covars.dt <- read_rds(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

# Adjusted HC/HVR
fpath       <- here("data/rds/hc-hvr_icv-adjusted.rds")
if (file.exists(fpath)) {
  hc_hvr_adj.dt <- read_rds(fpath)
} else {
  here("code/adjust_hc-hvr.R") |> source()
}

## Average sides for hc/hvr
hc_hvr_adj.dt <- hc_hvr_adj.dt[, .(VALUE = mean(VALUE)),
              .(ID, METHOD, MEASURE)]

## Merge
DT          <- covars.dt[, .(ID, SEX)][hc_hvr_adj.dt, on = "ID"]

# Remove NAs
DT          <- DT[!is.na(SEX) & !is.na(VALUE)]

## Effect sizes
# HC volume (sum of sides)
mtds  <- DT[, levels(METHOD)]

# HC CNN
fnames <- here(paste0("data/rds/effect-sizes_hcv-dl", c(".rds", "_sims.rds")))
if (all(file.exists(fnames), !ReRunSims)) {
  effvals_hcv_dl <- read_rds(fnames[1])
  #effsims_hcv_dl <- read_rds(fnames[2])
} else {
  effs <- bounds_l <- bounds_h <- vector()
  for (mtd in mtds) {
    dt <- DT[METHOD == mtd & MEASURE == "HC_dl"]
    effect <- cohen.d(dt[SEX == "M", VALUE], dt[SEX == "F", VALUE])
    effs        <- c(effs, effect$estimate)
    bounds_l    <- c(bounds_l, effect$conf.int[1])
    bounds_h    <- c(bounds_h, effect$conf.int[2])
  }

  effvals_hcv_dl <- data.table(MEASURE   = "HCvol_dl",
                               METHOD    = mtds,
                               EFFECT    = round(effs, 2),
                               BOUNDS_l  = round(bounds_l, 2),
                               BOUNDS_h  = round(bounds_h, 2))

  effvals_hcv_dl[, LABEL := paste0("d = ", EFFECT,
                                       " [", BOUNDS_l,
                                       ", ", BOUNDS_h, "]")]
  write_rds(effvals_hcv_dl, fnames[1])
  #write_rds(effsims_hcv_dl, fnames[2])
}


# HC FS
fnames <- here(paste0("data/rds/effect-sizes_hcv-fs", c(".rds", "_sims.rds")))
if (all(file.exists(fnames), !ReRunSims)) {
  effvals_hcv_fs <- read_rds(fnames[1])
  #effsims_hcv_fs <- read_rds(fnames[2])
} else {
  effs <- bounds_l <- bounds_h <- vector()
  for (mtd in mtds) {
    dt <- DT[METHOD == mtd & MEASURE == "HC_fs"]
    effect <- cohen.d(dt[SEX == "M", VALUE], dt[SEX == "F", VALUE])
    effs        <- c(effs, effect$estimate)
    bounds_l    <- c(bounds_l, effect$conf.int[1])
    bounds_h    <- c(bounds_h, effect$conf.int[2])
  }

  effvals_hcv_fs <- data.table(MEASURE   = "HCvol_fs",
                               METHOD    = mtds,
                               EFFECT    = round(effs, 2),
                               BOUNDS_l  = round(bounds_l, 2),
                               BOUNDS_h  = round(bounds_h, 2))

  effvals_hcv_fs[, LABEL := paste0("d = ", EFFECT,
                                       " [", BOUNDS_l,
                                       ", ", BOUNDS_h, "]")]
  write_rds(effvals_hcv_fs, fnames[1])
  #write_rds(effsims_hcv_fs, fnames[2])
}

# HVR
fnames <- here(paste0("data/rds/effect-sizes_hvr", c(".rds", "_sims.rds")))
if (all(file.exists(fnames), !ReRunSims)) {
  effvals_hvr <- read_rds(fnames[1])
  #effsims_hvr <- read_rds(fnames[2])
} else {
  effs <- bounds_l <- bounds_h <- vector()
  mtds <- mtds[-2:-3]
  for (mtd in mtds) {
    dt <- DT[METHOD == mtd & MEASURE == "HVR"]
    effect <- cohen.d(dt[SEX == "M", VALUE], dt[SEX == "F", VALUE])
    effs        <- c(effs, effect$estimate)
    bounds_l    <- c(bounds_l, effect$conf.int[1])
    bounds_h    <- c(bounds_h, effect$conf.int[2])
  }

  effvals_hvr <- data.table(MEASURE   = "HVR",
                            METHOD    = mtds,
                            EFFECT    = round(effs, 2),
                            BOUNDS_l  = round(bounds_l, 2),
                            BOUNDS_h  = round(bounds_h, 2))

  effvals_hvr[, LABEL := paste0("d = ", EFFECT, " [", BOUNDS_l,
                                ", ", BOUNDS_h, "]")]
  write_rds(effvals_hvr, fnames[1])
  #write_rds(effsims_hvr, fnames[2])
}

## Merge
effvals.dt  <- rbindlist(list(effvals_hcv_dl, effvals_hcv_fs, effvals_hvr))

## Plot
cbPalette     <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

effvals.dt[, MEASURE := factor(MEASURE,
                               levels = c("HCvol_dl", "HCvol_fs", "HVR"),
                               labels = c("HCvol (CNN)", "HCvol (FS)", "HVR"))]
effvals.dt[, METHOD := factor(METHOD,
                              levels = c("raw", "stx", "prop", "pcp", "res"),
                              labels = c("Raw", "STX", "Prop", "PCP", "Res"))]

DT[, SEX := factor(SEX, labels = c("Females", "Males"))]
DT[, MEASURE := factor(MEASURE,
                       levels = c("HC_dl", "HC_fs", "HVR"),
                       labels = c("HCvol (CNN)", "HCvol (FS)", "HVR"))]
DT[, METHOD := factor(METHOD,
                      levels = c("raw", "stx", "prop", "pcp", "res"),
                      labels = c("Raw", "STX", "Prop", "PCP", "Res"))]

g <- DT[MEASURE == "HCvol (CNN)"] |>
  ggplot(aes(x = VALUE, colour = SEX)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_fill_manual(values = cbPalette[c(2:3, 8)]) +
  scale_colour_manual(values = cbPalette[c(2:3, 8)]) +
  #geom_histogram(fill = "transparent", bins = 45) +
  geom_density(alpha = 1) +
  geom_vline(data = DT[MEASURE == "HCvol (CNN)",
                       mean(VALUE),
                       .(MEASURE, SEX, METHOD)],
             aes(xintercept = V1, colour = SEX), linetype = "dashed",
             alpha = .8) +
  #facet_grid(cols = vars(METHOD), scales = "free") +
  facet_wrap(facets = vars(METHOD), scales = "free", nrow = 1) +
  geom_richtext(data = effvals.dt[MEASURE == "HCvol (CNN)"],
                aes(label = LABEL), inherit.aes = FALSE,
                colour = "Black", fill = "White", size = 3,
                x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.25) +
  labs(title = "HCvol (CNN)", x = NULL, y = NULL, colour = "Sex")

fnames <- here(paste("plots/effsizes-sex_hc-dl", c("png", "tiff"), sep = "."))
if (!file.exists(fnames[1]) || ReDoPlots) {
  png(fnames[1], width = 10, height = 3, units = "in", res = 600)
  print(g)
  dev.off()
}

#if (!file.exists(fnames[2]) || ReDoPlots) {
  #tiff(fnames[2], width = 13, height = 5, units = "in", res = 600)
  #print(g)
  #dev.off()
#}

g <- DT[MEASURE == "HCvol (FS)"] |>
  ggplot(aes(x = VALUE, colour = SEX)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_fill_manual(values = cbPalette[c(2:3, 8)]) +
  scale_colour_manual(values = cbPalette[c(2:3, 8)]) +
  #geom_histogram(fill = "transparent", bins = 45) +
  geom_density(alpha = 1) +
  geom_vline(data = DT[MEASURE == "HCvol (FS)",
                       mean(VALUE),
                       .(MEASURE, SEX, METHOD)],
             aes(xintercept = V1, colour = SEX), linetype = "dashed",
             alpha = .8) +
  #facet_grid(cols = vars(METHOD), scales = "free") +
  facet_wrap(facets = vars(METHOD), scales = "free", nrow = 1) +
  geom_richtext(data = effvals.dt[MEASURE == "HCvol (FS)"],
                aes(label = LABEL), inherit.aes = FALSE,
                colour = "Black", fill = "White", size = 3,
                x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.25) +
  labs(title = "HCvol (FS)", x = NULL, y = NULL, colour = "Sex")

fnames <- here(paste("plots/effsizes-sex_hc-fs", c("png", "tiff"), sep = "."))
if (!file.exists(fnames[1]) || ReDoPlots) {
  png(fnames[1], width = 10, height = 3, units = "in", res = 600)
  print(g)
  dev.off()
}

g <- DT[MEASURE == "HVR"] |>
  ggplot(aes(x = VALUE, colour = SEX)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  scale_fill_manual(values = cbPalette[c(2:3, 8)]) +
  scale_colour_manual(values = cbPalette[c(2:3, 8)]) +
  #geom_histogram(fill = "transparent", bins = 45) +
  geom_density(alpha = 1) +
  geom_vline(data = DT[MEASURE == "HVR",
                       mean(VALUE),
                       .(MEASURE, SEX, METHOD)],
             aes(xintercept = V1, colour = SEX), linetype = "dashed",
             alpha = .8) +
  #facet_grid(cols = vars(METHOD), scales = "free") +
  facet_wrap(facets = vars(METHOD), scales = "free", nrow = 1) +
  geom_richtext(data = effvals.dt[MEASURE == "HVR"],
                aes(label = LABEL), inherit.aes = FALSE,
                colour = "Black", fill = "White", size = 3,
                x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.25) +
  labs(title = "HVR", x = NULL, y = NULL, colour = "Sex")

fnames <- here(paste("plots/effsizes-sex_hvr", c("png", "tiff"), sep = "."))
if (!file.exists(fnames[1]) || ReDoPlots) {
  png(fnames[1], width = 10, height = 3, units = "in", res = 600)
  print(g)
  dev.off()
}
