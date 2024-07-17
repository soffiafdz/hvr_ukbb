#!/usr/bin/env Rscript

library(here)
library(data.table)
library(readr)
library(ggplot2)

## Rerun permutation analyses
ReRunPerms      <- FALSE

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

# Memory scores
fpath       <- here("data/rds/memory_scores.rds")
if (file.exists(fpath)) {
  memory.dt <- read_rds(fpath)
} else {
  here("code/factor_memory.R") |> source()
}

rm(fpath)

## Average sides for hc/hvr
hc_hvr_adj.dt <- hc_hvr_adj.dt[, .(VALUE = mean(VALUE)),
              .(ID, METHOD, MEASURE)]

## Merge
DT          <- covars.dt[, .(ID, SEX, AGE)
                         ][memory.dt[, .(ID, MEMORY)], on = "ID"
                         ][hc_hvr_adj.dt, on = "ID"]

# Remove NAs
DT          <- DT[!is.na(SEX) & !is.na(MEMORY) & !is.na(VALUE)]

## Correlation | Permutation tests
fpaths      <- here("data/rds", paste0("hc-hvr_corrs_", c("", "perms_"),
                                       "non-parametric.rds"))

if (all(file.exists(fpaths), !ReRunPerms)) {
  corr.dt       <- read_rds(fpaths[1])
  perms.dif.dt  <- read_rds(fpaths[2])
} else {
  sex           <- DT[, levels(SEX)]
  msrs          <- DT[, levels(MEASURE)]
  mtds          <- DT[, levels(METHOD)]
  covs          <- c("AGE", "MEMORY")

  n_perms       <- 10000

  r <- t <- dfs <- pval <- cil <- cih <-
    rep(NA, length(sex) * length(msrs) * length(mtds) * length(covs))

  #TODO: think about how to do the comparisons
  #cor.difs1 <- rep(NA,
                   #length(sex) * length(msrs) * length(mtds) * length(covs)
                   #* n_perms)
  #cor.difs2 <- rep(NA, length(dxs) * length(covs))

  set.seed(1618)
  i <- j <- k <- 0
  #TODO: Adjust this
  #pb <- progress_bar$new(format = "Permutations | :what [:bar] :current/:total",
                         #total = length(dxs) * length(covs) * length(mtds1) *
                           #n_perms + length(dxs) * length(covs) * n_perms,
                         #clear = FALSE, width = 75)
  for (s in sex) {
    for (cov in covs) {
      for (mtd in mtds) {
        dt        <- DT[SEX == s & METHOD == mtd] |>
          melt(measure = covs, variable = "COVAR", value = "VALUE2")

        for (msr in msrs) {
          i <- i + 1
          if (dt[MEASURE == msr, .N] == 0) next
          corr    <- dt[COVAR == cov & MEASURE == msr,
                        cor.test(VALUE, VALUE2)]
          r[i]    <- corr$estimate
          t[i]    <- corr$statistic
          pval[i] <- corr$p.value
          cil[i]  <- corr$conf.int[1]
          cih[i]  <- corr$conf.int[2]
        }

        #for (p in 1:n_perms) {
          #pb$tick(tokens = list(what = paste(dx, mtd, sep = ":")))
          #longDT[, SHUFFLE := sample(MEASURE)]
          #cor.1   <- longDT[COVAR == cov & SHUFFLE == "HVR",
                            #cor(VAL1, VAL2, method = "spearman")]
          #cor.2   <- longDT[COVAR == cov & SHUFFLE == "HCv",
                            #cor(VAL1, VAL2, method = "spearman")]
          #cor.difs1[p + j * n_perms] <- cor.1 - cor.2
        #}
        #j <- j + 1 # Increase counter
      }
      #wideDT      <- DT[DX == dx & METHOD %in% c("cnn", "fs6")]

      #longDT      <- wideDT |>
                    #melt(measure.vars   = covs,
                         #variable.name  = "COVAR",
                         #value.name     = "VAL2")

      #for (p in 1:n_perms) {
        #pb$tick(tokens = list(what = paste(dx, "CNN vs FS", sep = ":")))
        #longDT[, SHUFFLE := sample(METHOD)]
        #cor.1   <- longDT[COVAR == cov & SHUFFLE == "cnn",
                          #cor(HVR, VAL2, method = "spearman")]
        #cor.2   <- longDT[COVAR == cov & SHUFFLE == "fs6",
                          #cor(HVR, VAL2, method = "spearman")]
        #cor.difs2[p + k * n_perms] <- cor.1 - cor.2
      #}
      #k <- k + 1
    }
  }

  corr.dt       <- data.table(SEX     = rep(sex, each = 2 * 5 * 3),
                              COVAR   = rep(covs, times = 2, each = 5 * 3),
                              METHOD  = rep(mtds, times = 2 * 2, each = 3),
                              MEASURE = rep(msrs, times = 2 * 2 * 5),
                              R       = r,
                              Tstat   = t,
                              Pval    = pval,
                              #DF      = dfs,
                              CIhigh  = cih,
                              CIlow   = cil)

  corr.dt <- corr.dt[!is.na(R)]
  write_rds(corr.dt, fpaths[1])

  ##perms.dif1.dt <- data.table(DX      = rep(dxs, each = 3 * 4 * n_perms),
                              ##COVAR   = rep(covs, times = 3,
                                            ##each = 4 * n_perms),
                              ##METHOD  = rep(mtds2, times = 3 * 3,
                                            ##each = n_perms),
                              ##DIFF_p  = cor.difs1)

  ##perms.dif2.dt <- data.table(DX      = rep(dxs, each = 3 * n_perms),
                              ##COVAR   = rep(covs, times = 3, each = n_perms),
                              ##DIFF_p  = cor.difs2)

  ##perms.dif2.dt[, METHOD := "CNN-FS_V6"]

  ##perms.dif.dt  <- rbindlist(list(perms.dif1.dt, perms.dif2.dt),
                             ##use.names = TRUE)

  ##write_rds(perms.dif.dt, fpaths[2])
}

## R differences DTs
#corr.hc.dt    <- corr.dt[, .(DX, COVAR, METHOD, HC, R)] |>
                #dcast(... ~ HC, value.var = "R")

#corr.seg.dt   <- corr.hc.dt[METHOD %in% c("CNN", "FS_V6"),
                            #.(DX, COVAR, METHOD, HVR)] |>
                #dcast(... ~ METHOD, value.var = "HVR")

#corr.hc.dt[, DIFF := HVR - HCv][, c("HCv", "HVR") := NULL]
#corr.seg.dt[, DIFF := CNN - FS_V6][, c("CNN", "FS_V6") := NULL]

#corr.seg.dt[, METHOD := "CNN-FS_V6"]
#corr.dif.dt   <- rbindlist(list(corr.hc.dt, corr.seg.dt), use.names = TRUE)
#rm(corr.hc.dt, corr.seg.dt)


## Add Comparison labels
#perms.dif.dt[, COMP := paste0(METHOD, ": HVR - HCv")]
#perms.dif.dt[METHOD == "CNN-FS_V6", COMP := "HVR: CNN - FS_V6"]


## Plots
cbPalette     <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Correlations
corr.dt[, COVAR := factor(COVAR, levels = c("AGE", "MEMORY"),
                          labels = c("Age", "Memory"))]
corr.dt[, Pval_adj := p.adjust(Pval, method = "bonferroni")]

corr.dt[Pval_adj < 0.05, SIGN := "*"]
corr.dt[Pval_adj < 0.01, SIGN := "**"]
corr.dt[Pval_adj < 0.001, SIGN := "***"]

## Factor DX
#corr.perm.dt1 <- corr.dif.dt[perms.dif.dt,
                            #on = .(DX, COVAR, METHOD)
                            #][COVAR != "RAVLT_learning",
                            #.(Pval = sum(DIFF_p <= DIFF) / .N),
                            #.(DX, COVAR, METHOD)]

#corr.perm.dt2 <- corr.dif.dt[perms.dif.dt,
                            #on = .(DX, COVAR, METHOD)
                            #][COVAR == "RAVLT_learning",
                            #.(Pval = sum(DIFF_p <= DIFF) / .N),
                            #.(DX, COVAR, METHOD)]

#corr.perm.dt <- rbindlist(list(corr.perm.dt1, corr.perm.dt2))
#rm(corr.perm.dt1, corr.perm.dt2)

## Create dt for permutation significance (only MCI&AD | CNN | AGE)
#corr.perm.sign.dt <- corr.perm.dt[Pval < 0.05,
                                  #.(DX, COVAR, METHOD, Pval,
                                    #HCv = "HCv", HVR = "HVR", LABEL = "*")]

#corr.perm.sign.dt[Pval < 0.01, LABEL := "**"]
#corr.perm.sign.dt[Pval < 0.001, LABEL := "***"]

#corr.perm.sign.dt[, COVAR := factor(COVAR,
                                    #levels = c("AGE", "RAVLT_learning", "ADAS13"),
                                    #labels = c("Age", "Memory", "Cognition"))]

#corr.perm.sign.dt <- corr.dt[, .(Y = max(CIhigh)), .(DX, COVAR, METHOD)
                             #][corr.perm.sign.dt, on = .(DX, COVAR, METHOD)]


## HC_DL
plot.dt <- corr.dt[MEASURE != "HC_fs"]
plot.dt[, SEX := factor(SEX, labels = c("Females", "Males"))]
plot.dt[, COVAR := factor(COVAR, levels = c("Memory", "Age"))]
plot.dt[, METHOD := factor(METHOD,
                           levels = c("raw", "stx", "prop", "pcp", "res"),
                           labels = c("Raw", "STX", "Prop", "PCP", "Res"))]
plot.dt[, MEASURE := factor(MEASURE,
                            levels = c("HC_dl", "HVR"),
                            labels = c("HCvol", "HVR"))]

p1  <- plot.dt |>
  ggplot(aes(METHOD, R, colour = MEASURE)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12)) +
  facet_grid(cols = vars(SEX), rows = vars(COVAR), scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed",
             alpha = .5, colour = cbPalette[1]) +
  geom_errorbar(data = plot.dt[MEASURE == "HCvol"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = -.2)) +
  geom_point(data = plot.dt[MEASURE == "HCvol"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = -.2)) +
  geom_text(data = plot.dt[MEASURE == "HCvol"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = -.2)) +
  #geom_text(data = plot.dt[MEASURE == "HCvol"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = .03, hjust = "right") +
  geom_errorbar(data = plot.dt[MEASURE == "HVR"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = .2)) +
  geom_point(data = plot.dt[MEASURE == "HVR"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = .2)) +
  geom_text(data = plot.dt[MEASURE == "HVR"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = .2)) +
  #geom_text(data = plot.dt[MEASURE == "HVR"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = -.03, hjust = "left") +
  scale_colour_manual(values = cbPalette[2:3]) +
  #ylim(-.6, .5) +
  labs(x = "TIV adjustment method", y = "Pearson's R",
       colour = "HC measure",
       caption = "* p < 0.05; ** p < 0.01; *** p < 0.001")

  here("plots/corrs_dl.png") |>
##here("plots/adni-bl_hcv-hvr_corrs_fs6.png") |>
  ggsave(p1, width = 7, height = 4, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_fs6.tiff") |>
  ##ggsave(width = 4, height = 7, units = "in",
           ##device = "tiff", dpi = 600)

## HC FreeSurfer
plot.dt <- corr.dt[MEASURE != "HC_dl"]
plot.dt[, SEX := factor(SEX, labels = c("Females", "Males"))]
plot.dt[, COVAR := factor(COVAR, levels = c("Memory", "Age"))]
plot.dt[, METHOD := factor(METHOD,
                           levels = c("raw", "stx", "prop", "pcp", "res"),
                           labels = c("Raw", "STX", "Prop", "PCP", "Res"))]
plot.dt[, MEASURE := factor(MEASURE,
                            levels = c("HC_fs", "HVR"),
                            labels = c("HCvol", "HVR"))]

p2  <- plot.dt |>
  ggplot(aes(METHOD, R, colour = MEASURE)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12)) +
  facet_grid(cols = vars(SEX), rows = vars(COVAR), scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed",
             alpha = .5, colour = cbPalette[1]) +
  geom_errorbar(data = plot.dt[MEASURE == "HCvol"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = -.2)) +
  geom_point(data = plot.dt[MEASURE == "HCvol"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = -.2)) +
  geom_text(data = plot.dt[MEASURE == "HCvol"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = -.2)) +
  #geom_text(data = plot.dt[MEASURE == "HCvol"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = .03, hjust = "right") +
  geom_errorbar(data = plot.dt[MEASURE == "HVR"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = .2)) +
  geom_point(data = plot.dt[MEASURE == "HVR"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = .2)) +
  geom_text(data = plot.dt[MEASURE == "HVR"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = .2)) +
  #geom_text(data = plot.dt[MEASURE == "HVR"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = -.03, hjust = "left") +
  scale_colour_manual(values = cbPalette[2:3]) +
  #ylim(-.6, .5) +
  labs(x = "TIV adjustment method", y = "Pearson's R",
       colour = "HC measure",
       caption = "* p < 0.05; ** p < 0.01; *** p < 0.001")

  here("plots/corrs_fs.png") |>
##here("plots/adni-bl_hcv-hvr_corrs_fs6.png") |>
  ggsave(p2, width = 7, height = 4, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_fs6.tiff") |>
  ##ggsave(width = 4, height = 7, units = "in",
           ##device = "tiff", dpi = 600)

## HC FreeSurfer
plot.dt <- corr.dt
plot.dt[, SEX := factor(SEX, labels = c("Females", "Males"))]
plot.dt[, COVAR := factor(COVAR, levels = c("Memory", "Age"))]
plot.dt[, METHOD := factor(METHOD,
                           levels = c("raw", "stx", "prop", "pcp", "res"),
                           labels = c("Raw", "STX", "Prop", "PCP", "Res"))]
plot.dt[, MEASURE := factor(MEASURE,
                            levels = c("HC_dl", "HC_fs", "HVR"),
                            labels = c("HCvol (CNN)", "HCvol (FS)", "HVR"))]

p3  <- plot.dt |>
  ggplot(aes(METHOD, R, colour = MEASURE)) +
  theme_classic(base_size = 12) +
  theme(text = element_text(size = 12)) +
  facet_grid(cols = vars(SEX), rows = vars(COVAR), scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed",
             alpha = .5, colour = cbPalette[1]) +
  geom_errorbar(data = plot.dt[MEASURE == "HCvol (CNN)"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = -.3)) +
  geom_point(data = plot.dt[MEASURE == "HCvol (CNN)"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = -.3)) +
  geom_text(data = plot.dt[MEASURE == "HCvol (CNN)"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = -.3)) +
  #geom_text(data = plot.dt[MEASURE == "HCvol"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = .03, hjust = "right") +
  geom_errorbar(data = plot.dt[MEASURE == "HCvol (FS)"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2) +
  geom_point(data = plot.dt[MEASURE == "HCvol (FS)"],
             shape = 21, fill = "white", size = 1.5, stroke = .5) +
  geom_text(data = plot.dt[MEASURE == "HCvol (FS)"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1) +
  geom_errorbar(data = plot.dt[MEASURE == "HVR"],
                aes(ymin = CIlow, ymax = CIhigh), width = 0.2,
                position = position_nudge(x = .3)) +
  geom_point(data = plot.dt[MEASURE == "HVR"],
             shape = 21, fill = "white", size = 1.5, stroke = .5,
             position = position_nudge(x = .3)) +
  geom_text(data = plot.dt[MEASURE == "HVR"],
            aes(label = SIGN, y = CIhigh), size = 3, vjust = .1,
            position = position_nudge(x = .3)) +
  #geom_text(data = plot.dt[MEASURE == "HVR"],
            #aes(label = round(R, 2)),
            #size = 3, nudge_x = -.03, hjust = "left") +
  scale_colour_manual(values = cbPalette[2:4]) +
  #ylim(-.6, .5) +
  labs(x = "TIV adjustment method", y = "Pearson's R",
       colour = "HC measure",
       caption = "* p < 0.05; ** p < 0.01; *** p < 0.001")

  here("plots/corrs.png") |>
##here("plots/adni-bl_hcv-hvr_corrs_fs6.png") |>
  ggsave(p3, width = 9, height = 4, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_fs6.tiff") |>
  ##ggsave(width = 4, height = 7, units = "in",
           ##device = "tiff", dpi = 600)
#### Permutation tests
### Age
##plot.dt <- corr.dif.dt[perms.dif.dt[COVAR == "AGE" & METHOD != "CNN-FS_V6"],
                       ##on = .(DX, COVAR, METHOD)]
##plot.dt <- corr.perm.dt[plot.dt, on = .(DX, COVAR, METHOD)]

##ggplot(plot.dt, aes(x = DIFF_p, y = DX, fill = factor(after_stat(quantile)))) +
  ##theme_classic(base_size = 12) +
  ##theme(text = element_text(size = 12), axis.text.y = element_blank(),
        ##axis.ticks.y = element_blank()) +
  ##facet_grid(rows = vars(DX), cols = vars(COMP), scales = "free_y") +
  ##stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      ##quantiles = 0.05, scale = 1, alpha = .3) +
  ##geom_vline(aes(xintercept = DIFF), colour = cbPalette[3]) +
  ##scale_fill_manual(values = cbPalette[2:1], name = "One-sided\nhypothesis",
                    ##labels = c("lower 5%", "upper 95%")) +
  ##geom_richtext(data = unique(plot.dt[, .(DX, COMP, Pval)]),
                ##aes(label = paste0("<i>p</i> = ", Pval)),
                ##inherit.aes = F, colour = "Black", fill = "White",
                ##size = 2.5, x = 0, y = -Inf, vjust = -0.25) +
  ##labs(title = "Permutation tests: Difference in correlation with Age",
       ##x = "Difference in r", y = NULL)

##here("plots/adni-bl_hcv-hvr_corrs_perms_age.png") |>
  ##ggsave(width = 13, height = 7, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_perms_age.tiff") |>
  ##ggsave(width = 13, height = 7, units = "in",
           ##device = "tiff", dpi = 600)

### Memory (RAVLT_learning)
### Correlation is in opposite direction
##plot.dt <- corr.dif.dt[perms.dif.dt[COVAR == "RAVLT_learning" & METHOD != "CNN-FS_V6"],
                       ##on = .(DX, COVAR, METHOD)]
##plot.dt <- corr.perm.dt[plot.dt, on = .(DX, COVAR, METHOD)]

##ggplot(plot.dt, aes(x = DIFF_p, y = DX, fill = factor(after_stat(quantile)))) +
  ##theme_classic(base_size = 12) +
  ##theme(text = element_text(size = 12), axis.text.y = element_blank(),
        ##axis.ticks.y = element_blank()) +
  ##facet_grid(rows = vars(DX), cols = vars(COMP), scales = "free_y") +
  ##stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      ##quantiles = 0.95, scale = 1, alpha = .3) +
  ##geom_vline(aes(xintercept = DIFF), colour = cbPalette[3]) +
  ##scale_fill_manual(values = cbPalette[1:2], name = "One-sided\nhypothesis",
                    ##labels = c("lower 95%", "upper 5%")) +
  ##geom_richtext(data = unique(plot.dt[, .(DX, COMP, Pval)]),
                ##aes(label = paste0("<i>p</i> = ", Pval)),
                ##inherit.aes = F, colour = "Black", fill = "White",
                ##size = 2.5, x = 0, y = -Inf, vjust = -0.25) +
  ##labs(title = "Permutation tests: Difference in correlation with Memory",
       ##x = "Difference in r", y = NULL)

##here("plots/adni-bl_hcv-hvr_corrs_perms_mem.png") |>
  ##ggsave(width = 13, height = 7, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_perms_mem.tiff") |>
  ##ggsave(width = 13, height = 7, units = "in",
           ##device = "tiff", dpi = 600)

### Cognition
##plot.dt <- corr.dif.dt[perms.dif.dt[COVAR == "ADAS13" & METHOD != "CNN-FS_V6"],
                       ##on = .(DX, COVAR, METHOD)]
##plot.dt <- corr.perm.dt[plot.dt, on = .(DX, COVAR, METHOD)]

##ggplot(plot.dt, aes(x = DIFF_p, y = DX, fill = factor(after_stat(quantile)))) +
  ##theme_classic(base_size = 12) +
  ##theme(text = element_text(size = 12), axis.text.y = element_blank(),
        ##axis.ticks.y = element_blank()) +
  ##facet_grid(rows = vars(DX), cols = vars(COMP), scales = "free_y") +
  ##stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      ##quantiles = 0.05, scale = 1, alpha = .3) +
  ##geom_vline(aes(xintercept = DIFF), colour = cbPalette[3]) +
  ##scale_fill_manual(values = cbPalette[2:1], name = "One-sided\nhypothesis",
                    ##labels = c("lower 5%", "upper 95%")) +
  ##geom_richtext(data = unique(plot.dt[, .(DX, COMP, Pval)]),
                ##aes(label = paste0("<i>p</i> = ", Pval)),
                ##inherit.aes = F, colour = "Black", fill = "White",
                ##size = 2.5, x = 0, y = -Inf, vjust = -0.25) +
  ##labs(title = "Permutation tests: Difference in correlation with Cognition",
       ##x = "Difference in r", y = NULL)

##here("plots/adni-bl_hcv-hvr_corrs_perms_cog.png") |>
  ##ggsave(width = 13, height = 7, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_perms_cog.tiff") |>
  ##ggsave(width = 13, height = 7, units = "in",
           ##device = "tiff", dpi = 600)

### HVR: CNN - FS6
### Correlation is in opposite direction
##plot.dt <- corr.dif.dt[perms.dif.dt[METHOD == "CNN-FS_V6"],
                       ##on = .(DX, COVAR, METHOD)]
##plot.dt <- corr.perm.dt[plot.dt, on = .(DX, COVAR, METHOD)]

##ggplot(plot.dt, aes(x = DIFF_p, y = DX, fill = factor(after_stat(quantile)))) +
  ##theme_classic(base_size = 12) +
  ##theme(text = element_text(size = 12), axis.text.y = element_blank(),
        ##axis.ticks.y = element_blank()) +
  ##facet_grid(rows = vars(DX), cols = vars(COVAR), scales = "free_y") +
  ##stat_density_ridges(data = plot.dt[COVAR == "RAVLT_learning"],
                      ##geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      ##quantiles = 0.95, scale = 1, alpha = .3) +
  ##stat_density_ridges(data = plot.dt[COVAR != "RAVLT_learning"],
                      ##geom = "density_ridges_gradient", calc_ecdf = TRUE,
                      ##quantiles = 0.95, scale = 1, alpha = .3) +
  ##geom_vline(aes(xintercept = DIFF), colour = cbPalette[3]) +
  ##scale_fill_manual(values = cbPalette[1:2], name = "One-sided\nhypothesis",
                    ##labels = c("lower 95%", "upper 5%")) +
  ##geom_richtext(data = unique(plot.dt[, .(DX, COMP, Pval)]),
                ##aes(label = paste0("<i>p</i> = ", Pval)),
                ##inherit.aes = F, colour = "Black", fill = "White",
                ##size = 2.5, x = 0, y = -Inf, vjust = -0.25) +
  ##labs(title = "Permutation tests: Difference in correlation with Memory",
       ##x = "Difference in r", y = NULL)

##here("plots/adni-bl_hcv-hvr_corrs_perms_mem.png") |>
  ##ggsave(width = 13, height = 7, units = "in", dpi = 600)

##here("plots/adni-bl_hcv-hvr_corrs_perms_mem.tiff") |>
  ##ggsave(width = 13, height = 7, units = "in",
           ##device = "tiff", dpi = 600)


### Correlations with Memory
#DT_long <- melt(DT,
                #measure.vars = patterns("^H"),
                #variable.name = "HC_msr",
                #value.name = "VOL")

#mem_hc_cnn <- DT_long[METHOD == "cnn",
                      #.(DX, HC_msr, VOL, RAVLT_learning)]

#ggplot(mem_hc_cnn, aes(x = RAVLT_learning, y = VOL, colour = DX)) +
  #theme_classic(base_size = 12) +
  #theme(text = element_text(size = 12), legend.position = "bottom") +
  #geom_point(size = 2, shape = 21) +
  #geom_abline(intercept = 0, slope = 1,
              #colour = cbPalette[1], linetype = "dashed") +
  #geom_smooth(method = "lm", alpha = .2) +
  #stat_cor(size = 2.7, label.x.npc = "right", label.y.npc = "bottom",
           #hjust = "inward") +
  #facet_wrap(vars(HC_msr, DX), ncol = 3, scales = "free") +
  #scale_colour_manual(values = cbPalette[-1]) +
  #labs(x = "Memory", y = "Volume",
       #colour = "Clinical label")

#ggsave("plots/adni-bl_hcv_hvr_memory_corrs.png", width = 12, height = 24,
       #units = "in", dpi = 600)

