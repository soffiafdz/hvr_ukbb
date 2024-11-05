#!/usr/bin/env Rscript

library(here)
library(data.table)
library(effsize)
library(progress)
library(ggplot2)
library(GGally)
library(ggtext)

### REMAKE plots
REMAKE_PLOTS  <- TRUE

### Sample size for density plots
SAMPLE_N      <- 500

### INPUT
## Covariates
fpath         <- here("data/arrow/UKBB_covars_all.arrow")
if (file.exists(fpath)) {
  covars.dt   <- arrow::read_feather(fpath)
} else {
  here("code/parse_covars.R") |> source()
}

## Adjusted HC/HVR
fpath         <- here("data/rds/hc-hvr_adj.rds")
if (file.exists(fpath)) {
  hc_hvr.lst  <- readRDS(fpath)
} else {
  here("code/adjust_hc-hvr.R") |> source()
}

rm(fpath)

### Data CLEANING
## Labels
adjs          <- c("Unadjusted", "Proportions", "Residuals")
rois          <- c(
  ICC = "Intracranial volume",
  HC  = "Hippocampus",
  VC  = "Ventricles",
  HVR = "HC-to-VC ratio"
)

## Average sides for HC/HVR & Scale & Make long format
hc_hvr_scl.lst <-
  lapply(hc_hvr.lst, function(crs_lvl) {
    # Keep only Cross-Sectional cohort for these analyses
    lapply(crs_lvl$CRS, function(match_lvl) {
      match_lvl[
        ,
        # Average sides for all ROIs, except ICC
        lapply(.SD, mean),
        by = .(EID, SEX, ICC, ADJ),
        .SDcols = names(rois)[-1]
      ][
        ,
        # Scale for easier comparisons
        (names(rois)) := lapply(.SD, scale),
        by = ADJ,
        .SDcols = names(rois)
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
      setkey(EID)
  })
})

### Data PROCESSING
## Effect sizes
fpath         <- here("data/rds/effect-sizes_sex.rds")
if (!file.exists(fpath)) {
  effs.lst    <- readRDS(fpath)
} else {
  effs.lst    <-
    lapply(hc_hvr_scl.lst, function(match_lvl) {
      lapply(match_lvl, function(DT) {
        DT[
          , cohen.d(VAL ~ SEX)[3:5], by = .(ADJ, ROI)
        ][
          ,
          .(conf_low = conf.int[1], conf_high = conf.int[2]),
          by = .(ADJ, ROI, estimate, sd)
        ][
          , LABEL := sprintf(
            "d = %.2f [%.2f, %.2f]", estimate, conf_low, conf_high
          )
        ]
      })
    })
  saveRDS(effs.lst, fpath)
}

### PLOT
## Colour palette
cbPalette     <- c(
  "#999999", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)

## Prepare data
segm.str      <- names(hc_hvr_scl.lst)
mtch.str      <- names(hc_hvr_scl.lst[[1]])
def_caption   <- sprintf("%s %s\n%s %s",
  "Data is scaled and centered.",
# TODO: change sample size to be a variable?
  sprintf(
    "Densities are calculated from a random sample of %i subjects per group.",
    SAMPLE_N
  ),
  "Dashed coloured lines represent the sample's means;",
  "dashed gray lines represent a 1:1 correlation."
)

metadata.dt   <-
  CJ(c("ICC", "HC_HVR", "HVR", "VC"), c("SIMP", "DET"), segm.str, mtch.str) |>
  setnames(c("LIST", "PLOT", "SEGM", "MTCH")) |>
  {function(DT) {
    DT[
      ,
      let(
        FNAME = sprintf("effs-sex_%s_%s_%s_%s.png", LIST, SEGM, MTCH, PLOT) |>
          tolower(),
        TITLE = sprintf(
          "Sex differences: %s",
          fcase(
            LIST == "ICC", "Intracranial volume",
            LIST %like% "HC", "Hippocampus and HC-to-VC Ratio",
            LIST == "HVR", "Conventional vs Residuals-adjusted HVR",
            LIST == "VC", "Ventricles"
          )
        ),
        SUBTITLE = fcase(
          SEGM %like% "CNN" & MTCH == "MTCH",
          "Head-size & age matched sample.",
          SEGM %like% "NET" & MTCH == "ALL",
          "Validation analysis.",
          SEGM %like% "NET" & MTCH == "MTCH",
          "Validation: Head-size & age matched sample."
        ),
        CAPTION = fcase(
          LIST == "HVR" & SEGM %like% "NET", sprintf(
            "Notes: %s\n%s\n%s\n%s",
            "HC-to-VC ratios are calculated from unadjusted (conventional)",
            "or size-adjusted volumes by the residuals method (residuals).",
            "Preprocessing & segmentation done with AssemblyNet.",
            def_caption
          ),
          LIST == "HVR", sprintf(
            "Notes: %s\n%s\n%s",
            "HC-to-VC ratios are calculated from unadjusted (conventional)",
            "or size-adjusted volumes by the residuals method (residuals).",
            def_caption
          ),
          SEGM %like% "NET", sprintf(
            "Notes: %s\n%s",
            "Preprocessing & segmentation done with AssemblyNet.",
            def_caption
          ),
          default = sprintf("Notes: %s", def_caption)
        ),
        HEIGHT = fcase(
          PLOT == "SIMP", 3,
          LIST == "HC_HVR", 8,
          LIST == "HVR", 6,
          LIST == "VC", 7
        ),
        WIDTH = fcase(
          LIST == "ICC", 5,
          LIST == "HC_HVR", 8,
          LIST == "HVR", 6,
          LIST == "VC", 7
        )
      )
      ][
        !(LIST == "ICC" & (PLOT == "DET" | MTCH == "MTCH"))
      ][
        "ICC",
        on = "LIST",
        let(MTCH = NA, FNAME = sub("_all_simp", "", FNAME))
      ]
  }}()

plot_dt.lst   <-
  # Parse Segmentation levels (CNN vs AssemblyNet)
  Map(function(id1, sublist1, sublist2) {
    # Parse Cohorts levels (All vs Matched sample)
    Map(function(id2, DT1, DT2) {
      list(
        # Lists for ICC comparisons
        # When using, will have to unlist to compare All v Matched
        ICC = list(
          # Metadata for the plot
          METADATA = metadata.dt[
            SEGM == id1 & LIST == "ICC",
            .(PLOT, TITLE, SUBTITLE, CAPTION, FNAME, HEIGHT, WIDTH)
          ],
          # Effect size labels for plots
          EFFS = DT1[
            rois["ICC"], on = "ROI"
          ][
            , .(VAR = id2, LABEL)
          ],
          # Individual data points
          DATA = DT2[
            rois["ICC"], on = "ROI", .(VAR = id2, VAL), by = .(EID, SEX)
          ]
        ),
        # Lists for HC & HVR comparisons
        HC_HVR = list(
          # Metadata for the plot
          METADATA = metadata.dt[
            SEGM == id1 & MTCH == id2 & LIST == "HC_HVR",
            .(PLOT, TITLE, SUBTITLE, CAPTION, FNAME, HEIGHT, WIDTH)
          ],
          # Effect size labels for plots
          EFFS = DT1[
            !rois["VC"], on = "ROI"
          ][
            !(ADJ == "Residuals" & ROI == rois["HVR"]),
            .(
              LABEL,
              VAR = fcase(
                ROI %in% rois[c("ICC", "HVR")], as.character(ROI),
                ADJ == "Unadjusted", "HC (Not adjusted)",
                default = sprintf("HC (%s)", ADJ)
              )
            )
          ],
          # Individual data points
          DATA = DT2[
            !rois["VC"], on = "ROI"
          ][
            !(ADJ == "Residuals" & ROI == rois["HVR"]),
            .(
              VAL,
              VAR = fcase(
                ROI %in% rois[c("ICC", "HVR")], as.character(ROI),
                ADJ == "Unadjusted", "HC (Not adjusted)",
                default = sprintf("HC (%s)", ADJ)
              )
            ),
            by = .(EID, SEX)
          ]
        ),
        # Lists for HVR from unadjusted and adjusted volumes
        # Supplementary data
        HVR = list(
          # Metadata for the plot
          METADATA = metadata.dt[
            SEGM == id1 & MTCH == id2 & LIST == "HVR",
            .(PLOT, TITLE, SUBTITLE, CAPTION, FNAME, HEIGHT, WIDTH)
          ],
          # Effect size labels for plots
          EFFS = DT1[
            rois[c("ICC", "HVR")],
            on = "ROI",
            .(
              LABEL,
              VAR = fcase(
                ROI == rois["ICC"], rois[["ICC"]],
                ADJ == "Residuals", "HVR (residuals)",
                default = "HVR (conventional)"
              )
            )
          ],
          # Individual data points
          DATA = DT2[
            rois[c("ICC", "HVR")],
            on = "ROI",
            .(
              VAL,
              VAR = fcase(
                ROI == rois["ICC"], rois[["ICC"]],
                ADJ == "Residuals", "HVR (residuals)",
                default = "HVR (conventional)"
              )
            ),
            by = .(EID, SEX)
          ]
        ),
        # Lists for Ventricle exploration
        VC = list(
          # Metadata for the plot
          METADATA = metadata.dt[
            SEGM == id1 & MTCH == id2 & LIST == "VC",
            .(PLOT, TITLE, SUBTITLE, CAPTION, FNAME, HEIGHT, WIDTH)
          ],
          # Effect size labels for plots
          EFFS = DT1[
            rois[c("ICC", "VC")],
            on = "ROI",
            .(
              LABEL,
              VAR = fcase(
                ROI == rois["ICC"], rois[["ICC"]],
                ADJ == "Unadjusted", "VC (Not adjusted)",
                default = sprintf("VC (%s)", ADJ)
              )
            ),
          ],
          # Individual data points
          DATA = DT2[
            rois[c("ICC", "VC")],
            on = "ROI",
            .(
              VAR = fcase(
                ROI == rois["ICC"], rois[["ICC"]],
                ADJ == "Unadjusted", "VC (Not adjusted)",
                default = sprintf("VC (%s)", ADJ)
              ),
              VAL
            ),
            by = .(EID, SEX)
          ]
        )
      )
    }, mtch.str, sublist1, sublist2)
  }, segm.str, effs.lst, hc_hvr_scl.lst
)

# Extract ICC from All & Matched and merge them:
plot_dt.lst   <- lapply(plot_dt.lst, function(sublist) {
  sublist1    <- sublist[[1]][[1]]
  sublist2    <- sublist[[2]][[1]]
  list_names  <- names(sublist1)
  # Create a new "ICC" sublist at the 2nd level
  sublist[["ICC"]] <- lapply(list_names, function(name) {
    # METADATA is the same for both ALL & MTCH levels
    if (name == "METADATA") {
      sublist1[[name]]
    } else {
      rbind(sublist1[[name]], sublist2[[name]]) |>
      {function(DT) {
        DT[, VAR := factor(VAR, labels = c("General", "Matched"))]
      }}()
    }
  })

  # Rename
  names(sublist[["ICC"]]) <- list_names

  #Delete originals
  sublist[[1]][[1]] <- sublist[[2]][[1]] <- NULL

  # Return
  sublist
})


## GGally custom functions
# Diagonal
diag_fun      <- function(data, subdata, mapping, var, effs_labs, ...) {
  ggplot(mapping = mapping) +
    #geom_histogram(colour = "transparent", alpha = .25) +
    # Calculate density with a subsample of the data for cheaper processing
    geom_density(data = subdata, alpha = .1) +
    # For mean lines, use the complete data
    stat_summary(
      aes(xintercept = ..x.., y = 0),
      data = data,
      fun = mean,
      geom = "vline",
      orientation = "y",
      linetype = "dashed",
      linewidth = .4,
      alpha = .75
    ) +
    # Effect size data is obtained from a different data.table
    geom_richtext(
      aes(label = LABEL),
      data = effs_labs[effs_labs$VAR == as_label(mapping$x)],
      size = 2.3,
      alpha = .7,
      colour = "Black",
      fill = "White",
      x = -Inf,
      y = -Inf,
      hjust = -0.1,
      vjust = -0.25
    )
}

# Lower
lower_fun     <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    stat_smooth(method = "lm", ..., linewidth = .75) +
    geom_abline(
      intercept = 0,
      slope = 1,
      lty = "dashed",
      colour = cbPalette[1]
    )
}

simp_plot     <- function(
  dataset,
  effs_labs,
  metadata,
  sample_n = 100
) {
  fplot       <- here("plots", metadata["SIMP", on = "PLOT", FNAME])
  if (any(!file.exists(fplot), REMAKE_PLOTS)) {
    time      <- system.time({
      p       <- dataset[, .SD[sample(.N, min(.N, sample_n))], .(SEX, VAR)] |>
        ggplot(aes(x = VAL, colour = SEX, fill = SEX)) +
        theme_classic(base_size = 10) +
        theme(
          text = element_text(size = 10),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = cbPalette[c(8, 6)]) +
        scale_colour_manual(values = cbPalette[c(8, 6)]) +
        #geom_histogram(fill = "transparent", bins = 45) +
        geom_density(alpha = .1) +
        geom_vline(
          aes(xintercept = VAL, colour = SEX),
          data = dataset[, lapply(.SD, mean), .(SEX, VAR)],
          lty = "longdash",
          linewidth = .5,
          alpha = .75
        ) +
        facet_wrap(vars(VAR), nrow = 1) +
        geom_richtext(
          aes(label = LABEL),
          data = effs_labs,
          size = 2,
          alpha = .8,
          colour = "Black",
          fill = "White",
          #x = -Inf, y = -Inf, hjust = -0.1, vjust = -0.25
          vjust = -.2,
          x = 0,
          y = 0
        ) +
        labs(
          title = metadata["SIMP", on = "PLOT", TITLE],
          subtitle = if (is.na(metadata["SIMP", on = "PLOT", SUBTITLE])) {
            NULL
          } else {
            metadata["SIMP", on = "PLOT", SUBTITLE]
          },
          caption = metadata["SIMP", on = "PLOT", CAPTION],
          x = NULL,
          y = NULL,
          colour = "Sex",
          fill = "Sex"
        )
    })

    # Save time for reference
    metadata["SIMP", on = "PLOT", TIME := time[3]]

    # Save plot
    #browser()
    ggsave(
      filename = fplot,
      plot = p,
      height = metadata["SIMP", on = "PLOT", HEIGHT],
      width = metadata["SIMP", on = "PLOT", WIDTH],
      units = "in",
      dpi = 600
    ) |>
    invisible()
  }
}

det_plot      <- function(
  dataset,
  columns,
  effs_labs,
  metadata,
  sample_n = 100
) {
  fplot       <- here("plots", metadata["DET", on = "PLOT", FNAME])
  if (any(!file.exists(fplot), REMAKE_PLOTS)) {
    time      <- system.time({
      dataset <- dcast(dataset, ... ~ VAR, value.var = "VAL")
      subdata <- dataset[, .SD[sample(.N, min(.N, sample_n))], SEX]
      p       <-  dataset |>
        setcolorder(rois[["ICC"]], after = "SEX") |>
        ggpairs(
          aes(colour = SEX, fill = SEX),
          columns = columns,
          lower = list(continuous = lower_fun),
          diag = list(continuous = wrap(
            diag_fun,
            subdata = subdata,
            effs_labs = effs_labs
          ))
        ) +
        theme_classic(base_size = 10) +
        theme(
          text = element_text(size = 10),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
          #axis.line.y = element_blank(),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = cbPalette[c(8, 6)]) +
        scale_colour_manual(values = cbPalette[c(8, 6)]) +
        labs(
          title = metadata["DET", on = "PLOT", TITLE],
          subtitle = if (is.na(metadata["DET", on = "PLOT", SUBTITLE])) {
            NULL
          } else {
            metadata["DET", on = "PLOT", SUBTITLE]
          },
          caption = metadata["DET", on = "PLOT", CAPTION],
          #x = NULL,
          #y = NULL,
          colour = "Sex",
          fill = "Sex"
        )
    })

    # Save time for reference
    metadata["DET", on = "PLOT", TIME := time[3]]

    # Save plot
    ggsave(
      filename = fplot,
      plot = p,
      height = metadata["DET", on = "PLOT", HEIGHT],
      width = metadata["DET", on = "PLOT", WIDTH],
      units = "in",
      dpi = 600
    ) |>
    invisible()
  }
}

## Plotting
lapply(plot_dt.lst, function(match_lvl) {
  lapply(names(match_lvl), function(sublist_name) {
    sublist <- match_lvl[[sublist_name]]
    if (sublist_name == "ICC") {
      # Plot simple comparison plots for only ICC
      sublist   <- match_lvl[[sublist_name]]
      dataset   <- sublist[["DATA"]]
      effs_labs <- sublist[["EFFS"]]
      metadata  <- sublist[["METADATA"]]
      simp_plot(
        dataset = dataset,
        sample_n = SAMPLE_N,
        effs_labs = effs_labs,
        metadata = metadata
      )
    } else {
      lapply(match_lvl[[sublist_name]], function(sublist) {
        # Sublists
        dataset   <- sublist[["DATA"]]
        effs_labs <- sublist[["EFFS"]]
        metadata  <- sublist[["METADATA"]]
        vars      <- dataset[, unique(VAR)]

        ## Simple plots
        ## Only density plots with median lines
        simp_plot(
          dataset = dataset,
          sample_n = SAMPLE_N,
          effs_labs = effs_labs,
          metadata = metadata
        )

        ## Detailed plots
        ## Comparison plots between adjustment methods
        det_plot(
          dataset = dataset,
          sample_n = SAMPLE_N,
          columns = vars,
          effs_labs = effs_labs,
          metadata = metadata
        )
      })
    }
  })
})
