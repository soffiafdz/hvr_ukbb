#!/usr/bin/env Rscript

library(here)
library(data.table)
library(effsize)
library(ggplot2)
library(GGally)
library(ggtext)

### REMAKE plots
REMAKE_PLOTS  <- TRUE

### INPUT
## Adjusted HC/HVR
fpath         <- here("data/rds/hc-hvr_adj.rds")
if (file.exists(fpath)) {
  hc_hvr.lst  <- readRDS(fpath)
} else {
  here("code/adjust_hc-hvr.R") |> source()
}

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
def_caption   <- sprintf("%s %s %s %s",
  "N: %i Males & %i Females. Data is scaled and centered.",
  "Solid lines are the regression slopes,",
  "dashed coloured lines show the group means, and",
  "dashed gray lines represent a 1:1 correlation."
)

metadata.dt   <-
  CJ(c("HC_HVR", "HVR", "VC"), segm.str, mtch.str) |>
  setnames(c("LIST", "SEGM", "MTCH")) |>
  {function(DT) {
    DT[
      ,
      let(
        FNAME = sprintf("effs-sex_%s_%s_%s.png", LIST, SEGM, MTCH) |>
          tolower(),
        TITLE = sprintf(
          "Sex differences: %s",
          fcase(
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
            "Notes: %s %s %s %s",
            "Validation analysis with AssemblyNet.",
            "HC-to-VC ratios are calculated from unadjusted (conventional)",
            "or size-adjusted (residuals) volumes.",
            def_caption
          ),
          LIST == "HVR", sprintf(
            "Notes: %s %s %s",
            "HC-to-VC ratios are calculated from unadjusted (conventional)",
            "or size-adjusted (residuals) volumes.",
            def_caption
          ),
          SEGM %like% "NET", sprintf(
            "Notes: %s %s",
            "Validation analysis with AssemblyNet.",
            def_caption
          ),
          default = paste("Notes:", def_caption)
        ),
        CAPTION_w = fcase(
          LIST == "HC_HVR", 150,
          LIST == "HVR", 115,
          LIST == "VC", 130
        ),
        SIZE = fcase(
          LIST == "HC_HVR", 8,
          LIST == "HVR", 6,
          LIST == "VC", 7
        )
      )
    ]
  }}()


plot_dt.lst   <-
  # Parse Segmentation levels (CNN vs AssemblyNet)
  Map(function(id1, sublist1, sublist2) {
    # Parse Cohorts levels (All vs Matched sample)
    Map(function(id2, DT1, DT2) {
      list(
        # Lists for HC & HVR comparisons
        HC_HVR = list(
          # Metadata for the plot
          METADATA = metadata.dt[
            SEGM == id1 & MTCH == id2 & LIST == "HC_HVR",
            .(TITLE, SUBTITLE, CAPTION, CAPTION_w, FNAME, SIZE)
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
            .(TITLE, SUBTITLE, CAPTION, CAPTION_w, FNAME, SIZE)
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
            .(TITLE, SUBTITLE, CAPTION, CAPTION_w, FNAME, SIZE)
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

## GGally custom functions
# Diagonal
diag_fun      <- function(data, mapping, var, effs_labs, ...) {
  # Calculate density first to get the y-axis limits
  p <- ggplot(mapping = mapping) + geom_density(data = data, alpha = .1)
  y_limits <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
  # Add mean lines (complete data) and labels (effs_labs)
  p + stat_summary(
    aes(xintercept = ..x.., y = 0),
    data = data,
    fun = mean,
    geom = "vline",
    orientation = "y",
    linetype = "dashed",
    linewidth = .4,
    alpha = .75
  ) + geom_richtext(
    aes(label = LABEL),
    data = effs_labs[effs_labs$VAR == as_label(mapping$x)],
    size = 2.3,
    #alpha = .7,
    colour = "Black",
    fill = "White",
    x = -Inf,
    y = y_limits[2] + 0.08,
    hjust = -0.2,
    #vjust = 0.25
  ) + scale_y_continuous(limits = c(y_limits[1], y_limits[2] + 0.13))

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
    ) |> invisible()
}


det_plot      <- function(dataset, columns, effs_labs, metadata) {
  fplot       <- here("plots", metadata$FNAME)
  if (any(!file.exists(fplot), REMAKE_PLOTS)) {
    time      <- system.time({
      dataset <- dcast(dataset, ... ~ VAR, value.var = "VAL")
      n_f     <- dataset["Female", on = "SEX", .N]
      n_m     <- dataset["Male", on = "SEX", .N]
      caption <- metadata$CAPTION |>
        sprintf(n_m, n_f) |>
        stringr::str_wrap(metadata$CAPTION_w, exdent = 10)
      p       <-  dataset |>
        setcolorder(rois[["ICC"]], after = "SEX") |>
        ggpairs(
          aes(colour = SEX, fill = SEX),
          columns = columns,
          lower = list(continuous = lower_fun),
          diag = list(continuous = wrap(diag_fun, effs_labs = effs_labs))
        ) +
        theme_classic(base_size = 10) +
        theme(
          text = element_text(size = 9),
          #axis.text.y = element_blank(),
          #axis.ticks.y = element_blank(),
          #axis.line.y = element_blank(),
          legend.position = "bottom"
        ) +
        scale_fill_manual(values = cbPalette[c(8, 6)]) +
        scale_colour_manual(values = cbPalette[c(8, 6)]) +
        labs(
          title = metadata$TITLE,
          subtitle = if (is.na(metadata$SUBTITLE)) {
            NULL
          } else {
            metadata$SUBTITLE
          },
          caption = caption,
          #x = NULL,
          #y = NULL,
          colour = "Sex",
          fill = "Sex"
        )
    })

    # Save time for reference
    metadata[, TIME := time[3]]

    # Save plot
    ggsave(
      filename = fplot,
      plot = p,
      height = metadata$SIZE,
      width = metadata$SIZE,
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
    lapply(match_lvl[[sublist_name]], function(sublist) {
      # Sublists
      dataset   <- sublist[["DATA"]]
      effs_labs <- sublist[["EFFS"]]
      metadata  <- sublist[["METADATA"]]
      vars      <- dataset[, unique(VAR)]

      ## Detailed plots
      ## Comparison plots between adjustment methods
      det_plot(
        dataset = dataset,
        columns = vars,
        effs_labs = effs_labs,
        metadata = metadata
      )
    })
  })
})

rm(fpath, def_caption, cbPalette, adjs, rois, mtch.str, segm.str)
