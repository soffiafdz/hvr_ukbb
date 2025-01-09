#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)
library(progress)

### IN
fpaths <- here(
  "data",
  c("rds/metadata.rds", "arrow/UKBB_covars_all.arrow", "rds/notes.rds")
)

if (all(file.exists(fpaths))) {
  metadata.dt <- readRDS(fpaths[1])
  covars.dt   <- arrow::read_feather(fpaths[2])
  notes.dt    <- readRDS(fpaths[3])
} else {
  here("code/parse_covars.R") |> source()
}

rm(fpaths)

### Clinical history
##TODO: Decide whether to keep this here or move to MCFA code.
## Move to MCFA script
## Filter out people with F, G or Q0 dx in ICD10 and bad QC
#icd_10.dt   <- covars.dt[ICD_10 %like% "F|G|Q0", .(EID)]
#cnn.dt      <- cnn.dt[!icd_10.dt, on = "EID"]
#rm(icd_10.dt)

### FUNCTIONS for parsing cognitive data
## Extract columns according to a regex pattern
ext_cols <- function(
  DT,
  pattern,
  include.eid = TRUE,
  ignore.case = FALSE
) {
  if (include.eid) pattern <- sprintf("EID|%s", pattern)
  DT[, .SD, .SDcols = patterns(pattern, ignore.case = ignore.case)]
}

## Convert to long format by Session from column name
ext_sess <- function(
  DT,
  id.vars = "EID",
  ses.pattern = "ses\\d",
  ses.name = "SESSION"
) {
  DT <- melt(DT, id.vars = id.vars) |> suppressWarnings()
  DT[, (ses.name) := str_extract(variable, ses.pattern)]
  DT[, variable := str_remove(variable, paste0("_", ses.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

## Convert to long format by Array from column name
ext_arrays <- function(
  DT,
  id.vars = "EID",
  include.sess = TRUE,
  sess.name = "SESSION",
  array.name = "ARRAY",
  array.pattern = "\\d+$"
) {
  if (include.sess) id.vars <- c(id.vars, sess.name)
  DT <- melt(DT, id.vars = id.vars)
  DT[, (array.name) := variable |> str_extract(array.pattern) |> as.integer()]
  # TODO: Why did I remove this? Is it not used anymore?
  #DT["", on = array.name, (array.name) := NA]
  DT[, variable := str_remove(variable, paste0("_", array.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

## Convert to long format by whether it was performed online/onsite
sep_online <- function(
  DT,
  id.vars = "EID",
  online.pattern = "onl",
  online.name = "ONLINE",
  include.sess = TRUE,
  sess.name = "SESSION",
  include.array = TRUE,
  array.name = "ARRAY"
) {
  if (include.sess) id.vars <- c(id.vars, sess.name)
  if (include.array) id.vars <- c(id.vars, array.name)
  DT <- melt(DT, id.vars = id.vars)
  DT[, (online.name) := fifelse(variable %like% online.pattern, TRUE, FALSE)]
  DT[, variable := str_remove(variable, paste0("_", online.pattern))]
  DT <- dcast(DT, ... ~ variable, value.var = "value")
  return(DT)
}

### Data CLEANING
## Extend metadata.dt for cognitive scores
# Extract cognitive rows from metadata (28:)
cog_meta.dt <- metadata.dt[
  28:.N,
  .(TITLE, UNITS, COLNAME, INSTANCED, ARRAYED)
]

# Fill Units
cog_meta.dt[TITLE %like% "incorrect", UNITS := "error-count"]
cog_meta.dt[TITLE %like% "digits|letters", UNITS := "correct-answers"]
cog_meta.dt[TITLE %like% "viewed", UNITS := "attempts"]
cog_meta.dt[COLNAME %in% c("VOCAB_lv", "PRMEM_res"), UNITS := "score"]
cog_meta.dt[TITLE %like% "Uncertainty", UNITS := "uncertainty"]
cog_meta.dt[TITLE %like% "completion", UNITS := "completion"]

# Disassociate online from COLNAME
cog_meta.dt[, ONLINE := fifelse(COLNAME %like% "onl", "UNIT_online", "UNIT")]
cog_meta.dt[, COLNAME := str_remove(COLNAME, "_onl")]

# Separate onsite/online units
cog_meta.dt <- dcast(cog_meta.dt, ... ~ ONLINE, value.var = "UNITS")

# Standardizing time units
cog_meta.dt[, CONV_time := !(is.na(UNIT_online) | UNIT == UNIT_online)]
cog_meta.dt[is.na(CONV_time), CONV_time := FALSE]
t_conv.dt   <-
  cog_meta.dt[UNIT != UNIT_online, .SD, .SDcols = patterns("COL|UNI")] |>
  melt(id.vars = "COLNAME") |>
  {function(DT)
    DT[
      , let(
        ONLINE = fifelse(variable %like% "onl", TRUE, FALSE),
        CONV_time = fcase(
          value %like% "dec", .1,
          value %like% "mil", .001,
          default = 1
        )
      )
    ][
      , c("variable", "value") := NULL
    ]
  }()


# Pattern is starting uppercase string from column name
cog_meta.dt[, PATTERN := str_extract(COLNAME, "^[A-Z]*")]
# Test names and COL pattern
cog_meta.dt <- data.table(
  NAME = c(
    "Pair_matching",
    "Fluid_intelligence",
    "Trail_making",
    "Symbol_digit_substitution",
    "Numeric_memory",
    "Matrix_pattern_recognition",
    "Reaction_time",
    #TODO: Why did I remove these?
    #"Broken_letter_recognition",
    #"Picture_vocabulary",
    "Tower_rearranging",
    "Prospective_memory"
  ),
  PATTERN = c(
    "PRS",
    "FLINT",
    "TRLS",
    "SYM",
    "NUM",
    "MATS",
    "REACT",
    #"BRLET",
    #"VOCAB",
    "TOWER",
    "PRMEM"
  )
)[cog_meta.dt, on = "PATTERN"]

## Create LIST with data.tables for cognitive scores
pb <- progress_bar$new(
  format = "Processing tests | :what [:bar] :current/:total",
  total  = cog_meta.dt[!duplicated(NAME), .N],
  clear  = FALSE,
  width  = 75
)
cog_tests.lst <- list()
for (cog_test in unique(cog_meta.dt$NAME)) {
  # Progress bar tick
  test_name <- str_replace_all(cog_test, "_", " ")
  pb$tick(tokens = list(what = test_name))
  rm(test_name)

  # Get specific metadata (1-liner)
  submetadata <- cog_meta.dt[
    cog_test,
    on = "NAME",
    .(
      PATTERN = unique(PATTERN),
      INSTANCED = any(INSTANCED),
      ARRAYED = any(ARRAYED),
      ONLINE = any(!is.na(UNIT_online)),
      CONV_T = any(CONV_time)
    )
  ]

  # Get column names (needed for time standardization and NA removal)
  columnames  <- cog_meta.dt[cog_test, on = "NAME", COLNAME]

  # Extract preliminary data.table
  subDT       <- covars.dt |> ext_cols(submetadata$PATTERN)

  # Disaggregate columns by type
  cols_num    <- subDT[, names(.SD), .SDcols = is.numeric]
  cols_chr    <- c("EID", subDT[, names(.SD), .SDcols = is.character])
  cols_fct    <- c("EID", subDT[, names(.SD), .SDcols = is.factor])
  sublist     <- list(
    subDT[, ..cols_num],
    subDT[, ..cols_chr],
    subDT[, ..cols_fct]
  )

  # Remove empty data.tables
  sublist     <- sublist[sapply(sublist, function(DT) length(DT) > 1)]

  # Extract sessions
  #TODO: check that this is deprecated before removing.
  #if (submetadata$INSTANCED) subDT <- ext_sess(subDT)
  if (submetadata$INSTANCED) sublist <- lapply(sublist, ext_sess)

  # Extract arrays
  if (submetadata$ARRAYED) {
    #TODO: check that this is deprecated before removing.
    #subDT <- ext_arrays(subDT, include.sess = submetadata$INSTANCED)
    sublist <- lapply(
      sublist,
      ext_arrays,
      include.sess = submetadata$INSTANCED
    )
  }

  # Separate online tests
  if (submetadata$ONLINE) {
    #TODO: check that this is deprecated before removing.
    #subDT <- sep_online(
      #subDT,
      #include.sess = submetadata$INSTANCED,
      #include.array = submetadata$ARRAYED
    #)
    sublist <- lapply(
      sublist,
      sep_online,
      include.sess = submetadata$INSTANCED,
      include.array = submetadata$ARRAYED
    )
  }

  # Standardize time
  if (submetadata$CONV_T) {
    for (columname in columnames) {
      if (columname %in% t_conv.dt$COLNAME) {
        # Time columns are by definition numeric, lapply is not needed
        subt_conv.dt <- t_conv.dt[columname, on = "COLNAME"]
        sublist[[1]][
          ,
          (columname) := .SD * subt_conv.dt[ONLINE == ONLINE, CONV_time],
          .SDcols = columname
        ]
      }
    }
  }

  # Merge back data.tables
  subDT <- Reduce(function(DT1, DT2) merge(DT1, DT2, all = TRUE), sublist)

  # Remove rows where all test columns are NA and save to list
  cog_tests.lst[[cog_test]] <- subDT[
    !(rowSums(is.na(subDT[, ..columnames])) == length(columnames))
  ]
}

rm(
  subDT, sublist, cog_test, cols_chr, cols_fct, cols_num,
  columname, columnames, submetadata, subt_conv.dt, t_conv.dt
)

## Parse notes for aid
notes.dt    <-
  metadata.dt[
    , FIELD, keyby = "COLNAME"
  ][
    cog_meta.dt[, NAME, keyby = "COLNAME"]
  ][
    notes.dt, on = "FIELD", .(NAME, COLNAME, NOTES), nomatch = NULL
  ]

### Specific cleaning
## Matrices (Abstract reasoning/Problem solving)
# There is only Offline scores.
# Make sure ONLINE is redundant before removing.
online_lvls <- unique(cog_tests.lst[["Matrix_pattern_recognition"]]$ONLINE)
rm_online   <- online_lvls == FALSE
if (rm_online) {
  # Remove useless ONLINE column
  cog_tests.lst[["Matrix_pattern_recognition"]][, ONLINE := NULL]
  # Summarize time among arrays: mean, min & max
  cog_tests.lst[["Matrix_pattern_recognition"]] <- merge(
    x = cog_tests.lst[["Matrix_pattern_recognition"]][
      !is.na(ARRAY),
      ## DEPRECATED::Propagate N correct & attempts for time arrays
      ##.(ARRAY = as.integer(ARRAY), MATS_time),
      .(MATS_time_mean = mean(MATS_time),
        MATS_time_min = min(MATS_time),
        MATS_time_max = max(MATS_time)),
      keyby = .(EID, SESSION)
    ],
    y = cog_tests.lst[["Matrix_pattern_recognition"]][
      is.na(ARRAY),
      .(MATS_corr_try = MATS_corr / MATS_try, MATS_corr, MATS_try),
      keyby = .(EID, SESSION)
    ]
  )
} else {
  # Failsafe; useless code
  cog_tests.lst[["Matrix_pattern_recognition"]] <- merge(
    x = cog_tests.lst[["Matrix_pattern_recognition"]][
      !is.na(ARRAY),
      .(MATS_time_mean = mean(MATS_time),
        MATS_time_min = min(MATS_time),
        MATS_time_max = max(MATS_time)),
      keyby = .(EID, SESSION, ONLINE)
    ],
    y = cog_tests.lst[["Matrix_pattern_recognition"]][
      is.na(ARRAY),
      .(MATS_corr_try = MATS_corr / MATS_try, MATS_corr, MATS_try),
      keyby = .(EID, SESSION, ONLINE)
    ]
  )
  # Reset keys
  setkey(
    cog_tests.lst[["Matrix_pattern_recognition"]],
    EID, SESSION, ONLINE, ARRAY
  )
}
rm(online_lvls, rm_online)

## Trail making (Processing speed/executive function)
# NA = 0 in Online follow-up
cols <- cog_tests.lst[["Trail_making"]][, names(.SD), .SDcols = is.numeric][-1]
for (column in cols) {
  cog_tests.lst[["Trail_making"]][is.na(get(column)), (column) := 0]
}
rm(column, cols) ## TODO: Make sure this does not break anything

# If time is 0, the test was abandoned
cog_tests.lst[["Trail_making"]][
  is.na(TRLS_complete),
  TRLS_complete := fcase(
  TRLS_alnum_time == 0 | TRLS_num_time == 0, "Abandoned",
  default = "Completed"
  )
]

# Order & binarize
cog_tests.lst[["Trail_making"]][
  , let(
    TRLS_complete_ord = factor(
      TRLS_complete,
      levels = c("Abandoned", "Timed-out due to inactivity", "Completed"),
      ordered = TRUE
    ),
    TRLS_complete_bin = fifelse(TRLS_complete == "Completed", 1, 0)
  )
][
  , TRLS_complete := NULL
]

# Summarize: -Errors / Time
# Numeric
cog_tests.lst[["Trail_making"]][
  TRLS_num_time > 0,
  TRLS_num_err_t := -TRLS_num_err / TRLS_num_time,
  .(EID, SESSION, ONLINE)
]
# Alpha-Numeric
cog_tests.lst[["Trail_making"]][
  TRLS_alnum_time > 0,
  TRLS_alnum_err_t := -TRLS_alnum_err / TRLS_alnum_time,
  .(EID, SESSION, ONLINE)
]

## Fluid_intelligence (Reasoning/Problem solving)
# Nothing to do

## Numeric memory (Working memory)
# -1 equals abandoned; only in-person.
# TODO: Change? Check if this works

## Reaction time (Processing speed)
# Nothing to do

## Pair matching (Memory/Visual-spatial memory)
# TODO: Test difference between:
# Mean of ratio errors/time
cog_tests.lst[["Pair_matching"]][
  PRS_time > 0, # TODO: Impute Time==0???
  PRS_inc_time := mean(PRS_inc / PRS_time),
  .(EID, SESSION, ONLINE)
]
# Mean errors w/time as weights
cog_tests.lst[["Pair_matching"]][
  ,
  PRS_wt_inc_time := sum(PRS_inc * PRS_time) / sum(PRS_time),
  .(EID, SESSION, ONLINE)
]
# Mean time w/errors as weights
cog_tests.lst[["Pair_matching"]][
  ,
  PRS_wt_time_inc := sum(PRS_inc * PRS_time) / sum(PRS_inc),
  .(EID, SESSION, ONLINE)
]

# Mean Errors & Time
cog_tests.lst[["Pair_matching"]][
  ,#PRS_time > 0, # TODO: Impute Time==0???
  let(PRS_mean_inc = mean(PRS_inc), PRS_mean_time = mean(PRS_time)),
  .(EID, SESSION, ONLINE)
]

# Remove array cols
cog_tests.lst[["Pair_matching"]][, c("ARRAY", "PRS_inc", "PRS_time") := NULL]

## Tower rearranging (Planning/Executive function)
# Correct/Attempted
cog_tests.lst[["Tower_rearranging"]][
  , TOWER_corr_try := TOWER_corr / TOWER_try
]

## Symbol-digit substitution (Processing speed/Attention)
# Correct/Attempted
cog_tests.lst[["Symbol_digit_substitution"]][
  , SYM_corr_try := SYM_corr / SYM_try
]

## Prospective memory (Memory/Prospective memory)
# Result to numeric
cog_tests.lst[["Prospective_memory"]][
  ,
  PRMEM_res_n := fcase(
    PRMEM_res %like% "incorrect", 0,
    PRMEM_res %like% "second", 1,
    PRMEM_res %like% "first", 2
  )
]

#TODO: Save cog_meta.dt & cog_tests.lst
## OUT
outfiles <- here(
  "data/rds",
  c("metadata_cog_tests.rds", "cognitive_tests.rds")
)

cog_meta.dt |> saveRDS(outfiles[1])
cog_tests.lst |> saveRDS(outfiles[2])
rm(outfiles)
