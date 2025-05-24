#!/usr/bin/env Rscript

library(here)
library(data.table)
library(stringr)

### IN
## Check existance of required files
fpaths      <- here("data", c("field.txt", "ukb676574"))
for (fpath in fpaths) {
  if (!file.exists(fpath)) {
    sprintf("File/Dir: %s is required but could not be found.", fpath) |>
    stop()
  }
  rm(fpath)
}

## Metadata (Schema 1 UKBB)
metadata.dt <- fread(
  fpaths[1],
  select = c(1:2, 24, 6:7, 10:14, 17:21),
  col.names = c(
    "FIELD",
    "TITLE",
    "N",
    "TYPE_value", # 11—integer, 21—single-choice, 22—multi-choice,
    #31—real, 41—string, # 51—date, 61—time, 101—compound, 201—blob
    "TYPE_base", # 0—value, 11—int, 31—real, 41—string, 51—date
    "INSTANCED", # 0—no, 1—defined, 2—variable
    "ARRAYED",   # 0—no, 1—yes
    "SEXED",     # 0—unisex, 1—male, 2—female
    "UNITS",
    "CATEGORY",
    "INSTANCE_min",
    "INSTANCE_max",
    "ARRAY_min",
    "ARRAY_max",
    "NOTES"
  )
)

## DICTIONARY column names & field ids
cols.dt     <- c(
  ## Demographics
  SEX_r                = 31,     # Sex (Reported)
  SEX_g                = 22001,  # Sex (Genetic)
  ANEUPLOIDY           = 22019,  # Chromosomal aneuploidy
  AGE                  = 21003,  # Age (years)
  BIRTH_m              = 52,     # Month of birth
  BIRTH_y              = 34,     # Month & Year of birth
  ETHNIC               = 21000,  # 1 white; 2 mixed; 3 asian; 4 black; 5 chinese
  ## Education & Employment
  EDUC_y               = 845,    # Years
  EDUC_lv              = 6138,   # Level
  #EDUC_score_eng       = 26414,  # Scores: England
  #EDUC_score_sct       = 26421,  # Scores: Scotland
  #EDUC_score_wls       = 26431,  # Scores: Wales
  EMPLOYMENT           = 6142,   # Employment status
  ## Indices of multiple deprivation
  EDUC_score_eng       = 26414,  # Scores: England
  EDUC_score_sct       = 26421,  # Scores: Scotland
  EDUC_score_wls       = 26431,  # Scores: Wales
  EMPL_score_eng       = 26412,  # Scores: England
  EMPL_score_sct       = 26429,  # Scores: Scotland
  EMPL_score_wls       = 26419,  # Scores: England
  HLTH_score_eng       = 26413,  # Scores: England
  HLTH_score_sct       = 26430,  # Scores: Scotland
  HLTH_score_wls       = 26420,  # Scores: England
  HOUS_score_eng       = 26415,  # Scores: England
  HOUS_score_sct       = 26432,  # Scores: Scotland
  HOUS_score_wls       = 26423,  # Scores: England
  INCM_score_eng       = 26411,  # Scores: England
  INCM_score_sct       = 26428,  # Scores: Scotland
  INCM_score_wls       = 26418,  # Scores: England
  IMDP_score_eng       = 26410,  # Scores: England
  IMDP_score_sct       = 26427,  # Scores: Scotland
  IMDP_score_wls       = 26426,  # Scores: England
  ## Clinical history
  ILL_father           = 20107,  # Illnesses Father (AD10)
  ILL_mother           = 20110,  # Illnesses Mother (AD10)
  ICD_9                = 41271,  # Diagnoses of ICD 9
  ICD_10               = 41270,  # Diagnoses of ICD 10
  AD_rep_f00           = 130836, # AD report date (F00)
  AD_rep_g30           = 131036, # AD report date (G30)
  ## FreeSurfer Hippocampus
  #HC_l_fs              = 26562,  # HCvol Left
  #HC_r_fs              = 26593,  # HCvol Right
  ## Important dates
  DATE_mri             = 21862,  # Date of MRI
  DATE_cog_scn         = 21825,  # Date of Cognitive tests (scan)
  DATE_cog_asm         = 53,     # Date ^^ (assessment)
  # Dates for online follow-up assessments of cognition
  DATE_onl_prs         = 20134,  # Date: Pairs
  DATE_onl_flint       = 20135,  # Date: Fluid Intelligence
  DATE_onl_trls        = 20136,  # Date: Trails
  DATE_onl_sym         = 20137,  # Date: Symbol-Digits Subst
  DATE_onl_num         = 20138,  # Date: Numeric memory
  DATE_onl_mats        = 20765,  # Date: Matrices
  ## Cognitive scores
  # Pairs matching
  PRS_inc              = 399,    # N incorrect
  PRS_inc_onl          = 20132,  # N incorrect (online)
  PRS_time             = 400,    # Time to complete
  PRS_time_onl         = 20133,  # Time to complete (online)
  # Fluid intelligence
  FLINT                = 20016,  # Score
  FLINT_onl            = 20191,  # Score (online)
  # Trail making
  TRLS_num_time        = 6348,   # Time - numeric
  TRLS_num_time_onl    = 20156,  # Time - numeric (online)
  TRLS_num_err         = 6349,   # Errors - numeric
  TRLS_num_err_onl     = 20247,  # Errors - numeric (online)
  TRLS_alnum_time      = 6350,   # Time - alphanumeric
  TRLS_alnum_time_onl  = 20157,  # Time - alphanumeric (online)
  TRLS_alnum_err       = 6351,   # Errors - alphanumeric
  TRLS_alnum_err_onl   = 20248,  # Errors - alphanumeric (online)
  TRLS_complete_onl    = 20246,  # Completed/Abandoned test (online)
  # Symbol-digit substitution
  SYM_try              = 23323,  # Attempted matches
  SYM_try_onl          = 20195,  # Attempted matches (online)
  SYM_corr             = 23324,  # Correct matches
  SYM_corr_onl         = 20159,  # Correct matches (online)
  # Numeric memory
  NUM                  = 4282,   # Max digits remembered
  NUM_onl              = 20240,  # Max digits remembered (online)
  # Matrix pattern completion
  MATS_corr            = 6373,   # Correct matrices
  MATS_corr_onl        = 20760,  # Correct matrices (online)
  MATS_try             = 6374,   # Viewed matrices
  MATS_try_onl         = 20761,  # Viewed matrices (online)
  MATS_time            = 6333,   # Puzzle duration
  MATS_time_onl        = 20763,  # Puzzle duration (online)
  # Reaction time
  REACT                = 20023,  # Mean time
  # ## Not in our database ##
  ## Broken letter recognition
  #BRLET                = 20139,  # Correctly identified
  ## Picture vocabulary
  #VOCAB_lv             = 6364,   # Vocabulary level
  #VOCAB_unc            = 6365,   # Vocabulary uncertainty
  #VOCAB_as             = 26302,  # Specific cogn ability (AS)
  # ## Not in our database ##
  # Tower rearranging
  TOWER_corr           = 21004,  # Correct puzzles
  TOWER_try            = 6383,   # Attempted puzzles
  # Prospective memory
  PRMEM_res            = 20018,  # Result
  PRMEM_time           = 4288#,  # Time to answer
  ## TODO: Name these
  ### Lancet Risk factors
  #6148,                # Visual loss (untreated)
  #28627, 28629, 28628, # Hearing loss: Current / Extent / Length
  #23404, 23405,        # [Clinical] LDL cholesterol; mmol/l
  #22035, 22036,        # >= mod/vig[/walking] recomm activity;
  ## Diabetes :: Mid-life onset of T2D, not late-life
  #2976, # Age Dx T2D; 100291 :: -1 IDK, -3 PrNA
  #130708, 130710,      # Report E1[12]: !ins-dep & malnutrition;
  ## Hypertension
  #131286, 131294,      # Date of report
  ## Obesity :: Mid-life
  #21001, 23104,        # BMI; continuous
  ## Smoking :: Mid-life rather than late-life
  #20116,               # status
  #3436,                # age started (current)
  #2867,                # age started (former)
  #2897,                # age stopped
  #2926,                # attempts to stop (unsuccesSful)
  ## Excessive Alcohol consumption
  #20117,               # Status
  #1558,                # Freq
  #3731,                # Former
  #1628,                # 10y before
  ### Other Gender stuff
  #2040,                # Risk taking
  #2050,                # Depression
  #6155,                # Vitamin use
  ### Neuroticism
  #20127,               # Score
  ## TODO: Organize these
  #1920, 1930, 1940, 1950, 1960, 1970, # Behaviours 1-6
  #1980, 1990, 2000, 2010, 2020, 2030, # Behaviours 6-12
  ### Social isolation
  ## TODO: Organize these
  #29176, 29179, 29169, 29166, 29171, 29174,
  #29173, 29163, 29164, 29165, 29172, 29178,
  #29162, 29177, 29167, 29168, 29175, 29180,
  #29208, 29195, 29170,
  ### Female specific factors
  ## TODO: Organize these
  #2714, 2724, 3581, 3700, 2734, 3872, 2754,
  #2764, 2774, 3829, 3839, 3849, 2784, 2794,
  #2804, 2814, 3536, 3546, 3591, 2824, 2834, 3882)
) |>
  as.data.table(keep.rownames = TRUE) |>
  setnames(c("COLNAME", "FIELD"))

metadata.dt <- metadata.dt[cols.dt, on = "FIELD"] |>
  setcolorder("COLNAME", after = "FIELD")

notes.dt    <- metadata.dt[, .(FIELD, NOTES)]
metadata.dt[, NOTES := NULL]
rm(cols.dt)

## MAIN data.table
# Search pattern for column extraction
cols.pttn    <- metadata.dt$FIELD |>
  sprintf(fmt = "f\\.%i\\.") |>
  paste(collapse = "|")

# Add first column with SUBJ_id
cols.pttn    <- paste0("f\\.eid|", cols.pttn)

# Database extraction using arrow+dplyr
# When rerruning, don't reload main data.table if it is already in environment
if (!exists("DT")) {
  system.time({
    DT <- fpaths[2] |>
    arrow::open_dataset() |>
    dplyr::select(matches(cols.pttn)) |>
    dplyr::collect() |>
    setDT()
  })
  #TODO: Need to remove this, after checking it's no longer necessary.
  DTorig  <- copy(DT)
} else {
  DT      <- copy(DTorig)
}
rm(fpaths, cols.pttn)

### Data cleaning
## Rename cols using metadata.dt
#TODO: Need to remove this, after checking it's no longer necessary.
#if (exists("DTorig")) DT <- DTorig
#DTorig <- DT
setnames(DT, "f.eid", "EID", skip_absent = T)
all.cols    <- names(DT)
last.col    <- "EID"
for (i in 1:metadata.dt[, .N]) {
  ## Metadata
  field     <- metadata.dt[i, FIELD]
  name.col  <- metadata.dt[i, COLNAME]
  instanced <- metadata.dt[i, INSTANCED]
  arrayed   <- metadata.dt[i, ARRAYED]

  # Extract relevant columns
  orig.cols <- grep(
    x = all.cols,
    pattern = sprintf("f\\.%i\\.", field),
    value = TRUE
  )

  if (length(orig.cols) == 0) next

  # Rename
  new.cols  <- str_replace(orig.cols, sprintf("f\\.%i", field), name.col)

  # Need to keep sessions?
  new.cols  <- if (instanced) {
    str_replace(new.cols, "\\.(\\d)\\.", "_ses\\1.")
  } else {
    str_remove(new.cols, "\\.\\d(?=\\.)")
  }

  # Delete unneded arrays
  new.cols  <- if (arrayed) {
    str_replace(new.cols, "\\.(\\d*)$", "_\\1")
  } else {
    str_remove(new.cols, "\\.\\d*$")
  }

  # Rename columns
  setnames(DT, orig.cols, new.cols)

  # Keep order
  setcolorder(DT, new.cols, after = last.col)
  last.col    <- new.cols[length(new.cols)]

  # Aggregate Dxs: Family History | ICD[9,10]
  if (name.col %like% "ILL|ICD") {
    if (instanced) {
      for (j in metadata.dt[i, seq(INSTANCE_min, INSTANCE_max)]) {
        out.col   <- sprintf("%s_ses%i", name.col, j)
        src.cols  <- sprintf("ses%i", j) |> grep(new.cols, value = TRUE)
        DT[
          , (out.col) := do.call(paste, c(.SD, sep = ",")), .SDcols = src.cols
        ] |> suppressWarnings()
        DT[, (out.col) := str_remove_all(get(out.col), ",NA")]
        DT[get(out.col) == "NA", (out.col) := NA]
        setcolorder(DT, out.col, before = src.cols[1])
        DT[, (src.cols) := NULL] |> suppressWarnings()
      }
      rm(j)
    } else {
      out.col   <- name.col
      src.cols  <- new.cols
      DT[
        , (out.col) := do.call(paste, c(.SD, sep = ",")), .SDcols = src.cols
      ] |> suppressWarnings()
      DT[, (out.col) := str_remove_all(get(out.col), ",NA")]
      DT[get(out.col) == "NA", (out.col) := NA]
      setcolorder(DT, out.col, before = src.cols[1])
      DT[, (src.cols) := NULL] |> suppressWarnings()
    }
    last.col <- out.col
    rm(out.col, src.cols)
  }

  # Clean
  rm(i, field, name.col, instanced, arrayed, orig.cols, new.cols)
}
rm(all.cols, last.col)

covars.dt   <- copy(DT)

## OUT
outfiles <- here(
  "data/rds",
  c("metadata.rds", "notes.rds", "../arrow/UKBB_covars_all.arrow")
)

metadata.dt |> saveRDS(outfiles[1])
notes.dt |> saveRDS(outfiles[2])
covars.dt |> arrow::write_feather(outfiles[3])
rm(outfiles)
