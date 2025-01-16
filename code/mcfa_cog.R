#!/usr/bin/env Rscript

library(here)
library(data.table)
library(lavaan)
#library(stringr)

# TODO: Re-test models with scaling turned on.
# Later figure out the implications about it.
SCALING <- F

### IN
fpaths <- here(
  "data", c(
    "rds/cognitive_tests.rds",
    "rds/metadata_cog_tests.rds",
    "arrow/UKBB_covars_all.arrow"
  )
)

if (all(file.exists(fpaths))) {
  if (!exists("cog_tests.lst")) cog_tests.lst <- readRDS(fpaths[1])
  if (!exists("cog_meta.dt")) cog_meta.dt <- readRDS(fpaths[2])
  if (!exists("covars.dt")) covars.dt <- arrow::read_feather(fpaths[3])
} else {
  here("code/clean_cog.R") |> source()
}

rm(fpaths)

### FUNCTIONS
## Extract specific cog_tests from list. Concatenate (scaled) columns.
##TODO: Include smt here to exclude online tests
ext_tests   <- function(
  cog_list,
  subDTs,
  id_vars = c("EID", "SESSION", "ONLINE"),
  scaling = FALSE
  ) {
  ##TODO: For now, this omits all character variables
  ## Implement separating numeric & character and finally join, if needed
  DT  <- cog_list[subDTs] |>
    lapply(function(DT) {
      # Makes EID character to avoid repeating it
      DT[, EID := as.character(EID)]
      # For now, this only keeps numeric columns
      # TODO: if categorical data is needed, repeat this w/is.char then join
      DT[, .SD, .SDcols = is.numeric, keyby = id_vars] |>
      melt(id = id_vars) |>
      suppressWarnings()
    }) |>
    rbindlist() |>
    {function(DT) DT[, EID := as.integer(EID)]}()

  if ("ONLINE" %in% id_vars) {
    DT[(ONLINE), variable := paste0(variable, "_online")]
    DT[, ONLINE := NULL]
  }

  DT <- DT[!is.na(value)] |> unique() |> dcast(... ~ variable)

  if (scaling) {
    cols <- names(DT)[!names(DT) %in% id_vars]
    if ("SESSION" %in% id_vars) {
      DT[, (cols) := lapply(.SD, scale), by = SESSION, .SDcols = cols]
    } else {
      DT[, (cols) := lapply(.SD, scale), .SDcols = cols]
    }
  }

  return(DT)
}

### Theoretical groupings::
#1 Matrices (Abstract reasoning/Problem solving)           :: N -  58937
#2 Trail making (Processing speed/executive function)      :: N - 222011
#3 Fluid_intelligence (Reasoning/Problem solving)          :: N - 313359
#4 Numeric memory (Working memory)                         :: N - 172471
#5 Reaction time (Processing speed)                        :: N - 496659
#6 Pair matching (Memory/Visual-spatial memory)            :: N - 498916
#7 Tower rearranging (Planning/Executive function)         :: N -  43352
#8 Symbol-digit substitution (Processing speed/Attention)  :: N - 219795
#9 Prospective memory (Memory/Prospective memory)          :: N - 227774

fpath       <- here("data/rds/cfa_grouped_data.rds")
#if (file.exists(fpath)) {
if (F) {
  if (!exists("DTs")) DTs <- readRDS(fpath)
} else {
  DTs <- list(
    Memory = ext_tests(cog_tests.lst, c(6,4,9), scaling = SCALING),
    Proc_speed = ext_tests(cog_tests.lst, c(5,2,8), scaling = SCALING),
    Reas_Exec = ext_tests(cog_tests.lst, c(1:3,7), scaling = SCALING)
  )

  saveRDS(DTs, fpath)
}
rm(fpath)


### CFA MODELING
test_names <- models <- fits <- list()

## Cognitive tests included in the factors for each domain
# TODO: Remove useless derivative measures in code/clean_cog.R?
test_names <- list(
  # Memory: Pair matching; Numeric memory; Prospective memory
  Memory = c("PRS_mean_time", "PRS_mean_inc", "NUM", "PRMEM_res_n"),
  # Processing speed: Reaction time, Trail making (numeric), Symbol-digit
  Proc_speed = c("REACT", "TRLS_num_err_t", "SYM_corr", "SYM_try"),
  # Reasoning & Exec function:
  #   Fluid intelligence, Matrices, Trail making (alphanum), Tower rearranging
  Reas_Exec = c("FLINT", "MATS_corr_try", "TRLS_alnum_err_t", "TOWER_corr_try")
)

## Factor loadings for CFA
models <- Map(
  function(cog_tests, factor_name) {
    loadings <- paste(cog_tests, collapse = " + ")
    sprintf("%s =~ %s", factor_name, loadings)
  },
  test_names,
  c("MEMORY", "PROCSPEED", "REAS_EXFN")
)

## Model fitting
fits <- Map(
  function(Data, Model) {
    cfa(
      model = Model,
      data = Data,
      std.lv = TRUE,
      cluster = "EID", # Multilevel CFA for longitudinal data
      missing = "fiml" # Full Information Maximum Likelihood (FIML)
    ) |> suppressWarnings()
  },
  DTs,
  models
)

## Explore all 3 cognitive domains
## This doesn't give good results, but might be due high levels of missing data
#single_fit <- DTs |>
  #lapply(melt, id = 1:2) |>
  #rbindlist() |>
  #{function(DT) DT[variable %in% unlist(test_names)]}() |>
  #unique() |>
  #na.omit() |>
  #dcast(... ~ variable) |>
  #cfa(
    #model = paste(models, collapse = "\n"),
    #std.lv = TRUE,
    #cluster = "EID",
    #missing = "fiml"
  #)

### Extract latent factors
latent_cog.dt <- Map(
  function(Data, Fit) {
    Data[, .(EID, SESSION, lavPredict(Fit))] |>
      na.omit() |>
      melt(id = 1:2)
  },
  DTs,
  fits
) |>
  rbindlist() |>
  dcast(... ~ variable)


### OUT
# MCFA fittings
here("data/rds/cog_mcfa_fits.rds") |> saveRDS(object = fits)

# Latent Cognitive Domains
here("data/rds/cog_mcfa_domains.rds") |> saveRDS(object = latent_cog.dt)
