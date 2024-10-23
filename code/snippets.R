### Columns of interest:
## Dates
# 21862-[23].0: Brain MRI timestamp; Date
# 21825-[23].0: Touchscreen cognitive timestamp - scan; Date
## Demographics
# 31-0.0: Sex (reported); 0-F,1-M
# 22001-0.0: Sex (Chromosomal); 0-F,1-M
# 22019-0.0: chromosomal aneuploidy; 1-TRUE
# 21003-[0-3].0: Age
# 52-0.0: Month of birth
# 22200-0.0: Year of birth
# 845-[0-2].0: Education (years); 100306 :: Years, -1 IDK, -2 Never, -3 PrNA
# Education scores (Need to scale)
# 26414-0.0: England
# 26431-0.0: Scotland
# 26421-0.0: Wales
## Alzheimer's Family
# : 20107-[0-3].[0-9]: Illnesses of father; AD10
# : 20110-[0-3].[0-9]: Illnesses of mother; AD10
## ICD9 dx
# 41271-0.[0-46]: ALL diagnoses of ICD9
# Gender Dysphoria: 302.5 302.50 302.51 302.52 302.53 302.6 302.85
# Intersex: 225.2 752 752.4 752.40 752.49 752.61 752.64 752.69 752.7
## ICD10 dx
# 41270-0.[0-258]: ALL diagnoses of ICD10
# E: Intersex: E25 E250 E345
# F: Mental disorders; F00: dementia due to AD
# Gender Dysphoria: F64 F641 F642 F648 F649 Z87890
# G: Diseases of the NS; G30: AD
# Q0: congenital malformations of NS
# Q: Intersex: Q50 Q500 Q5001 Q5002 Q501 Q502 Q503 Q5031 Q5032 Q5039
# Q504 Q505 Q506 Q51 Q510 Q511 Q5110 Q5111 Q512 Q5121 Q5122 Q5128 Q513
# Q514 Q515 Q516 Q517 Q518 Q5181 Q51810 Q51811 Q51818 Q5182 Q51821 Q51828
# Q519 Q52 Q520 Q521 Q5210 Q5211 Q5212 Q52120 Q52121 Q52122 Q52123 Q52124
# Q52129 Q522 Q523 Q524 Q525 Q526 Q527 Q5270 Q5271 Q5279 Q528 Q529 Q53
# Q530 Q5300 Q5301 Q5302 Q531 Q5310 Q5311 Q53111 Q53112 Q5312 Q5313 Q532
# Q5320 Q5321 Q53211 Q53212 Q5322 Q5323 Q539 Q54 Q540 Q541 Q542 Q543 Q544
# Q548 Q549 Q55 Q550 Q551 Q552 Q5520 Q5521 Q5522 Q5523 Q5529 Q553 Q554
# Q555 Q556 Q5561 Q5562 Q5563 Q5564 Q5569 Q557 Q558 Q559 Q56 Q560 Q561
# Q562 Q563 Q564 Q64 Q640 Q641 Q6410 Q6411 Q6412 Q6419 Q642 Q643 Q6431
# Q6432 Q6433 Q6439 Q644 Q645 Q646 Q647 Q6470 Q6471 Q6472 Q6473 Q6474
# Q6475 Q6479 Q648 Q649
## Alzheimer's report date
# 130836-0.0: F00 first reported; 819 ::
# 1900-01 No event date; 1901-01 Before birth; 1902-02 Birth; 1903-03 Yearbirth
# 1909-09 & 2037-07 Future system default
# 131036-0.0: G30 first reported; 819 ::
# 1900-01 No event date; 1901-01 Before birth; 1902-02 Birth; 1903-03 Yearbirth
# 1909-09 & 2037-07 Future system default
## Cognitive scores
# 53-[0-3]: Date attending assessment centre (cognition)
# Pairs matching
# 399-[0-3].[1-3] & 20132-0-[0-2]:  n incorrect; lower better
# 400-[0-3].[1-3] & 20133-0-[0-2]: Time to complete round; decisecs, millisecs
# 20134-0: Date pairs test; Date
# Fluid intelligence
# 20016-[0-3] & 20191-[01]: Score; correct answers
# 20135-[01]: Date Fluid intelligence test (20191); Date
# Trail making
# 6348-[23] & 20156-[01]: numeric path; secs
# 6349-[23] & 20247-[01]: numeric path; errors
# 6350-[23] & 20157-[01]: alphanumeric path; secs
# 6351-[23] & 20248-[01]: alphanumeric path; erros
# 20136-[01]: Date Trail making (online); Date
# Symbol-digit substitution
# 23323-[23] & 20159-[01]: correct matches; N
# 23324-[23] & 20195-[01]: attempted matches; N
# 20137-[01]: Date symbol-digit sub; Date
# Numeric memory
# 4282-[0-3] & 20240-0: max remembered; 100969 :: -1 Abandoned
# 20138-0: Date Num memory test (20240); Date
# Matrix pattern completion:
# 6373-[23] & 20760-1: correct matrices; N
# 6374-[23] & 20761-1: viewed matrices; N
# 6333-[03]-[0-14] & 20763-1-[0-14]: Duration for each puzzle; miliseconds
# 20765-1: Date Matrices puzzles; Date
# Reaction time
# 20023-[0-3]: mean; ms
# Broken letter recognition
# 20139-[23]: Correctly IDed; N
# Picture vocabulary:
# 6364-[23]: Vocabulary level; float
# 6365-[23]: Uncertainty of level; float
# 26302-[23]: Specific cognitive ability (AS); percentage
# Tower rearranging:
# 21004-[23]: Puzzles correct; N
# 6386-[23]: Puzzles attempted; N
# Paired associate learning ## Not found in our data ##
# 20197-2: Score # Not found #
# Prospective memory
# 20018-[0-3]: Result; 18 :: NEEDS REORDERING
# 0-skipped,incorrect, 1-first attempt, 2-second attempt;
# 4288-[0-3]: Time to answer; seconds
## FreeSurfer Hippocampus
# 26562-[23].0: HCvol left
# 26593-[23].0: HCvol right
## Femeninity Scores (Levinsson, 2022)
# 34-0.0: Birth year
# 2040-[0-3].0: Risk-taking; Binary; Reverse it
# 2050-[0-3].0: Depression; Likert
# 6138-[0-3].[0-5]: Education level: Use highest level
# 1: College/University :: 0.84;
# 2 A/AS levels (Y12-13; 16-18yo):: 1.04;
# 3 O/GCSE (Y11, 15yo):: 1.5;
# 4 CSE (equiv. O levels 1965-87):: 1.17;
# 5 NVQ/HND  (vocational training):: 0.51;
# 6 Other (e.g. nursing, teaching):: 1.29;
# 7 None :: 0.98;
# 6155-[0-3].[0-6]: Vitamin Use; 1-7|-7 (None); Binarize it
# 6142-[0-3].[0-6]: Employment status
# 1 Paid/Self :: 0.9;
# 2 Retired :: 1.13;
# 3 Home/Family :: 9.63;
# 4 sickness/disability :: 0.69;
# 5 unemployed :: 0.45;
# 6 Unpaid/volunteer :: 2.19;
# 7 Student :: 1.66;
# 20116-[0-3].0: Smoking status; 0-3 Never, Before, Current;
# Binarize it: Current or Not
# 20127.0.0: Neuroticism score: Summary score based on 12 neurotic bhvrs:
# 19[2-9]0, 20[1-3]0
## Potentially gender:
# Social isolation
# 29176-0.0: Ability manage stressful events; 1931
# 29179-0.0: Diff times with little trouble ; 1931
# 29169-0.[0-5]: Curr. situtation (employment); 1928
# 29166-0.0: Freq confiding in someone; 1927
# 29171-0.0: Freq 'in tune' w/people; 1930
# 29174-0.0: Freq feeling isolated; 1930
# 29173-0.0: Freq feeling left-out; 1930
# 29163-0.0: Freq seeing friends/family (person); 1927
# 29164-0.0: Freq seeing friends/family (zoom); 1927
# 29165-0.0: Freq seeing friends/family (phone); 1927
# 29172-0.0: Freq feeling lack companionship; 1930
# 29178-0.0: Hard "snap-back" when smt bad happens; 1931
# 29162-0.0: N people living in house; 1926
# 29177-0.0: Quick recovery from stress; 1931
# 29167-0.[0-4]: Sports/socials per week (person); 1953
# 29168-0.[0-4]: Sports/socials per week (virtual); 1953
# 29175-0.0: Bounce back quickly after hardships; 1931
# 29180-0.0: Take long time over setbacks; 1931
# 29208-0.0: When Qtionnare was completed; Date
# 29195-0.0: When Qtionnare was started; Date
# 29170-0.0: When retired; Date
# Codings:
# 1926: 0 Me, 1 Me & other, 2 +2:<5, 3 +5, -3 Pr NA
# 1927: 0-5: Never<Daily; -3 Pref not to answer
# 1928: 1 Employed; 2 Self-Emp; 3 Retired; 4 Home-looker; 5 Carer; 6 Childcare
# 7 Sick-leave; 8 Unemployed; 9 Unpaid/volunteer; 10 Student; -1 IDK; -3 Pr NA
# 1930: 0-2: Hardly-ever<often; -1 IDK; -3 Pr NA
# 1931: 0-5: Likert Disagree<Agree
# 1953: 1 Sports, 2 Pub, 3 Church, 4 Adult Educ, 5 Other, 0 None, -3 Pr NA
## Female-specific factors
# 2714-0.0: Menarche; Age 100291 :: -1 IDK, -3 PrNA
# 2724: Menopause; 100579 :: Bool, 2 NS (hyst), 3 NS (other), -3 Pr NA
# 3581: Menopause; Age 100291 :: -1 IDK, -3 PrNA
# 3700: Time since last period; 10029 :: -1 IDK, -3 PrNA
# 2734: N live births; 100584 :: -3 PrNA
# 3872: Age birth of child; 100586 :: -4 IDR, -3 PrNA
# 2754: Age first live birth; 100586 :: -4 IDR, -3 PrNA
# 2764: Age last live birth; 100586 :: -4 IDR, -3 PrNA
# 2774: Stillbirths, miscarriages, terms; 100349 :: Bool, -1 IDK, -3 PrNA
# 3829: N stillbirths; 100291 :: -1 IDK, -3 PrNA
# 3839: N miscarriages; 100291 :: -1 IDK, -3 PrNA
# 3849: N terminations; 100291 :: -1 IDK, -3 PrNA
# 2784: Oral contraceptive (ever) 100349 :: Bool, -1 IDK, -3 Pr NA
# 2794: Age started OC; 100291 :: -1 IDK, -3 PrNA
# 2804: Age last OC; 100595 :: -1 IDK, -3 Pr NA, -11 still taking
# 2814: HRT (ever); 100349 :: Bool, -1 IDK, -3 Pr NA
# 3536: Age started HRT; 100291 :: -1 IDK, -3 PrNA
# 3546: Age last HRT; 100598 :: -1 IDK, -11 Still taking, -3 PrNA
# 3591: Hysterectomy; 100599 :: Bool, -5 Not sure, -3 PrNA
# 2824: Age hysterectomy; 100291 :: -1 IDK, -3 PrNA
# 2834: Oophorectomy; 100599 :: Bool, -5 Not sure, -3 PrNA
# 3882: Age oophorectomy; 100291 :: -1 IDK, -3 PrNA
## Lancet Risk factors:
## Education ^^ :: not attending secondary school
## Hearing loss
# 28627: Current; 100349 :: Bool, -1 IDK, -3 PrNA
# 28629: Extent; 149 :: 0 Not, 1 avoid/reduce/spread out act, -1 IDK, -3 PrNA
# 28628: Length; 135 :: 0 -2w, 1 2-4w, 2 4-12w, 3 +12w, -1 IDK, -3 PrNA
## High LDL cholesterol
# 2340[45]: [Clinical] LDL cholesterol; mmol/l
## Depression ^^
## TBI:
# 41270-0.[0-225]: ALL diagnoses of ICD10
# S06: Intracranial injury
## Physical inactivity
# 2203[56]: >= mod/vig[/walking] recomm; 7 :: Bool
## Diabetes :: Mid-life onset of T2D, not late-life
# 2976: Age Dx; 100291 :: -1 IDK, -3 PrNA
# 130708 & 130710: Date E1[12] first report (non-ins. dep. & malnutrition);
# 819 ::
# 1900-01 No event date; 1901-01 Before birth; 1902-02 Birth; 1903-03 Yearbirth
# 1909-09 & 2037-07 Future system default
## Smoking :: Mid-life rather than late-life
# 20116: status; 90 :: 0 Never, 1 Before, 2 Yes, -3 PrNA
# 3436: age started (current); 100291 :: -1 IDK, -3 PrNA
# 2867: age started (former); 100291 :: -1 IDK, -3 PrNA
# 2897: age stopped; 100291 :: -1 IDK, -3 PrNA
# 2926: attempts to stop (unsuccesSful); 100291 :: -1 IDK, -3 PrNA
## Hypertension
# 131286 & 131294: Date of rep Hypertension; 819 ::
# 1900-01 No event date; 1901-01 Before birth; 1902-02 Birth; 1903-03 Yearbirth
# 1909-09 & 2037-07 Future system default
## Obesity :: Mid-life
# 21001 & 23104: BMI; continuous
## Excessive Alcohol consumption
# 20117: Status; 90 :: 0 Never, 1 Before, 2 Yes, -3 PrNA
# 1558: Freq; 100402 ::
# 1 Daily, 2 3-4p/w, 3 1-2p/w, 4 1-3p/m, 5 spec occ, 6 Never, -3 PrNA
# 3731: Former; 100352 :: Bool, -3 PrNA
# 1628: 10y before; 100417 :: 1 +, 2 =, 3 -, -1 IDK, -3 PrNA
## Social isolation ^^
## Air pollution :: Too much/too Specific to UK—Ignore ##
## Visual loss (untreated)
# 6148: Eye problems; 100523 ::
# 1 diabetes, 2 glaucoma, 3 injury/trauma, 4 cataract,
# 5 macular deg., 6 other, -7 None, -1 IDK, 3 PrNA


### Parse AD history columns
## AD in paternal side
#p_cols    <- str_subset(names(covars.dt), "AD_pater")
#pater.dt  <- covars.dt[, .(ID, DX = do.call(paste, c(.SD, sep = ","))),
                       #.SDcols = p_cols
                       #][, .(ID, AD_pat = str_detect(DX, "10"))]

### Find subjects with significant Diagnoses
## FXX: Mental & behavioural disorders
## GXX: Diseases of the nervous system
## Q0X: Congenital malformations of the nervous system
#icd_cols  <- str_subset(names(covars.dt), "ICD10")
#icd_10.dt <- covars.dt[, .(ID, DX = do.call(paste, c(.SD, sep = ","))),
                       #.SDcols = icd_cols
                       #][, .(ID,
                             #ICD10_f = str_detect(DX, "F"),
                             #ICD10_g = str_detect(DX, "G"),
                             #ICD10_q = str_detect(DX, "Q0"))]


### Calculate intervale between MRI scans
## Parse dates
#covars.dt[, `:=`(SCAN_bl = ymd_hms(SCAN_bl), SCAN_fu = ymd_hms(SCAN_fu))]

## Infer follow-up time from age
#covars.dt[is.na(SCAN_fu) & !is.na(AGE_fu),
          #SCAN_fu := SCAN_bl + years(AGE_fu - AGE_bl)]

## Calculate time intervals
#covars.dt[, SCAN_diff := time_length(interval(SCAN_bl, SCAN_fu), "years")]
#covars.dt[, c("SCAN_bl", "SCAN_fu") := NULL]

### Education standardize for equivalence between countries
## All subjects
#covars.dt[!is.na(EDUC.1),
          #EDUC_all := scale(EDUC.1, center = TRUE, scale = TRUE)]
#covars.dt[!is.na(EDUC.2),
          #EDUC_all := scale(EDUC.2, center = TRUE, scale = TRUE)]
#covars.dt[!is.na(EDUC.3),
          #EDUC_all := scale(EDUC.3, center = TRUE, scale = TRUE)]

## Healthy subjects
#covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.1),
          #EDUC_hty := scale(EDUC.1, center = TRUE, scale = TRUE)]
#covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.2),
          #EDUC_hty := scale(EDUC.2, center = TRUE, scale = TRUE)]
#covars.dt[!ICD10_f & !ICD10_g & !ICD10_q & !is.na(EDUC.3),
          #EDUC_hty := scale(EDUC.3, center = TRUE, scale = TRUE)]

## Long format regarding Scan visit
#covars.dt <- melt(covars.dt, measure = patterns("_bl$|_fu$"))

#covars.dt[grepl("_bl$", variable), SCAN_visit := 1]
#covars.dt[grepl("_fu$", variable), SCAN_visit := 2]

#covars.dt[, variable := str_sub(variable, end = -4)]

#covars.dt <- dcast(covars.dt, ... + SCAN_visit ~ variable, value = "value")

#covars.dt[, SCAN_visit := factor(SCAN_visit, levels = 1:2,
                                 #labels = c("Baseline", "Follow-up"))]

library(data.table)

get_sample_dt <- function(distance_age, distance_icc, dt) {
  ## Function to create samples matched by age and head size (ICC)

  # Split data into male and female
  male <- dt[SEX == "M", .(PTID, AGE, ICC)]
  female <- dt[SEX == "F", .(PTID, AGE, ICC)]

  # Initialize empty data.table for matched pairs
  matched <- data.table(PTID_M = character(), AGE_M = numeric(), ICC_M = numeric(),
                        PTID_F = character(), AGE_F = numeric(), ICC_F = numeric(),
                        round = integer())

  for (i in seq_len(nrow(male))) {
    # Find females within the specified distance for both AGE and ICC
    ind <- female[
      abs(male$AGE[i] - AGE) < distance_age & abs(male$ICC[i] - ICC) < distance_icc,
      which = TRUE
    ]

    if (length(ind) > 0) {
      # Randomly sample if multiple matches
      ind_matched <- if (length(ind) == 1) ind else sample(ind, 1)

      # Add matched pair to result
      matched <- rbind(matched, data.table(
        PTID_M = male$PTID[i], AGE_M = male$AGE[i], ICC_M = male$ICC[i],
        PTID_F = female$PTID[ind_matched], AGE_F = female$AGE[ind_matched], ICC_F = female$ICC[ind_matched],
        round = i
      ), fill = TRUE)

      # Remove matched female to avoid reusing
      female <- female[-ind_matched]
    }
  }

  return(matched)
}

# Example usage:
# dt <- data.table(PTID = c(...), SEX = c(...), AGE = c(...), ICC = c(...))
# result <- get_sample_dt(distance_age = 1, distance_icc = 100, dt = dt)

