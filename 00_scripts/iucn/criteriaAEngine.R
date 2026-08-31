library(dplyr)
library(readr)
source("00_scripts/iucn/config_iucn.R")

# ============================================================
# 1. READ MANUAL DECLINE DATA
# ============================================================

manual_decline <- if (file.exists(popDeclinefile)) {
  read_csv(popDeclinefile) %>%  
    mutate(
      EnglishName = trimws(Species),
      Method = trimws(Method),
      
      # These fields are not applicable for manual input
      RangeCoverage = NA_character_,
      MeanGridCoverage = NA_character_,
      LTC = NA_character_,
      CAT = NA_character_
    ) %>% 
    filter(!is.na(Method))
} else {
  tibble()
}

# Process only if there are rows
if(nrow(manual_decline) > 0)
{

# ============================================================
# 2. READ SOIB DATA AND PREPARE GENERATION LENGTH
# ============================================================

soib <- read.csv(get_metadata("none")$SOIBMAIN.PATH)

gen_data <- soib %>%
  select(
    EnglishName = India.Checklist.Common.Name,
    GenerationLength = Generation.Length
  ) %>%
  mutate(
    EnglishName = trimws(EnglishName)
  )

# 3. Since we are using generation length directly from SoIB file, we dont need 3rd step
#   
# ============================================================
# 4. APPLY GENERATION LENGTH TO MANUAL DATA
# ============================================================

  manual_decline <- manual_decline %>%
    left_join(gen_data, by = "EnglishName") %>%
    mutate(
      # IUCN rule: 3GEN or minimum 10 years
      Years3GEN = pmax(10, round(3 * GenerationLength))
    )


# ============================================================
# 5. PREPARE DECLINE METRICS
# ============================================================

  manual_decline <- manual_decline %>%
    mutate(
      OrgStartYear = as.numeric(StartYear),
      OrgEndYear = as.numeric(EndYear),
      DeclinePercent = as.numeric(DeclinePercentRci), #always take 95% RCI
      DeclinePercentMean = as.numeric(DeclinePercentMean),
      DeclinePercentLci = as.numeric(DeclinePercentLci),
      ActualDecline = paste0 ("-",round(DeclinePercentMean, 1),"% (-",round(DeclinePercentRci, 1)," , -", round(DeclinePercentLci, 1),")"),
      
      Duration = OrgEndYear - OrgStartYear,
      
      Years3GEN = as.numeric(Years3GEN),
      
      YearsToExtend = Years3GEN - Duration,
      NeedsExtension = Duration < Years3GEN,
      
      # Only these methods allow extrapolation (correct IUCN interpretation)
      CanExtrapolate = Method %in% c("Observed", "Inferred", "Projected"),
      WasExtrapolated = NeedsExtension & CanExtrapolate
    )


# ============================================================
# 6. STANDARDISE DECLINE TO 3GEN WINDOW
# ============================================================

  manual_decline <- manual_decline %>%
    mutate(
      # Convert decline → remaining proportion
      RemainingProp =
        ifelse(!is.na(DeclinePercent),
               1 - (DeclinePercent / 100),
               NA_real_),
      RemainingPropMean =
        ifelse(!is.na(DeclinePercentMean),
               1 - (DeclinePercentMean / 100),
               NA_real_),
      RemainingPropLci =
        ifelse(!is.na(DeclinePercentLci),
               1 - (DeclinePercentLci / 100),
               NA_real_),
      
      # Annualised multiplicative rate
      AnnualRate =
        ifelse(Duration > 0 & !is.na(RemainingProp),
               RemainingProp^(1 / Duration),
               NA_real_),
      AnnualRateMean =
        ifelse(Duration > 0 & !is.na(RemainingPropMean),
               RemainingPropMean^(1 / Duration),
               NA_real_),
      AnnualRateLci =
        ifelse(Duration > 0 & !is.na(RemainingPropLci),
               RemainingPropLci^(1 / Duration),
               NA_real_),
      
      # Decide evaluation period
      EvalYears =
        case_when(
          Duration >= Years3GEN ~ Years3GEN,
          NeedsExtension & CanExtrapolate ~ Years3GEN,
          TRUE ~ Duration
        ),
      
      # Adjust EndYear to match evaluation window
      EndYear =
        ifelse(!is.na(StartYear) & !is.na(EvalYears),
               OrgStartYear + EvalYears,
               OrgEndYear),
      
      StartYear = OrgStartYear,
      
      # Project decline to 3GEN
      FinalRemaining =
        ifelse(!is.na(AnnualRate),
               AnnualRate^EvalYears,
               NA_real_),
      FinalRemainingMean =
        ifelse(!is.na(AnnualRateMean),
               AnnualRateMean^EvalYears,
               NA_real_),
      FinalRemainingLci =
        ifelse(!is.na(AnnualRateLci),
               AnnualRateLci^EvalYears,
               NA_real_),
      
      Decline =
        ifelse(!is.na(FinalRemaining),
               (1 - FinalRemaining) * 100,
               DeclinePercent),
      DeclineMean =
        ifelse(!is.na(FinalRemainingMean),
               (1 - FinalRemainingMean) * 100,
               DeclinePercentMean),
      DeclineLci =
        ifelse(!is.na(FinalRemainingLci),
               (1 - FinalRemainingLci) * 100,
               DeclinePercentLci),
      
      # If extrapolated → method becomes Projected (important correction)
      Method =
        case_when(
          NeedsExtension & CanExtrapolate ~ "Projected",
          TRUE ~ Method
        )
    )


# ============================================================
# 7. FINAL CLEANING OF MANUAL DATA
# ============================================================

  manual_decline <- manual_decline %>%
    mutate(
      Years3GEN = as.numeric(Years3GEN),
      Decline = round(Decline, 1)
    )

  manual_decline <- manual_decline %>%
    mutate(
      # Convert all logical flags to numeric (0/1 standardisation)
      Reversible = as.numeric(Reversible),
      ReasonUnderstood = as.numeric(ReasonUnderstood),
      ReasonCeased = as.numeric(ReasonCeased),
      
      DirectObservation = as.numeric(DirectObservation),
      AbundanceIndex = as.numeric(AbundanceIndex),
      EOODecline = as.numeric(EOODecline),
      AOODecline = as.numeric(AOODecline),
      HabitatQuality = as.numeric(HabitatQuality),
      Exploitation = as.numeric(Exploitation),
      OtherEffects = as.numeric(OtherEffects),
      
      Decline = as.integer(Decline),
      DeclineMean = as.integer(DeclineMean),
      DeclineLci = as.integer(DeclineLci),
      RangeCoverage = as.numeric(RangeCoverage)
      
    ) %>%
    select(
      EnglishName,
      Method,
      Reversible,
      ReasonUnderstood,
      ReasonCeased,
      OrgStartYear,
      OrgEndYear,
      ActualDecline,
      StartYear,
      EndYear,
      Years3GEN,
      Decline,
      DeclineMean,
      DeclineLci,
      DirectObservation,
      AbundanceIndex,
      EOODecline,
      AOODecline,
      HabitatQuality,
      Exploitation,
      OtherEffects,
      RangeCoverage,
      MeanGridCoverage,
      LTC,
      CAT
    )
}


# ============================================================
# 8. READ REDLIST (SoIB-DERIVED) DECLINE DATA
# ============================================================

redlist_decline <- read_csv(soibredlistfile) %>%
  filter(`3GEN valid` == TRUE) %>%
  mutate(
    EnglishName = trimws(Species),
    
    Method = "Inferred",
    
    # Conservative defaults (no reversibility assumptions)
    Reversible = 0,
    ReasonUnderstood = 0,
    ReasonCeased = 0,
    
    StartYear = 2015,
    OrgStartYear = 2015,
    OrgEndYear = latestYear,
    EndYear = StartYear + Years3GEN,
    
    Decline = as.integer(`3GEN Decline`),
    DeclineMean = as.integer(`3GEN Decline Mean`),
    DeclineLci = as.integer(`3GEN Decline LCI`),
    ActualDecline = `Current Annual Decline`,
    
    # Evidence pathway (index-based inference)
    DirectObservation = 0,
    AbundanceIndex = 1,
    EOODecline = 0,
    AOODecline = 0,
    HabitatQuality = 0,
    Exploitation = 0,
    OtherEffects = 0,
    
    RangeCoverage = `Range Coverage`,
    MeanGridCoverage = `Mean Grid Coverage`,
    LTC = `Long-term Decline`,
    CAT = `Current Annual Decline`
  ) %>%
  select(
    EnglishName,
    Method,
    Reversible,
    ReasonUnderstood,
    ReasonCeased,
    OrgStartYear,
    OrgEndYear,
    ActualDecline,
    StartYear,
    EndYear,
    Years3GEN,
    Decline,
    DeclineMean,
    DeclineLci,
    DirectObservation,
    AbundanceIndex,
    EOODecline,
    AOODecline,
    HabitatQuality,
    Exploitation,
    OtherEffects,
    RangeCoverage,
    MeanGridCoverage,
    LTC,
    CAT
  )


# ============================================================
# 9. COMBINE ALL DECLINE DATA
# ============================================================

criteriaA_data <- bind_rows(
  manual_decline,
  redlist_decline
) %>% 
  filter(!is.na(Decline))  # Ensure quantitative basis exists


# ============================================================
# 10. ASSIGN SUBCRITERIA (a–e) AND FILTER VALID EVIDENCE
# ============================================================

criteriaA_data <- criteriaA_data %>%
  mutate(
    A_a = DirectObservation == 1,
    A_b = AbundanceIndex == 1,
    A_c = (EOODecline == 1 | AOODecline == 1 | HabitatQuality == 1),
    A_d = Exploitation == 1,
    A_e = OtherEffects == 1,
    
    HasEvidence = (A_a | A_b | A_c | A_d | A_e)
  ) %>% 
  filter(HasEvidence)  # Enforces IUCN requirement


# ============================================================
# 11. ASSIGN CRITERION TYPE (A1–A4)
# ============================================================


criteriaA_data <- criteriaA_data %>%
  mutate(
    
    # Timing of the decline
    IsPast = EndYear <= latestYear,
    
    # Decline reaches the current assessment year
    ReachesCurrent = OrgEndYear == latestYear,
    
    # Decline continues beyond current year
    IsOngoing = StartYear < latestYear & EndYear >= latestYear,
    
    # Entirely future
    IsFuture = StartYear == latestYear & EndYear > latestYear,
    
    # A1: past decline where cause is understood,
    # ceased, and reversible
    IsA1 = IsPast &
      Reversible == 1 &
      ReasonUnderstood == 1 &
      ReasonCeased == 1,
    
    # A2: past decline not qualifying for A1
    IsA2 = IsPast & !IsA1,
    
    # A3: future decline
    IsA3 = IsFuture,
    
    # A4: decline that continues beyond the current year
    IsA4 = IsOngoing | ReachesCurrent
  )

# ============================================================
# 12. ASSIGN IUCN CATEGORY
# ============================================================

criteriaA_data <- criteriaA_data %>%
  mutate(
    
    Category = case_when(
      
      # A1 thresholds
      IsA1 & Decline > 90 ~ "CR",
      IsA1 & Decline > 70 ~ "EN",
      IsA1 & Decline > 50 ~ "VU",
      IsA1 & Decline > 20 ~ "NT",
      
      # A2 / A3 / A4 thresholds
      !IsA1 & Decline > 80 ~ "CR",
      !IsA1 & Decline > 50 ~ "EN",
      !IsA1 & Decline > 30 ~ "VU",
      !IsA1 & Decline > 20 ~ "NT",
      
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 13. BUILD SUBCRITERIA STRING (e.g., A2bcde)
# ============================================================

criteriaA_data <- criteriaA_data %>%
  rowwise() %>%
  mutate(
    Subcriteria = ifelse(
      A_a | A_b | A_c | A_d | A_e,
      paste0(
        ifelse(A_a, "a", ""),
        ifelse(A_b, "b", ""),
        ifelse(A_c, "c", ""),
        ifelse(A_d, "d", ""),
        ifelse(A_e, "e", "")
      ),
      NA_character_
    )    
  ) %>%
  ungroup()


# ============================================================
# 14. COMBINE CRITERION TYPE + SUBCRITERIA
# ============================================================

criteriaA_data <- criteriaA_data %>%
  rowwise() %>%
  mutate(
    
    A1_string = ifelse(
      IsA1,
      paste0("A1", Subcriteria),
      NA_character_
    ),
    
    A2_string = ifelse(
      IsA2,
      paste0("A2", Subcriteria),
      NA_character_
    ),
    
    A3_string = ifelse(
      IsA3,
      paste0("A3", Subcriteria),
      NA_character_
    ),
    
    A4_string = ifelse(
      IsA4,
      paste0("A4", Subcriteria),
      NA_character_
    ),
    
    CriteriaA_String = paste(
      na.omit(c(A1_string, A2_string, A3_string, A4_string)),
      collapse = "+ "
    )
  ) %>%
  ungroup()

# ============================================================
# 15. SELECT BEST RECORD PER SPECIES
# ============================================================

criteriaA_final <- criteriaA_data %>%
  mutate(
    # Ranking system: severity first, then method robustness
    SeverityScore = case_when(
      Category == "CR" ~ 4,
      Category == "EN" ~ 3,
      Category == "VU" ~ 2,
      Category == "NT" ~ 1,
      TRUE ~ 0
    ),
    
    MethodScore = case_when(
      Method == "Observed" ~ 2,
      Method == "Inferred" ~ 1,
      Method == "Projected" ~ 0,
      TRUE ~ 0
    ),
    
    CriteriaA_Category = Category,
    CriteriaA_String   = CriteriaA_String,
  ) %>%
  group_by(EnglishName) %>%
  arrange(desc(SeverityScore), desc(MethodScore), desc(Decline)) %>%
  slice(1) %>%
  ungroup()


# ============================================================
# 16. PREPARE FINAL OUTPUT TABLE
# ============================================================

criteriaA_output <- criteriaA_final %>%
  select(
    
    # Species
    EnglishName,
    
    # Final Red List result
    CriteriaA_Category,
    CriteriaA_String,
    
    # Decline info
    Decline, #LCI is actual decline
    DeclineMean,
    DeclineLci,
    Years3GEN,
    StartYear,
    EndYear,
    OrgStartYear,
    OrgEndYear,
    ActualDecline,

    
    # Method / assumptions
    Method,
    Reversible,
    ReasonUnderstood,
    ReasonCeased,
    
    # Subcriteria flags
    A_a,
    A_b,
    A_c,
    A_d,
    A_e,
    
    # Evidence detail (traceability)
    DirectObservation,
    AbundanceIndex,
    EOODecline,
    AOODecline,
    HabitatQuality,
    Exploitation,
    OtherEffects,
    
    # Ranking diagnostics
    SeverityScore,
    MethodScore
  )


# ============================================================
# 17. WRITE OUTPUT FILE
# ============================================================

write_csv(
  criteriaA_output,
  criteriaAResultsfile
)


# ============================================================
# END OF SCRIPT
# ============================================================