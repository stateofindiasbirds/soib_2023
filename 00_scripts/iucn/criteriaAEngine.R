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
# 2. READ 3GEN DATA AND SOIB TAXONOMIC MAPPING
# ============================================================

  threegen <- read.csv(threegenfile)
  
  soib <- read.csv(get_metadata("none")$SOIBMAIN.PATH)
  
  soib <- soib %>% 
    select(
      "India.Checklist.Common.Name",
      "India.Checklist.Scientific.Name",
      "BLI.Scientific.Name",
    )


# ============================================================
# 3. GENERATION LENGTH PREPARATION
# ============================================================

  gen_data <- threegen %>%
    select(
      BLI,
      GEN
    ) %>%
    rename(
      BLI.Scientific.Name = BLI,
      GenerationLength = GEN
    )

# Map BLI → EnglishName using SOIB
  gen_data <- soib %>%
    select(
      EnglishName = India.Checklist.Common.Name,
      BLI.Scientific.Name
    ) %>%
    inner_join(gen_data, by = "BLI.Scientific.Name") %>%
    mutate(EnglishName = trimws(EnglishName)) %>%
    select(EnglishName, GenerationLength)


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
      DeclinePercent = as.numeric(DeclinePercentLci), #always take 95% LCI
      ActualDecline = paste0 ("-",round(DeclinePercentMean, 1),"% (-",round(DeclinePercentLci, 1)," , -", round(DeclinePercentRci, 1),")"),
      
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
      
      # Annualised multiplicative rate
      AnnualRate =
        ifelse(Duration > 0 & !is.na(RemainingProp),
               RemainingProp^(1 / Duration),
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
      
      Decline =
        ifelse(!is.na(FinalRemaining),
               (1 - FinalRemaining) * 100,
               DeclinePercent),
      
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
    IsPast = OrgEndYear <= latestYear,
    IsFuture = StartYear > latestYear,
    IsOngoing = OrgStartYear <= latestYear & OrgEndYear > latestYear,
    
    CriterionType = case_when(
      IsPast & Reversible == 1 & ReasonUnderstood == 1 & ReasonCeased == 1 ~ "A1",
      IsPast ~ "A2",
      IsFuture ~ "A3",
      IsOngoing ~ "A4",
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 12. ASSIGN IUCN CATEGORY (INCLUDING NT)
# ============================================================

criteriaA_data <- criteriaA_data %>%
  mutate(
    Category = case_when(
      Decline >= 80 ~ "CR",
      Decline >= 50 ~ "EN",
      Decline >= 30 ~ "VU",
      Decline >= 20 ~ "NT",  # Added NT band (20–29%)
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
  mutate(
    Criteria = case_when(
      !is.na(Subcriteria) & Subcriteria != "" ~ paste0(CriterionType, Subcriteria),
      !is.na(CriterionType) ~ CriterionType,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    CriteriaA_String = Criteria
  )


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
    Decline,
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