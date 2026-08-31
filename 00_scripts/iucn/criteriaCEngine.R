# ============================================================
# CRITERION C RED LIST ASSESSMENT
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
source("00_scripts/iucn/config_iucn.R")

# ============================================================
# 1. READ INPUT DATA
# ============================================================

# ============================================================
# 1a. READ POPULATION DATA
# ============================================================

population_data <- if (file.exists(populationsfile)) {
  read_csv(populationsfile) %>%  
    rename(
      Largest_Sub_Pop = `Largest SubPop`
    ) %>%
    mutate(
      EnglishName = trimws(EnglishName)
    )
} else {
  tibble()
}

if (nrow(population_data) == 0) {
  
  message("No population data — skipping Criterion C")
  

  return;
}

# ============================================================
# 1b. READ MANUAL DECLINE DATA
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


# 1c. READ SOIB DATA FOR CONTINUING DECLINE
#
# SoIB is NOT used for C1.
# Continuing decline is indicated by currentsloperci < 0.

soib_main <- read.csv(
  get_metadata("none")$SOIBMAIN.PATH
) %>%
  mutate(
    EnglishName = trimws(India.Checklist.Common.Name)
  ) %>%
  filter(
    Selected.NRL == 1
  )

# ============================================================
# 1d. READ EXTREME FLUCTUATIONS (ONLY MATURE INDIVIDUALS)
# ============================================================

fluctuations <- if (file.exists(extremefluctationsfile)) {
  read_csv(extremefluctationsfile) %>%  
    mutate(
      EnglishName = trimws(Species),
      ExtremeFluctuation = ExtremeFluctuationsinNoOfMatureIndividuals
    ) %>% 
    select(EnglishName, ExtremeFluctuation)
} else {
  tibble(EnglishName = character(), ExtremeFluctuation = logical())
}


# ============================================================
# 2. PREPARE MANUAL TRENDS
# ============================================================

# ============================================================
# 2a. Prepare manual trend eligibility
# ============================================================

manual_trends <- manual_decline %>%
  mutate(
    # --------------------------------------------------------
    # C1:
    # Only Observed, Estimated and Projected are eligible
    # --------------------------------------------------------
    C1Eligible = Method %in% c(
      "Observed",
      "Estimated",
      "Projected"
    ),
    
    # --------------------------------------------------------
    # C2:
    # Observed, Estimated, Inferred and Projected are eligible
    # Suspected is NOT eligible
    # --------------------------------------------------------
    C2Eligible = Method %in% c(
      "Observed",
      "Estimated",
      "Inferred",
      "Projected"
    )
  )

# ============================================================
# 2b. PREPARE GENERATION-LENGTH WINDOWS FOR MANUAL TRENDS
# ============================================================

gen_data <- read.csv(get_metadata("none")$SOIBMAIN.PATH) %>%
  select(
    EnglishName = India.Checklist.Common.Name,
    GenerationLength = Generation.Length
  ) %>%
  mutate(
    EnglishName = trimws(EnglishName)
  )

# ============================================================
# 2c. PREPARE SPECIES GENERATION-LENGTH WINDOWS
# ============================================================
generation_windows <- gen_data %>%
  mutate(
    Years1GEN = pmax(3, round(GenerationLength)),
    Years2GEN = pmax(5, round(2 * GenerationLength)),
    Years3GEN = pmax(10, round(3 * GenerationLength))
  ) %>%
  select(
    EnglishName,
    Years1GEN,
    Years2GEN,
    Years3GEN
  )

# ============================================================
# 2d. Add all multiples of generation lengths
# ============================================================

manual_trends <- manual_trends %>%
  left_join(
    gen_data,
    by = "EnglishName"
  ) %>%
  mutate(
    Years1GEN = pmax(3, round(GenerationLength)),
    Years2GEN = pmax(5, round(2 * GenerationLength)),
    Years3GEN = pmax(10, round(3 * GenerationLength))
  )

# ============================================================
# 2e. PREPARE MANUAL TREND DURATION
# ============================================================

# ============================================================
# 2e. PREPARE MANUAL TREND DURATION
# ============================================================

manual_trends <- manual_trends %>%
  mutate(
    OrgStartYear = as.numeric(StartYear),
    OrgEndYear = as.numeric(EndYear),
    
    Duration = OrgEndYear - OrgStartYear,
    
    DeclinePercent = as.numeric(DeclinePercentRci),
    DeclinePercentMean = as.numeric(DeclinePercentMean),
    DeclinePercentLci = as.numeric(DeclinePercentLci),
    
    # --------------------------------------------------------
    # Preserve the original observed/estimated/projected trend
    # for downstream display
    # --------------------------------------------------------
    
    ActualDecline = DeclinePercent,
    ActualDeclineMean = DeclinePercentMean,
    ActualDeclineLci = DeclinePercentLci,
    ActualDeclineStartYear = OrgStartYear,
    ActualDeclineEndYear = OrgEndYear,
    ActualDeclineYears = Duration,
    ActualDeclineMethod = Method
  )

# ============================================================
# 2f. EXPONENTIAL TREND SCALING / EXTRAPOLATION FUNCTION
# ============================================================

scale_decline_exponential <- function(
    decline,
    observed_years,
    target_years
) {
  
  remaining <- 1 - decline / 100
  
  valid <- 
    !is.na(decline) &
    !is.na(observed_years) &
    !is.na(target_years) &
    observed_years > 0 &
    target_years > 0 &
    remaining >= 0 &
    remaining <= 1
  
  result <- rep(NA_real_, length(decline))
  
  annual_rate <- remaining[valid]^(1 / observed_years[valid])
  
  result[valid] <-
    (1 - annual_rate^target_years[valid]) * 100
  
  result
}


# ============================================================
# 2g. CALCULATE SCALED/EXTRAPOLATED MANUAL C1 DECLINES
# ============================================================

manual_trends <- manual_trends %>%
  mutate(
    
    # --------------------------------------------------------
    # 1 generation / minimum 3 years
    # --------------------------------------------------------
    
    C1_1GEN_Decline = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercent,
        Duration,
        Years1GEN
      )),
      NA_real_
    ),
    
    C1_1GEN_DeclineMean = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentMean,
        Duration,
        Years1GEN
      )),
      NA_real_
    ),
    
    C1_1GEN_DeclineLci = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentLci,
        Duration,
        Years1GEN
      )),
      NA_real_
    ),
    
    # --------------------------------------------------------
    # 2 generations / minimum 5 years
    # --------------------------------------------------------
    
    C1_2GEN_Decline = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercent,
        Duration,
        Years2GEN
      )),
      NA_real_
    ),
    
    C1_2GEN_DeclineMean = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentMean,
        Duration,
        Years2GEN
      )),
      NA_real_
    ),
    
    C1_2GEN_DeclineLci = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentLci,
        Duration,
        Years2GEN
      )),
      NA_real_
    ),
    
    # --------------------------------------------------------
    # 3 generations / minimum 10 years
    # --------------------------------------------------------
    
    C1_3GEN_Decline = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercent,
        Duration,
        Years3GEN
      )),
      NA_real_
    ),
    
    C1_3GEN_DeclineMean = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentMean,
        Duration,
        Years3GEN
      )),
      NA_real_
    ),
    
    C1_3GEN_DeclineLci = ifelse(
      C1Eligible,
      round(scale_decline_exponential(
        DeclinePercentLci,
        Duration,
        Years3GEN
      )),
      NA_real_
    ),
    
    # --------------------------------------------------------
    # Method used for each C1 generation window
    # --------------------------------------------------------
    
    C1_1GEN_Method = ifelse(
      C1Eligible,
      Method,
      NA_character_
    ),
    
    C1_2GEN_Method = ifelse(
      C1Eligible,
      Method,
      NA_character_
    ),
    
    C1_3GEN_Method = ifelse(
      C1Eligible,
      Method,
      NA_character_
    )
  )


# ============================================================
# 2h. Record C1 scaling/extrapolation direction
# ============================================================

manual_trends <- manual_trends %>%
  mutate(
    C1_1GEN_Direction = case_when(
      Years1GEN > Duration ~ "Extrapolated",
      Years1GEN < Duration ~ "Scaled",
      Years1GEN == Duration ~ "Unchanged",
      TRUE ~ NA_character_
    ),
    
    C1_2GEN_Direction = case_when(
      Years2GEN > Duration ~ "Extrapolated",
      Years2GEN < Duration ~ "Scaled",
      Years2GEN == Duration ~ "Unchanged",
      TRUE ~ NA_character_
    ),
    
    C1_3GEN_Direction = case_when(
      Years3GEN > Duration ~ "Extrapolated",
      Years3GEN < Duration ~ "Scaled",
      Years3GEN == Duration ~ "Unchanged",
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 3. PREPARE POPULATION DATA
# ============================================================

# ============================================================
# 3a. MERGE POPULATION AND AUXILLARY DATA
# ============================================================

criteriaC_data <- population_data %>%
  left_join(
    fluctuations,
    by = "EnglishName"
  ) %>%
  left_join(
    generation_windows,
    by = "EnglishName"
  ) 

# ============================================================
# 3b. CALCULATE MATURE POPULATION SIZE
# ============================================================

criteriaC_data <- criteriaC_data %>%
  mutate(
    
    MinMaturePop =
      MinPop *
      `Min Breeding Pop Percent` *
      ifelse(`Male Ratio` > 0.5, 1 - `Male Ratio`, `Male Ratio`) * 2,
    
    MaxMaturePop =
      MaxPop *
      `Max Breeding Pop Percent` *
      ifelse(`Male Ratio` > 0.5, 1 - `Male Ratio`, `Male Ratio`) * 2,
    
    LargestSubPopMature =
      Largest_Sub_Pop *
      `Max Breeding Pop Percent` *
      ifelse(`Male Ratio` > 0.5, 1 - `Male Ratio`, `Male Ratio`) * 2,
    
    BestMaturePop =
      ifelse(
        !is.na(MinMaturePop) & !is.na(MaxMaturePop) &
          MinMaturePop > 0 & MaxMaturePop > 0,
        round(sqrt(MinMaturePop * MaxMaturePop), 0),
        NA
      )

  )

# ============================================================
# 3c. CALCULATE LARGEST SUBPOPULATION PROPORTION
# ============================================================

criteriaC_data <- criteriaC_data %>%
  mutate(
    
    LargestSubPopPercent =
      ifelse(
        !is.na(BestMaturePop) & BestMaturePop > 0,
        pmin(100, round(100 * LargestSubPopMature / BestMaturePop)),
        NA
      )    
    
  )


# ============================================================
# 4. PREPARE C1 AND C2 TREND RESULTS
# ============================================================


# ============================================================
# 4a. C1 — MANUAL POPULATION DECLINE
#
# Only Observed, Estimated and Projected trends are eligible.
#
# For each species, retain:
#   - maximum 1-generation decline
#   - maximum 2-generation decline
#   - maximum 3-generation decline
#   - method associated with each generation window
#
# The final C1 category/method is selected later, after
# CR_C1, EN_C1 and VU_C1 have been evaluated.
# ============================================================

# ============================================================
# 4a. C1 — MANUAL POPULATION DECLINE
#
# Only Observed, Estimated and Projected trends are eligible.
#
# For each species, retain:
#   - maximum 1-generation decline
#   - maximum 2-generation decline
#   - maximum 3-generation decline
#   - method associated with each generation window
#
# IMPORTANT:
# The ActualDecline_* fields are taken from the SAME original
# populationdeclines.csv row that produced the maximum scaled
# decline for that generation window.
# ============================================================

c1_trends <- manual_trends %>%
  filter(C1Eligible) %>%
  group_by(EnglishName) %>%
  summarise(
    
    # --------------------------------------------------------
    # SCALED / EXTRAPOLATED C1 VALUES
    # --------------------------------------------------------
    
    C1_1GEN_Decline = max(C1_1GEN_Decline, na.rm = TRUE),
    C1_2GEN_Decline = max(C1_2GEN_Decline, na.rm = TRUE),
    C1_3GEN_Decline = max(C1_3GEN_Decline, na.rm = TRUE),
    
    C1_1GEN_DeclineMean = max(C1_1GEN_DeclineMean, na.rm = TRUE),
    C1_2GEN_DeclineMean = max(C1_2GEN_DeclineMean, na.rm = TRUE),
    C1_3GEN_DeclineMean = max(C1_3GEN_DeclineMean, na.rm = TRUE),
    
    C1_1GEN_DeclineLci = max(C1_1GEN_DeclineLci, na.rm = TRUE),
    C1_2GEN_DeclineLci = max(C1_2GEN_DeclineLci, na.rm = TRUE),
    C1_3GEN_DeclineLci = max(C1_3GEN_DeclineLci, na.rm = TRUE),
    
    # --------------------------------------------------------
    # METHOD ASSOCIATED WITH EACH MAXIMUM
    # --------------------------------------------------------
    
    C1_1GEN_Method =
      Method[which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))],
    
    C1_2GEN_Method =
      Method[which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))],
    
    C1_3GEN_Method =
      Method[which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))],
    
    # --------------------------------------------------------
    # ORIGINAL ACTUAL TREND ASSOCIATED WITH EACH MAXIMUM
    # --------------------------------------------------------
    
    ActualDecline_1GEN =
      ActualDecline[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineMean_1GEN =
      ActualDeclineMean[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineLci_1GEN =
      ActualDeclineLci[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineStartYear_1GEN =
      ActualDeclineStartYear[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineEndYear_1GEN =
      ActualDeclineEndYear[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineYears_1GEN =
      ActualDeclineYears[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    ActualDeclineMethod_1GEN =
      ActualDeclineMethod[
        which.max(replace(C1_1GEN_Decline, is.na(C1_1GEN_Decline), -Inf))
      ],
    
    
    ActualDecline_2GEN =
      ActualDecline[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineMean_2GEN =
      ActualDeclineMean[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineLci_2GEN =
      ActualDeclineLci[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineStartYear_2GEN =
      ActualDeclineStartYear[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineEndYear_2GEN =
      ActualDeclineEndYear[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineYears_2GEN =
      ActualDeclineYears[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    ActualDeclineMethod_2GEN =
      ActualDeclineMethod[
        which.max(replace(C1_2GEN_Decline, is.na(C1_2GEN_Decline), -Inf))
      ],
    
    
    ActualDecline_3GEN =
      ActualDecline[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineMean_3GEN =
      ActualDeclineMean[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineLci_3GEN =
      ActualDeclineLci[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineStartYear_3GEN =
      ActualDeclineStartYear[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineEndYear_3GEN =
      ActualDeclineEndYear[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineYears_3GEN =
      ActualDeclineYears[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    ActualDeclineMethod_3GEN =
      ActualDeclineMethod[
        which.max(replace(C1_3GEN_Decline, is.na(C1_3GEN_Decline), -Inf))
      ],
    
    .groups = "drop"
  ) %>%
  mutate(
    C1_1GEN_Decline = ifelse(is.infinite(C1_1GEN_Decline), NA_real_, C1_1GEN_Decline),
    C1_2GEN_Decline = ifelse(is.infinite(C1_2GEN_Decline), NA_real_, C1_2GEN_Decline),
    C1_3GEN_Decline = ifelse(is.infinite(C1_3GEN_Decline), NA_real_, C1_3GEN_Decline),
    
    C1_1GEN_DeclineMean = ifelse(is.infinite(C1_1GEN_DeclineMean), NA_real_, C1_1GEN_DeclineMean),
    C1_2GEN_DeclineMean = ifelse(is.infinite(C1_2GEN_DeclineMean), NA_real_, C1_2GEN_DeclineMean),
    C1_3GEN_DeclineMean = ifelse(is.infinite(C1_3GEN_DeclineMean), NA_real_, C1_3GEN_DeclineMean),
    
    C1_1GEN_DeclineLci = ifelse(is.infinite(C1_1GEN_DeclineLci), NA_real_, C1_1GEN_DeclineLci),
    C1_2GEN_DeclineLci = ifelse(is.infinite(C1_2GEN_DeclineLci), NA_real_, C1_2GEN_DeclineLci),
    C1_3GEN_DeclineLci = ifelse(is.infinite(C1_3GEN_DeclineLci), NA_real_, C1_3GEN_DeclineLci)
  )

# ============================================================
# 4b. C2 — MANUAL CONTINUING DECLINE
#
# Eligible methods:
# Observed, Estimated, Inferred, Projected
#
# The trend must extend to latestYear or beyond.
# Suspected is NOT eligible.
# ============================================================

c2_manual <- manual_trends %>%
  filter(
    C2Eligible,
    !is.na(EndYear),
    as.numeric(EndYear) >= latestYear,
    !is.na(DeclinePercent),
    DeclinePercent > 0
  ) %>%
  distinct(EnglishName, .keep_all = TRUE) %>%
  transmute(
    EnglishName,
    ContinuingDecline = TRUE,
    Method
  )

# ============================================================
# 4c. C2 — SOIB CONTINUING DECLINE
#
# SoIB trends are NOT used for C1.
#
# For C2, a negative currentsloperci indicates continuing
# decline. SoIB trends may represent current or future
# projected trends, so no EndYear filter is required here.
# ============================================================

c2_soib <- soib_main %>%
  filter(
    !is.na(currentsloperci),
    currentsloperci < 0
  ) %>%
  distinct(EnglishName) %>%
  mutate(
    ContinuingDecline = TRUE,
    Method = "Inferred"
  )


# ============================================================
# 4d. COMBINE MANUAL AND SOIB CONTINUING DECLINE
# ============================================================

# ============================================================
# 4d. COMBINE MANUAL AND SOIB CONTINUING DECLINE
# ============================================================

c2_trends <- bind_rows(
  c2_manual,
  c2_soib
) %>%
  distinct(EnglishName, .keep_all = TRUE) %>%
  rename(
    ContinuingDeclineMethod = Method
  )

# ============================================================
# 4e. ADD TREND RESULTS TO POPULATION DATA
# ============================================================

criteriaC_data <- criteriaC_data %>%
  left_join(
    c1_trends,
    by = "EnglishName"
  ) %>%
  left_join(
    c2_trends,
    by = "EnglishName"
  ) %>%
  mutate(
    ContinuingDecline = coalesce(
      ContinuingDecline,
      FALSE
    )
  )

# ============================================================
# 5. EVALUATE CRITERION C CONDITIONS
# ============================================================

criteriaC_data <- criteriaC_data %>%
  mutate(
    
    # -------------------------
    # Population thresholds
    # -------------------------
    
    CR_pop = MaxMaturePop < 250,
    EN_pop = MaxMaturePop < 2500,
    VU_pop = MaxMaturePop < 10000,
    
    
    # -------------------------
    # C1 – Decline thresholds
    # -------------------------
    
    CR_C1 =
      !is.na(C1_1GEN_Decline) &
      C1_1GEN_Decline >= 25,
    
    EN_C1 =
      !is.na(C1_2GEN_Decline) &
      C1_2GEN_Decline >= 20,
    
    VU_C1 =
      !is.na(C1_3GEN_Decline) &
      C1_3GEN_Decline >= 10,
    
    # --------------------------------------------------------
    # Highest C1 category determines C1 method
    # --------------------------------------------------------
    
    C1Method = case_when(
      CR_C1 ~ C1_1GEN_Method,
      EN_C1 ~ C1_2GEN_Method,
      VU_C1 ~ C1_3GEN_Method,
      TRUE ~ NA_character_
    ),
  
    
    # --------------------------------------------------------
    # ACTUAL ORIGINAL TREND USED FOR FINAL C1 CATEGORY
    #
    # This is NOT the scaled/extrapolated value.
    # It is the original value from populationdeclines.csv
    # corresponding to the trend that generated the highest
    # qualifying C1 category.
    # --------------------------------------------------------
    
    ActualDecline = case_when(
      CR_C1 ~ ActualDecline_1GEN,
      EN_C1 ~ ActualDecline_2GEN,
      VU_C1 ~ ActualDecline_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineMean = case_when(
      CR_C1 ~ ActualDeclineMean_1GEN,
      EN_C1 ~ ActualDeclineMean_2GEN,
      VU_C1 ~ ActualDeclineMean_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineLci = case_when(
      CR_C1 ~ ActualDeclineLci_1GEN,
      EN_C1 ~ ActualDeclineLci_2GEN,
      VU_C1 ~ ActualDeclineLci_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineStartYear = case_when(
      CR_C1 ~ ActualDeclineStartYear_1GEN,
      EN_C1 ~ ActualDeclineStartYear_2GEN,
      VU_C1 ~ ActualDeclineStartYear_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineEndYear = case_when(
      CR_C1 ~ ActualDeclineEndYear_1GEN,
      EN_C1 ~ ActualDeclineEndYear_2GEN,
      VU_C1 ~ ActualDeclineEndYear_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineYears = case_when(
      CR_C1 ~ ActualDeclineYears_1GEN,
      EN_C1 ~ ActualDeclineYears_2GEN,
      VU_C1 ~ ActualDeclineYears_3GEN,
      TRUE ~ NA_real_
    ),
    
    ActualDeclineMethod = case_when(
      CR_C1 ~ ActualDeclineMethod_1GEN,
      EN_C1 ~ ActualDeclineMethod_2GEN,
      VU_C1 ~ ActualDeclineMethod_3GEN,
      TRUE ~ NA_character_
    ),
    
    # -------------------------
    # C2a(i) – Largest subpopulation size
    # -------------------------
    
    CR_C2ai = LargestSubPopMature <= 50,
    EN_C2ai = LargestSubPopMature <= 250,
    VU_C2ai = LargestSubPopMature <= 1000,
    
    
    # -------------------------
    # C2a(ii) – Population concentration
    # -------------------------
    
    CR_C2aii = LargestSubPopPercent >= 90,
    EN_C2aii = LargestSubPopPercent >= 95,
    VU_C2aii = LargestSubPopPercent == 100,
    
    
    # -------------------------
    # C2b – Extreme fluctuations
    # -------------------------
    
    CR_C2b = ExtremeFluctuation,
    EN_C2b = ExtremeFluctuation,
    VU_C2b = ExtremeFluctuation,
    
    
    # -------------------------
    # C2
    # -------------------------
    
    CR_C2 =
      CR_C2ai |
      CR_C2aii |
      CR_C2b,
    
    EN_C2 =
      EN_C2ai |
      EN_C2aii |
      EN_C2b,
    
    VU_C2 =
      VU_C2ai |
      VU_C2aii |
      VU_C2b,
    
    
    # -------------------------
    # Final Criterion C tests
    # -------------------------
    
    CR_met =
      CR_pop &
      ContinuingDecline &
      (CR_C1 | CR_C2),
    
    EN_met =
      EN_pop &
      ContinuingDecline &
      (EN_C1 | EN_C2),
    
    VU_met =
      VU_pop &
      ContinuingDecline &
      (VU_C1 | VU_C2)
  )

# ============================================================
# 6. COLLECT CRITERION TRIGGERS
# ============================================================

criteriaC_data <- criteriaC_data %>%
  rowwise() %>%
  mutate(
    CR_triggers = paste(
      na.omit(c(
        ifelse(CR_met & CR_C1, "C1", NA),
        ifelse(CR_met & CR_C2ai, "C2a(i)", NA),
        ifelse(CR_met & CR_C2aii, "C2a(ii)", NA),
        ifelse(CR_met & CR_C2b, "C2b", NA)        
      )),
      collapse = "+"
    ),
    
    EN_triggers = paste(
      na.omit(c(
        ifelse(EN_met & EN_C1, "C1", NA),
        ifelse(EN_met & EN_C2ai, "C2a(i)", NA),
        ifelse(EN_met & EN_C2aii, "C2a(ii)", NA),
        ifelse(EN_met & EN_C2b, "C2b", NA)
      )),
      collapse = "+"
    ),

    VU_triggers = paste(
      na.omit(c(
        ifelse(VU_met & VU_C1, "C1", NA),
        ifelse(VU_met & VU_C2ai, "C2a(i)", NA),
        ifelse(VU_met & VU_C2aii, "C2a(ii)", NA),
        ifelse(VU_met & VU_C2b, "C2b", NA)
      )),
      collapse = "+"
    )
    
  ) %>%
  ungroup()

criteriaC_data <- criteriaC_data %>%
  mutate(
    CR_triggers = na_if(CR_triggers, ""),
    EN_triggers = na_if(EN_triggers, ""),
    VU_triggers = na_if(VU_triggers, "")
  )

# ============================================================
# 7. ASSIGN FINAL CRITERION C CATEGORY
# ============================================================

  criteriaC_data <- criteriaC_data %>%
  mutate(
    
    CriteriaC_Category = case_when(
      CR_met ~ "CR",
      EN_met ~ "EN",
      VU_met ~ "VU",
      TRUE ~ NA_character_
    ),
    
    CriteriaC_String = case_when(
      CriteriaC_Category == "CR" ~ CR_triggers,
      CriteriaC_Category == "EN" ~ EN_triggers,
      CriteriaC_Category == "VU" ~ VU_triggers,
      TRUE ~ NA_character_
    )
    
  )

  
  # ============================================================
  # 8. NEAR THREATENED CALCULATION (BLI-ALIGNED)
  # ============================================================
  
  criteriaC_data <- criteriaC_data %>%
    mutate(
      
      # -------------------------
      # Population threshold
      # -------------------------
      
      Pop_Met = !is.na(MaxMaturePop) & MaxMaturePop < 10000,
      
      Pop_Near =
        (!is.na(BestMaturePop) & (BestMaturePop <= 15000)) |
        (!is.na(MinMaturePop) & (MinMaturePop < 10000)),
      
      
      # -------------------------
      # Continuing decline
      # -------------------------
      
      Decline_Met = ContinuingDecline,
      
      Decline_Near = ContinuingDecline,

      
      # -------------------------
      # Near subcriteria
      # -------------------------

      Near_C1 =
        !is.na(C1_3GEN_Decline) &
        C1_3GEN_Decline >= 8 &
        C1_3GEN_Decline < 10,            

      Near_C2ai =
        !is.na(LargestSubPopMature) &
        LargestSubPopMature < 1500,
      
      Near_C2aii =
        !is.na(LargestSubPopPercent) &
        LargestSubPopPercent >= 90,
      
      Near_C2b = ExtremeFluctuation
      
      
    )    
  
  # Collect NT triggers
  criteriaC_data <- criteriaC_data %>%
    rowwise() %>%
    mutate(
      NT_triggers = paste(
        na.omit(c(
          if (Near_C1) "C1",
          if (Near_C2ai) "C2a(i)",
          if (Near_C2aii) "C2a(ii)",
          if (isTRUE(as.logical(Near_C2b))) "C2b"
        )),
        collapse = "+"
      )
    ) %>%
    ungroup()
  
  # Assign NT category
  criteriaC_data <- criteriaC_data %>%
    mutate(
      
      CriteriaC_Category = case_when(
        
        # Keep existing CR/EN/VU
        !is.na(CriteriaC_Category) ~ CriteriaC_Category,
        
        # NT logic (BLI aligned)
        (Pop_Met | Pop_Near) &
          (Decline_Met | Decline_Near) &
          (Near_C1 | Near_C2ai | Near_C2aii | Near_C2b) ~ "NT",
        
        TRUE ~ NA_character_
      ),
      
      MainCriteriaC = case_when(
        CriteriaC_Category == "NT" ~ NT_triggers,
        TRUE ~ CriteriaC_String
      )
      
    )  
# ============================================================
# 9. IMPOSSIBILITY CHECKS
# ============================================================

# CR assigned but population too large
CR_population_error <- criteriaC_data %>%
  filter(CriteriaC_Category == "CR" & MaxMaturePop >= 250)

# EN assigned but population too large
EN_population_error <- criteriaC_data %>%
  filter(CriteriaC_Category == "EN" & MaxMaturePop >= 2500)

# VU assigned but population too large
VU_population_error <- criteriaC_data %>%
  filter(CriteriaC_Category == "VU" & MaxMaturePop >= 10000)

# Impossible subpopulation percentages
subpop_percent_error <- criteriaC_data %>%
  filter(LargestSubPopPercent > 100)

# ============================================================
# 10. FINAL OUTPUT TABLE
# ============================================================

criteriaC_output <- criteriaC_data %>%
  select(
    
    # --------------------------------------------------------
    # SPECIES
    # --------------------------------------------------------
    EnglishName,
    
    # --------------------------------------------------------
    # FINAL OUTPUT
    # --------------------------------------------------------
    CriteriaC_Category,
    CriteriaC_String,
    
    # --------------------------------------------------------
    # POPULATION SIZE (CORE OF CRITERION C)
    # --------------------------------------------------------
    MinMaturePop,
    MaxMaturePop,
    BestMaturePop,
    
    # Raw inputs (useful for audit)
    MinPop,
    MaxPop,
    `Min Breeding Pop Percent`,
    `Max Breeding Pop Percent`,
    `Male Ratio`,
    
    # --------------------------------------------------------
    # SUBPOPULATION STRUCTURE (C2)
    # --------------------------------------------------------
    `Largest SubPop` = LargestSubPopMature,
    LargestSubPopPercent,
    
    # --------------------------------------------------------
    # GENERATION LENGTH WINDOWS
    # --------------------------------------------------------
    Years1GEN,
    Years2GEN,
    Years3GEN,
    
    # --------------------------------------------------------
    # DECLINE METRICS (C1)
    # --------------------------------------------------------
    
    C1_1GEN_Decline,
    C1_2GEN_Decline,
    C1_3GEN_Decline,
    C1Method,
    
    ContinuingDecline,
    ContinuingDeclineMethod,
    
    # --------------------------------------------------------
    # EXTREME FLUCTUATIONS (C2b)
    # --------------------------------------------------------
    ExtremeFluctuation,
    
    ActualDecline,
    ActualDeclineMean,
    ActualDeclineLci,
    ActualDeclineStartYear,
    ActualDeclineEndYear,
    ActualDeclineYears,
    ActualDeclineMethod,
    # --------------------------------------------------------
    # THRESHOLD FLAGS (VERY IMPORTANT)
    # --------------------------------------------------------
    CR_pop,
    EN_pop,
    VU_pop,
    
    CR_C1,
    EN_C1,
    VU_C1,
    
    CR_C2ai,
    EN_C2ai,
    VU_C2ai,
    
    CR_C2aii,
    EN_C2aii,
    VU_C2aii,
    
    CR_C2b,
    EN_C2b,
    VU_C2b,
    
    # --------------------------------------------------------
    # TRIGGER SUMMARIES
    # --------------------------------------------------------
    CR_triggers,
    EN_triggers,
    VU_triggers,
    
    # --------------------------------------------------------
    # NEAR-THREATENED SUPPORT (VERY USEFUL)
    # --------------------------------------------------------
    Pop_Met,
    Pop_Near,
    
    Decline_Met,
    Decline_Near,
    
    Near_C1,
    Near_C2ai,
    Near_C2aii,
    Near_C2b,
    
    NT_triggers,
    
  )

# ============================================================
# 11. WRITE OUTPUT FILE
# ============================================================

write_csv(
  criteriaC_output,
  criteriaCResultsfile
)

# ============================================================
# END OF SCRIPT
# ============================================================