# ============================================================
# CRITERION C RED LIST ASSESSMENT
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# ============================================================
# 1a. READ INPUT DATA
# ============================================================

population_data <- read_csv(populationsfile) %>%
  rename(
    Largest_Sub_Pop = `Largest SubPop`
  ) %>%
  mutate(
    EnglishName = trimws(EnglishName)
  )

# ============================================================
# 1b. EXTREME FLUCTUATIONS (ONLY MATURE INDIVIDUALS)
# ============================================================

redlist_trends <- read_csv(soibredlistfile) %>%
  mutate(
    EnglishName = trimws(Species)
  ) %>%
  select(
    EnglishName,
    Years1GEN,
    Years2GEN,
    Years3GEN,
    `1GEN Decline`,
    `2GEN Decline`,
    `3GEN Decline`,
    `1GEN valid`,
    `2GEN valid`,
    `3GEN valid`
  )


# ============================================================
# 1c. EXTREME FLUCTUATIONS (ONLY MATURE INDIVIDUALS)
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
# 3. MERGE POPULATION AND TREND DATA
# ============================================================

criteriaC_data <- population_data %>%
  left_join(redlist_trends, by = "EnglishName") %>%
  left_join(fluctuations, by = "EnglishName") 
  

# ============================================================
# 4. CALCULATE MATURE POPULATION SIZE
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
    
    BestMaturePop =
      ifelse(
        !is.na(MinMaturePop) & !is.na(MaxMaturePop) &
          MinMaturePop > 0 & MaxMaturePop > 0,
        round(sqrt(MinMaturePop * MaxMaturePop), 0),
        NA
      )

  )

# ============================================================
# 5. CALCULATE LARGEST SUBPOPULATION PROPORTION
# ============================================================

criteriaC_data <- criteriaC_data %>%
  mutate(
    
    LargestSubPopPercent =
      ifelse(
        !is.na(BestMaturePop) & BestMaturePop > 0,
        pmin(100, round(100 * Largest_Sub_Pop / BestMaturePop)),
        NA
      )    
    
  )

# ============================================================
# 6. EVALUATE CRITERION C CONDITIONS
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
    # C1 – Decline thresholds (WITH VALIDATION)
    # -------------------------
    
    CR_C1 = `1GEN valid` & !is.na(`1GEN Decline`) & `1GEN Decline` >= 25,
    EN_C1 = `2GEN valid` & !is.na(`2GEN Decline`) & `2GEN Decline` >= 20,
    VU_C1 = `3GEN valid` & !is.na(`3GEN Decline`) & `3GEN Decline` >= 10,    
    
    ContinuingDecline =
      (`3GEN valid` & !is.na(`3GEN Decline`) & `3GEN Decline` > 0) |
      (`2GEN valid` & !is.na(`2GEN Decline`) & `2GEN Decline` > 0) |
      (`1GEN valid` & !is.na(`1GEN Decline`) & `1GEN Decline` > 0),
    
    # -------------------------
    # C2a(i) – Largest subpopulation size
    # -------------------------
    
    CR_C2ai = Largest_Sub_Pop <= 50,
    EN_C2ai = Largest_Sub_Pop <= 250,
    VU_C2ai = Largest_Sub_Pop <= 1000,
    
    
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
    
    CR_C2 = CR_C2ai | CR_C2aii | CR_C2b,
    EN_C2 = EN_C2ai | EN_C2aii | EN_C2b,
    VU_C2 = VU_C2ai | VU_C2aii | VU_C2b,
    
    CR_met = CR_pop & ContinuingDecline & (CR_C1 | CR_C2),
    EN_met = EN_pop & ContinuingDecline & (EN_C1 | EN_C2),
    VU_met = VU_pop & ContinuingDecline & (VU_C1 | VU_C2)
  )

# ============================================================
# 7. COLLECT CRITERION TRIGGERS
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
# 8. ASSIGN FINAL CRITERION C CATEGORY
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
  # 10. NEAR THREATENED CALCULATION (BLI-ALIGNED)
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
        `3GEN valid` &
        !is.na(`3GEN Decline`) &
        `3GEN Decline` >= 8 &
        `3GEN Decline` < 10,
      
      Near_C2ai =
        !is.na(Largest_Sub_Pop) &
        Largest_Sub_Pop < 1500,
      
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
# 10. IMPOSSIBILITY CHECKS
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
# 11. FINAL OUTPUT TABLE
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
    Largest_Sub_Pop,
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
    `1GEN Decline`,
    `2GEN Decline`,
    `3GEN Decline`,
    
    `1GEN valid`,
    `2GEN valid`,
    `3GEN valid`,
    
    ContinuingDecline,
    
    # --------------------------------------------------------
    # EXTREME FLUCTUATIONS (C2b)
    # --------------------------------------------------------
    ExtremeFluctuation,
    
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
# 12. WRITE OUTPUT FILE
# ============================================================

write_csv(
  criteriaC_output,
  criteriaCResultsfile
)

# ============================================================
# END OF SCRIPT
# ============================================================