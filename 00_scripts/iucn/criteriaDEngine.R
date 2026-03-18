# ============================================================
# CRITERION D RED LIST ASSESSMENT (FINAL)
# ============================================================

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

# ============================================================
# 1. READ INPUT DATA
# ============================================================

population_data <- read_csv(populationsfile) %>% 
  mutate(EnglishName = trimws(EnglishName))

# ============================================================
# 2. READ AOO DATA
# ============================================================

basicaoo <- read_csv(eooaoofile) %>%
  mutate(EnglishName = trimws(Species)) %>%
  select(
    EnglishName,
    MaxAOO
  )

# ============================================================
# 3. READ PLAUSIBLE THREAT DATA
# ============================================================
plausiblethreat <- if (file.exists(plausiblethreatfile)) {
  read_csv(plausiblethreatfile) %>%  
  mutate(EnglishName = trimws(Species)) %>%
  select(
    EnglishName,
    TimeToVU,
    TimeToEN,
    TimetoCR
  )
} else {
  tibble(EnglishName = character(), ExtremeFluctuation = logical())
}

# ============================================================
# 4. READ GENERATION LENGTH + SOIB DATA
# ============================================================

threegen <- read.csv(threegenfile)

soib <- read.csv(get_metadata("none")$SOIBMAIN.PATH)

soib <- soib %>% 
  select(
    "India.Checklist.Common.Name",
    "India.Checklist.Scientific.Name",
    "BLI.Scientific.Name",
    "Current.Analysis",
    "currentsloperci",
    "currentslopemean",
    "currentslopelci",
    "mean5km",
    "SoIB.Latest.Current.Status"
  )


# ============================================================
# 5. GENERATION LENGTH FOR ALL SPECIES
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

# Map BLI → EnglishName using SOIB (NO FILTERING)
gen_data <- soib %>%
  select(
    EnglishName = India.Checklist.Common.Name,
    BLI.Scientific.Name
  ) %>%
  inner_join(gen_data, by = "BLI.Scientific.Name") %>%
  mutate(EnglishName = trimws(EnglishName)) %>%
  select(EnglishName, GenerationLength)

# ============================================================
# 6. READ LOCATIONS
# ============================================================

NoOfLocations <- if (file.exists(nooflocationsfile)) {
  read_csv(nooflocationsfile) %>%  
    mutate(EnglishName = trimws(Species)) %>% 
    select(EnglishName, Locations)
} else {
  tibble()
}

# ============================================================
# 7. BUILD MASTER SPECIES LIST (CRITICAL FIX)
# ============================================================

master_species <- bind_rows(
  population_data %>% select(EnglishName),
  plausiblethreat %>% select(EnglishName),
  basicaoo %>% select(EnglishName),
  NoOfLocations %>% select(EnglishName)
) %>%
  distinct() %>%
  mutate(EnglishName = trimws(EnglishName))

# ============================================================
# 7b. JOIN ALL DATA TO MASTER LIST
# ============================================================

criteriaD_data <- master_species %>%
  left_join(population_data, by = "EnglishName") %>%
  left_join(basicaoo, by = "EnglishName") %>%
  left_join(NoOfLocations, by = "EnglishName") %>%
  left_join(plausiblethreat, by = "EnglishName") %>%
  left_join(gen_data, by = "EnglishName")


# ============================================================
# 8. CALCULATE MATURE POPULATION
# ============================================================

criteriaD_data <- criteriaD_data %>%
  mutate(
    
    MinMaturePop =
      MinPop *
      `Min Breeding Pop Percent` *
      ifelse(`Male Ratio` > 0.5, 1 - `Male Ratio`, `Male Ratio`) * 2,
    
    MaxMaturePop =
      MaxPop *
      `Max Breeding Pop Percent` *
      ifelse(`Male Ratio` > 0.5, 1 - `Male Ratio`, `Male Ratio`) * 2
  )

# ============================================================
# 9. DEFINE THREAT WINDOWS USING GENERATION LENGTH
# ============================================================

criteriaD_data <- criteriaD_data %>%
  mutate(
    
    # 1 generation window
    OneGen = GenerationLength,
    
    # 3 generation window
    ThreeGen = 3 * GenerationLength,
    
    PlausibleThreat_VU =
      !is.na(TimeToVU) & TimeToVU <= OneGen,
    
    PlausibleThreat_NT =
      (!is.na(TimeToEN) & TimeToEN <= OneGen) |
      (!is.na(TimeToVU) & TimeToVU <= OneGen) |
      (!is.na(TimetoCR) & TimetoCR <= ThreeGen)
  )

# ============================================================
# 10. CRITERION D THRESHOLDS
# ============================================================

criteriaD_data <- criteriaD_data %>%
  mutate(
    
    CR_D = !is.na(MaxMaturePop) & MaxMaturePop < 50,
    EN_D = !is.na(MaxMaturePop) & MaxMaturePop < 250,
    VU_D1 = !is.na(MaxMaturePop) & MaxMaturePop < 1000,
    
    NT_D1 =
      !is.na(MaxMaturePop) &
      (
        (MaxMaturePop < 1500 & MaxMaturePop >= 1000) |
          (MinMaturePop < 1000 & MaxMaturePop > 1500)
      ),
    
    VU_D2 =
      PlausibleThreat_VU &
      (
        (!is.na(MaxAOO) & MaxAOO < 20) |
          (!is.na(Locations) & Locations <= 5)
      ),
    
    NT_D2 =
      PlausibleThreat_NT &
      (
        (!is.na(MaxAOO) & MaxAOO < 20) |
          (!is.na(Locations) & Locations <= 5)
      )
  )

# ============================================================
# 11. COLLECT TRIGGERS (IUCN FORMAT)
# ============================================================

criteriaD_data <- criteriaD_data %>%
  rowwise() %>%
  mutate(
    
    CR_D_triggers = if(CR_D) "D" else "",
    
    EN_D_triggers = if(EN_D) "D" else "",
    
    VU_D_triggers = paste(
      na.omit(c(
        if(VU_D1) "D1",
        if(VU_D2) "D2"
      )),
      collapse = "+"
    ),
    
    NT_D_triggers = paste(
      na.omit(c(
        if(NT_D1) "D1",
        if(NT_D2) "D2"
      )),
      collapse = "+"
    )
    
  ) %>%
  ungroup()

# ============================================================
# 12. FINAL CATEGORY ASSIGNMENT
# ============================================================

criteriaD_data <- criteriaD_data %>%
  mutate(
    

    CriteriaD_Category = case_when(
      CR_D_triggers != "" ~ "CR",
      EN_D_triggers != "" ~ "EN",
      VU_D_triggers != "" ~ "VU",
      NT_D_triggers != "" & VU_D_triggers == "" ~ "NT",
      TRUE ~ NA_character_
    ),
    
    CriteriaD_String = case_when(
      CriteriaD_Category == "CR" ~ CR_D_triggers,
      CriteriaD_Category == "EN" ~ EN_D_triggers,
      CriteriaD_Category == "VU" ~ VU_D_triggers,
      CriteriaD_Category == "NT" ~ NT_D_triggers,
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 13. FINAL OUTPUT
# ============================================================

criteriaD_output <- criteriaD_data %>%
  select(
    
    # --------------------------------------------------------
    # SPECIES
    # --------------------------------------------------------
    EnglishName,
    
    # --------------------------------------------------------
    # FINAL OUTPUT
    # --------------------------------------------------------
    CriteriaD_Category,
    CriteriaD_String,
    
    # --------------------------------------------------------
    # POPULATION SIZE (D1 CORE)
    # --------------------------------------------------------
    MinMaturePop,
    MaxMaturePop,
    
    # Raw inputs (audit)
    MinPop,
    MaxPop,
    `Min Breeding Pop Percent`,
    `Max Breeding Pop Percent`,
    `Male Ratio`,
    
    # --------------------------------------------------------
    # SUBPOPULATION STRUCTURE (SUPPORTING CONTEXT)
    # --------------------------------------------------------
    `Largest SubPop`,
    
    # --------------------------------------------------------
    # RANGE RESTRICTION (D2 CORE)
    # --------------------------------------------------------
    MaxAOO,
    Locations,
    
    # --------------------------------------------------------
    # GENERATION / TIME WINDOWS (FOR PLAUSIBILITY)
    # --------------------------------------------------------
    GenerationLength,
    OneGen,
    ThreeGen,
    
    TimeToVU,
    TimeToEN,
    TimetoCR,
    
    # --------------------------------------------------------
    # PLAUSIBLE THREATS (CRITICAL FOR D2)
    # --------------------------------------------------------
    PlausibleThreat_VU,
    PlausibleThreat_NT,
    
    # --------------------------------------------------------
    # THRESHOLD FLAGS (TRANSPARENCY)
    # --------------------------------------------------------
    CR_D,
    EN_D,
    VU_D1,
    NT_D1,
    VU_D2,
    NT_D2,
    
    # --------------------------------------------------------
    # TRIGGER SUMMARIES
    # --------------------------------------------------------
    CR_D_triggers,
    EN_D_triggers,
    VU_D_triggers,
    NT_D_triggers
  )

# ============================================================
# 14. WRITE OUTPUT
# ============================================================

write_csv(
  criteriaD_output,
  criteriaDResultsfile
)

# ============================================================
# END
# ============================================================