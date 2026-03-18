library(dplyr)
library(readr)

latestYear <- 2024 #Obtain this from a configuration 

# ============================================================
# 1. READ BASIC EOO/AOO DATA & PREPARE. ALWAYS AVAILABLE
# ============================================================
basiceooaoo <- read_csv(eooaoofile) %>%
  mutate(
    EnglishName = trimws(Species)
  ) %>%
  select(
    EnglishName,
    MinAOO,
    MaxAOO,
    MinEstimate_2km, # Number of 2x2 km grids
    LikelyEOO,
    MaxEOO,
    EOOYearBandChange,
    EOOChange,
    EOOChangePercent
  ) %>%
  mutate (
    MinAOO = as.integer(round(MinAOO,0)),
    MaxAOO = as.integer(round(MaxAOO,0)),
    LikelyEOO = as.integer(round(LikelyEOO,0)),
    MaxEOO = as.integer(round(MaxEOO,0))
  )


# ============================================================
# 2. PREPARE POPULATION DECLINE DATA. ALWAYS AVAILABLE
# ============================================================
soib_decline <- read_csv(soibmainfile) %>%
  mutate(
    EnglishName = trimws(eBird.English.Name.2024)
  ) %>%
    select(
      EnglishName,
      Current.Analysis,
      currentsloperci,
      currentslopemean,
      currentslopelci,
      mean5km,
      SoIB.Latest.Current.Status
    )
  
# ============================================================
# 3. PREPARE OTHER DECLINE DATA IF AVAILABLE. THIS WILL MOSTLY NOT BE AVAILABLE AND HENCE IS OPTIONAL
# ============================================================
ContinuingDeclineExt <- if (file.exists(continuingdeclineexfile)) {
  read_csv(continuingdeclineexfile) %>%  
    mutate(
      EnglishName = trimws(Species)
    ) %>% 
    select (
      EnglishName,
      AOOYearBandChange,	
      AOOChange,	
      AOOChangePrecent,	
      AOHYearBandChange,	#Area of Habitat
      AOHChange,	
      AOHPrecent,	
      EOHYearBandChange,	#Extent of Habitat
      EOHChange,	
      EOHPercent,	
      NoOfLocationYearBandChange,	
      NoOfLocationsChange,	
      NoOfSubPopYearBandChange,	
      NoOfSubPopulationsChange    )
} else {
  tibble()
}

# ============================================================
# 4. PREPARE SEVERELY FRAGMENTED RANGE SPECIES IF AVAILABLE
# ============================================================
SeverelyFragmented <- if (file.exists(severelyfragmentedfile)) {
  read_csv(severelyfragmentedfile) %>%  
    mutate(
      EnglishName = trimws(Species)
    ) %>% 
    select (
      EnglishName,
      SeverelyFragmented)
} else {
  tibble()
}

# ============================================================
# 5. PREPARE LOCATIONS FOR SPECIES IF AVAILABLE
# ============================================================
NoOfLocations <- if (file.exists(nooflocationsfile)) {
  read_csv(nooflocationsfile) %>%  
    mutate(
      EnglishName = trimws(Species)
    ) %>% 
    select (
      EnglishName,
      Locations)
} else {
  tibble()
}

# ============================================================
# 6. EXTREME FLUCTUATIONS KNOWN FOR SPECIES IF AVAILABLE
# ============================================================
fluctuations <- if (file.exists(extremefluctationsfile)) {
  read_csv(extremefluctationsfile) %>%  
    mutate(
      EnglishName = trimws(Species)
    ) %>% 
    select (
      EnglishName,
      ExtremeFluctuationsinEOO,
      ExtremeFluctuationsinAOO,
      ExtremeFluctuationsinNoOfLocations,
      ExtremeFluctuationsinNoOfSubPopulations,
      ExtremeFluctuationsinNoOfMatureIndividuals
    )
} else {
  tibble()
}


# ============================================================
# 7. JOIN DATA
# ============================================================

criteriaB_data <- basiceooaoo %>%
                    left_join(soib_decline, by = "EnglishName") %>%
                    left_join(ContinuingDeclineExt, by = "EnglishName") %>%
                    left_join(SeverelyFragmented, by = "EnglishName") %>%
                    left_join(NoOfLocations, by = "EnglishName") %>%
                    left_join(fluctuations, by = "EnglishName")
  

# ============================================================
# 8. SUBCRITERIA CONDITIONS (a, b, c)
# ============================================================

criteriaB_data <- criteriaB_data %>%
  mutate(
    
    # ============================================================
    # (a) — CATEGORY-SPECIFIC (HIERARCHICAL)
    # ============================================================
    
    # severely fragmented overrides everything
    a_severely_fragmented =
      !is.na(SeverelyFragmented) & SeverelyFragmented == TRUE,
    
    # location thresholds (only if NOT severely fragmented)
    a_CR_locations =
      !a_severely_fragmented &
      !is.na(Locations) & Locations <= 1,
    
    a_EN_locations =
      !a_severely_fragmented &
      !is.na(Locations) & Locations <= 5,
    
    a_VU_locations =
      !a_severely_fragmented &
      !is.na(Locations) & Locations <= 10,
    
    # final hierarchical assignment
    a_level = case_when(
      a_severely_fragmented ~ "CR",   # qualifies for all, assign highest
      a_CR_locations ~ "CR",
      a_EN_locations ~ "EN",
      a_VU_locations ~ "VU",
      TRUE ~ NA_character_
    ),
    
    Cond_a = !is.na(a_level),
    
    
    # ============================================================
    # (b) CONTINUING DECLINE — FULL WITH YEAR FILTER
    # ============================================================
    
    # b(i) EOO
    b_i =
      !is.na(EOOChange) &
      EOOChange < 0 &
      !is.na(EOOYearBandChange) &
      grepl(latestYear, EOOYearBandChange),
    
    # b(ii) AOO
    b_ii =
      !is.na(AOOChange) &
      AOOChange < 0 &
      !is.na(AOOYearBandChange) &
      grepl(latestYear, AOOYearBandChange),
    
    # b(iii) Habitat (AOH / EOH — combined)
    b_iii =
      (
        !is.na(AOHChange) &
          AOHChange < 0 &
          !is.na(AOHYearBandChange) &
          grepl(latestYear, AOHYearBandChange)
      ) |
      (
        !is.na(EOHChange) &
          EOHChange < 0 &
          !is.na(EOHYearBandChange) &
          grepl(latestYear, EOHYearBandChange)
      ),
    
    # b(iv) Locations / subpopulations
    b_iv =
      (
        !is.na(NoOfLocationsChange) &
          NoOfLocationsChange < 0 &
          !is.na(NoOfLocationYearBandChange) &
          grepl(latestYear, NoOfLocationYearBandChange)
      ) |
      (
        !is.na(NoOfSubPopulationsChange) &
          NoOfSubPopulationsChange < 0 &
          !is.na(NoOfSubPopYearBandChange) &
          grepl(latestYear, NoOfSubPopYearBandChange)
      ),
    
    # b(v) Mature individuals (SoIB proxy)
    b_v =
      !is.na(Current.Analysis) &
      Current.Analysis == "X" &
      !is.na(currentsloperci) &
      currentsloperci < 0 &
      !is.na(mean5km) &
      mean5km > 8 &
      !is.na(currentslopemean) &
      !is.na(currentslopelci) &
      1.96 * abs(currentslopemean) >
      abs(currentsloperci - currentslopelci) &
      (SoIB.Latest.Current.Status %in%
         c("Stable","Decline","Rapid Decline")),
    
    Cond_b = b_i | b_ii | b_iii | b_iv | b_v,
    
    
    # ============================================================
    # (c) EXTREME FLUCTUATIONS (UNCHANGED STRUCTURE)
    # ============================================================
    
    c_i  = !is.na(ExtremeFluctuationsinEOO) & ExtremeFluctuationsinEOO == TRUE,
    
    c_ii = !is.na(ExtremeFluctuationsinAOO) & ExtremeFluctuationsinAOO == TRUE,
    
    c_iii =
      (!is.na(ExtremeFluctuationsinNoOfLocations) & ExtremeFluctuationsinNoOfLocations == TRUE) |
      (!is.na(ExtremeFluctuationsinNoOfSubPopulations) & ExtremeFluctuationsinNoOfSubPopulations == TRUE),
    
    c_iv =
      !is.na(ExtremeFluctuationsinNoOfMatureIndividuals) &
      ExtremeFluctuationsinNoOfMatureIndividuals == TRUE,
    
    Cond_c = c_i | c_ii | c_iii | c_iv,
    
    
    # ============================================================
    # FINAL SUBCRITERIA COUNT (ONLY a / b / c)
    # ============================================================
    
    SubcriteriaCount =
      rowSums(cbind(Cond_a, Cond_b, Cond_c), na.rm = TRUE)
  )

# ============================================================
# 9. RANGE THRESHOLDS
# ============================================================

criteriaB_data <- criteriaB_data %>%
  mutate(
    
    # ----------------------------
    # B1: EOO thresholds
    # ----------------------------
    CR_B1 = !is.na(LikelyEOO) & LikelyEOO < 100,
    
    EN_B1 = !is.na(LikelyEOO) & LikelyEOO < 5000,
    
    VU_B1 = !is.na(LikelyEOO) & LikelyEOO < 20000,
    
    NT_B1 =
      (!is.na(LikelyEOO) & LikelyEOO < 30000) |
      (!is.na(MaxEOO) & MaxEOO < 22000),
    
    
    # ----------------------------
    # B2: AOO thresholds
    # ----------------------------
    CR_B2 = !is.na(MinAOO) & MinAOO < 10,
    
    EN_B2 = !is.na(MinAOO) & MinAOO < 500,
    
    VU_B2 = !is.na(MinAOO) & MinAOO < 2000,
    
    NT_B2 =
      (!is.na(MinAOO) & MinAOO < 3000) |
      (!is.na(MaxAOO) & MaxAOO < 2200)
    
  )

# ============================================================
# 10. FINAL CATEGORY (STRICT IUCN + NT)
# ============================================================

criteriaB_data <- criteriaB_data %>%
  mutate(
    
    # ============================================================
    # (a) VALIDITY PER LEVEL
    # ============================================================
    
    a_CR = a_level == "CR",
    a_EN = a_level %in% c("CR","EN"),
    a_VU = a_level %in% c("CR","EN","VU"),
    
    
    # ============================================================
    # COUNT PER LEVEL (a + b + c)
    # ============================================================
    
    count_CR = rowSums(cbind(a_CR, Cond_b, Cond_c), na.rm = TRUE),
    count_EN = rowSums(cbind(a_EN, Cond_b, Cond_c), na.rm = TRUE),
    count_VU = rowSums(cbind(a_VU, Cond_b, Cond_c), na.rm = TRUE),
    
    
    # ============================================================
    # QUALIFICATION PER LEVEL
    # ============================================================
    
    CR_qual = (CR_B1 | CR_B2) & count_CR >= 2,
    EN_qual = (EN_B1 | EN_B2) & count_EN >= 2,
    VU_qual = (VU_B1 | VU_B2) & count_VU >= 2,
    
    
    # ============================================================
    # NT LOGIC (BIRDLIFE STYLE)
    # ============================================================
    
    NT_from_failed =
      (CR_B1 | CR_B2 | EN_B1 | EN_B2 | VU_B1 | VU_B2) &
      !(CR_qual | EN_qual | VU_qual) &
      SubcriteriaCount >= 1,
    
    NT_from_range =
      (NT_B1 | NT_B2) &
      SubcriteriaCount >= 1,    
    
    # ============================================================
    # FINAL CATEGORY (HIERARCHICAL)
    # ============================================================
    
    CriteriaB_Category = case_when(
      CR_qual ~ "CR",
      EN_qual ~ "EN",
      VU_qual ~ "VU",
      NT_from_failed ~ "NT",
      NT_from_range ~ "NT",
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 11. BUILD IUCN STRING (FULL FORMAT)
# ============================================================

criteriaB_data <- criteriaB_data %>%
  rowwise() %>%
  mutate(
    
    # ============================================================
    # SELECT LEVEL-SPECIFIC a
    # ============================================================
    
    a_used = case_when(
      CriteriaB_Category == "CR" ~ a_CR,
      CriteriaB_Category == "EN" ~ a_EN,
      CriteriaB_Category == "VU" ~ a_VU,
      CriteriaB_Category == "NT" ~ Cond_a,
      TRUE ~ FALSE
    ),
    
    a_string = ifelse(a_used, "a", NA),
    
    
    # ============================================================
    # (b) SUBPARTS
    # ============================================================
    
    b_parts = paste(na.omit(c(
      ifelse(b_i, "i", NA),
      ifelse(b_ii, "ii", NA),
      ifelse(b_iii, "iii", NA),
      ifelse(b_iv, "iv", NA),
      ifelse(b_v, "v", NA)
    )), collapse = ","),
    
    b_string = ifelse(Cond_b,
                      paste0("b(", b_parts, ")"),
                      NA),
    
    
    # ============================================================
    # (c) SUBPARTS
    # ============================================================
    
    c_parts = paste(na.omit(c(
      ifelse(c_i, "i", NA),
      ifelse(c_ii, "ii", NA),
      ifelse(c_iii, "iii", NA),
      ifelse(c_iv, "iv", NA)
    )), collapse = ","),
    
    c_string = ifelse(Cond_c,
                      paste0("c(", c_parts, ")"),
                      NA),
    
    
    # ============================================================
    # COMBINE a b c
    # ============================================================
    
    subcriteria_string =
      paste(na.omit(c(a_string, b_string, c_string)), collapse = ""),
    
    subcriteria_string =
      ifelse(subcriteria_string == "", NA, subcriteria_string),
    
    # ============================================================
    # B1 / B2 — LEVEL AWARE
    # ============================================================
    
    B1_used = case_when(
      CriteriaB_Category == "CR" ~ CR_B1,
      CriteriaB_Category == "EN" ~ EN_B1,
      CriteriaB_Category == "VU" ~ VU_B1,
      CriteriaB_Category == "NT" ~ NT_B1,
      TRUE ~ FALSE
    ),
    
    B2_used = case_when(
      CriteriaB_Category == "CR" ~ CR_B2,
      CriteriaB_Category == "EN" ~ EN_B2,
      CriteriaB_Category == "VU" ~ VU_B2,
      CriteriaB_Category == "NT" ~ NT_B2,
      TRUE ~ FALSE
    ),
    
    B1_string = ifelse(B1_used & !is.na(subcriteria_string),
                       paste0("B1", subcriteria_string),
                       NA),
    
    B2_string = ifelse(B2_used & !is.na(subcriteria_string),
                       paste0("B2", subcriteria_string),
                       NA),    
    CriteriaB_String =
      ifelse(
        is.na(CriteriaB_Category),
        NA_character_,
        paste(na.omit(c(B1_string, B2_string)), collapse = "+")
      )    
  ) %>%
  ungroup()

# ============================================================
# 12. FINAL OUTPUT TABLE
# ============================================================

criteriaB_output <- criteriaB_data %>%
  select(
    
    # --------------------------------------------------------
    # SPECIES
    # --------------------------------------------------------
    EnglishName,
    
    # Final Red List result
    CriteriaB_Category,
    CriteriaB_String,
    
    # --------------------------------------------------------
    # RANGE METRICS (CORE OF CRITERION B)
    # --------------------------------------------------------
    MinAOO,
    MaxAOO,
    LikelyEOO,
    MaxEOO,
    
    # --------------------------------------------------------
    # RANGE CHANGE / TREND (SUPPORTING b)
    # --------------------------------------------------------
    EOOChange,
    EOOChangePercent,
    EOOYearBandChange,
    AOOChange,
    AOOChangePrecent,
    
    # Optional habitat proxies (keep if useful)
    AOHChange,
    AOHPrecent,
    EOHChange,
    EOHPercent,
    
    # --------------------------------------------------------
    # STRUCTURE (a: fragmentation / locations)
    # --------------------------------------------------------
    SeverelyFragmented,
    Locations,
    
    a_severely_fragmented,
    a_CR_locations,
    a_EN_locations,
    a_VU_locations,
    a_level,
    Cond_a,
    
    # --------------------------------------------------------
    # CONTINUING DECLINE (b)
    # --------------------------------------------------------
    b_i,
    b_ii,
    b_iii,
    b_iv,
    b_v,
    Cond_b,
    
    # --------------------------------------------------------
    # EXTREME FLUCTUATIONS (c)
    # --------------------------------------------------------
    c_i,
    c_ii,
    c_iii,
    c_iv,
    Cond_c,
    
    # --------------------------------------------------------
    # SUBCRITERIA SUMMARY
    # --------------------------------------------------------
    SubcriteriaCount,
    subcriteria_string,
    
    a_string,
    b_string,
    c_string,
    
    # --------------------------------------------------------
    # THRESHOLD FLAGS (TRANSPARENCY)
    # --------------------------------------------------------
    CR_B1,
    EN_B1,
    VU_B1,
    NT_B1,
    
    CR_B2,
    EN_B2,
    VU_B2,
    NT_B2,
    
    # Qualification counts
    count_CR,
    count_EN,
    count_VU,
    
    # --------------------------------------------------------
    # FINAL DECISION
    # --------------------------------------------------------
    B1_used,
    B2_used,
    B1_string,
    B2_string,
    
    # --------------------------------------------------------
    # NT FLAGS (VERY IMPORTANT FOR REVIEW)
    # --------------------------------------------------------
    NT_from_failed,
    NT_from_range
  )

# ============================================================
# 13. WRITE OUTPUT
# ============================================================

write_csv(
  criteriaB_output,
  criteriaBResultsfile
)