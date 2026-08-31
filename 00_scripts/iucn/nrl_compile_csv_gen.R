library(dplyr)
library(readr)
library(purrr)
source("00_scripts/iucn/config_iucn.R")

category_map <- c(
  "EX"  = "Extinct",
  "EW"  = "Extinct in Wild",
  "CR(PE)" = "Critically Endangered (Possibly Extinct)",
  "CR"  = "Critically Endangered",
  "EN"  = "Endangered",
  "VU"  = "Vulnerable",
  "NT"  = "Near Threatened",
  "LC"  = "Least Concern",
  "DD"  = "Data Deficient"
)
category_rank <- c(
  "Extinct",
  "Extinct in Wild",
  "Critically Endangered (Possibly Extinct)",
  "Critically Endangered",
  "Endangered",
  "Vulnerable",
  "Near Threatened",
  "Least Concern",
  "Data Deficient",
  "Not Assessed"
)

format_num <- function(x) {
  suppressWarnings({
    
    format_one <- function(s) {
      if (is.na(s) || s == "") return("")
      
      # regex to capture numbers (integers/decimals)
      pattern <- "\\d+\\.?\\d*"
      
      matches <- gregexpr(pattern, s)[[1]]
      
      if (matches[1] == -1) return(s)  # no numbers
      
      nums <- regmatches(s, gregexpr(pattern, s))[[1]]
      
      # format each number
      formatted_nums <- vapply(nums, function(n) {
        num <- as.numeric(n)
        if (is.na(num)) return(n)
        prettyNum(num, big.mark = ",", scientific = FALSE)
      }, character(1))
      
      # replace back into string
      regmatches(s, gregexpr(pattern, s))[[1]] <- formatted_nums
      
      return(s)
    }
    
    vapply(x, format_one, character(1))
  })
}
# ============================================================
# 1. READ INPUT FILES
# ============================================================

criteriaA_results <- read_csv(criteriaAResultsfile)
criteriaB_results <- read_csv(criteriaBResultsfile) 
criteriaC_results <- read_csv(criteriaCResultsfile)
criteriaD_results <- read_csv(criteriaDResultsfile)

# ============================================================
# READ SOIB DATA
# ============================================================

soib_main <- read_csv(get_metadata("none")$SOIBMAIN.PATH) %>% 
  mutate(
    EnglishName = trimws(India.Checklist.Common.Name),
    eBirdName = trimws(eBird.English.Name.2025),
    BirdLifeName = trimws(BLI.Scientific.Name),
    ScientificName = trimws(India.Checklist.Scientific.Name),
    
    # Formatting for final output
    SubspeciesCount = as.integer(No.of.Subspecies),
    GenerationLength = Generation.Length,
    
    regionalrange = case_when(
      is.na(Percent.of.Global.Range) ~ NA_character_,
      Percent.of.Global.Range < 0.01 ~ "< 1%",
      TRUE ~ paste0(round(Percent.of.Global.Range * 100, 0), "%") 
    )
  ) %>%
  filter(
    Selected.NRL == 1
  )

soib_main %>%
  count(EnglishName) %>%
  filter(n > 1)

soib_main <- soib_main %>% 
  distinct(EnglishName, .keep_all = TRUE)


# ============================================================
# IUCN ASSESSMENTS
# ============================================================

iucn_assessments <- read_csv(assessmentsflattenedfile) %>% 
  mutate(
    EnglishName = trimws(india_checklist_common_name_2025)
  ) %>%
  filter(
    EnglishName %in% soib_main$EnglishName
  )

iucn_assessments %>%
  count(EnglishName) %>%
  filter(n > 1)

iucn_assessments <- iucn_assessments %>% 
  select(
    EnglishName,
    red_list_category_code,
    criteria,
    url,
    supplementary_info_json_population_size,
    supplementary_info_json_estimated_extent_of_occurence,
    supplementary_info_json_estimated_area_of_occupancy,
    population_trend_description_en
  ) %>%
  distinct(EnglishName, .keep_all = TRUE)


# ============================================================

# CHECK FOR NON-MATCHING SPECIES BEFORE JOINS

# ============================================================

cat("\n=============================\n")
cat("ANTI-JOIN CHECKS\n")
cat("=============================\n")


# ------------------------------------------------------------

# 2. IUCN scientific-name joins

# ------------------------------------------------------------

cat("\n--- soib_main names missing in iucn_assessments ---\n")

anti_join(
  soib_main %>%
    select(EnglishName),
  
  iucn_assessments %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

cat("\n--- iucn_assessments names missing in soib_main ---\n")

anti_join(
  iucn_assessments %>%
    select(EnglishName),
  
  soib_main %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

# ------------------------------------------------------------

# 3. Criteria A English-name joins

# ------------------------------------------------------------

cat("\n--- Criteria A English names missing in soib_main ---\n")

anti_join(
  criteriaA_results %>%
    select(EnglishName),
  
  soib_main %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

# ------------------------------------------------------------

# 4. Criteria B English-name joins

# ------------------------------------------------------------

cat("\n--- soib_main English names missing in Criteria B ---\n")

anti_join(
  soib_main %>%
    select(EnglishName),
  
  criteriaB_results %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

cat("\n--- Criteria B English names missing in soib_main ---\n")

anti_join(
  criteriaB_results %>%
    select(EnglishName),
  
  soib_main %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

# ------------------------------------------------------------

# 5. Criteria C English-name joins

# ------------------------------------------------------------

cat("\n--- Criteria C English names missing in soib_main ---\n")

anti_join(
  criteriaC_results %>%
    select(EnglishName),
  
  soib_main %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)

# ------------------------------------------------------------

# 6. Criteria D English-name joins

# ------------------------------------------------------------

cat("\n--- Criteria D English names missing in soib_main ---\n")

anti_join(
  criteriaD_results %>%
    select(EnglishName),
  
  soib_main %>%
    select(EnglishName),
  
  by = "EnglishName"
) %>%
  print(n = Inf)


criteriaA_results <- criteriaA_results %>%
  mutate(EnglishName = trimws(EnglishName)) %>%
  distinct(EnglishName, .keep_all = TRUE)

criteriaB_results <- criteriaB_results %>%
  mutate(EnglishName = trimws(EnglishName)) %>%
  distinct(EnglishName, .keep_all = TRUE)

criteriaC_results <- criteriaC_results %>%
  mutate(EnglishName = trimws(EnglishName)) %>%
  distinct(EnglishName, .keep_all = TRUE)

criteriaD_results <- criteriaD_results %>%
  mutate(EnglishName = trimws(EnglishName)) %>%
  distinct(EnglishName, .keep_all = TRUE)

# ============================================================
# 3. MERGE ALL DATA (SoIB AS BASE)
# ============================================================

merged <- soib_main %>%
  
  # ---- IUCN ----
left_join(iucn_assessments,
                                       by = "EnglishName", relationship = "many-to-one") %>%
  
  # ---- Criteria A ----
left_join(
  criteriaA_results %>%
    select(
      EnglishName, 
      CriteriaA_Category, 
      CriteriaA_String,
      ActualDecline,
      OrgStartYear,
      OrgEndYear,
      StartYear,
      EndYear,
      Decline,
      DeclineMean,
      DeclineLci,
      Years3GEN
    ) %>%
    rename(
      Years3GEN_A = Years3GEN
    ),
  by = "EnglishName",
  relationship = "many-to-one"
) %>%
  
  # ---- Criteria B ----
left_join(criteriaB_results %>%
            select(EnglishName,
                   CriteriaB_Category,
                   CriteriaB_String,
                   MinAOO,
                   MaxAOO,
                   LikelyEOO,
                   MaxEOO,
                   MinLocations,
                   Locations,
                   MaxLocations,
                   EOOChangePercent,
                   EOOYearBandChange
                   ),
          by = "EnglishName",
          relationship = "many-to-one") %>%
  
# ---- Criteria C ----
left_join(
  criteriaC_results %>%
    select(
      EnglishName,
      CriteriaC_Category,
      CriteriaC_String,
      MinMaturePop,
      MaxMaturePop,
      BestMaturePop,
      C1_1GEN_Decline,
      C1_2GEN_Decline,
      C1_3GEN_Decline,
      C1Method,
      Years1GEN,
      Years2GEN,
      Years3GEN,
      ContinuingDecline,
      ContinuingDeclineMethod,
      ActualDecline,
      ActualDeclineMean,
      ActualDeclineLci,
      ActualDeclineStartYear,
      ActualDeclineEndYear,
      ActualDeclineYears
    ) %>%
    rename(
      Years1GEN_C = Years1GEN,
      Years2GEN_C = Years2GEN,
      Years3GEN_C = Years3GEN,
      ActualDecline_C1 = ActualDecline,
      ActualDeclineMean_C1 = ActualDeclineMean,
      ActualDeclineLci_C1 = ActualDeclineLci,
      ActualDeclineStartYear_C1 = ActualDeclineStartYear,
      ActualDeclineEndYear_C1 = ActualDeclineEndYear,
      ActualDeclineYears_C1 = ActualDeclineYears
    ),
  by = "EnglishName",
  relationship = "many-to-one"
) %>%
  
  # ---- Criteria D ----
left_join(criteriaD_results %>%
            select(EnglishName,
                   CriteriaD_Category,
                   CriteriaD_String
                   ),
          by = "EnglishName",
          relationship = "many-to-one")
  
# ============================================================
# 4. FINAL OUTPUT TABLE (UNCHANGED STRUCTURE)
# ============================================================

species <- merged %>%
  transmute(
    
    # --------------------------------------------------------
    # CORE IDENTIFIERS
    # --------------------------------------------------------
    EnglishName = EnglishName,
    ScientificName = ScientificName,
    RegionalRedlist = pmap_chr(
      list(CriteriaA_Category, CriteriaB_Category, CriteriaC_Category, CriteriaD_Category),
      function(...) {
        vals <- c(...)
        # remove NA
        vals <- vals[!is.na(vals)]
        # map codes → full names
        vals <- category_map[vals]
        # remove anything unmapped (just in case)
        vals <- vals[!is.na(vals)]
        # if no threat categories → default LC
        if (!length(vals)) return("Least Concern")
        # pick highest threat
        category_rank[min(match(vals, category_rank))]
      }
    ),
    GlobalRedlist = IUCN.Category,
    AdjustedRegionalRedlist = "To be done",
    # --------------------------------------------------------
    # SOIB FIELDS
    # --------------------------------------------------------
    SoIBPriority = SoIB.Latest.Priority.Status,
    LTC = SoIB.Latest.Long.Term.Status,
    CAT = SoIB.Latest.Current.Status,
    
    # --------------------------------------------------------
    # CRITERIA
    # --------------------------------------------------------
    CriteriaA_Category = CriteriaA_Category,
    CriteriaB_Category = CriteriaB_Category,
    CriteriaC_Category = CriteriaC_Category,
    CriteriaD_Category = CriteriaD_Category,
    
    CriteriaA_String = CriteriaA_String,
    CriteriaB_String = CriteriaB_String,
    CriteriaC_String = CriteriaC_String,
    CriteriaD_String = CriteriaD_String,
    
    GlobalCriteriaString = criteria,
    # --------------------------------------------------------
    # DECLINE METRICS
    # --------------------------------------------------------
    #Stringly everything
    #Note, Decline comes from Criteria A & C file, others from Criteria C
    Decline3GEN_A = ifelse(
      is.na(Decline),
      "NA",
      paste0(round(Decline, 1))
    ),
    Decline3GEN = Decline3GEN_A,
    
    Decline3GEN_A_Mean = ifelse(
      is.na(DeclineMean),
      "NA",
      paste0(round(DeclineMean, 1))
    ),
    Decline3GENMean = Decline3GEN_A_Mean,
    
    Decline3GEN_A_Lci = ifelse(
      is.na(DeclineLci),
      "NA",
      paste0(round(DeclineLci, 1))
    ),
    Decline3GENLci = Decline3GEN_A_Lci,
    
    Decline3GEN_A_Method = "Inferred",
    Years3GEN = Years3GEN_A,

    Decline1GEN = ifelse(
      is.na(C1_1GEN_Decline),
      "NA",
      paste0(round(C1_1GEN_Decline, 1))
    ),
    
    Years1GEN = Years1GEN_C,
    
    Decline2GEN = ifelse(
      is.na(C1_2GEN_Decline),
      "NA",
      paste0(round(C1_2GEN_Decline, 1))
    ),
    
    Years2GEN = Years2GEN_C,
    
    Decline3GEN_C = ifelse(
      is.na(C1_3GEN_Decline),
      "NA",
      paste0(round(C1_3GEN_Decline, 1))
    ),
    
    Decline3GEN_C1_Method = C1Method,
    Decline3GEN_ContinuingDeclineMethod = ContinuingDeclineMethod,
    Years3GEN = Years3GEN_C,
    
    GenerationLength = GenerationLength,
    ActualDeclinePercentage = ifelse (`SoIB.Latest.Current.Status` %in% c("Stable", "Decline", "Rapid Decline", "Rapid Increase", "Increase"),
                                      paste0(round(currentslopemean,2),
                                     "% (",
                                     round(currentsloperci,2),
                                     ", ",
                                     round(currentslopelci,2),") pa, "),
                                     ifelse (is.na(ActualDecline), NA, ActualDecline)),

    YearsActualDecline = ifelse (`SoIB.Latest.Current.Status` %in% c("Stable", "Decline", "Rapid Decline", "Rapid Increase", "Increase"),
                                      paste0(as.integer(latestYear-2015),"y, ",2015,"-",latestYear),
                                 ifelse (is.na(ActualDecline) | is.na(OrgStartYear) | is.na(OrgEndYear),
                                 "",
                                 paste0(as.integer(OrgEndYear-OrgStartYear),"y, ",OrgStartYear,"-",OrgEndYear))),
    ContinuingDecline  = ifelse( is.na(currentslopelci), "Unknown",ifelse (currentslopelci > 0, "No", ifelse(currentsloperci < 0, "Yes", "Uncertain"))),
    
    ActualDeclinePercentage_C1 = ifelse(
      is.na(ActualDecline_C1),
      "NA",
      paste0(round(ActualDecline_C1, 1), "%")
    ),
    
    YearsActualDecline_C1 = ifelse(
      is.na(ActualDeclineStartYear_C1) | is.na(ActualDeclineEndYear_C1),
      "",
      paste0(
        as.integer(ActualDeclineYears_C1),
        "y, ",
        as.integer(ActualDeclineStartYear_C1),
        "-",
        as.integer(ActualDeclineEndYear_C1)
      )
    ),
    
    # --------------------------------------------------------
    # POPULATION
    # --------------------------------------------------------
    BiogPop1Percent = format_num (Onepercent.Estimates),
    CMS = CMS.Appendix,
    CITES = CITES.Appendix,
    Schedule = WPA.Schedule,
    Endemic = India.Endemic,
    # --------------------------------------------------------
    # DISTRIBUTION
    # --------------------------------------------------------
    EOO = format_num(LikelyEOO),
    MaxEOO = ifelse (is.na(MaxEOO) | (MaxEOO == 0), "" ,paste0("(Max ",format_num(MaxEOO), ")")),
    
    DeclineEOO = ifelse (is.na(EOOChangePercent),NA,paste0(round (EOOChangePercent, 1),"%")), #If available, it should be a string
    
    EOOYearBandChange = EOOYearBandChange,

    # --------------------------------------------------------
    # AOO
    # --------------------------------------------------------
    MinAOO = format_num(MinAOO),
    MaxAOO = ifelse (is.na(MaxAOO) | (MaxAOO == 0), "" ,paste0("(Max ",format_num(MaxAOO), ")")),
    MinLocations = MinLocations,
    Locations = Locations,
    MaxLocations = MaxLocations,
    
    
    # --------------------------------------------------------
    # POPULATION COUNTS
    # --------------------------------------------------------
    Subspecies = SubspeciesCount,
    TotalLikelyPop = format_num(round(BestMaturePop,0)),
    TotalMaxPop = ifelse (is.na(MaxMaturePop),"", paste0("(",format_num(round(MinMaturePop,0)),"-", format_num(round(MaxMaturePop,0)),")")),
    
    GlobalPopulation = ifelse (is.na(supplementary_info_json_population_size) | (supplementary_info_json_population_size == "U"), "Unknown", format_num(supplementary_info_json_population_size)), 
    GlobalEOO = format_num(supplementary_info_json_estimated_extent_of_occurence),
    GlobalAOO = format_num(supplementary_info_json_estimated_area_of_occupancy),
    
    GlobalRangePercent = regionalrange, 
    GlobalPopulationTrend = population_trend_description_en,
    
    MigratoryStatusIndia = Migratory.Status.Within.India,
    GlobalRedlistURL = url
  )

# ============================================================
# 5. APPLY RARITIES OVERRIDES (IF FILE EXISTS)
# ============================================================

if (file.exists(raritiesfile)) {
  
  rarities <- read_csv(raritiesfile, show_col_types = FALSE) %>%
    mutate(
      EnglishName = trimws(EnglishName),
      .in_rarities = TRUE
    )
  
  species <- species %>%
    left_join(rarities, by = "EnglishName", suffix = c("", ".rar"))
  
  # Columns to update (common columns only)
  cols <- intersect(names(rarities), names(species))
  cols <- setdiff(cols, c("EnglishName", ".in_rarities"))
  
  for (col in cols) {
    
    rar_col <- paste0(col, ".rar")
    
    if (!rar_col %in% names(species)) next
    
    species[[col]] <- ifelse(
      # Apply override ONLY if species is in rarities
      species$.in_rarities %in% TRUE,
      
      # If rarities value is "" → keep original
      ifelse(
        species[[rar_col]] == "",
        species[[col]],
        species[[rar_col]]   # includes NA and real values
      ),
      
      # Species not in rarities → keep original
      species[[col]]
    )
  }
  
  # Cleanup
  species <- species %>%
    select(-ends_with(".rar"), -.in_rarities)
  
  cat("Rarities file applied.\n")
  
} else {
  cat("Rarities file not found, skipping overrides.\n")
}

species <- species %>%
  mutate(
    AdjustedRegionalRedlist = if_else(
      Endemic == "Yes",
      RegionalRedlist,
      "To be done"
    )
  )


# ============================================================
# 6. WRITE OUTPUT
# ============================================================

write_csv(species, nrloutputfile)

# ============================================================
# 7. QUICK CHECK
# ============================================================

cat("Total species:", nrow(species), "\n")
cat("Missing A:", sum(is.na(species$CriteriaA_Category)), "\n")
cat("Missing B:", sum(is.na(species$CriteriaB_Category)), "\n")
cat("Missing C:", sum(is.na(species$CriteriaC_Category)), "\n")
cat("Missing D:", sum(is.na(species$CriteriaD_Category)), "\n")