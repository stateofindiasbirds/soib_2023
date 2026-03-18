library(dplyr)
library(readr)
source("00_scripts/iucn/config_iucn.R")

category_rank <- c(
  "Extinct",
  "Extinct in Wild",
  "Critically Endangered",
  "Endangered",
  "Vulnerable",
  "Near Threatened",
  "Least Concern",
  "Data Deficient"
)
# ============================================================
# 1. READ INPUT FILES
# ============================================================

criteriaA_results <- read_csv(criteriaAResultsfile)
criteriaB_results <- read_csv(criteriaBResultsfile)
criteriaC_results <- read_csv(criteriaCResultsfile)
criteriaD_results <- read_csv(criteriaDResultsfile)

soib_main        <- read_csv(soibmainfile)
iucn_assessments <- read_csv(assessmentsflattenedfile)
subpopulations   <- read_csv(subpopulationsfile)
species_list     <- read.csv(nrlspecieslistfile)

# ============================================================
# 2. NORMALIZE KEYS
# ============================================================

soib_main <- soib_main %>%
  mutate(
    EnglishName    = trimws(eBird.English.Name.2024),
    ScientificName = trimws(eBird.Scientific.Name.2024),
    BirdLifeName   = trimws(BLI.Scientific.Name)
  ) %>%
  distinct(ScientificName, .keep_all = TRUE)

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

iucn_assessments <- iucn_assessments %>%
  mutate(
    BirdLifeName = trimws(taxon_object_json_scientific_name)
  ) %>%
  distinct(BirdLifeName, .keep_all = TRUE)

subpopulations <- subpopulations %>%
  mutate(
    ScientificName = trimws(Species),
    SubspeciesCount = as.integer(No_of_Subspecies)
  ) %>%
  distinct(ScientificName, .keep_all = TRUE)

# ============================================================
# 3. MERGE ALL DATA (SoIB AS BASE)
# ============================================================

merged <- soib_main %>%
  
  # ---- IUCN ----
left_join(iucn_assessments, by = "BirdLifeName", relationship = "many-to-one") %>%
  
  # ---- Criteria A ----
left_join(criteriaA_results %>%
            select(EnglishName, 
                   CriteriaA_Category, 
                   CriteriaA_String,
                   ActualDecline,
                   OrgStartYear,
                   OrgEndYear,
                   StartYear,
                   EndYear,
                   Decline,
                   Years3GEN
                   ),
          by = "EnglishName",
          relationship = "many-to-one") %>%
  
  # ---- Criteria B ----
left_join(criteriaB_results %>%
            select(EnglishName,
                   CriteriaB_Category,
                   CriteriaB_String,
                   MinAOO,
                   MaxAOO,
                   LikelyEOO,
                   MaxEOO,
                   Locations,
                   EOOChangePercent,
                   EOOYearBandChange
                   ),
          by = "EnglishName",
          relationship = "many-to-one") %>%
  
  # ---- Criteria C ----
left_join(criteriaC_results %>%
            select(EnglishName,
                   CriteriaC_Category,
                   CriteriaC_String,
                   MinMaturePop,
                   MaxMaturePop,
                   BestMaturePop,
                   `1GEN Decline`,
                   `2GEN Decline`,
                   Years1GEN,
                   Years2GEN),
          by = "EnglishName",
          relationship = "many-to-one") %>%
  
  # ---- Criteria D ----
left_join(criteriaD_results %>%
            select(EnglishName,
                   CriteriaD_Category,
                   CriteriaD_String
                   ),
          by = "EnglishName",
          relationship = "many-to-one") %>%
  
  # ---- Subspecies ----
left_join(subpopulations,
          by = "ScientificName",
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
    RegionalRedlist = case_when(
      all(is.na(c(CriteriaA_Category, CriteriaB_Category, CriteriaC_Category, CriteriaD_Category))) ~ "Least Concern",
      TRUE ~ {
        vals <- c(CriteriaA_Category, CriteriaB_Category, CriteriaC_Category, CriteriaD_Category)
        vals <- vals[!is.na(vals)]
        
        category_rank[min(match(vals, category_rank))]
      }
    ),    
    GlobalRedlist = red_list_category_code,
    AdjustedRegionalRedlist = NA_character_,
    
    # --------------------------------------------------------
    # SOIB FIELDS
    # --------------------------------------------------------
    SoIBPriority = SoIB.Latest.Priority.Status,
    LTC = SoIB.Latest.Long.Term.Status,
    CAT = SoIB.Latest.Range.Status,
    
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
    # --------------------------------------------------------
    # DECLINE METRICS
    # --------------------------------------------------------
    Decline3GEN = round(Decline,1),
    Years3GEN = Years3GEN,
    
    Decline2GEN = round(`2GEN Decline`,1),
    Years2GEN = Years2GEN,
    
    Decline1GEN = round(`1GEN Decline`,1),
    Years1GEN = Years1GEN,
    
    GenerationLength = supplementary_info_json_generational_length,
    ActualDeclinePercentage = ActualDecline,
    YearsActualDecline = ifelse (is.na(ActualDeclinePercentage), "NA",paste0(as.integer(OrgEndYear-OrgStartYear),"years, ",OrgStartYear,"-",OrgEndYear)),
    
    # --------------------------------------------------------
    # POPULATION
    # --------------------------------------------------------
    BiogPop1Percent = Onepercent.Estimates,
    CMS = CMS.Appendix,
    CITES = CITES.Appendix,
    Schedule = WPA.Schedule,
    
    # --------------------------------------------------------
    # DISTRIBUTION
    # --------------------------------------------------------
    EOO = LikelyEOO,
    MaxEOO = MaxEOO,
    
    DeclineEOO3GEN = NA_real_, #If available, it should be a string
    
    EOOYearBandChange = EOOYearBandChange,

    # --------------------------------------------------------
    # AOO
    # --------------------------------------------------------
    MinAOO = MinAOO,
    MaxAOO = MaxAOO,
    Locations = Locations,
    
    # --------------------------------------------------------
    # POPULATION COUNTS
    # --------------------------------------------------------
    Subspecies = SubspeciesCount,
    TotalLikelyPop = BestMaturePop,
    TotalMaxPop = MaxMaturePop,
    
    GlobalPopulation = supplementary_info_json_population_size,
    GlobalEOO = supplementary_info_json_estimated_extent_of_occurence,
    GlobalAOO = supplementary_info_json_estimated_area_of_occupancy,
    
    GlobalRangePercent = NA_character_, #Awaiting info from Alex
    GlobalPopulationTrend = population_trend_description_en,
    
    MigratoryStatusIndia = Migratory.Status.Within.India,
    GlobalRedlistURL = url
  )

# ============================================================
# 5. DATA TYPES
# ============================================================

species <- species %>%
  mutate(across(where(is.character), as.character)) %>%
  mutate(across(where(is.numeric), as.numeric))

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