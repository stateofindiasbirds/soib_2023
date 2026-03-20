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
    BirdLifeName   = trimws(BLI.Scientific.Name),
    
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
    GlobalRedlist = ifelse (is.na(red_list_category_code), "Not Assessed", category_map[red_list_category_code]),
    AdjustedRegionalRedlist = "To be done",
    
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
    
    GlobalCriteriaString = criteria,
    # --------------------------------------------------------
    # DECLINE METRICS
    # --------------------------------------------------------
    #Stringly everything
    #Note, Decline comes from Criteria A file, others from Criteria C
    Decline3GEN = ifelse(is.na(Decline),"NA",paste0(round(Decline,1))),
    Years3GEN = Years3GEN,
    
    Decline2GEN = ifelse(is.na(`2GEN Decline`),"NA",paste0(round(`2GEN Decline`,1))),
    Years2GEN = Years2GEN,

    Decline1GEN = ifelse(is.na(`1GEN Decline`),"NA",paste0(round(`1GEN Decline`,1))),
    Years1GEN = Years1GEN,

    GenerationLength = supplementary_info_json_generational_length,
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
    
    # --------------------------------------------------------
    # POPULATION
    # --------------------------------------------------------
    BiogPop1Percent = format_num (Onepercent.Estimates),
    CMS = CMS.Appendix,
    CITES = CITES.Appendix,
    Schedule = WPA.Schedule,
    
    # --------------------------------------------------------
    # DISTRIBUTION
    # --------------------------------------------------------
    EOO = format_num(LikelyEOO),
    MaxEOO = ifelse (is.na(MaxEOO), "" ,paste0("(Max ",format_num(MaxEOO), ")")),
    
    DeclineEOO = ifelse (is.na(EOOChangePercent),NA,paste0(round (EOOChangePercent, 1),"%")), #If available, it should be a string
    
    EOOYearBandChange = EOOYearBandChange,

    # --------------------------------------------------------
    # AOO
    # --------------------------------------------------------
    MinAOO = format_num(MinAOO),
    MaxAOO = ifelse (is.na(MaxAOO), "" ,paste0("(Max ",format_num(MaxAOO), ")")),
    Locations = Locations,
    
    # --------------------------------------------------------
    # POPULATION COUNTS
    # --------------------------------------------------------
    Subspecies = SubspeciesCount,
    TotalLikelyPop = format_num(round(BestMaturePop,0)),
    TotalMaxPop = ifelse (is.na(MaxMaturePop),"", paste0(" (Max ", format_num(round(MaxMaturePop,0)),")")),
    
    GlobalPopulation = ifelse (is.na(supplementary_info_json_population_size) | (supplementary_info_json_population_size == "U"), "Unknown", format_num(supplementary_info_json_population_size)), 
    GlobalEOO = format_num(supplementary_info_json_estimated_extent_of_occurence),
    GlobalAOO = format_num(supplementary_info_json_estimated_area_of_occupancy),
    
    GlobalRangePercent = NA_character_, #Awaiting info from Alex
    GlobalPopulationTrend = population_trend_description_en,
    
    MigratoryStatusIndia = Migratory.Status.Within.India,
    GlobalRedlistURL = url
  )

# ============================================================
# 5. DATA TYPES
# ============================================================

#species <- species %>%
#  mutate(across(where(is.character), as.character)) %>%
#  mutate(across(where(is.numeric), as.numeric))

species <- species %>%
  filter(EnglishName %in% species_list$English.Name)

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