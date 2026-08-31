# Approach function - sum_district_max
sum_district_max <- function(data, eu_row, mapping_df) {
  
  start_m <- match(eu_row$START.MONTH, month.abb)
  end_m   <- match(eu_row$END.MONTH, month.abb)
  
  # Step 1: Filter data
  # Parse region codes from REGION.CODE
  target_regions <- str_split(eu_row$REGION.CODE, ",")[[1]]
  target_regions <- str_trim(target_regions)
  
  ########################################
  # Parse data sources
  ########################################
  
  target_sources <- str_split(
    eu_row$DATA.SOURCE, "," )[[1]]
  target_sources <- str_trim(target_sources)
  
  ########################################
  # Resolve all species name variants
  ########################################
  
  valid_names <- get_species_variants(
    eu_row$COMMON.NAME,
    mapping_df
  )
  
  filtered <- data %>%
    filter(
      (
        COMMON.NAME %in% valid_names |
        SCIENTIFIC.NAME %in% valid_names
      ),
      (
        COUNTY.CODE %in% target_regions |
        STATE.CODE %in% target_regions
      ),
      DATA.SOURCE %in% target_sources,
      SEASON.YEAR >= eu_row$START.YEAR,
      SEASON.YEAR <= eu_row$END.YEAR
    ) %>% 
    
    # Assign matching aggregation region
    mutate(
      MATCHED.REGION =
        case_when(
          COUNTY.CODE %in% target_regions ~ COUNTY.CODE,
          STATE.CODE %in% target_regions ~ STATE.CODE,
          TRUE ~ NA_character_
        )
    ) %>%
    
    # seasonal month filter (handles wrap-around like Nov–Mar)
    filter(if (start_m <= end_m) {
      MONTH.NUM >= start_m & MONTH.NUM <= end_m
    } else {
      MONTH.NUM >= start_m | MONTH.NUM <= end_m
    })
  
  cat(
    "EU:", eu_row$EU.NAME,
    "| Species:", eu_row$COMMON.NAME,
    "| Region Code:", eu_row$REGION.CODE,
    "| Rows after filter:", nrow(filtered),
    "\n"
  )
  
  # if no data pass the filter for a specific EU - guards against empty data
  if (nrow(filtered) == 0) {
    return(data.frame(
      EU.NAME = eu_row$EU.NAME,
      COMMON.NAME = eu_row$COMMON.NAME,
      REGION.CODE = eu_row$REGION.CODE,
      START.MONTH = eu_row$START.MONTH,
      END.MONTH = eu_row$END.MONTH,
      ESTIMATE.MIN = NA,
      ESTIMATE.MAX = NA
    ))
  }
  
  # Step 2: Maximum count per district across ALL years
  region_max <- filtered %>%
    group_by(MATCHED.REGION) %>%
    summarise(
      MaxCount = max(OBSERVATION.COUNT, na.rm = TRUE),
      .groups = "drop")
  
  # Step 3: Sum of all districts in an EU
  eu_total <- sum(region_max$MaxCount, na.rm = TRUE)
  
  print(nrow(eu_total))
  print(eu_total)
cat("EU:", eu_row$EU.NAME, "Rows:", nrow(filtered), "\n") 
cat("EU:", eu_row$EU.NAME, "| Regions:", nrow(region_max), "| Estimate:", eu_total, "\n")

  # Step 5: clean output
  return(data.frame(
    EU.NAME = eu_row$EU.NAME,
    COMMON.NAME = eu_row$COMMON.NAME,
    REGION.CODE = eu_row$REGION.CODE,
    START.MONTH = eu_row$START.MONTH,
    END.MONTH = eu_row$END.MONTH,
    ESTIMATE.MIN = eu_total,
    ESTIMATE.MAX = eu_total
  ))
}
