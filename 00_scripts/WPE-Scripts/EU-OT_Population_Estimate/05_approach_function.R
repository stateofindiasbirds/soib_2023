library(dplyr)
library(stringr)
library(sf)

#########################################################################
## NEW HELPER -- run ONCE on the full data, BEFORE looping over EUs
#########################################################################
# Gives every record a 1km x 1km grid-cell ID (GRID.1KM).
# Coordinates are projected to an equal-area projection for India (in
# metres), then floored to 1000 m, so every cell is exactly 1 km^2.
# Doing this once (not inside sum_district_max) means the projection is
# not repeated for every EU, and every EU uses the identical grid.

add_1km_grid <- function(data,
                         crs_proj  = "+proj=aea +lat_0=0 +lon_0=82 +lat_1=12 +lat_2=28 +datum=WGS84 +units=m +no_defs",
                         cell_size = 1000) {
  
  has_xy <- !is.na(data$LATITUDE) & !is.na(data$LONGITUDE)
  data$GRID.1KM <- NA_character_
  
  if (any(has_xy)) {
    pts <- st_as_sf(data[has_xy, c("LONGITUDE", "LATITUDE")],
                    coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
    xy  <- st_coordinates(st_transform(pts, crs_proj))
    data$GRID.1KM[has_xy] <- paste0(floor(xy[, 1] / cell_size), "_",
                                    floor(xy[, 2] / cell_size))
  }
  
  cat("1km grid assigned:", sum(has_xy), "rows |",
      sum(!has_xy), "rows without coordinates |",
      n_distinct(data$GRID.1KM[has_xy]), "distinct 1km cells\n")
  
  data
}

#########################################################################
## Approach function - sum_district_max (1km-grid version)
#########################################################################
# Max count per 1km cell (all years pooled), summed within each
# district/state, then summed across districts/states in the EU.

sum_district_max <- function(data, eu_row, mapping_df) {
  
  if (!"GRID.1KM" %in% names(data)) {
    stop("GRID.1KM column missing -- run add_1km_grid() on the data first.")
  }
  
  start_m <- match(eu_row$START.MONTH, month.abb)
  end_m   <- match(eu_row$END.MONTH, month.abb)
  
  # Step 1: Filter data
  # Parse region codes from REGION.CODE
  target_regions <- str_split(eu_row$REGION.CODE, ",")[[1]]
  target_regions <- str_trim(target_regions)
  
  ########################################
  # Parse data sources
  ########################################
  
  target_sources <- str_split(eu_row$DATA.SOURCE, ",")[[1]]
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
    
    # seasonal month filter (handles wrap-around like Nov-Mar)
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
      EU.NAME      = eu_row$EU.NAME,
      COMMON.NAME  = eu_row$COMMON.NAME,
      REGION.CODE  = eu_row$REGION.CODE,
      START.MONTH  = eu_row$START.MONTH,
      END.MONTH    = eu_row$END.MONTH,
      N.1KM.CELLS  = 0,
      ESTIMATE.MIN = NA,
      ESTIMATE.MAX = NA
    ))
  }
  
  # Safe max / sum: return NA (not -Inf / 0) when every value is NA
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  safe_sum <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  
  # Records without coordinates cannot be placed in a 1km cell; they are
  # pooled into ONE "no_coords" unit per district/state (i.e. treated
  # exactly as the old district-level max), so they are not dropped and
  # not over-counted.
  filtered <- filtered %>%
    mutate(GRID.1KM = ifelse(is.na(GRID.1KM), "no_coords", GRID.1KM))
  
  # Step 2: 1km cell maxima, summed within each district/state
  
  # 2a. Max count per 1km cell within each district/state (all years pooled)
  cell_max <- filtered %>%
    group_by(MATCHED.REGION, GRID.1KM) %>%
    summarise(CellMax = safe_max(OBSERVATION.COUNT), .groups = "drop")
  
  # 2b. Sum of 1km cell maxima within each district/state
  region_max <- cell_max %>%
    group_by(MATCHED.REGION) %>%
    summarise(
      MaxCount = safe_sum(CellMax),
      N.Cells  = n(),
      .groups  = "drop"
    )
  
  # Step 3: Sum of all districts/states in an EU
  eu_total <- sum(region_max$MaxCount, na.rm = TRUE)
  
  cat("EU:", eu_row$EU.NAME,
      "| Regions:", nrow(region_max),
      "| 1km cells:", sum(region_max$N.Cells),
      "| Estimate:", eu_total, "\n")
  
  # Step 4: clean output
  return(data.frame(
    EU.NAME      = eu_row$EU.NAME,
    COMMON.NAME  = eu_row$COMMON.NAME,
    REGION.CODE  = eu_row$REGION.CODE,
    START.MONTH  = eu_row$START.MONTH,
    END.MONTH    = eu_row$END.MONTH,
    N.1KM.CELLS  = sum(region_max$N.Cells),
    ESTIMATE.MIN = eu_total,
    ESTIMATE.MAX = eu_total
  ))
}
