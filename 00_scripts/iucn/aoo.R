# This list of specie is used for testing. It can be overridden for all species
test_species <- c (
  "Brahminy Kite",
  "White-browed Bulbul",
  "Yellow-browed Bulbul",
  "Malabar Gray Hornbill",
  "Lesser Coucal",
  "Oriental Scops-Owl",
  "Rufous-bellied Eagle",
  "Sanderling",
  "White-cheeked Barbet",
  "Malabar Barbet",
  "Bay-backed Shrike",
  "Jerdon's Baza",
  "Legge's Hawk-Eagle",
  "Lesser Fish-Eagle",
  "Spot-bellied Eagle-Owl",
  "Golden-headed Cisticola"
)

test_species <- c (test_species, c(
  "Banasura Laughingthrush",
  "Nilgiri Laughingthrush",
  "Ashambu Laughingthrush",
  "Black-headed Greenfinch"
))

scriptpath <- "00_scripts/iucn/"
datapath   <- "00_scripts/iucn/"

library (tidyverse)
library (lubridate)
library (data.table)
library(sf)
library(dplyr)
source(file.path(scriptpath, "config.R"))

centroid <- readRDS("00_data/centroids_sanitized_final.rds")

centroid <- centroid %>%
  select(
    checklist_id,
    centroid_longitude,
    centroid_latitude,
    longitude_min,
    longitude_max,
    latitude_min,
    latitude_max,
    location_score
  )

obsv    <- readRDS(file.path(datapath,"ebd.RDS"))
species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)
species <- readRDS(file.path(datapath,"eoo.RDS")) %>% 
            #filter(Species %in% test_species) %>%
            select (Species) %>%
            pull(Species)

obsv    <- obsv %>% 
              filter (PROTOCOL.NAME == 'Stationary' | PROTOCOL.NAME == 'Traveling') %>%
              filter (EFFORT.DISTANCE.KM <= MaxDistanceThresholdforAOO)

obsv <- obsv %>%
  left_join(
    centroid,
    by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id")
  )

compute_bbox_diagonal <- function(
    lon_min, lon_max,
    lat_min, lat_max
) {
  
  lon_span_km <- (lon_max - lon_min) * 111 * cos(pi * (lat_min + lat_max) / 360)
  lat_span_km <- (lat_max - lat_min) * 111
  
  sqrt(lon_span_km^2 + lat_span_km^2)
}

# Compute diagonal and insert
obsv <- obsv %>%
  mutate(
    bbox_diagonal_km =
      if_else(
        !is.na(longitude_min),
        compute_bbox_diagonal(
          longitude_min,
          longitude_max,
          latitude_min,
          latitude_max
        ),
        NA_real_
      )
  )


################# Centroid metrics #######################
message(
  "Matched checklists: ",
  n_distinct(
    obsv$SAMPLING.EVENT.IDENTIFIER[
      !is.na(obsv$centroid_latitude)
    ]
  ),
  " of ",
  n_distinct(obsv$SAMPLING.EVENT.IDENTIFIER)
)

table(obsv$location_score, useNA = "always")

message(
  "2km centroid checklists = ",
  n_distinct(
    obsv$SAMPLING.EVENT.IDENTIFIER[
      !is.na(obsv$centroid_latitude) &
        obsv$location_score == 0
    ]
  )
)

message(
  "4km centroid checklists = ",
  n_distinct(
    obsv$SAMPLING.EVENT.IDENTIFIER[
      !is.na(obsv$centroid_latitude) &
        obsv$location_score %in% c(0,1)
    ]
  )
)

message(
  "8km centroid checklists = ",
  n_distinct(
    obsv$SAMPLING.EVENT.IDENTIFIER[
      !is.na(obsv$centroid_latitude) &
        obsv$location_score %in% c(0,1,2)
    ]
  )
)

old_grid_4 <- compute_grid_id(
  obsv$LATITUDE,
  obsv$LONGITUDE,
  4
)

new_grid_4 <- compute_grid_id(
  ifelse(
    !is.na(obsv$centroid_latitude) &
      obsv$location_score %in% c(0,1),
    obsv$centroid_latitude,
    obsv$LATITUDE
  ),
  ifelse(
    !is.na(obsv$centroid_longitude) &
      obsv$location_score %in% c(0,1),
    obsv$centroid_longitude,
    obsv$LONGITUDE
  ),
  4
)

mean(old_grid_4 != new_grid_4, na.rm = TRUE)

old_grid_8 <- compute_grid_id(
  obsv$LATITUDE,
  obsv$LONGITUDE,
  8
)

new_grid_8 <- compute_grid_id(
  ifelse(
    !is.na(obsv$centroid_latitude) &
      obsv$location_score %in% c(0,1,2),
    obsv$centroid_latitude,
    obsv$LATITUDE
  ),
  ifelse(
    !is.na(obsv$centroid_longitude) &
      obsv$location_score %in% c(0,1,2),
    obsv$centroid_longitude,
    obsv$LONGITUDE
  ),
  8
)

mean(old_grid_8 != new_grid_8, na.rm = TRUE)
###############################Centorid diagnostics done



load_species_grids <- function(species_names) {
  
  safe_names <- gsub("[^A-Za-z0-9_]", "_", species_names)
  files <- file.path(datapath,paste0("tmp/", safe_names, ".rds"))
  
  existing_files <- files[file.exists(files)]
  
  if (length(existing_files) == 0) {
    warning("No grid files found for given species")
    return(NULL)
  }
  
  grid_data_list <- purrr::map(existing_files, function(f) {
    
    res <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(res) || length(res) == 0) return(NULL)
    
    # 🔥 Extract ALL grid_data entries (handles duplicate names)
    gd_list <- res[names(res) == "grid_data"]
    
    if (length(gd_list) == 0) return(NULL)
    
    # Bind all resolutions for this species
    gd <- dplyr::bind_rows(gd_list)
    
    if (is.null(gd) || nrow(gd) == 0) return(NULL)
    
    # 🔒 Enforce type consistency (prevents join crashes)
    gd <- gd %>%
      mutate(
        Species = as.character(Species),
        GridResolution = as.numeric(GridResolution),
        GridID = as.character(GridID)
      )
    
    return(gd)
  })
  
  grid_data <- bind_rows(grid_data_list)
  
  if (is.null(grid_data) || nrow(grid_data) == 0) {
    warning("No valid grid data found after reading files")
    return(NULL)
  }
  
  # Ensure uniqueness (important for joins later)
  grid_data <- grid_data %>%
    distinct(Species, GridResolution, GridID)
  
  return(grid_data)
}

compute_grid_id <- function(lat, lon, grid_size_km,
                            origin_lon = 68,
                            origin_lat = 6) {
  
  grid_size_deg <- grid_size_km / 111
  
  lat_na <- is.na(lat)
  lon_na <- is.na(lon)
  na_idx <- lat_na | lon_na
  
  row <- floor((lat - origin_lat) / grid_size_deg)
  col <- floor((lon - origin_lon) / grid_size_deg)
  
  grid_id <- paste(grid_size_km, row, col, sep = "_")
  
  grid_id[na_idx] <- NA_character_
  
  return(grid_id)
}

compute_grid_number <- function(lat, lon, extent_km, protocol, grid_size_km) {
  # Check if the distance is greater than the grid size
  return (
  ifelse ( (protocol != 'Stationary' & protocol != 'Traveling') | (protocol == 'Traveling' & extent_km > grid_size_km), 
           NA_character_,
          {
              compute_grid_id (lat, lon, grid_size_km)
          }))
}

bbox_grid_ids <- function(
    lon_min, lon_max,
    lat_min, lat_max,
    grid_size_km,
    origin_lon = 68,
    origin_lat = 6
) {
  
  if (
    is.na(lon_min) ||
    is.na(lon_max) ||
    is.na(lat_min) ||
    is.na(lat_max)
  ) {
    return(character(0))
  }
  
  grid_size_deg <- grid_size_km / 111
  
  row_min <- floor((lat_min - origin_lat) / grid_size_deg)
  row_max <- floor((lat_max - origin_lat) / grid_size_deg)
  
  col_min <- floor((lon_min - origin_lon) / grid_size_deg)
  col_max <- floor((lon_max - origin_lon) / grid_size_deg)
  
  expand.grid(
    row = row_min:row_max,
    col = col_min:col_max
  ) %>%
    transmute(
      GridID = paste(grid_size_km, row, col, sep = "_")
    ) %>%
    pull(GridID)
}

prepare_checklist_grid <- function(obsv) {
  
  
  
  # Step 1: Checklist → Grid mapping
  Checklist2Grid <- obsv %>%
    distinct(GROUP.ID, .keep_all = TRUE) %>%
    mutate(
      Extent_2km = ifelse( !is.na(centroid_latitude) & location_score == 0 & !is.na(bbox_diagonal_km), bbox_diagonal_km, EFFORT.DISTANCE.KM ),
      
      Extent_4km = ifelse( !is.na(centroid_latitude) & location_score %in% c(0,1) & !is.na(bbox_diagonal_km), bbox_diagonal_km, EFFORT.DISTANCE.KM ),
      
      Extent_8km = ifelse( !is.na(centroid_latitude) & location_score %in% c(0,1,2) & !is.na(bbox_diagonal_km), bbox_diagonal_km, EFFORT.DISTANCE.KM ),
      
      Grid_2km = compute_grid_number(ifelse( !is.na(location_score) & location_score == 0 & !is.na(centroid_latitude), centroid_latitude, LATITUDE), 
                                     ifelse( !is.na(location_score) & location_score == 0 & !is.na(centroid_longitude),centroid_longitude,LONGITUDE),
                                     Extent_2km, 
                                     PROTOCOL.NAME, 
                                     2),
      
      Grid_4km = compute_grid_number(ifelse( !is.na(location_score) & location_score %in% c(0,1) & !is.na(centroid_latitude), centroid_latitude, LATITUDE), 
                                     ifelse( !is.na(location_score) & location_score %in% c(0,1) & !is.na(centroid_longitude),centroid_longitude,LONGITUDE), 
                                     Extent_4km, 
                                     PROTOCOL.NAME, 
                                     4),
      
      Grid_8km = compute_grid_number(ifelse( !is.na(location_score) & location_score %in% c(0,1,2) & !is.na(centroid_latitude), centroid_latitude, LATITUDE),  
                                     ifelse( !is.na(location_score) & location_score %in% c(0,1,2) & !is.na(centroid_longitude),centroid_longitude,LONGITUDE), 
                                     Extent_8km, 
                                     PROTOCOL.NAME, 
                                     8),
      TrackCells_4km = pmap(
        list(
          longitude_min,
          longitude_max,
          latitude_min,
          latitude_max
        ),
        ~ {
          cells <- bbox_grid_ids(
            ..1, ..2, ..3, ..4,
            grid_size_km = 4
          )
          
          if(length(cells) <= MaxTrackCellsForExpansion)
            cells
          else
            character(0)
        }
      ),
      
      TrackCells_8km = pmap(
        list(
          longitude_min,
          longitude_max,
          latitude_min,
          latitude_max
        ),
        ~ {
          cells <- bbox_grid_ids(
            ..1, ..2, ..3, ..4,
            grid_size_km = 8
          )
          
          if(length(cells) <= MaxTrackCellsForExpansion)
            cells
          else
            character(0)
        }
      )      
    ) %>%
    select(GROUP.ID, Grid_2km, Grid_4km, Grid_8km, TrackCells_4km, TrackCells_8km, ALL.SPECIES.REPORTED, location_score) %>% 
    mutate(
      Grid_2km = as.character(Grid_2km),
      Grid_4km = as.character(Grid_4km),
      Grid_8km = as.character(Grid_8km)
    )
  
  colnames(Checklist2Grid) <- c("Checklist", "Grid_2km", "Grid_4km", "Grid_8km", "TrackCells_4km", "TrackCells_8km", "Complete","location_score")
  
  # Step 2: Checklist counts per grid
  ChecklistCount <- Checklist2Grid %>%
    filter(Complete == 1) %>%
    pivot_longer(
      cols = c(Grid_2km, Grid_4km, Grid_8km),
      names_to = "GridResolution",
      values_to = "GridID"
    ) %>%
    filter(!is.na(GridID)) %>%
    mutate(
      GridResolution = case_when(
        GridResolution == "Grid_2km" ~ 2,
        GridResolution == "Grid_4km" ~ 4,
        GridResolution == "Grid_8km" ~ 8
      )
    ) %>%
    group_by(GridResolution, GridID) %>%
    summarise(ChecklistCount = n_distinct(Checklist), .groups = 'drop')
  
  message(
    "Centroid usage (checklists):\n",
    "Total = ", nrow(Checklist2Grid),
    "\n2km = ",
    sum(Checklist2Grid$location_score == 0, na.rm = TRUE),
    "\n4km = ",
    sum(Checklist2Grid$location_score %in% c(0,1), na.rm = TRUE),
    "\n8km = ",
    sum(Checklist2Grid$location_score %in% c(0,1,2), na.rm = TRUE)
  )
  
  return(list(
    Checklist2Grid = Checklist2Grid,
    ChecklistCount = ChecklistCount
  ))
}

proc_aoo <- function (obsv, targetSpeciesObsv, Checklist2Grid, ChecklistCount)
{
  if (nrow(targetSpeciesObsv) == 0) {
    message("Skipping species: ", paste(unique(targetSpeciesObsv$Species), collapse = ", "))
    return(tibble())
  }
  message("Starting AOO for ", paste(unique(targetSpeciesObsv$Species), collapse = ", "))
  
  
  # Step 3: Determine species presence in the grids and calculate frequency of occurrence.
  ChecklistPresenceInGrid <- targetSpeciesObsv %>%
                              inner_join(Checklist2Grid, by = "Checklist") %>%
                              pivot_longer(cols = starts_with("Grid_"), 
                                           names_to = "GridResolution", 
                                           values_to = "GridID", 
                                           names_prefix = "Grid_") %>%
                                # Step 3: Filter out rows where GridID is NA (no matching grid for that resolution)
                                filter(!is.na(GridID)) %>%
                                # Step 4: Convert the GridResolution column to numeric
                                mutate(GridResolution = case_when(
                                  GridResolution == "2km" ~ 2,
                                  GridResolution == "4km" ~ 4,
                                  GridResolution == "8km" ~ 8,
                                  TRUE ~ NA_real_  # This will handle any unexpected values
                                )) %>%
                                # Step 5: Select only relevant columns
                                select(Species, GridResolution, GridID, Complete)
  
  message("ChecklistPresenceInGrid ", nrow(ChecklistPresenceInGrid))
  
  ChecklistTrackCells <- targetSpeciesObsv %>%
    inner_join(Checklist2Grid, by = "Checklist")
  
  UT_4km <- ChecklistTrackCells %>%
    filter(location_score %in% c(0,1), lengths(TrackCells_4km) > 1) %>%
    rowwise() %>%
    mutate(
      GridID = list(
        setdiff(
          TrackCells_4km,
          Grid_4km
        )
      )
    ) %>%
    unnest(GridID) %>%
    transmute(
      Species,
      GridResolution = 4,
      GridID
    )
  
  UT_8km <- ChecklistTrackCells %>%
    filter(location_score %in% c(0,1,2), lengths(TrackCells_8km) > 1) %>%
    rowwise() %>%
    mutate(
      GridID = list(
        setdiff(
          TrackCells_8km,
          Grid_8km
        )
      )
    ) %>%
    unnest(GridID) %>%
    transmute(
      Species,
      GridResolution = 8,
      GridID
    )
  
  SpeciesTrackUncertainty <- bind_rows(
    UT_4km,
    UT_8km
  )
  # Step 4: Calculate species presence in a grid
  SpeciesPresenceInGrid <- ChecklistPresenceInGrid %>%
      distinct(Species, GridResolution, GridID, .keep_all = TRUE) %>%
        select(Species, GridResolution, GridID)
  message("SpeciesPresenceInGrid ", nrow(SpeciesPresenceInGrid))
  
  # Step 5: Calculate number of complete checklists with species present
  species_checklists <-   ChecklistPresenceInGrid %>%
                          filter(Complete == 1) %>%
                          group_by(Species, GridResolution, GridID) %>%
                          summarize(PresenceCount = n(), .groups = 'drop') 
  message("species_checklists ", nrow(species_checklists))
  
  # Step 6: Calculate species frequency in the grids.
  SpeciesPresenceInGridWithFreq <- species_checklists %>%
                                      inner_join(ChecklistCount, by = c("GridResolution", "GridID")) %>%
                                      filter (ChecklistCount > MinChecklistCount) %>% #Minimum number of checklists to consider for analysis is 5
                                      mutate(Frequency = PresenceCount / ChecklistCount) %>%
                                      select(Species, GridResolution, GridID, Frequency)
  message("SpeciesPresenceInGridWithFreq ", nrow(SpeciesPresenceInGridWithFreq))
  
  # Step 7: Calculate threshold to determine species absence in grids based on checklist frequency.
  SpeciesIndvGridEffortThreshold <- SpeciesPresenceInGridWithFreq %>% 
                                      mutate (Threshold = log(1 - EffortThesholdWithinGridValue/100) / log(1 - Frequency))
  message("SpeciesIndvGridEffortThreshold ", nrow(SpeciesIndvGridEffortThreshold))
  

  # Step 8: Find the nth percentile of all threshold values to obtain overall threshold for a grid resolution
  SpeciesOverallGridEffortThreshold <- SpeciesIndvGridEffortThreshold %>%
                                          group_by(Species, GridResolution) %>%
                                          summarize(
                                            OverallThreshold = quantile(Threshold, EffortThesholdAcrossGridsPercentile/100, na.rm = TRUE),
                                            .groups = 'drop'
                                          )
  message("SpeciesOverallGridEffortThreshold ", nrow(SpeciesOverallGridEffortThreshold))
  
  # Step 9: Calculate absence status based on threshold.
  SpeciesAbsenceInGrid <- SpeciesOverallGridEffortThreshold %>%
                              inner_join(ChecklistCount, by = "GridResolution") %>%
                              mutate(
                                Absence = if_else(ChecklistCount >= OverallThreshold, 1, 0)
                              ) %>%
                              select(Species, GridResolution, GridID, Absence)
  message("SpeciesAbsenceInGrid ", nrow(SpeciesAbsenceInGrid))
  
  species_grids <- load_species_grids(unique(targetSpeciesObsv$Species))
  
  if (is.null(species_grids)) {
    warning(paste("Missing grid file for:", paste(unique(targetSpeciesObsv$Species), collapse = ", ")))
    return(tibble())
  }

  stopifnot(is.character(SpeciesPresenceInGrid$GridID))
  stopifnot(is.character(species_grids$GridID))
  message("Grid overlap: ",
          length(intersect(
            unique(SpeciesPresenceInGrid$GridID),
            unique(species_grids$GridID)
          ))
  )
  
  # Step 10: Calculate the final occupancy status for each species in each grid.
  SpeciesOccupancyInGrid <- species_grids %>%
    mutate(Species = as.character(Species)) %>%
    
    # Presence
    left_join(
      SpeciesPresenceInGrid %>%
        mutate(Present = TRUE),
      by = c("Species", "GridResolution", "GridID")
    ) %>%
    
    # Track uncertainty
    left_join(
      SpeciesTrackUncertainty %>%
        mutate(TrackUncertainty = TRUE),
      by = c("Species", "GridResolution", "GridID")
    ) %>%
    
    # Absence
    left_join(
      SpeciesAbsenceInGrid %>%
        filter(Absence == 1) %>%
        mutate(Absent = TRUE),
      by = c("Species", "GridResolution", "GridID")
    ) %>%
    
    # Final occupancy assignment
    mutate(
      Occupancy = case_when(
        !is.na(Present) ~ "P",             # Presence always wins
        !is.na(TrackUncertainty) ~ "UT",   # Track-derived uncertainty
        !is.na(Absent) ~ "A",              # Absence
        TRUE ~ "U"                         # Remaining unknown
      )
    ) %>%
    
    select(
      Species,
      GridResolution,
      GridID,
      Occupancy
    )
  
  message(
    "SpeciesOccupancyInGrid ",
    nrow(SpeciesOccupancyInGrid)
  )
  
  message(
    "Occupancy breakdown: ",
    paste(
      names(table(SpeciesOccupancyInGrid$Occupancy)),
      table(SpeciesOccupancyInGrid$Occupancy),
      collapse = ", "
    )
  )

  # Define the area for each grid resolution (in square km)
  grid_areas <- c(`2` = 4, `4` = 16, `8` = 64)

  # Step 11: Estimate AOO by summing the areas of grids where species are present or absent.
  AOOEstimates <- SpeciesOccupancyInGrid %>%
                      group_by(Species, GridResolution) %>%
                      summarize(
                        MinEstimate = sum(Occupancy == "P") * grid_areas[as.character(first(GridResolution))],
                        MaxEstimate = sum(Occupancy %in% c("P", "UT", "U")) * grid_areas[as.character(first(GridResolution))],
                        GridIDs_P = list(GridID[Occupancy == "P"]),  # Collect grid IDs for occupancy "P"
                        GridIDs_UT = list(GridID[Occupancy == "UT"]),
                        GridIDs_U = list(GridID[Occupancy == "U"]),  # Collect grid IDs for occupancy "U"
                        .groups = 'drop'
                      ) %>%
                      # Check if there are any issues in the summarization
                      arrange(Species, GridResolution) %>%
                      distinct() %>%
                      # Pivot to wide format
                      pivot_wider(
                        names_from = GridResolution,
                        values_from = c(MinEstimate, MaxEstimate, GridIDs_P, GridIDs_UT, GridIDs_U),
                        names_sep = "_"
                      ) %>%
                      # Rename columns appropriately
                      rename(
                        MinEstimate_2km = MinEstimate_2,
                        MaxEstimate_2km = MaxEstimate_2,
                        GridIDs_P_2km = GridIDs_P_2,
                        GridIDs_UT_2km = GridIDs_UT_2,
                        GridIDs_U_2km = GridIDs_U_2,
                        MinEstimate_4km = MinEstimate_4,
                        MaxEstimate_4km = MaxEstimate_4,
                        GridIDs_P_4km = GridIDs_P_4,
                        GridIDs_UT_4km = GridIDs_UT_4,
                        GridIDs_U_4km = GridIDs_U_4,
                        MinEstimate_8km = MinEstimate_8,
                        MaxEstimate_8km = MaxEstimate_8,
                        GridIDs_P_8km = GridIDs_P_8,
                        GridIDs_UT_8km = GridIDs_UT_8,
                        GridIDs_U_8km = GridIDs_U_8
                      )

  message(
    "P=", sum(SpeciesOccupancyInGrid$Occupancy == "P"),
    " UT=", sum(SpeciesOccupancyInGrid$Occupancy == "UT"),
    " A=", sum(SpeciesOccupancyInGrid$Occupancy == "A"),
    " U=", sum(SpeciesOccupancyInGrid$Occupancy == "U")
  )  
  return (AOOEstimates)
}

# List of observations of target species
targetSpeciesObsv <- obsv %>% 
  filter (COMMON.NAME %in% species) %>% 
  select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv) <- c ("Species", "Checklist")

prep_full <- prepare_checklist_grid (obsv)

AOOEstimates <- map(species, function(sp) {
  
  targetSpeciesObsv_sp <- targetSpeciesObsv %>% filter(Species == sp)
  
  proc_aoo(obsv, targetSpeciesObsv_sp, prep_full$Checklist2Grid, prep_full$ChecklistCount)
}) %>% bind_rows()

# Observations made in a constrained manner
obsv_2km <- obsv %>% 
              filter (PROTOCOL.NAME == 'Stationary' | PROTOCOL.NAME == 'Traveling') %>%
              filter (EFFORT.DISTANCE.KM <= 2)

targetSpeciesObsv <- obsv_2km %>% 
                        filter (COMMON.NAME %in% species) %>% 
                        select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv) <- c ("Species", "Checklist")

prep_full <- prepare_checklist_grid (obsv_2km)

AOOEstimates_2km <- map(species, function(sp) {
  
  targetSpeciesObsv_sp <- targetSpeciesObsv %>% filter(Species == sp)
  
  proc_aoo(obsv_2km, targetSpeciesObsv_sp, prep_full$Checklist2Grid, prep_full$ChecklistCount)
}) %>% bind_rows()

# Calculate C-value for MinEstimate and MaxEstimate
C_value_table <- AOOEstimates_2km %>%
                    rowwise() %>%
                    mutate(
                      # Calculate C for MinEstimate
                      C_8to4 = ifelse (MinEstimate_8km >0 & MinEstimate_4km >0, log(MinEstimate_8km / MinEstimate_4km) / log(64 / 16), NA_real_),
                      C_4to2 = ifelse (MinEstimate_4km >0 & MinEstimate_2km >0, log(MinEstimate_4km / MinEstimate_2km) / log(16 / 4), NA_real_),
                      C_value = mean(c(C_8to4, C_4to2), na.rm = TRUE),
                      
                    ) %>%
                    # Select only Species, C_Min, and C_Max for the final table
                    select(Species, C_value)


# Final AOO table extrapolating usign C-value
AOO_table <- AOOEstimates %>%
                      inner_join(C_value_table, by = "Species") %>%
                      # Calculate new AOO for Min and Max using the formula
                      mutate(
                        MinAOO = MinEstimate_4km * 10^(C_value * log10(4/16)),
                        MaxAOO = MaxEstimate_4km * 10^(C_value * log10(4/16))
                      ) %>%
                      # Select only the required columns
                      select(Species, 
                                MinAOO, MaxAOO, 
                                MinEstimate_2km, 
                                GridIDs_P_2km, GridIDs_P_4km, GridIDs_P_8km,
                                GridIDs_U_2km, GridIDs_U_4km, GridIDs_U_8km)

saveRDS(AOO_table, file.path(datapath,"aoo.RDS"))



#####################Comparisons####################
old <- readRDS(file.path(datapath, "aoo_after_centroid_integration.RDS"))
new <- readRDS(file.path(datapath, "aoo_after_bounding_box.RDS"))

comparison <- old %>%
  select(Species, MinAOO_old = MinAOO, MaxAOO_old = MaxAOO) %>%
  inner_join(
    new %>%
      select(Species, MinAOO_new = MinAOO, MaxAOO_new = MaxAOO),
    by = "Species"
  ) %>%
  mutate(
    MinPctChange =
      100 * (MinAOO_new - MinAOO_old) / MinAOO_old,
    
    MaxPctChange =
      100 * (MaxAOO_new - MaxAOO_old) / MaxAOO_old
  )

comparison %>%
  arrange(desc(abs(MinPctChange)))