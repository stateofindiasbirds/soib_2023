# This list of specie is used for testing. It can be overridden for all species
species <- c (
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

library (tidyverse)
library (lubridate)
library (data.table)
library(sf)
library(dplyr)
source("utils.R")
source("config.R")

obsv    <- readRDS("ebd.RDS")
species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)
species <- readRDS("eoo.RDS") %>% select(Species) %>% pull()

obsv    <- obsv %>% 
              filter (PROTOCOL.TYPE == 'Stationary' | PROTOCOL.TYPE == 'Traveling') %>%
              filter (EFFORT.DISTANCE.KM <= MaxDistanceThresholdforAOO)

master_grid_list_with_species <- readRDS("grids.RDS")

compute_grid_number <- function(lat, lon, distance, protocol, grid_size_km) {
  # Check if the distance is greater than the grid size
  return (
  ifelse ( (protocol != 'Stationary' & protocol != 'Traveling') | (protocol == 'Traveling' & distance > grid_size_km), 
                    NA,
          {
              compute_grid_id (lat, lon, grid_size_km)
          }))
}

proc_aoo <- function (obsv, targetSpeciesObsv)
{
  # Step 1: Create a mapping from checklists to grids of different sizes
  Checklist2Grid <- obsv %>%
                      distinct(GROUP.ID, .keep_all = TRUE) %>%
                        mutate (
                          Grid_2km = compute_grid_number(LATITUDE, LONGITUDE, EFFORT.DISTANCE.KM, PROTOCOL.TYPE, 2),
                          Grid_4km = compute_grid_number(LATITUDE, LONGITUDE, EFFORT.DISTANCE.KM, PROTOCOL.TYPE, 4),
                          Grid_8km = compute_grid_number(LATITUDE, LONGITUDE, EFFORT.DISTANCE.KM, PROTOCOL.TYPE, 8)
                          ) %>% 
                            select ('GROUP.ID', 'Grid_2km', 'Grid_4km', 'Grid_8km', 'ALL.SPECIES.REPORTED')

  colnames(Checklist2Grid) <- c ("Checklist", 'Grid_2km', 'Grid_4km', 'Grid_8km', 'Complete')

  # Step 2: Count checklists in different grids and reshape to long format.
  ChecklistCount <- Checklist2Grid %>%
                          # Keep only rows where Complete == 1
                          filter(Complete == 1) %>%
                          # Reshape to long format to get a column for GridResolution and GridID
                          pivot_longer(cols = c(Grid_2km, Grid_4km, Grid_8km),
                                       names_to = "GridResolution", 
                                       values_to = "GridID") %>%
                          filter(!is.na(GridID)) %>%
                          # Map GridResolution column to the numeric value (2, 4, or 8)
                          mutate(GridResolution = case_when(
                            GridResolution == "Grid_2km" ~ 2,
                            GridResolution == "Grid_4km" ~ 4,
                            GridResolution == "Grid_8km" ~ 8
                          )) %>%
                          # Group by GridResolution and GridID, count unique GROUP.IDs
                          group_by(GridResolution, GridID) %>%
                          summarise(ChecklistCount = n_distinct(Checklist), .groups = 'drop')

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
  
  # Step 4: Calculate species presence in a grid
  SpeciesPresenceInGrid <- ChecklistPresenceInGrid %>%
      distinct(Species, GridResolution, GridID, .keep_all = TRUE) %>%
        select(Species, GridResolution, GridID)
  
  # Step 5: Calculate number of complete checklists with species present
  species_checklists <-   ChecklistPresenceInGrid %>%
                          filter(Complete == 1) %>%
                          group_by(Species, GridResolution, GridID) %>%
                          summarize(PresenceCount = n(), .groups = 'drop') 

  # Step 6: Calculate species frequency in the grids.
  SpeciesPresenceInGridWithFreq <- species_checklists %>%
                                      inner_join(ChecklistCount, by = c("GridResolution", "GridID")) %>%
                                      filter (ChecklistCount > MinChecklistCount) %>% #Minimum number of checklists to consider for analysis is 5
                                      mutate(Frequency = PresenceCount / ChecklistCount) %>%
                                      select(Species, GridResolution, GridID, Frequency)

  # Step 7: Calculate threshold to determine species absence in grids based on checklist frequency.
  SpeciesIndvGridEffortThreshold <- SpeciesPresenceInGridWithFreq %>% 
                                      mutate (Threshold = log(1 - EffortThesholdWithinGridValue/100) / log(1 - Frequency))


  # Step 8: Find the nth percentile of all threshold values to obtain overall threshold for a grid resolution
  SpeciesOverallGridEffortThreshold <- SpeciesIndvGridEffortThreshold %>%
                                          group_by(Species, GridResolution) %>%
                                          summarize(
                                            OverallThreshold = quantile(Threshold, EffortThesholdAcrossGridsPercentile/100, na.rm = TRUE),
                                            .groups = 'drop'
                                          )
  # Step 9: Calculate absence status based on threshold.
  SpeciesAbsenceInGrid <- ChecklistCount %>%
                              inner_join(SpeciesOverallGridEffortThreshold, by = c("GridResolution")) %>%
                              mutate(
                                Absence = if_else(ChecklistCount >= OverallThreshold, 1, 0)
                              ) %>%
                              select(Species, GridResolution, GridID, Absence)


  # Step 10: Calculate the final occupancy status for each species in each grid.
  SpeciesOccupancyInGrid <-     master_grid_list_with_species %>% 
                                # Join with the presence dataframe
                                left_join(SpeciesPresenceInGrid %>%
                                            mutate(Occupancy = "P"), 
                                          by = c("Species", "GridResolution", "GridID")) %>%
                                # Join with the absence dataframe
                                left_join(SpeciesAbsenceInGrid %>%
                                            filter(Absence == 1) %>%
                                            mutate(Occupancy = "A"),
                                          by = c("Species", "GridResolution", "GridID")) %>%
                                # Determine the final Occupancy status
                                mutate(Occupancy = case_when(
                                  !is.na(Occupancy.x) ~ Occupancy.x, # Presence takes priority
                                  !is.na(Occupancy.y) ~ Occupancy.y, # Absence if not present
                                  TRUE ~ "U"                       # Unknown if neither presence nor absence
                                )) %>%
                                # Select and rename columns
                                select(Species, GridResolution, GridID, Occupancy) 


  # Define the area for each grid resolution (in square km)
  grid_areas <- c(`2` = 4, `4` = 16, `8` = 64)

  # Step 11: Estimate AOO by summing the areas of grids where species are present or absent.
  AOOEstimates <- SpeciesOccupancyInGrid %>%
                      group_by(Species, GridResolution) %>%
                      summarize(
                        MinEstimate = sum(Occupancy == "P") * grid_areas[as.character(GridResolution)],
                        MaxEstimate = sum(Occupancy %in% c("P", "U")) * grid_areas[as.character(GridResolution)],
                        GridIDs_P = list(GridID[Occupancy == "P"]),  # Collect grid IDs for occupancy "P"
                        GridIDs_U = list(GridID[Occupancy == "U"]),  # Collect grid IDs for occupancy "U"
                        .groups = 'drop'
                      ) %>%
                      # Check if there are any issues in the summarization
                      arrange(Species, GridResolution) %>%
                      distinct() %>%
                      # Pivot to wide format
                      pivot_wider(
                        names_from = GridResolution,
                        values_from = c(MinEstimate, MaxEstimate, GridIDs_P, GridIDs_U),
                        names_sep = "_"
                      ) %>%
                      # Rename columns appropriately
                      rename(
                        MinEstimate_2km = MinEstimate_2,
                        MaxEstimate_2km = MaxEstimate_2,
                        MinEstimate_4km = MinEstimate_4,
                        MaxEstimate_4km = MaxEstimate_4,
                        MinEstimate_8km = MinEstimate_8,
                        MaxEstimate_8km = MaxEstimate_8,
                        GridIDs_P_2km = GridIDs_P_2,
                        GridIDs_U_2km = GridIDs_U_2,
                        MinEstimate_4km = MinEstimate_4,
                        MaxEstimate_4km = MaxEstimate_4,
                        GridIDs_P_4km = GridIDs_P_4,
                        GridIDs_U_4km = GridIDs_U_4,
                        MinEstimate_8km = MinEstimate_8,
                        MaxEstimate_8km = MaxEstimate_8,
                        GridIDs_P_8km = GridIDs_P_8,
                        GridIDs_U_8km = GridIDs_U_8
                      )
  return (AOOEstimates)
}

# List of observations of target species
targetSpeciesObsv <- obsv %>% 
  filter (COMMON.NAME %in% species) %>% 
  select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv) <- c ("Species", "Checklist")

AOOEstimates      <- proc_aoo(obsv, targetSpeciesObsv)

# Observations made in a constrained manner
obsv_2km <- obsv %>% 
              filter (PROTOCOL.TYPE == 'Stationary' | PROTOCOL.TYPE == 'Traveling') %>%
              filter (EFFORT.DISTANCE.KM <= 2)

targetSpeciesObsv <- obsv_2km %>% 
                        filter (COMMON.NAME %in% species) %>% 
                        select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv) <- c ("Species", "Checklist")

AOOEstimates_2km  <- proc_aoo(obsv_2km, targetSpeciesObsv)

# Calculate C-value for MinEstimate and MaxEstimate
C_value_table <- AOOEstimates_2km %>%
                    rowwise() %>%
                    mutate(
                      # Calculate C for MinEstimate
                      C_8to4 = log(MinEstimate_8km / MinEstimate_4km) / log(64 / 16),
                      C_4to2 = log(MinEstimate_4km / MinEstimate_2km) / log(16 / 4),
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

saveRDS(AOO_table, "aoo.RDS")


