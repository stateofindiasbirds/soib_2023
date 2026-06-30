library(stats)
# This list of specie is used for testing. It can be overridden for all species
test_species <- c (
  "Brahminy Kite",
#  "White-browed Bulbul",
#  "Yellow-browed Bulbul",
#  "Malabar Gray Hornbill",
#  "Lesser Coucal",
#  "Oriental Scops-Owl",
#  "Rufous-bellied Eagle",
  "Sanderling",
#  "White-cheeked Barbet",
#  "Malabar Barbet",
#  "Bay-backed Shrike",
  "Jerdon's Baza",
#  "Legge's Hawk-Eagle",
#  "Lesser Fish-Eagle",
#  "Spot-bellied Eagle-Owl",
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
source(file.path(scriptpath, "aoo_utils.R"))

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

proc_aoo <- function (obsv, targetSpeciesObsv, Checklist2Grid, ChecklistCount, ChecklistRichness, dataset_name)
{
  if (nrow(targetSpeciesObsv) == 0) {
    message("Skipping species: ", paste(unique(targetSpeciesObsv$Species), collapse = ", "))
    return(tibble())
  }
  message("Starting AOO for ", paste(unique(targetSpeciesObsv$Species), collapse = ", "))
  
  # Diagnostics
  diag_row <- tibble(
  Species = NA,
  Dataset = dataset_name,
  
  # Detection model
  Detections = NA_integer_,
  UseModel = FALSE,
  ModelConverged = NA,
  ModelAIC = NA_real_,
  Intercept = NA_real_,
  RichnessCoef = NA_real_,
  RichnessP = NA_real_,
  
  # Detectability
  MeanDetectability = NA_real_,
  MinDetectability = NA_real_,
  MaxDetectability = NA_real_,
  
  # Thresholds
  Threshold2km = NA_real_,
  Threshold4km = NA_real_,
  Threshold8km = NA_real_,
  MeanThreshold = NA_real_,
  MinThreshold = NA_real_,
  MaxThreshold = NA_real_,
  
  # Species checklist stats
  Checklists = NA_integer_,
  Centroid2km_Checklists = NA_integer_,
  Centroid4km_Checklists = NA_integer_,
  Centroid8km_Checklists = NA_integer_,
  
  # Grid counts before occupancy assignment
  PresenceGrids = NA_integer_,
  AbsenceGrids = NA_integer_,
  TrackUncertaintyGrids = NA_integer_,
  TrackUT_4km = NA_integer_,
  TrackUT_8km = NA_integer_,
  
  # Occupancy totals
  P = NA_integer_,
  UT = NA_integer_,
  A = NA_integer_,
  U = NA_integer_,
  
  # Occupancy by resolution
  P_2km = NA_integer_,
  P_4km = NA_integer_,
  P_8km = NA_integer_,
  
  UT_2km = NA_integer_,
  UT_4km = NA_integer_,
  UT_8km = NA_integer_,
  
  A_2km = NA_integer_,
  A_4km = NA_integer_,
  A_8km = NA_integer_,
  
  U_2km = NA_integer_,
  U_4km = NA_integer_,
  U_8km = NA_integer_,
  
  # Raw AOO estimates
  MinEstimate_2km = NA_real_,
  MinEstimate_4km = NA_real_,
  MinEstimate_8km = NA_real_,
  
  MaxEstimate_2km = NA_real_,
  MaxEstimate_4km = NA_real_,
  MaxEstimate_8km = NA_real_
  )

  
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
  
  diag_row$Checklists <-
    n_distinct(ChecklistTrackCells$Checklist)
  
  diag_row$Centroid2km_Checklists <-
    n_distinct(
      ChecklistTrackCells$Checklist[
        ChecklistTrackCells$location_score == 0
      ]
    )
  
  diag_row$Centroid4km_Checklists <-
    n_distinct(
      ChecklistTrackCells$Checklist[
        ChecklistTrackCells$location_score %in% c(0,1)
      ]
    )
  
  diag_row$Centroid8km_Checklists <-
    n_distinct(
      ChecklistTrackCells$Checklist[
        ChecklistTrackCells$location_score %in% c(0,1,2)
      ]
    )
  
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
  
  diag_row$TrackUT_4km <-
    n_distinct(UT_4km$GridID)
  
  diag_row$TrackUT_8km <-
    n_distinct(UT_8km$GridID)
  
  diag_row$TrackUncertaintyGrids <-
    n_distinct(SpeciesTrackUncertainty$GridID)
  
  # Step 4: Calculate species presence in a grid
  SpeciesPresenceInGrid <- ChecklistPresenceInGrid %>%
      distinct(Species, GridResolution, GridID, .keep_all = TRUE) %>%
        select(Species, GridResolution, GridID)
  message("SpeciesPresenceInGrid ", nrow(SpeciesPresenceInGrid))
  
  diag_row$PresenceGrids <-
    nrow(SpeciesPresenceInGrid)
  
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
  
  
  # Obtain the occuppied grids
  occupied_grids <- SpeciesPresenceInGridWithFreq %>%
                        distinct(GridResolution, GridID)
  
  Checklist2Grid_long <- Checklist2Grid %>%
                              tidyr::pivot_longer(
                                cols = c(Grid_2km, Grid_4km, Grid_8km),
                                names_to = "GridResolution",
                                values_to = "GridID"
                              ) %>%
                              mutate(
                                GridResolution = case_when(
                                  GridResolution == "Grid_2km" ~ 2,
                                  GridResolution == "Grid_4km" ~ 4,
                                  GridResolution == "Grid_8km" ~ 8
                                )
                              )
  
  # Obtain checklists in occupied grids
  checklists_in_occupied_grids <- Checklist2Grid_long %>%
      inner_join(occupied_grids, by = c("GridResolution", "GridID"))
  
  # Step 7: OldMethod: Calculate threshold to determine species absence in grids based on checklist frequency.
  #SpeciesIndvGridEffortThreshold <- SpeciesPresenceInGridWithFreq %>% 
  #                                    mutate (Threshold = log(1 - EffortThesholdWithinGridValue/100) / log(1 - Frequency))
  # Step 7: NewMethod: Calculate threshold to determine species absence in grids based on model-based detectability
  use_model <- modelForSpeciesAbsence
  
  species_detection_data <- ChecklistRichness %>%
    inner_join(checklists_in_occupied_grids, by = "Checklist") %>%
    mutate(
      Detection = if_else(
        Checklist %in% targetSpeciesObsv$Checklist,
        1L,
        0L
      )
    )
  
  n_detect <- sum(species_detection_data$Detection)
  species_name <- unique(targetSpeciesObsv$Species)
  diag_row$Detections <- n_detect
  diag_row$Species = species_name
  
  if(n_detect < 20)
    use_model <- FALSE
  
  model <- NULL
  
  if (use_model)
  {
    model <- tryCatch(
      glm(
        Detection ~ Richness,
        family = binomial(),
        data = species_detection_data
      ),
      error = function(e) NULL
    )
    if(is.null(model))
    {
      use_model <- FALSE
    }
    else
    {
      diag_row$UseModel <- TRUE
      diag_row$ModelConverged <- model$converged
      diag_row$ModelAIC <- AIC(model)
      
      coef_tab <- summary(model)$coefficients
      
      diag_row$Intercept <- coef_tab[1,1]
      
      if(nrow(coef_tab) > 1)
      {
        diag_row$RichnessCoef <- coef_tab[2,1]
        diag_row$RichnessP <- coef_tab[2,4]
      }
    }
  
    message(
      unique(targetSpeciesObsv$Species),
      " detections = ",
      n_detect,
      " use_model = ",
      use_model
    )
  }
  
  if(use_model)
  {
    species_detection_data <- species_detection_data %>%
      mutate(
        Detectability = predict(
          model,
          newdata = .,
          type = "response"
        )
      )
    
    diag_row$MeanDetectability <- mean(species_detection_data$Detectability, na.rm = TRUE)
    
    diag_row$MinDetectability <- min(species_detection_data$Detectability, na.rm = TRUE)
    
    diag_row$MaxDetectability <- max(species_detection_data$Detectability, na.rm = TRUE)
    
    GridDetectability <- species_detection_data %>%
      select(Checklist, Detectability) %>%
      inner_join(Checklist2Grid, by = "Checklist") %>%
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
      summarise(
        MeanDetectability = mean(Detectability, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Species = first(targetSpeciesObsv$Species)
      )
    
    message(
      "GridDetectability grids = ",
      nrow(GridDetectability)
    )
    
    SpeciesIndvGridEffortThreshold <- GridDetectability %>%
                                          mutate(
                                            MeanDetectability =
                                              pmin(
                                                pmax(MeanDetectability, 1e-6),
                                                1 - 1e-6
                                              ),
                                            
                                            Threshold =
                                              log(1 - EffortThesholdWithinGridValue / 100) /
                                              log(1 - MeanDetectability)
                                          )
    
  }
  else
  {
    SpeciesIndvGridEffortThreshold <- SpeciesPresenceInGridWithFreq %>% 
                                        mutate (Threshold = log(1 - EffortThesholdWithinGridValue/100) / log(1 - Frequency))
    
  }
  
  message("SpeciesIndvGridEffortThreshold ", nrow(SpeciesIndvGridEffortThreshold))
  diag_row$MeanThreshold <-
    mean(
      SpeciesIndvGridEffortThreshold$Threshold,
      na.rm = TRUE
    )
  
  diag_row$MinThreshold <-
    min(
      SpeciesIndvGridEffortThreshold$Threshold,
      na.rm = TRUE
    )
  
  diag_row$MaxThreshold <-
    max(
      SpeciesIndvGridEffortThreshold$Threshold,
      na.rm = TRUE
    )

  message(
    "Threshold summary: mean=",
    round(mean(SpeciesIndvGridEffortThreshold$Threshold, na.rm = TRUE),1),
    " min=",
    round(min(SpeciesIndvGridEffortThreshold$Threshold, na.rm = TRUE),1),
    " max=",
    round(max(SpeciesIndvGridEffortThreshold$Threshold, na.rm = TRUE),1)
  )
  # Step 8: Find the nth percentile of all threshold values to obtain overall threshold for a grid resolution (robust global effort standard derived from model-based detectability)
  SpeciesOverallGridEffortThreshold <- SpeciesIndvGridEffortThreshold %>%
                                          group_by(Species, GridResolution) %>%
                                          summarize(
                                            OverallThreshold = quantile(Threshold, EffortThesholdAcrossGridsPercentile/100, na.rm = TRUE),
                                            .groups = 'drop'
                                          )
  message("SpeciesOverallGridEffortThreshold ", nrow(SpeciesOverallGridEffortThreshold))

  if(use_model)
  {
    diag_thresholds <- SpeciesOverallGridEffortThreshold %>%
      select(GridResolution, OverallThreshold)
    
    if(2 %in% diag_thresholds$GridResolution)
    {
      diag_row$Threshold2km <-
        diag_thresholds$OverallThreshold[
          diag_thresholds$GridResolution == 2
        ]
    }
    
    if(4 %in% diag_thresholds$GridResolution)
    {
      diag_row$Threshold4km <-
        diag_thresholds$OverallThreshold[
          diag_thresholds$GridResolution == 4
        ]
    }
    
    if(8 %in% diag_thresholds$GridResolution)
    {
      diag_row$Threshold8km <-
        diag_thresholds$OverallThreshold[
          diag_thresholds$GridResolution == 8
        ]
    }
  }
  
  # Step 9: Calculate absence status based on threshold.
  SpeciesAbsenceInGrid <- SpeciesOverallGridEffortThreshold %>%
                              inner_join(ChecklistCount, by = "GridResolution") %>%
                              mutate(GridID = as.character(GridID)) %>%
                              mutate(
                                Absence = if_else(ChecklistCount >= OverallThreshold, 1, 0)
                              ) %>%
                              select(Species, GridResolution, GridID, Absence)
  message("SpeciesAbsenceInGrid ", nrow(SpeciesAbsenceInGrid))
  
  diag_row$AbsenceGrids <-
    nrow(
      SpeciesAbsenceInGrid %>%
        filter(Absence == 1)
    )
  
  species_grids <- load_species_grids(unique(targetSpeciesObsv$Species))
  
  if (is.null(species_grids)) {
    warning(paste("Missing grid file for:", paste(unique(targetSpeciesObsv$Species), collapse = ", ")))
    return(tibble())
  }

  SpeciesPresenceInGrid <- force_gridid(SpeciesPresenceInGrid)
  SpeciesTrackUncertainty <- force_gridid(SpeciesTrackUncertainty)
  SpeciesAbsenceInGrid <- force_gridid(SpeciesAbsenceInGrid)
  species_grids <- force_gridid(species_grids)
  
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
  
  occ_summary <- SpeciesOccupancyInGrid %>%
    count(GridResolution, Occupancy)
  
  diag_row$P_2km <-
    occ_summary %>%
    filter(GridResolution == 2, Occupancy == "P") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$P_4km <-
    occ_summary %>%
    filter(GridResolution == 4, Occupancy == "P") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$P_8km <-
    occ_summary %>%
    filter(GridResolution == 8, Occupancy == "P") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$UT_2km <-
    occ_summary %>%
    filter(GridResolution == 2, Occupancy == "UT") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$UT_4km <-
    occ_summary %>%
    filter(GridResolution == 4, Occupancy == "UT") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$UT_8km <-
    occ_summary %>%
    filter(GridResolution == 8, Occupancy == "UT") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$A_2km <-
    occ_summary %>%
    filter(GridResolution == 2, Occupancy == "A") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$A_4km <-
    occ_summary %>%
    filter(GridResolution == 4, Occupancy == "A") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$A_8km <-
    occ_summary %>%
    filter(GridResolution == 8, Occupancy == "A") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$U_2km <-
    occ_summary %>%
    filter(GridResolution == 2, Occupancy == "U") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$U_4km <-
    occ_summary %>%
    filter(GridResolution == 4, Occupancy == "U") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$U_8km <-
    occ_summary %>%
    filter(GridResolution == 8, Occupancy == "U") %>%
    pull(n) %>%
    sum(na.rm = TRUE)
  
  diag_row$P  <- sum(SpeciesOccupancyInGrid$Occupancy == "P")
  diag_row$UT <- sum(SpeciesOccupancyInGrid$Occupancy == "UT")
  diag_row$A  <- sum(SpeciesOccupancyInGrid$Occupancy == "A")
  diag_row$U  <- sum(SpeciesOccupancyInGrid$Occupancy == "U")
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
  
  message(
    "ModelUsed=", use_model,
    " Detections=", n_detect
  )
  
  if(nrow(AOOEstimates) > 0)
  {
    cols <- intersect(
      names(diag_row),
      names(AOOEstimates)
    )
    
    diag_row[cols] <- AOOEstimates[1, cols]
  }
  
  return(
    list(
      AOO = AOOEstimates,
      Diagnostics = diag_row
    )
  )
}

# ----------------------------------------------------------------------------------------------------
###################################Start of the Main Run##############################################
# ----------------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------------
#Read EBD and process
# ----------------------------------------------------------------------------------------------------
obsv    <- readRDS(file.path(datapath,"ebd.RDS"))

obsv    <- obsv %>% 
  filter (PROTOCOL.NAME == 'Stationary' | PROTOCOL.NAME == 'Traveling') %>%
  filter (EFFORT.DISTANCE.KM <= MaxDistanceThresholdforAOO)

# ----------------------------------------------------------------------------------------------------
#Read files and decide the species list for the run
# ----------------------------------------------------------------------------------------------------

species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)

species <- readRDS(file.path(datapath,"eoo.RDS")) %>% 
#  filter(Species %in% test_species) %>%
  select (Species) %>%
  pull(Species) %>%
  unique()


# ----------------------------------------------------------------------------------------------------
#Read and process centroids
# ----------------------------------------------------------------------------------------------------
centroid <- readRDS(centroidfile)

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


obsv <- obsv %>%
  left_join(
    centroid,
    by = c("SAMPLING.EVENT.IDENTIFIER" = "checklist_id")
  )

# ----------------------------------------------------------------------------------------------------
# Checklist richness
# ----------------------------------------------------------------------------------------------------

ChecklistRichness <- obsv %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  distinct(GROUP.ID, COMMON.NAME) %>%
  count(GROUP.ID, name = "Richness")

colnames(ChecklistRichness)[1] <- "Checklist"


# ----------------------------------------------------------------------------------------------------
# Compute diagonal and insert
# ----------------------------------------------------------------------------------------------------

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

# Observations made in a constrained manner
obsv_2km <- obsv %>% 
  filter (EFFORT.DISTANCE.KM <= 2)

# ----------------------------------------------------------------------------------------------------
################# Centroid metrics #######################
# ----------------------------------------------------------------------------------------------------

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
###############################Centorid diagnostics done##################

# List of observations of target species
targetSpeciesObsv <- obsv %>% 
  filter (COMMON.NAME %in% species) %>% 
  select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv) <- c ("Species", "Checklist")

targetSpeciesObsv_2km <- obsv_2km %>% 
  filter (COMMON.NAME %in% species) %>% 
  select('COMMON.NAME', 'GROUP.ID')
colnames (targetSpeciesObsv_2km) <- c ("Species", "Checklist")

prep_full <- prepare_checklist_grid (obsv)
saveRDS(prep_full, file.path(datapath, "prepared_checklist_grid.rds"))

prep_full_2km <- prepare_checklist_grid (obsv_2km)
saveRDS(prep_full_2km, file.path(datapath, "prepared_checklist_grid_2km.rds"))

# Read this file to save time        
prep_full <- readRDS(file.path(datapath, "prepared_checklist_grid.rds"))
results <- map(species, function(sp) {
  
  targetSpeciesObsv_sp <-
    targetSpeciesObsv %>%
    filter(Species == sp)
  
  proc_aoo(
    obsv,
    targetSpeciesObsv_sp,
    prep_full$Checklist2Grid,
    prep_full$ChecklistCount,
    ChecklistRichness,
    dataset_name = "Full"
  )
})

AOOEstimates <-
  map_dfr(results, "AOO")

SpeciesDiagnostics <-
  map_dfr(results, "Diagnostics")


prep_full_2km <- readRDS(file.path(datapath, "prepared_checklist_grid_2km.rds"))

results_2km <- map(species, function(sp) {
  
  targetSpeciesObsv_2km_sp <- targetSpeciesObsv_2km %>%
    filter(Species == sp)
  
  proc_aoo(
    obsv_2km,
    targetSpeciesObsv_2km_sp,
    prep_full_2km$Checklist2Grid,
    prep_full_2km$ChecklistCount,
    ChecklistRichness,
    dataset_name = "2km"
  )
})

AOOEstimates_2km <-
  map_dfr(results_2km, "AOO")

SpeciesDiagnostics_2km <-
  map_dfr(results_2km, "Diagnostics")

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
                                GridIDs_U_2km, GridIDs_U_4km, GridIDs_U_8km) %>%
                      distinct_all()

saveRDS(AOO_table, file.path(datapath,"aoo.RDS"))

AllDiagnostics <-
  bind_rows(
    SpeciesDiagnostics,
    SpeciesDiagnostics_2km
  )

saveRDS(
  AllDiagnostics,
  file.path(datapath, "aoo_diagnostics.RDS")
)

write.csv(
  AllDiagnostics,
  file.path(datapath, "aoo_diagnostics.csv"),
  row.names = FALSE
)

#####################Comparisons####################
old <- readRDS(file.path(datapath, "aoo_all_species_bounding_box.RDS")) %>%
  distinct_all()
new <- readRDS(file.path(datapath, "aoo.RDS")) %>%
  distinct_all()

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