# This script assesses the original centroids file for errors and makes
# inclusion/exclusion decisions for each checklist.
# Decisions are based on whether a checklist centroid location lies inside or
# outside India and whether centroid locations fall back into India if latitude
# and longitude are swapped. It produces an assessment report where each
# checklist in the centroid file is assigned a decision and a
# reason for it and a summary json file. 

library(DBI)
library(glue)
library(jsonlite)
library(digest)

source("00_scripts/private.R")
source("00_scripts/functions_centroids.R") # Load helper functions

# Load Data ----
load("00_data/maps_sf.RData")
india_sf_4326 <- st_transform(india_sf, crs = 4326)

raw_file_path <- "00_data/ebird_tracks_IN_2026-01-08.csv.zip"
unzip(raw_file_path, exdir = "00_data")
centroids_file_name <-
  list.files(path = "00_data/",
             pattern = "ebird_tracks_IN.*\\.csv$",
             full.names = TRUE)[1]
centroids <- read.csv(centroids_file_name, header = TRUE)

# Validation file structure ----
validate_centroid_schema(centroids)

# Get location status (within/outside India) ----
# Before and after swapping the centroid coordinates

centroids$location_status <-
  get_location_status(centroids,
                      "centroid_longitude",
                      "centroid_latitude",
                      india_sf_4326)
centroids$location_status_swapped <-
  get_location_status(centroids,
                      "centroid_latitude",
                      "centroid_longitude",
                      india_sf_4326)

# Extract checklist id from database (if present) ----
dbExecute(con, 
          "CREATE TEMP TABLE temp_ids (
          sampling_event_identifier text
          ) ON COMMIT DROP")
dbWriteTable(
  con,
  "temp_ids",
  data.frame(sampling_event_identifier = centroids$checklist_id),
  append = TRUE,
  row.names = FALSE
)


# Add index (VERY important for millions of rows)
# dbExecute(con, "
#     CREATE INDEX idx_temp_ids
#     ON temp_ids(sampling_event_identifier)
# ")

ebd_presence <-
  dbGetQuery(
    con,
    "SELECT DISTINCT 
    e.\"SAMPLING.EVENT.IDENTIFIER\" as checklist_id 
    FROM ebd e 
    INNER JOIN temp_ids t 
    ON e.\"SAMPLING.EVENT.IDENTIFIER\" = t.sampling_event_identifier"
  )

centroids_ebd_check <- centroids %>%
  mutate(list_found_in_ebd = checklist_id %in% ebd_presence$checklist_id)

centroids_updated <- centroids_ebd_check %>%
  mutate(action = case_when(
    location_status == 1 & list_found_in_ebd == TRUE ~ "KEEP",
    location_status == 1 & list_found_in_ebd == FALSE ~ "REMOVE_NOT_IN_EBD",
    location_status == 0 &
      location_status_swapped == 1 & list_found_in_ebd == TRUE ~ "SWAP_AND_KEEP",
    TRUE ~ "REMOVE_OUTSIDE_INDIA"
  )
  )


# Write Report & Summary ----

# Report CSV
assessment_report <- centroids_updated %>%
  dplyr::select(checklist_id,
         action,
         location_status,
         location_status_swapped,
         list_found_in_ebd)

write.csv(assessment_report,
          "00_data/centroids_assessment_report.csv",
          row.names = FALSE)


# Summary JSON
summary_stats <- list(
  input_file = basename(centroids_file_name),
  input_sha256 = digest(centroids_file_name, algo = "sha256", file = TRUE),
  n_total_raw = nrow(centroids_updated),
  n_remove_not_in_ebd = sum(centroids_updated$action == "REMOVE_NOT_IN_EBD"),
  n_keep_from_original = sum(centroids_updated$action == "KEEP"),
  n_swap_and_keep = sum(centroids_updated$action == "SWAP_AND_KEEP"),
  n_remove_outside_india = sum(centroids_updated$action == "REMOVE_OUTSIDE_INDIA")
  
)

write_json(
  summary_stats,
  "00_data/centroids_assessment_summary.json",
  auto_unbox = TRUE,
  pretty = TRUE
)
