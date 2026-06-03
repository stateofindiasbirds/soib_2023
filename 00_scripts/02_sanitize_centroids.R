# This script implements the decisions made in the assessment script 
# (01_assess_centroids.R) and add a field called "location score" to
# the original centroids file. Based on how far the checklist location 
# specified in the EBD is from the bounding box, the location score
# is assigned a value of 0 (inside the bounding box), 1 (less than
# 2 km outside the bounding box), 2 (between 2 and 5 km from the bounding
# box) and 3 (for all other distances)  

library(DBI)
library(glue)
library(tidyverse)
library(sf)
library(digest)
library(jsonlite)

sf::sf_use_s2(FALSE)
source("00_scripts/private.R")
source("00_scripts/functions_centroids.R") # Load helper functions

# Verify Integrity of the centroids file  ----
summary_json <- read_json("00_data/centroids_assessment_summary.json")
current_sha <- digest("00_data/ebird_tracks_IN_2026-01-08.csv", algo="sha256", file=TRUE)

if (current_sha != summary_json$input_sha256) {
  stop("Data integrity check failed! Input file has changed since assessment.")
}

# Read and filter ----
report <- read.csv("00_data/centroids_assessment_report.csv")
raw_data <- read.csv("00_data/ebird_tracks_IN_2026-01-08.csv")

# Only keep valid checklists
centroids_to_process <- report %>% 
  filter(action %in% c("KEEP", "SWAP_AND_KEEP") & 
           list_found_in_ebd == TRUE) %>%
  left_join(raw_data, by = "checklist_id")

# Make new columns for swapped coordinates ----
centroids_sanitized <- centroids_to_process %>%
  mutate(
    # Swap centroids if action is SWAP_AND_KEEP
    final_centroid_lat = if_else(action == "SWAP_AND_KEEP", centroid_longitude, centroid_latitude),
    final_centroid_lon = if_else(action == "SWAP_AND_KEEP", centroid_latitude, centroid_longitude),
    # Swap bounding box coordinates
    final_lat_min = if_else(action == "SWAP_AND_KEEP", longitude_min, latitude_min),
    final_lat_max = if_else(action == "SWAP_AND_KEEP", longitude_max, latitude_max),
    final_lon_min = if_else(action == "SWAP_AND_KEEP", latitude_min, longitude_min),
    final_lon_max = if_else(action == "SWAP_AND_KEEP", latitude_max, longitude_max)
  ) 
# %>%
#   separate(obs_dt, into = c("date", "time"), sep = " ")

# Pull EBD Columns ----
dbExecute(con, 
          "CREATE TEMP TABLE clean_ids (sampling_event_identifier text) ON COMMIT DROP")
dbWriteTable(con, 
             "clean_ids", 
             data.frame(
               sampling_event_identifier = 
                 centroids_sanitized$checklist_id), 
             append = TRUE, row.names = FALSE)

qry <- glue('
SELECT DISTINCT
    e."LOCALITY.ID",
    e."LOCALITY.TYPE",
    e."STATE",
    e."COUNTY",
    e."LATITUDE",
    e."LONGITUDE",
    e."DURATION.MINUTES",
    e."EFFORT.DISTANCE.KM",
    e."SAMPLING.EVENT.IDENTIFIER"
FROM ebd e
INNER JOIN clean_ids t
ON e."SAMPLING.EVENT.IDENTIFIER" =
   t.sampling_event_identifier
')

ebd_columns <- dbGetQuery(con,
                       qry)


centroids_sanitized_ebd_col <- centroids_sanitized %>%
  left_join(ebd_columns, 
            by = join_by(checklist_id == SAMPLING.EVENT.IDENTIFIER)
  )


saveRDS(centroids_sanitized_ebd_col, file = "00_data/centroids_sanitized_ebd_col_wo_dist.rds")


centroids_sanitized_ebd_col$dist_meters <- compute_bbox_distance(centroids_sanitized_ebd_col,
                                                                 "final_lon_min",
                                                                 "final_lon_max",
                                                                 "final_lat_min",
                                                                 "final_lat_max",
                                                                 "LONGITUDE",
                                                                 "LATITUDE")


saveRDS(centroids_sanitized_ebd_col, file = "00_data/centroids_sanitized_ebd_col_dist.rds")

# Final Scoring and Output ----
final_output <- centroids_sanitized_ebd_col %>%
  mutate(
    location_score = calculate_location_score(dist_meters/1000)
  ) %>%
  select(
    checklist_id, obs_dt, 
    longitude_min = final_lon_min, longitude_max = final_lon_max, 
    latitude_min = final_lat_min, latitude_max = final_lat_max, 
    centroid_longitude = final_lon_min, centroid_latitude = final_lat_min,
    location_score
  )

write.csv(final_output, "00_data/centroids_sanitized_final.csv", row.names = FALSE)

saveRDS(final_output, file = "00_data/centroids_sanitized_final.rds")

