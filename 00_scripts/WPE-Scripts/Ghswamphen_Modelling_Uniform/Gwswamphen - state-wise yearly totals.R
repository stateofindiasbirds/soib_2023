# ================================
# LOAD LIBRARIES
# ================================
library(dplyr)
library(sf)
library(geodata)

# ================================
# STEP 1: Get India states (GADM level 1)
# ================================
india_states <- gadm(country = "IND", level = 1, path = tempdir())

# Convert to sf
india_states <- st_as_sf(india_states)

# ================================
# STEP 2: Convert your data → sf
# ================================
points_sf <- st_as_sf(
  ghswamphen_aggcounts,
  coords = c("centroid_lon", "centroid_lat"),
  crs = 4326
)

# Match CRS
points_sf <- st_transform(points_sf, st_crs(india_states))

# ================================
# STEP 3: Spatial join (points → states)
# ================================
points_with_states <- st_join(points_sf, india_states)

# ================================
# STEP 4: Aggregate (STATE × YEAR)
# ================================
state_year_totals <- points_with_states %>%
  group_by(NAME_1, YEAR) %>%
  summarise(
    total_count = sum(OBSERVATION.COUNT, na.rm = TRUE),
    .groups = "drop"
  )

# ================================
# VIEW RESULT
# ================================
head(state_year_totals)
write.csv(state_year_totals, "State-wise yearly total count.csv")
