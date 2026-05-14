############################################################
# 0. LOAD LIBRARIES
############################################################
library(dplyr)
library(sf)
library(ggplot2)
library(ebirdst)

############################################################
# 1. LOAD YOUR GOI INDIA MAP
############################################################
load("maps_sf.RData")
load("maps.RData")
load("map_DEM.RData")

# 👉 IMPORTANT: pick the correct object
# Based on your workspace, this is most likely:
india <- india_buff_sf   # change if needed

############################################################
# 2. CLEAN GEOMETRY
############################################################
india <- st_make_valid(india)

############################################################
# 3. LOAD EBIRD RANGE
############################################################
ranges <- load_ranges("purswa3", resolution = "9km")
ranges <- st_make_valid(ranges)

############################################################
# 4. MATCH CRS
############################################################
india <- st_transform(india, st_crs(ranges))

############################################################
# 5. CLIP RANGE TO INDIA
############################################################
range_india <- st_intersection(ranges, india)

############################################################
# 6. CONVERT YOUR DATA → SF
############################################################
points_sf <- st_as_sf(
  ghswamphen_aggcounts,
  coords = c("centroid_lon", "centroid_lat"),
  crs = 4326,
  remove = FALSE
)

# Transform to match CRS
points_sf <- st_transform(points_sf, st_crs(ranges))

############################################################
# 7. GET UNIQUE YEARS
############################################################
years <- sort(unique(points_sf$YEAR))

############################################################
# 8. CREATE OUTPUT FOLDER
############################################################
dir.create("yearly_maps", showWarnings = FALSE)

############################################################
# 9. LOOP OVER EACH YEAR AND SAVE MAP
############################################################
for (yr in years) {
  
  cat("Processing year:", yr, "\n")
  
  # Subset data for that year
  pts_year <- points_sf %>% filter(YEAR == yr)
  
  # OPTIONAL: filter points within range
  pts_year_in_range <- st_intersection(pts_year, range_india)
  
  ##########################################################
  # PLOT
  ##########################################################
  p <- ggplot() +
    
    # India boundary
    geom_sf(data = india,
            fill = "grey95",
            color = "black",
            linewidth = 0.4) +
    
    # eBird range
    geom_sf(data = range_india,
            fill = "#2C7FB8",
            color = NA,
            alpha = 0.5) +
    
    # All points
    geom_sf(data = pts_year,
            color = "red",
            size = 1.0,
            alpha = 0.5) +
    
    labs(
      title = paste("eBird Range + Aggregated Counts", yr),
      subtitle = unique(ranges$common_name),
    ) +
    
    theme_minimal(base_size = 14) +
    
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  ##########################################################
  # SAVE FILE
  ##########################################################
  ggsave(
    filename = paste0("yearly_maps/swamphen_", yr, ".png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
}
