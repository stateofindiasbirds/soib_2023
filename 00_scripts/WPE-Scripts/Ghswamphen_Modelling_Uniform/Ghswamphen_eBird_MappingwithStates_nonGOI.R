# ================================
# LOAD LIBRARIES
# ================================
library(dplyr)
library(sf)
library(ggplot2)
library(geodata)
library(ebirdst)

# ================================
# STEP 1: Load eBird range
# ================================
ranges <- load_ranges("purswa3", resolution = "9km")

# ================================
# STEP 2: Get India states
# ================================
india_states <- gadm(country = "IND", level = 1, path = tempdir())
india_states <- st_as_sf(india_states)

# ================================
# STEP 3: Match CRS
# ================================
india_states <- st_transform(india_states, st_crs(ranges))

# ================================
# STEP 4: Clip range to India
# ================================
range_india <- st_intersection(ranges, india_states) %>%
  st_make_valid()

# ================================
# STEP 5: Convert your data → sf
# ================================
points_sf <- st_as_sf(
  ghswamphen_aggcounts,
  coords = c("centroid_lon", "centroid_lat"),
  crs = 4326
)

points_sf <- st_transform(points_sf, st_crs(ranges))

# ================================
# STEP 6: Create output folder
# ================================
dir.create("yearly_maps", showWarnings = FALSE)

# ================================
# STEP 7: Loop over each year
# ================================
years <- sort(unique(points_sf$YEAR))

for (yr in years) {
  
  # Filter points for that year
  pts_year <- points_sf %>% filter(YEAR == yr)
  
  # Create plot
  p <- ggplot() +
    
    # State boundaries
    geom_sf(data = india_states,
            fill = "grey95",
            color = "black",
            linewidth = 0.3) +
    
    # Range map
    geom_sf(data = range_india,
            fill = "#2C7FB8",
            color = NA,
            alpha = 0.4) +
    
    # Points
    geom_sf(data = pts_year,
            color = "red",
            size = 1.5,
            alpha = 0.8) +
    
    labs(
      title = paste("Gray-headed Swamphen -", yr),
      subtitle = unique(ranges$common_name)
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  # Save each map separately
  ggsave(
    filename = paste0("yearly_maps/swamphen_map_", yr, ".png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
}
