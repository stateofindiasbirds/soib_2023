library(terra)
library(sf)
library(ebirdst)

# Your raster is: Equal Earth (EPSG:8857)
# But your points are almost certainly:WGS84 (EPSG:4326)

# 1. Load a raster from the download (e.g., abundance.tif)
#r <- rast("purswa3_abundance_seasonal_max_9km_2023.tif")
r <- rast("purswa3_abundance_seasonal_max_3km_2023.tif")
# 2. Convert to spatial points in correct original CRS
my_data <- vect(ghswamphen_clean, geom = c("LONGITUDE", "LATITUDE"), crs = "EPSG:4326")
# 3. Reproject to match raster
my_data <- project(my_data, crs(r))
# 4. Assign raster cell IDs
ghswamphen_clean$cell_id <- cellFromXY(r, crds(my_data))
# 5. Aggregate per grid
ghswamphen_max <- ghswamphen_clean %>%
  #filter(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>%
  group_by(cell_id, YEAR) %>%
  slice_max(order_by = OBSERVATION.COUNT, n = 1, with_ties = FALSE) %>%
  ungroup()
# 6. Add centroid coordinates from raster (not lat, long)
centroids <- xyFromCell(r, ghswamphen_max$cell_id)
# 7. Attach centroid to your data (not lat, long)
ghswamphen_aggcounts <- ghswamphen_max %>%
  mutate(
    centroid_x = centroids[,1],
    centroid_y = centroids[,2]
  )
# 8. Convert centroid x, y to lat, long
centroid_points <- vect(centroids, crs = crs(r))
centroid_ll <- project(centroid_points, "EPSG:4326")
ll_coords <- crds(centroid_ll)
ghswamphen_aggcounts <- ghswamphen_aggcounts %>%
  mutate(
    centroid_lon = ll_coords[,1],
    centroid_lat = ll_coords[,2]
  )
######################## Minimum Count ##########################
monthly_totals <- ghswamphen_aggcounts %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    total_count = sum(OBSERVATION.COUNT, na.rm = TRUE),
    .groups = "drop"
  )

yearly_totals <- ghswamphen_aggcounts %>%
  group_by(YEAR) %>%
  summarise(
    total_count = sum(OBSERVATION.COUNT, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(monthly_totals, "monthly_totals_9x9.csv")
write.csv(yearly_totals, "yearly_totals_9x9.csv")

##########################################################################
# Checks to debug issues with the spatial data
# 1. Check CRS
crs(my_data)
crs(r)
# 2. Check coordinate values
head(crds(my_data))
# 3. Visual check
plot(r)
points(my_data, col = "red")

# Convert raster cells to polygons (this becomes your grid)
grid <- as.polygons(r)
# Convert to sf
grid_sf <- st_as_sf(grid)

plot(grid_sf)
india <- rnaturalearth::ne_countries(country = "India", returnclass = "sf")

grid_india <- st_intersection(grid_sf, india)
