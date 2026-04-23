#### eBird Status Data Products Applications
library(dplyr)
library(ebirdst)
library(fields)
library(ggplot2)
library(lubridate)
library(rnaturalearth)
library(scico)
library(sf)
library(terra)
library(tidyr)
extract <- terra::extract

# weekly, 9km res, median relative abundance
abd_seasonal <- load_raster("purswa3", product = "abundance", metric = "mean", period = "seasonal", 
                      resolution = "9km", path = data_path)
abd_seasonal
plot(abd_seasonal, axes = FALSE)

# region boundary (India)
region_boundary <- ne_countries(country = "India", returnclass = "sf")

# project boundary to match raster data
region_boundary_proj <- st_transform(region_boundary, st_crs(abd_seasonal))

# crop and mask raster to India
abd_seasonal_mask <- crop(abd_seasonal, vect(region_boundary_proj)) |> 
  mask(vect(region_boundary_proj))

# map the cropped data
plot(abd_seasonal_mask, axes = FALSE)


# find the centroid of the region
region_centroid <- region_boundary |> 
  st_geometry() |> 
  st_transform(crs = 4326) |> 
  st_centroid() |> 
  st_coordinates() |> 
  round(1)

# define projection
crs_laea <- paste0("+proj=laea +lat_0=", region_centroid[2],
                   " +lon_0=", region_centroid[1])

# transform to the custom projection using nearest neighbor resampling
abd_seasonal_laea <- project(abd_seasonal_mask, crs_laea, method = "near") |> 
  # remove areas of the raster containing no data
  trim()

# map the cropped and projected data
plot(abd_seasonal_laea, axes = FALSE, breakby = "cases")

# quantiles of non-zero values
v <- values(abd_seasonal_laea, na.rm = TRUE, mat = FALSE)
v <- v[v > 0]
breaks <- quantile(v, seq(0, 1, by = 0.1))
# add a bin for 0
breaks <- c(0, breaks)

# status and trends palette
pal <- ebirdst_palettes(length(breaks) - 2)
# add a color for zero
pal <- c("#e6e6e6", pal)

# map using the quantile bins
plot(abd_seasonal_laea, breaks = breaks, col = pal, axes = FALSE)

# project boundary to match raster data
region_boundary_proj <- st_transform(region_boundary, st_crs(abd_median))
# extract values within region and calculate the mean
abd_median_region <- extract(abd_median, region_boundary_proj,
                             fun = "mean", na.rm = TRUE, ID = FALSE)
abd_lower_region <- extract(abd_lower, region_boundary_proj,
                            fun = "mean", na.rm = TRUE, ID = FALSE)
abd_upper_region <- extract(abd_upper, region_boundary_proj,
                            fun = "mean", na.rm = TRUE, ID = FALSE)

# transform to data frame format with rows corresponding to weeks
chronology <- data.frame(week = as.Date(names(abd_median)),
                         median = as.numeric(abd_median_region),
                         lower = as.numeric(abd_lower_region),
                         upper = as.numeric(abd_upper_region))
ggplot(chronology) +
  aes(x = week, y = median) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line() +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Week", 
       y = "Mean relative abundance in India",
       title = "Chronology for Gray-headed Swamphen in India")
