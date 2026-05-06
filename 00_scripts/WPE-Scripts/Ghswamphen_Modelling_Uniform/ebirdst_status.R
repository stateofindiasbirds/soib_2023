if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("ebird/ebirdst")

set_ebirdst_access_key("4opllaim92nh")

library(dplyr)
library(sf)
library(terra)
library(ebirdst)

# download a simplified example dataset for Yellow-bellied Sapsucker in Michigan
ebirdst_download_status(species = "Gray-headed Swamphen", download_all = TRUE)
glimpse(ebirdst_runs)
# to see which directory the data got downloaded in:
ebirdst_data_dir()
# create the directory where you want to store the status data
dir.create("/home/subhasmitap/Documents/Data/species_data_all/status_data/Gray-headed_Swamphen", recursive = TRUE)
library(usethis)

# open .Renviron
edit_r_environ()
# then add this line in the file:
# EBIRDST_DATA_DIR=/home/subhasmitap/Documents/Data/species_data_all/status_data/Gray-headed_Swamphen

library(ggplot2)

ggplot(map_data) +
  geom_sf(aes(fill = Count), color = "grey40", size = 0.1) +
  scale_fill_viridis_c(na.value = "white") +
  theme_minimal() +
  labs(
    fill = "Swamphen Count",
    title = "Distribution of Gray-headed Swamphen in India"
  )

ebirdst_runs |> 
  filter(species_code == "purswa3") |> 
  glimpse()

# define path once
data_path <- "/home/subhasmitap/Documents/Data/species_data_all/status_data/Gray-headed_Swamphen"
data_path <- "/home/subhasmitap/Documents/Data/species_data_all/status_data/Gray-headed_Swamphen/ebirdst/2023"
data_path <- ebirdst_data_dir()

# weekly, 9km res, median proportion of population
prop_pop_lr <- load_raster("purswa3", product = "proportion-population", 
                           resolution = "9km", path = data_path)

# weekly, 9km res, median relative abundance
abd_median <- load_raster("purswa3", product = "abundance", 
                      resolution = "9km", path = data_path)
# weekly, 9km res, abundance confidence intervals
abd_lower <- load_raster("purswa3", product = "abundance", metric = "lower", 
                         resolution = "9km", path = data_path)
abd_upper <- load_raster("purswa3", product = "abundance", metric = "upper", 
                         resolution = "9km", path = data_path)

plot(abd_median[[1]])
as.Date(names(abd_median))

# seasonal, 9km res, mean relative abundance
abd_seasonal_mean <- load_raster("purswa3", product = "abundance", 
                                 period = "seasonal", metric = "mean", 
                                 resolution = "9km", path = data_path)
# season that each layer corresponds to
names(abd_seasonal_mean)

# full year, 9km res, maximum relative abundance
# Full-year products are not available for residents, use period = 'seasonal' instead
abd_fy_max <- load_raster("purswa3", product = "abundance", 
                          period = "full-year", metric = "max", 
                          resolution = "9km", path = data_path)


# seasonal, 27km res, smoothed ranges
ranges <- load_ranges("purswa3", resolution = "9km", path = data_path)
ranges
plot(ranges$geom)
# Clip abundance rasters
abd_range <- mask(abd_lr, vect(ranges))
# Calculate population inside the range
global(abd_range, "sum")


# Calculate the total geographic range area (km²) of the Gray-headed Swamphen from this polygon
library(sf)
# range area only inside India
range_india <- st_intersection(ranges, india)
# Reproject to an equal-area coordinate system
ranges_proj <- st_transform(range_india, 6933)   # project to equal-area CRS
# Calculate polygon area and Convert to km²
range_area_km2 <- sum(st_area(ranges_proj)) / 1e6
range_area_km2

regional <- load_regional_stats("purswa3", path = data_path)
glimpse(regional)

pr_auc <- load_ppm("purswa3", ppm = "occ_pr_auc_normalized")
print(pr_auc)
plot(trim(pr_auc[[1]]))


