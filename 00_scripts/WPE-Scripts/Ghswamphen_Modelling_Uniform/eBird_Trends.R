library(dplyr)
library(fields)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(terra)
library(ebirdst)

trends_runs <- ebirdst_runs |> 
  filter(has_trends) |> 
  select(species_code, common_name,
         trends_season, trends_region,
         trends_start_year, trends_end_year,
         trends_start_date, trends_end_date,
         rsquared, beta0, trends_version_year)
glimpse(trends_runs)

trends_runs |> 
  filter(common_name == "Canvasback") |> 
  select(trends_start_year, trends_end_year, 
         trends_start_date, trends_end_date)

ebirdst_download_trends("Gray-headed Swamphen")
trends_sagthr <- load_trends("Gray-headed Swamphen")

trends_runs |> 
  filter(common_name == "Gray-headed Swamphen") |> 
  select(trends_start_year, trends_end_year,
         trends_start_date, trends_end_date)

trends_sf <- st_as_sf(trends_sagthr, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)
print(trends_sf)
