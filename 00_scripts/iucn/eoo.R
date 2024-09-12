species <- c (
  "Brahminy Kite",
  "White-browed Bulbul",
  "Yellow-browed Bulbul",
  "Malabar Gray Hornbill",
  "Lesser Coucal",
  "Oriental Scops-Owl",
  "Rufous-bellied Eagle",
  "Sanderling",
  "White-cheeked Barbet",
  "Malabar Barbet",
  "Bay-backed Shrike",
  "Jerdon's Baza",
  "Legge's Hawk-Eagle",
  "Lesser Fish-Eagle",
  "Spot-bellied Eagle-Owl",
  "Golden-headed Cisticola"
)


library(dplyr)
library(sf)
library(units)
library(magrittr)
source("config.R")

# Read the eBird data
obsv <- readRDS("ebd_sf.RDS")
species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)
#system.time({x <- compute_eoo(obsv, 2015)})

species_attr <- data.frame(
  Species = species,
  SeasonEndDay = 60,
  SeasonStartDay = 61
)

# Initialize the species_attr with default values
eoo <- setNames(
        lapply(species, function(s) list(
            EOOStartYear = lastYearforEOOCalculation,
            LikelyEOO = 0,
            MaxEOO = 0,
            EOOMap = NULL
  )),
  species
)

obsv <- obsv %>%
          left_join (species_attr, by = c("COMMON.NAME" = "Species")) %>%
              filter( EFFORT.DISTANCE.KM < MaxChecklistDistanceforEOO,
                      COMMON.NAME %in% species,
                      SOIB_YEAR >= lastYearforEOOCalculation,
                      DAY <= SeasonEndDay | DAY >= SeasonStartDay)

obsv <- obsv %>% select (COMMON.NAME, SOIB_YEAR, geometry)

# Calculates EOO of all species in data using YEAR1 to YEAR2 as filters
compute_eoo <- function (data, startYear, endYear) {

  # Filter observations based on year and group by species with more than 5 observations
  eoo <- data %>%
    filter(SOIB_YEAR >= startYear, SOIB_YEAR <= endYear) %>%
    group_by(COMMON.NAME) %>%
    filter(n() > 5) %>%
    summarize(
      mcp = st_convex_hull(st_union(geometry)),  # Calculate MCP only once
      area_km2 = as.numeric(st_area(st_transform(mcp, crs = 32643)) / 1e6)  # Project and calculate area
    ) %>%
    select(COMMON.NAME, area_km2, mcp)  # Select relevant columns
  
  # Rename columns
  colnames(eoo) <- c("Species", "LikelyEOO", "EOOMap")
  
  print(year)
  return(eoo)
}

calculateEOOwithEndYear <- function ()
{
  # Iterate from 2022 to 2000
  for (year in 2022:lastYearforEOOCalculation) {
    # Compute EOO for the current year
    eoo_current_year <- compute_eoo(obsv, year)
    
    # Check for each species
    for (species_name in eoo_current_year$Species) {
      # Get EOO values for the current and previous years
      previous_eoo <- eoo[[species_name]]$LikelyEOO
      current_eoo <- eoo_current_year %>% filter(Species == species_name) %>% pull(LikelyEOO)
      if (length(current_eoo) > 0) { #If species no longer there, it will be empty. Just protection
        # Update eoo if the EOO value has changed
        if( current_eoo != previous_eoo) {
          eoo[[species_name]]$LikelyEOO <- current_eoo
          eoo[[species_name]]$EOOStartYear <- year
          eoo[[species_name]]$EOOMap <- eoo_current_year %>% filter(Species == species_name) %>% pull(EOOMap)
        } 
        else {
          # Track if EOO is the same for 5 consecutive years
          consistent_years <- year - eoo[[species_name]]$EOOStartYear
      
          if (consistent_years >= (consistent_eoo_limit - 1)) { # 5 years total (including current year)
            # Update species_attr and remove observations for this species
            species_attr[species_attr$Species == species_name, "LikelyEOO"] <- current_eoo
            species_attr[species_attr$Species == species_name, "EOOStartYear"] <- year
            
            # Remove observations for this species
            obsv <- obsv %>% filter(COMMON.NAME != species_name)
          }
        }
      }
    }
  }

# For species remaining in obsv, assign, 2000

  eoo_df <- bind_rows(
    lapply(names(eoo), function(species_name) {
      tibble(
        Species = species_name,
        EOOStartYear = eoo[[species_name]]$EOOStartYear,
        LikelyEOO = eoo[[species_name]]$LikelyEOO,
        MaxEOO = eoo[[species_name]]$MaxEOO,
        EOOMap = list(eoo[[species_name]]$EOOMap)
      )
    })
  )
  
  # Ensure EOOMap has the correct class
  eoo_df$EOOMap <- lapply(eoo_df$EOOMap, function(x) {
    if (inherits(x, "list") && length(x) > 0 && inherits(x[[1]], "sfc")) {
      x[[1]]
    } else {
      x
    }
  })
}

eoo_df <- eoo_df %>% filter (EOOStartYear < 2023, LikelyEOO > 0)

saveRDS(eoo_df, "eoo.RDS")
