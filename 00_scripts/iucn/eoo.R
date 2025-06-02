# This list of specie is used for testing. It can be overridden for all species
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

library(tidyr)
library(dplyr)
library(sf)
library(units)
library(magrittr)
library(multidplyr)
source("config.R")

#parallel::detectCores()
# Read the eBird data
obsv <- readRDS("ebd_sf.RDS")
species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)

#How to calculate time of a function
#system.time({x <- compute_eoo(obsv, 2015)})

# This table will be filled from the seasonal data of SoIB. Not done right now
species_attr <- data.frame(
  Species = species,
  SeasonEndDay = 60,
  SeasonStartDay = 61
)

obsv <- obsv %>%
  left_join (species_attr, by = c("COMMON.NAME" = "Species")) %>%
  filter( EFFORT.DISTANCE.KM < MaxChecklistDistanceforEOO,
          COMMON.NAME %in% species,
          SOIB_YEAR >= lastYearforEOOCalculation,
          DAY <= SeasonEndDay | DAY >= SeasonStartDay)

obsv <- obsv %>% select (COMMON.NAME, SOIB_YEAR, geometry)

# Calculates EOO of all species in data using one start Year and species specific end years as filters
compute_eoo_parallel <- function(data, years, radius, pointsOnCircle, noCores) {
  minStartYear <- min(years$startYear)
  maxEndYear <- max(years$endYear)
  
  cl <- new_cluster(noCores)
  cluster_library(cl, "dplyr")
  cluster_library(cl, "sf")
  # Filter observations based on year and group by species with more than 5 observations
  eoo <- data %>%
    filter(SOIB_YEAR >= minStartYear, SOIB_YEAR <= maxEndYear) %>%
    inner_join(years, by = c("COMMON.NAME" = "Species")) %>%
    filter(SOIB_YEAR >= startYear, SOIB_YEAR <= endYear) %>%
    group_by(COMMON.NAME) %>%
    filter(n() > 5) %>%
    partition(cluster = cl) %>%
    summarize(
      # Calculate MCP and area_km2 when radius is 0
      multipolygons = if (radius == 0) {
                          st_union(geometry)  # Compute MCP
                      } else {
                        union_result <- st_union(st_buffer(geometry, dist = radius * 1000, nQuadSegs = pointsOnCircle))
                        union_result[st_geometry_type(union_result) == "MULTIPOLYGON"] #Sometimes, it can return MULTILINESTRING
                      },
      mcp = st_convex_hull(st_make_valid(multipolygons)),
      area_km2 = as.numeric(st_area(st_transform(mcp, crs = 32643)) / 1e6)  
    ) %>%
    collect() %>%
    as.data.frame() %>%
    select(COMMON.NAME, area_km2, mcp)  # Select relevant columns
  
  # Rename columns
  colnames(eoo) <- c("Species", "EOO", "EOOMap")
  
  return(eoo)
}

# Calculates EOO of all species in data using one start Year and species specific end years as filters
compute_eoo <- function(data, years, radius, pointsOnCircle) {
  minStartYear <- min(years$startYear)
  maxEndYear <- max(years$endYear)
  
  # Filter observations based on year and group by species with more than 5 observations
  eoo <- data %>%
    filter(SOIB_YEAR >= minStartYear, SOIB_YEAR <= maxEndYear) %>%
    inner_join(years, by = c("COMMON.NAME" = "Species")) %>%
    filter(SOIB_YEAR >= startYear, SOIB_YEAR <= endYear) %>%
    group_by(COMMON.NAME) %>%
    filter(n() > 5) %>%
    summarize(
      # Calculate MCP and area_km2 when radius is 0
      multipolygons = if (radius == 0) {
                          st_union(geometry)  # Compute MCP
                        } else {
                          union_result <- st_union(st_buffer(geometry, dist = radius * 1000, nQuadSegs = pointsOnCircle))
                          union_result[st_geometry_type(union_result) == "MULTIPOLYGON"] #Sometimes, it can return MULTILINESTRING
                        },
      mcp = st_convex_hull(st_make_valid(multipolygons)),
      area_km2 = as.numeric(st_area(st_transform(mcp, crs = 32643)) / 1e6)  
    ) %>% 
    as.data.frame() %>%
    select(COMMON.NAME, area_km2, mcp)  # Select relevant columns
  
  # Rename columns
  colnames(eoo) <- c("Species", "EOO", "EOOMap")

  return(eoo)
}
# Calculates EOO of all species in data by iterating back to get a start year while using species specific end years as filters
calculateEOOwithEndYear <- function (obsv, species, EOOEndYear_list)
{
  # Named array for easy manipulation
  eoo <- setNames(
    lapply(species, function(s) list(
      EOOStartYear = lastYearforEOOCalculation,
      EOOEndYear = EOOEndYear_list,
      EOO = 0,
      EOOMap = NULL
    )),
    species
  )
  
  endYear <- EOOEndYear_list %>% max()
  
  # Iterate from 2022 to 2000
  for (year in endYear:lastYearforEOOCalculation) {
    # Compute EOO for the current year
    years <- tibble (Species = species, startYear = rep(year, length(species)), endYear = EOOEndYear_list)
    eoo_current_year <- tryCatch({ compute_eoo(obsv, years, 0, 0)},
                                      error = function (e) {
                                      print (e)
                                    # Return an empty dataframe
                                      data.frame (Species = character(), EOO = numeric(),EOOMap = list())
                                })
#    eoo_current_year <- compute_eoo_parallel(obsv, years, 0, 0,3)
    # Check for each species
    for (species_name in eoo_current_year$Species) {
      # Get EOO values for the current and previous years
      previous_eoo <- eoo[[species_name]]$EOO
      current_eoo <- eoo_current_year %>% filter(Species == species_name) %>% pull(EOO)
      if ( current_eoo > 0 ) { #If species no longer there, it will be empty. Just protection
        # Update eoo if the EOO value has changed
        if( current_eoo > previous_eoo) {
          eoo[[species_name]]$EOO <- current_eoo
          eoo[[species_name]]$EOOStartYear <- year
          eoo[[species_name]]$EOOEndYear <- tibble(Species = species, endYear = EOOEndYear_list) %>% filter (Species == species_name) %>% pull (endYear)
          eoo[[species_name]]$EOOMap <- eoo_current_year %>% filter(Species == species_name) %>% pull(EOOMap)
          print(paste(species_name, round(eoo[[species_name]]$EOO,0), eoo[[species_name]]$EOOStartYear, eoo[[species_name]]$EOOEndYear))
        } 
        else {
          # Track if EOO is the same for 5 consecutive years
          consistent_years <- eoo[[species_name]]$EOOStartYear - year

          if (consistent_years >= (consistent_eoo_limit - 1)) { # 5 years total (including current year)
            # Remove observations for this species
            obsv <- obsv %>% filter(COMMON.NAME != species_name)
          }
        }
      }
    }
  }


  # Convert named array to a dataframe
  eoo_df <- bind_rows(
    lapply(names(eoo), function(species_name) {
      tibble(
        Species = species_name,
        EOOStartYear = eoo[[species_name]]$EOOStartYear,
        EOOEndYear = eoo[[species_name]]$EOOEndYear,
        LikelyEOO = eoo[[species_name]]$EOO,
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
  eoo_df <- eoo_df %>% filter (LikelyEOO > 0)
  return (eoo_df)
}


# Main function
eoo_agg_df <- tibble(
  Species = character(),        # character type
  EOOStartYear = integer(),     # integer type
  LikelyEOO = numeric(),        # numeric type
  EOOMap = list()               # list type
)

endYear <- obsv$SOIB_YEAR %>% unique() %>% max()
species_list <- species
EOOEndYear_list <- rep ( endYear, length(species)) # Start with uniform endYear (which is the highest)
iteration <- 1

# Repeat until no data is present or all species have reached lastYearforEOOCalculation
# The first EOO is the latest EOO. Remaining are historical EOO for calculationg EOO difference
repeat {
  
  print (iteration)
  eoo_df <-calculateEOOwithEndYear (obsv, species_list, EOOEndYear_list)
  
  if (nrow(eoo_df) == 0) { break;}
  
  eoo_agg_df <- eoo_df %>% bind_rows(eoo_agg_df)
  if (iteration == 1 ) { saveRDS(eoo_agg_df, "eoo_df.RDS")} #We need this file for AOO calculation. Latest EOO.
  
  eoo_summary <- eoo_agg_df %>%
                  group_by(Species) %>%
                  summarize(EOOStartYear = min(EOOStartYear, na.rm = TRUE) - 1)  # Get the minimum EOOStartYear for each species and one before is the new EndYear
  
  # Remove observations for each species that is more than on equal to EOOStartYear
  obsv <- obsv %>% 
              left_join (eoo_summary, by = c("COMMON.NAME" = "Species")) %>% 
              filter (SOIB_YEAR < EOOStartYear) %>%
              select (COMMON.NAME, SOIB_YEAR, geometry)
  
  
  # Remove species observations whose EOOStartYear is lastYearforEOOCalculation
  removespecies <- eoo_agg_df %>% filter (EOOStartYear == lastYearforEOOCalculation) %>% pull(Species)
  obsv <- obsv %>% filter(!(COMMON.NAME %in% removespecies))
  
  eoo_summary <- eoo_agg_df %>%
                  filter(!(Species %in% removespecies)) %>%
                  group_by(Species) %>%
                  summarize(EOOStartYear = min(EOOStartYear, na.rm = TRUE) - 1)  # Get the minimum EOOStartYear for each species and one before is the new EndYear
  
  species_list <- eoo_summary$Species
  EOOEndYear_list <- eoo_summary$EOOStartYear
  
  if ( (max(EOOEndYear_list) <= lastYearforEOOCalculation) || (nrow(obsv) < 3))
  {
    break;
  }
}

years_df <- eoo_agg_df %>%
              group_by(Species) %>%
              summarize(
                startYear = max(EOOStartYear),  # Get the highest start year per species
                endYear = max(EOOEndYear)       # Get the highest end year per species
              ) %>%
              ungroup()  # Ensure the result is no longer grouped

saveRDS(eoo_agg_df %>% filter (EOOEndYear == eoo_agg_df$EOOEndYear %>% max()), "eoo.RDS")

# Read the eBird data
obsv <- readRDS("ebd_sf.RDS")
#species <- obsv$COMMON.NAME %>% unique() %>% grep(pattern = "[()/\\\\.]", value = TRUE, invert = TRUE)
#system.time({x <- compute_eoo(obsv, 2015)})

obsv <- obsv %>%
  left_join (species_attr, by = c("COMMON.NAME" = "Species")) %>%
  filter( EFFORT.DISTANCE.KM < MaxChecklistDistanceforEOO,
          COMMON.NAME %in% species,
          SOIB_YEAR >= lastYearforEOOCalculation,
          DAY <= SeasonEndDay | DAY >= SeasonStartDay)

obsv <- obsv %>% select (COMMON.NAME, SOIB_YEAR, geometry)


# Function to calculate max eoo
tryCatch({
          eoo_max <- compute_eoo(obsv, years_df, 10, 5) %>% as.data.frame() %>% select ("Species", "EOO")
          colnames(eoo_max) <- c("Species", "MaxEOO")
          eoo_agg_df <- eoo_agg_df %>% inner_join (eoo_max, by = c("Species"))
          },
          error = function (e) {
              print (e)
          })
# Save all EOO, including historical
saveRDS(eoo_agg_df, "eoo_t.RDS")

