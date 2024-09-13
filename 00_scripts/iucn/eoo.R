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

obsv <- obsv %>%
  left_join (species_attr, by = c("COMMON.NAME" = "Species")) %>%
  filter( EFFORT.DISTANCE.KM < MaxChecklistDistanceforEOO,
          COMMON.NAME %in% species,
          SOIB_YEAR >= lastYearforEOOCalculation,
          DAY <= SeasonEndDay | DAY >= SeasonStartDay)

obsv <- obsv %>% select (COMMON.NAME, SOIB_YEAR, geometry)


# Calculates EOO of all species in data using YEAR1 to YEAR2 as filters
compute_eoo <- function (data, startYear, species, endYears) {

  # Filter observations based on year and group by species with more than 5 observations
  eoo <- data %>%
          filter(SOIB_YEAR >= startYear) %>%
          inner_join (tibble(Species = species, endYear = endYears), by = c("COMMON.NAME" = "Species")) %>%
          filter(SOIB_YEAR <= endYear) %>%
          group_by(COMMON.NAME) %>%
          filter(n() > 5) %>%
          summarize(
            mcp = st_convex_hull(st_union(geometry)),  # Calculate MCP only once
            area_km2 = as.numeric(st_area(st_transform(mcp, crs = 32643)) / 1e6)  # Project and calculate area
          ) %>%
          select(COMMON.NAME, area_km2, mcp)  # Select relevant columns
  
  # Rename columns
  colnames(eoo) <- c("Species", "LikelyEOO", "EOOMap")
  
  print(startYear)
  return(eoo)
}

calculateEOOwithEndYear <- function (obsv, species, EOOEndYear_list)
{
  eoo <- setNames(
    lapply(species, function(s) list(
      EOOStartYear = lastYearforEOOCalculation,
      EOOEndYear = EOOEndYear_list,
      LikelyEOO = 0,
      MaxEOO = 0,
      EOOMap = NULL
    )),
    species
  )
  
  endYear <- EOOEndYear_list %>% max()
  
  # Iterate from 2022 to 2000
  for (year in endYear:lastYearforEOOCalculation) {
    # Compute EOO for the current year
    eoo_current_year <- compute_eoo(obsv, year, species, EOOEndYear_list)
    
    # Check for each species
    for (species_name in eoo_current_year$Species) {
      # Get EOO values for the current and previous years
      previous_eoo <- eoo[[species_name]]$LikelyEOO
      current_eoo <- eoo_current_year %>% filter(Species == species_name) %>% pull(LikelyEOO)
      if ( current_eoo > 0 ) { #If species no longer there, it will be empty. Just protection
        # Update eoo if the EOO value has changed
        if( current_eoo > previous_eoo) {
          eoo[[species_name]]$LikelyEOO <- current_eoo
          eoo[[species_name]]$EOOStartYear <- year
          eoo[[species_name]]$EOOEndYear <- tibble(Species = species, endYear = EOOEndYear_list) %>% filter (Species == species_name) %>% pull (endYear)
          eoo[[species_name]]$EOOMap <- eoo_current_year %>% filter(Species == species_name) %>% pull(EOOMap)
          print(paste(species_name, round(eoo[[species_name]]$LikelyEOO,0), eoo[[species_name]]$EOOStartYear, eoo[[species_name]]$EOOEndYear))
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


  eoo_df <- bind_rows(
    lapply(names(eoo), function(species_name) {
      tibble(
        Species = species_name,
        EOOStartYear = eoo[[species_name]]$EOOStartYear,
        EOOEndYear = eoo[[species_name]]$EOOEndYear,
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
  eoo_df <- eoo_df %>% filter (LikelyEOO > 0)
  return (eoo_df)
}


eoo_agg_df <- tibble(
  Species = character(),        # character type
  EOOStartYear = integer(),     # integer type
  LikelyEOO = numeric(),        # numeric type
  MaxEOO = numeric(),           # numeric type
  EOOMap = list()               # list type
)

endYear <- obsv$SOIB_YEAR %>% unique() %>% max()
species_list <- species
EOOEndYear_list <- rep ( endYear, length(species))
iteration <- 1

repeat {
  
  print (iteration)
  eoo_df <-calculateEOOwithEndYear (obsv, species_list, EOOEndYear_list)
  
  if (nrow(eoo_df) == 0) { break;}
  
  eoo_agg_df <- eoo_df %>% bind_rows(eoo_agg_df)
  if (iteration == 1 ) { saveRDS("eoo_df.RDS")}
  
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


saveRDS(eoo_agg_df, "eoo_t.RDS")
