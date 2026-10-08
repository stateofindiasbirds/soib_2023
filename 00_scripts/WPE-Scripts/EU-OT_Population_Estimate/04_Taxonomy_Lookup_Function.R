library(dplyr)
library(stringr)

source("config.R")
names_lookup <- read.csv(taxonomy_mapping, header = T, 
                         stringsAsFactors = F, na.strings = c(""," ",NA))

# 1. Clean the mapping table
names_lookup <- names_lookup %>%
  mutate(
    across(
      everything(),
      ~ str_to_lower(
        str_trim(.x)
      )
    )
  )

# Taxonomy lookup function
get_species_variants <- function(species_name,
                                 mapping_df) {
  
  # Clean input
  species_name <- str_to_lower(
    str_trim(species_name)
  )
  
  # Find matching rows
  matched_rows <- mapping_df %>%
    
    filter(
      eBird.English.Name.2022 == species_name |
        eBird.Scientific.Name.2022 == species_name |
        BLI.Common.Name == species_name |
        BLI.Scientific.Name == species_name |
        India.Checklist.Common.Name == species_name |
        India.Checklist.Scientific.Name == species_name
    )
  
  # Extract ALL variants
  variants <- unique(
    c(
      matched_rows$eBird.English.Name.2022,
      matched_rows$eBird.Scientific.Name.2022,
      matched_rows$BLI.Common.Name,
      matched_rows$BLI.Scientific.Name,
      matched_rows$India.Checklist.Common.Name,
      matched_rows$India.Checklist.Scientific.Name
    )
  )
  
  # Remove missing values
  variants <- variants[
    !is.na(variants)
  ]
  
  return(variants)
}
