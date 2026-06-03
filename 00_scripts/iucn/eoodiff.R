library(dplyr)

# Sample dataframe
# df <- data.frame(Species, EOOStartYear, EOOEndYear, LikelyEOO)
resultspath <- "01_analyses_full/results"

calculate_eoo_diff <- function(df) {
  
  # Process each species
  df_processed <- df %>%
    group_by(Species) %>%
    arrange(Species, EOOStartYear) %>%  # Sort by EOOStartYear for each species
    mutate(
      # Create a time band difference text (e.g., 2000-2007 to 2008-2022)
      YearBand = paste0(EOOStartYear, "-", EOOEndYear),
      NextYearBand = lead(paste0(EOOStartYear, "-", EOOEndYear), 1),
      EOOYearBandChange = paste(YearBand, "to", NextYearBand),
      
      # Calculate the difference between consecutive LikelyEOO values
      EOOChange = lead(LikelyEOO) - LikelyEOO,
      
      # Calculate percentage change with respect to the previous LikelyEOO
      EOOChangePercent = (lead(LikelyEOO) - LikelyEOO) / LikelyEOO * 100
    ) %>%
    
    # Filter out rows where there is no next year band to compare
    filter(!is.na(NextYearBand)) %>%
    select(Species, EOOYearBandChange, EOOChange, EOOChangePercent)
  
  return(df_processed)
}
df <- readRDS(file.path(resultspath, "eoo.RDS"))

# Example usage with the original dataframe (df)
result <- calculate_eoo_diff(df)

declingspecies <- result %>% filter (EOOChangePercent < 0) 

saveRDS(declingspecies, file.path(resultspath, "eoodiff.RDS"))