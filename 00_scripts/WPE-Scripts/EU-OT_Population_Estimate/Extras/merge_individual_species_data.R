library(dplyr)

# Folder containing CSV files
folder_path <- "D:/NCF - Bird Mon/SoIB - Wetland Population Estimate/Data and Codes - from Linux Server/Data/species_data_all/To merge"

# Get all CSV filenames
csv_files <- list.files(
  folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Read and merge
merged_data <- csv_files %>%
  lapply(read.csv, stringsAsFactors = FALSE, colClasses = "character") %>%
  bind_rows()

# Save merged file
write.csv(
  merged_data,
  "ebird_data.csv",
  row.names = FALSE
)
