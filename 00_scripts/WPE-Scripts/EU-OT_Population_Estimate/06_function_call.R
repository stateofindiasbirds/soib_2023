# Read EU table
source("config.R")
library(googlesheets4)
gs4_auth()
estimation_units <- read_sheet(eut_and_ot, sheet = "estimation_units")
estimation_units <- estimation_units %>%
  mutate(
    COMMON.NAME = str_to_lower(str_trim(COMMON.NAME)),
    DATA.SOURCE = str_to_lower(str_trim(DATA.SOURCE))
  )

# Lookup table of functions
# STEP 1 — Define your function registry
function_registry <- list(
  sum_district_max = sum_district_max
#  occupancy_model = occupancy_model,
#  peak_detection = peak_detection,
#  simple_max = simple_max
)

# STEP 2 - Runner function
run_procedure <- function(row, data, function_registry, mapping_df) {
  func_name <- row$APPROACH
  
  # pick correct function safely
  func <- function_registry[[func_name]]
  if (is.null(func)) {
    stop(paste("No function defined for APPROACH:", func_name))
  }
  
  # call function
  result <- func(data, row, mapping_df)
  return(result)
}

# STEP 3 - Run all estimation units
results_list <- list()

for (i in 1:nrow(estimation_units)) {
  row <- estimation_units[i, ]
  res <- run_procedure(row, merged_data, function_registry, mapping_df = names_lookup)
  results_list[[i]] <- res
}

# STEP 4 - Convert results to table
eu_results <- bind_rows(results_list)

# STEP 5 - Write results back to estimation_units sheet

# Remove old estimate columns if they exist
estimation_units <- estimation_units %>%
  select(
    -any_of(c(
      "ESTIMATE.MIN",
      "ESTIMATE.MAX"
    )))

eu_sheet_updated <- estimation_units %>%
  
  left_join(
    eu_results %>%
      select(
        EU.NAME,
        ESTIMATE.MIN,
        ESTIMATE.MAX
      ),
    by = "EU.NAME"
  )

sheet_write(
  eu_sheet_updated,
  ss = eut_and_ot,
  sheet = "estimation_units"
)

