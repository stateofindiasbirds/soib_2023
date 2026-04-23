# Lookup table of functions

# STEP 1 — Define your function registry
function_registry <- list(
  sum_district_max = sum_district_max
#  occupancy_model = occupancy_model,
#  peak_detection = peak_detection,
#  simple_max = simple_max
)

# STEP 2 - Runner function
run_procedure <- function(row, data, function_registry) {
  func_name <- row$APPROACH
  
  # pick correct function safely
  func <- function_registry[[func_name]]
  if (is.null(func)) {
    stop(paste("No function defined for APPROACH:", func_name))
  }
  
  # call function
  result <- func(data, row)
  return(result)
}

# STEP 3 - Run all estimation units
results_list <- list()

for (i in 1:nrow(estimation_units)) {
  row <- estimation_units[i, ]
  res <- run_procedure(row, species_data, function_registry)
  results_list[[i]] <- res
}

# STEP 4 - Convert results to table
eu_results <- bind_rows(results_list)
