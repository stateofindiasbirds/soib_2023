
estimation_units <- read_sheet(eut_and_ot, sheet = "estimation_units")
eu_results <- estimation_units
operation_table <- read_sheet(eut_and_ot, sheet = "operation_table")

# Clean operation_table
operation_table <- operation_table %>%
  mutate(across(
    c(OPERATION_MIN, OPERATION_MAX),
    ~ gsub("[\r\n\t\u00A0\u200B]", " ", .x)
  ))

#################################################
# Custom functions
#################################################

SUM <- function(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}
MAX <- function(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}
MIN <- function(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}
MEAN <- function(...) {
  x <- c(...)
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# Operation Table - Formula evaluation
evaluate_formula <- function(formula, eu_table, value_col) {
  
  # Sort EU names by decreasing length
  eu_table <- eu_table %>%
    arrange(desc(nchar(EU.NAME)))
  
  # Replace EU names with estimates
  for (i in 1:nrow(eu_table)) {
    
    replacement_value <- ifelse(
      is.na(eu_table[[value_col]][i]),
      "NA",
      as.character(eu_table[[value_col]][i])
    )
    
    # Replace safely using literal matching
    formula <- str_replace_all(
      formula,
      fixed(eu_table$EU.NAME[i]),
      replacement_value
    )
  }
  cat("Formula being evaluated:", formula, "\n")
  eval(parse(text = formula))
}

# Final estimates
final_results <- operation_table %>%
  rowwise() %>%
  mutate(
    FINAL_MIN = evaluate_formula(OPERATION_MIN, eu_results, "ESTIMATE.MIN"),
    FINAL_MAX = evaluate_formula(OPERATION_MAX, eu_results, "ESTIMATE.MAX")
  )

# Write back
sheet_write(
  final_results,
  ss = eut_and_ot,
  sheet = "operation_table"
)
