
# Operation Table - Formula evaluation
evaluate_formula <- function(formula, eu_table, value_col) {
  
  for (i in 1:nrow(eu_table)) {
    formula <- str_replace_all(
      formula,
      paste0("\\b", eu_table$EU[i], "\\b"),
      as.character(eu_table[[value_col]][i])
    )
  }
  
  formula <- str_replace_all(formula, "SUM", "sum")
  formula <- str_replace_all(formula, "MAX", "max")
  formula <- str_replace_all(formula, "MIN", "min")
  formula <- str_replace_all(formula, "MEAN", "mean")
  
  eval(parse(text = formula))
}

# Final estimates
final_results <- operation_table %>%
  rowwise() %>%
  mutate(
    FINAL_MIN = evaluate_formula(OPERATION, eu_results, "ESTIMATE_MIN")
  )
