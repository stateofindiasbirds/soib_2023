parse_population <- function(x) {
  
  # --------------------------------------------------
  # Initialise output
  # --------------------------------------------------
  
  result <- data.frame(
    GlobalMinPopulation = NA_real_,
    GlobalMaxPopulation = NA_real_,
    GlobalBestPopulation = NA_real_,
    GlobalBestPopulationMin = NA_real_,
    GlobalBestPopulationMax = NA_real_
  )
  
  # --------------------------------------------------
  # Clean input
  # --------------------------------------------------
  
  x <- trimws(x)
  
  # --------------------------------------------------
  # Unknown / missing
  # --------------------------------------------------
  
  if (is.na(x) || x == "" || toupper(x) == "U") {
    return(result)
  }
  
  # --------------------------------------------------
  # Split at comma
  # --------------------------------------------------
  
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  
  # --------------------------------------------------
  # No comma
  # --------------------------------------------------
  
  if (length(parts) == 1) {
    
    # ----------------------------------------------
    # Single value
    # ----------------------------------------------
    
    if (grepl("^\\d+$", parts[1])) {
      
      value <- as.numeric(parts[1])
      
      result$GlobalMinPopulation <- value
      result$GlobalMaxPopulation <- value
      result$GlobalBestPopulation <- value
      
      # ----------------------------------------------
      # Min-max range
      # ----------------------------------------------
      
    } else if (grepl(
      "^\\d+\\s*[-–—]\\s*\\d+$",
      parts[1]
    )) {
      
      nums <- as.numeric(
        unlist(
          strsplit(
            parts[1],
            "\\s*[-–—]\\s*"
          )
        )
      )
      
      result$GlobalMinPopulation <- nums[1]
      result$GlobalMaxPopulation <- nums[2]
      
      # Best estimate = geometric mean
      result$GlobalBestPopulation <-
        sqrt(nums[1] * nums[2])
    }
    
    # --------------------------------------------------
    # Comma: min-max + best estimate/range
    # --------------------------------------------------
    
  } else if (length(parts) == 2) {
    
    # ----------------------------------------------
    # First part must be min-max
    # ----------------------------------------------
    
    if (grepl(
      "^\\d+\\s*[-–—]\\s*\\d+$",
      parts[1]
    )) {
      
      global_range <- as.numeric(
        unlist(
          strsplit(
            parts[1],
            "\\s*[-–—]\\s*"
          )
        )
      )
      
      result$GlobalMinPopulation <- global_range[1]
      result$GlobalMaxPopulation <- global_range[2]
      
      # --------------------------------------------
      # Second part = single best estimate
      # --------------------------------------------
      
      if (grepl("^\\d+$", parts[2])) {
        
        result$GlobalBestPopulation <-
          as.numeric(parts[2])
        
        # --------------------------------------------
        # Second part = best-estimate range
        # --------------------------------------------
        
      } else if (grepl(
        "^\\d+\\s*[-–—]\\s*\\d+$",
        parts[2]
      )) {
        
        best_range <- as.numeric(
          unlist(
            strsplit(
              parts[2],
              "\\s*[-–—]\\s*"
            )
          )
        )
        
        result$GlobalBestPopulationMin <-
          best_range[1]
        
        result$GlobalBestPopulationMax <-
          best_range[2]
        
        # Best estimate = geometric mean
        result$GlobalBestPopulation <-
          sqrt(
            best_range[1] *
              best_range[2]
          )
      }
    }
  }
  
  return(result)
}