# -------------------------------
# CONFIG
# -------------------------------
input_csv  <- "iucn_assessments_full_output.csv"
output_csv <- "IUCN_assessments_Full_fully_flattened.csv"

# -------------------------------
# LIBRARIES
# -------------------------------
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# -------------------------------
# READ DATA
# -------------------------------
df <- read.csv(input_csv, stringsAsFactors = FALSE)

# -------------------------------
# SAFE JSON COLUMN DETECTION
# -------------------------------
is_json_column <- function(x) {
  if (!is.character(x)) return(FALSE)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(FALSE)
  any(str_detect(x, "^\\s*[\\{\\[]"))
}

json_cols <- names(df)[sapply(df, is_json_column)]
json_cols <- intersect(json_cols, names(df))

cat("JSON columns detected:\n")
print(json_cols)

# -------------------------------
# RECURSIVE FLATTENER (SAFE)
# -------------------------------
flatten_recursive <- function(x, parent = "") {
  
  out <- list()
  
  if (is.null(x)) {
    out[[parent]] <- NA
    return(out)
  }
  
  if (is.atomic(x)) {
    out[[parent]] <- paste(x, collapse = "; ")
    return(out)
  }
  
  if (is.list(x)) {
    
    # Object (named list)
    if (!is.null(names(x))) {
      for (n in names(x)) {
        key <- if (parent == "") n else paste(parent, n, sep = "_")
        out <- c(out, flatten_recursive(x[[n]], key))
      }
    } else {
      # Array (unnamed list)
      for (i in seq_along(x)) {
        key <- paste(parent, i, sep = "_")
        out <- c(out, flatten_recursive(x[[i]], key))
      }
    }
  }
  
  out
}

# -------------------------------
# FLATTEN SINGLE JSON CELL
# -------------------------------
flatten_json_safe <- function(json_text) {
  
  if (is.na(json_text) || json_text == "") {
    return(tibble())
  }
  
  parsed <- tryCatch(
    fromJSON(json_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(parsed)) {
    return(tibble())
  }
  
  flat <- flatten_recursive(parsed)
  
  if (length(flat) == 0) {
    return(tibble())
  }
  
  as_tibble(flat)
}

# -------------------------------
# FLATTEN EACH JSON COLUMN
# -------------------------------
flattened_columns <- lapply(json_cols, function(col) {
  
  cat("Flattening:", col, "\n")
  
  flat_rows <- lapply(df[[col]], flatten_json_safe)
  
  # bind_rows handles different schemas safely
  flat_df <- bind_rows(flat_rows)
  
  # Ensure row count consistency
  if (nrow(flat_df) < nrow(df)) {
    flat_df[(nrow(flat_df) + 1):nrow(df), ] <- NA
  }
  
  colnames(flat_df) <- paste(col, colnames(flat_df), sep = "_")
  
  flat_df
})

# -------------------------------
# REMOVE JSON COLUMNS
# -------------------------------
df_clean <- df %>% select(-all_of(json_cols))

# -------------------------------
# COMBINE & EXPORT
# -------------------------------
final_df <- bind_cols(df_clean, flattened_columns)

write.csv(final_df, output_csv, row.names = FALSE)

cat("\n Fully flattened CSV created successfully:\n", output_csv, "\n")
