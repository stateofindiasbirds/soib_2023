# -------------------------------
# CONFIG
# -------------------------------
input_csv  <- "IUCN_assessments_objects_format.csv"
output_csv <- "IUCN_assessments_flattened.csv"

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
  
  # NULL → NA
  if (is.null(x)) {
    out[[parent]] <- NA
    return(out)
  }
  
  # Atomic → collapse
  if (is.atomic(x)) {
    out[[parent]] <- paste(x, collapse = "; ")
    return(out)
  }
  
  # Lists
  if (is.list(x)) {
    
    # ---- CASE 1: Array of objects (unnamed list of named lists)
    if (is.null(names(x)) && all(sapply(x, is.list))) {
      
      # Empty array
      if (length(x) == 0) {
        out[[parent]] <- NA
        return(out)
      }
      
      # Collect all field names across array
      fields <- unique(unlist(lapply(x, names)))
      
      for (f in fields) {
        vals <- sapply(x, function(obj) {
          v <- obj[[f]]
          if (is.null(v)) NA else paste(v, collapse=" ")
        })
        key <- paste(parent, f, sep = "_")
        out[[key]] <- paste(vals, collapse = "; ")
      }
      
      return(out)
    }
    
    # ---- CASE 2: Named object
    if (!is.null(names(x))) {
      for (n in names(x)) {
        key <- if (parent == "") n else paste(parent, n, sep = "_")
        out <- c(out, flatten_recursive(x[[n]], key))
      }
      return(out)
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
  
  flat_rows <- lapply(df[[col]], function(x) {
    tb <- flatten_json_safe(x)
    
    # If empty tibble → return tibble with 1 row and explicit NA
    if (ncol(tb) == 0) {
      return(tibble(.placeholder = NA))
    }
    tb
  })
  
  flat_df <- bind_rows(flat_rows)
  
  # Remove placeholder column if it exists
  flat_df <- flat_df %>% select(-any_of(".placeholder"))
  
  
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

cat("\n✅ Fully flattened CSV created successfully:\n", output_csv, "\n")
