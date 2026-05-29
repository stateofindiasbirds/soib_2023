library(dplyr)
library(knitr)
library(kableExtra)
library(purrr)

soibmain_file <- "01_analyses_full/results/SoIB_main.csv"

long_term_threshold <- c("Rapid Decline" = "<-50", "Decline" = "-50<x<-25", "Increase" = "50>x>25", "Rapid Increase" = "x>50", "Stable" = "None" )
current_threshold   <- c("Rapid Decline" = "<-2.7", "Decline" = "-2.7<x<-1.1", "Increase" = "1.6>x>0.9", "Rapid Increase" = "x>1.6", "Stable" = "None" )
range_threshold     <- c("Very Restricted" = "x<7,500", "Restricted" = "7,500<x<42,500", "Moderate" = "42,500<x<250,000", "Large" = "250,000<x<1,000,000", "Very Large" = "x>1,000,000" )

# Define ranks
rank_trend <- c(
  "Rapid Increase" = 1, "Increase" = 2, "Stable" = 3,
  "Trend Inconclusive" = 4, "Insufficient Data" = 5,
  "Decline" = 6, "Rapid Decline" = 7
)
rank_range    <- c("Very Large" = 1, "Large" = 2, "Moderate" = 3, "Restricted" = 4, "Very Restricted" = 5)
rank_priority <- c("Low" = 1, "Moderate" = 2, "High" = 3)

ignore_vals <- c("Trend Inconclusive", "Insufficient Data")

# Exact category order for sorting
category_order <- c(
  "Low → High", "Moderate → High", "Low → Moderate",
  "High → Low", "High → Moderate", "Moderate → Low",
  "High", "Moderate", "Low"
)

# Read CSV
df <- read.csv(soibmain_file, stringsAsFactors = FALSE)

# Hover tooltip
hover_values <- function(lci, mean, rci, new, type = c("long", "current", "range")) {
  type <- match.arg(type)
  
  # Format LCI/mean/RCI
  vals <- c(lci, mean, rci)
  vals[is.na(vals)] <- NA_real_
  non_na_vals <- vals[!is.na(vals)]
  if (length(non_na_vals) > 0 && all(non_na_vals == floor(non_na_vals))) {
    vals_fmt <- formatC(vals, format = "f", big.mark = ",", digits = 0)
  } else {
    vals_fmt <- formatC(vals, format = "f", big.mark = ",", digits = 1)
  }
  vals_fmt[is.na(vals_fmt)] <- "-"
  
  # Pick threshold based on type and latest value
  threshold_val <- switch(type,
                          long = long_term_threshold[new],
                          current = current_threshold[new],
                          range = range_threshold[new])
  
  # Threshold is string for range, numeric for trends
  if (!is.null(threshold_val)) {
    threshold_fmt <- as.character(threshold_val)
  } else {
    threshold_fmt <- "-"
  }
  
  paste0("(", paste(vals_fmt, collapse = ", "), ") [Threshold: ", threshold_fmt, "]")
}


# Color change with hover
color_change_hover <- function(old, new, rank_map, lci, mean, rci, type) {
  if (is.na(old) | is.na(new)) return(NA)
  if (old == new) return(old) # arrow only if changed
  if (old %in% ignore_vals & new %in% ignore_vals) return(old)
  
  color <- "black"
  if (old %in% ignore_vals & new %in% c("Stable", "Increase", "Rapid Increase")) color <- "blue"
  if (old %in% ignore_vals & new %in% c("Decline", "Rapid Decline")) color <- "red"
  if (!(old %in% ignore_vals) & !(new %in% ignore_vals)) {
    old_rank <- rank_map[old]
    new_rank <- rank_map[new]
    color <- if (new_rank > old_rank) "red" else if (new_rank < old_rank) "blue" else "black"
  }
  
  hover <- hover_values(lci, mean, rci, new, type = type)
  paste0("<span style='color:", color, "' title='", hover, "'>", old, " → ", new, "</span>")
}


# Color change for Priority (no hover)
color_change_priority <- function(old, new, rank_map) {
  if (is.na(old) | is.na(new)) return(NA)
  if (old == new) return(old)
  
  old_rank <- rank_map[old]
  new_rank <- rank_map[new]
  color <- if (new_rank > old_rank) "red" else if (new_rank < old_rank) "blue" else "black"
  paste0("<span style='color:", color, "'>", old, " → ", new, "</span>")
}

# Determine category for sorting
get_category <- function(old, new) {
  if (is.na(old) | is.na(new)) return(NA)
  if (old == "Low" & new == "High") return("Low → High")
  if (old == "Moderate" & new == "High") return("Moderate → High")
  if (old == "Low" & new == "Moderate") return("Low → Moderate")
  if (old == "High" & new == "Low") return("High → Low")
  if (old == "High" & new == "Moderate") return("High → Moderate")
  if (old == "Moderate" & new == "Low") return("Moderate → Low")
  if (old == "High") return("High")
  if (old == "Moderate") return("Moderate")
  if (old == "Low") return("Low")
  return(NA)
}

# Compute changes
changes <- df %>%
  dplyr::select(
    Species = eBird.English.Name.2024,
    Scientific = eBird.Scientific.Name.2024,
    Latest_LongTerm = SoIB.Latest.Long.Term.Status,
    Latest_Current = SoIB.Latest.Current.Status,
    Latest_Range = SoIB.Latest.Range.Status,
    Latest_Priority = SoIB.Latest.Priority.Status,
    Major_LongTerm = SoIB.Major.Update.Long.Term.Status,
    Major_Current = SoIB.Major.Update.Current.Status,
    Major_Range = SoIB.Major.Update.Range.Status,
    Major_Priority = SoIB.Major.Update.Priority.Status,
    longtermlci, longtermmean, longtermrci,
    currentslopelci, currentslopemean, currentsloperci,
    rangelci, rangemean, rangerci
  ) %>%
  mutate(
    Change_LongTerm = pmap_chr(list(Major_LongTerm, Latest_LongTerm, longtermlci, longtermmean, longtermrci),
                               color_change_hover, rank_map = rank_trend, type = "long"),
    Change_Current  = pmap_chr(list(Major_Current, Latest_Current, currentslopelci, currentslopemean, currentsloperci),
                               color_change_hover, rank_map = rank_trend, type = "current"),
    Change_Range    = pmap_chr(list(Major_Range, Latest_Range, rangelci, rangemean, rangerci),
                               color_change_hover, rank_map = rank_range, type = "range"),
    Change_Priority = mapply(color_change_priority, Major_Priority, Latest_Priority, MoreArgs = list(rank_priority)),
    Cat_LongTerm = mapply(get_category, Major_LongTerm, Latest_LongTerm),
    Cat_Current = mapply(get_category, Major_Current, Latest_Current),
    Cat_Range = mapply(get_category, Major_Range, Latest_Range),
    Cat_Priority = mapply(get_category, Major_Priority, Latest_Priority),
    First_Category = pmap_chr(list(Cat_LongTerm, Cat_Current, Cat_Range, Cat_Priority),
                              function(a, b, c, d) {
                                cats <- c(a, b, c, d)
                                cats <- cats[!is.na(cats)]
                                for (cat in category_order) {
                                  if (cat %in% cats) return(cat)
                                }
                                return(NA)
                              }),
    First_Category = factor(First_Category, levels = category_order)
  ) %>%
  filter(Change_LongTerm != Major_LongTerm | Change_Current != Major_Current |
           Change_Range != Major_Range | Change_Priority != Major_Priority) %>%
  arrange(First_Category)  # ← sorting ONLY by category_order

# Create HTML table
html_summary <- changes %>%
  dplyr::select(Species, Change_LongTerm, Change_Current, Change_Range, Change_Priority) %>%
  rename(
    "Species" = Species,
    "Potential Category Change for Long-term" = Change_LongTerm,
    "Potential Category Change for Current" = Change_Current,
    "Potential Category Change for Range" = Change_Range,
    "Potential Change in Priority" = Change_Priority
  ) %>%
  kable("html", escape = FALSE,
        caption = "SoIB: Changes from Major Update (2023) to Latest (2025). Improved: Blue, Worsen: Red") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )

save_kable(html_summary, "01_analyses_full/results/SoIB_changes_summary.html")
