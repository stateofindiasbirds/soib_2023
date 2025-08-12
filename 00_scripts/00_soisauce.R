# helper functions

# pathfinder (get analyses metadata) -------------------------------------------------

# maybe add an argument which allows to view the list of available columns
# could just add manual text output grouping similar-function-columns together

pathfinder <- function(mask = NULL) {
  
  require(tidyverse)
  require(here)
  
  load(here("00_data/analyses_metadata.RData"))
  
  if(is.null(mask)) {
    return(analyses_metadata)
  } else {
    
    if (!mask %in% analyses_metadata$MASK) {
      stop("Select valid mask name")
    } else {
      return(analyses_metadata |> filter(MASK == mask))
    }
    
  }
  
}


# what are the latest migratory years under consideration? -----------------

soib_year_info <- function(what = "latest_year") {
  
  # catch input errors
  valid_inputs <- c("latest_year", "timegroup_lab", "timegroup_med", 
                    "cat_years", "cat_start", "iucn_projection")
  
  if (!what %in% valid_inputs) {
    stop(paste("Choose valid info to obtain regarding current SoIB years: {", 
               stringr::str_flatten_comma(valid_inputs),
               "}"))
  }
  
  
  # load latest year data
  load("00_data/current_soib_migyears.RData")
  
  
  # latest year
  if (what == "latest_year") {
    return(latest_soib_my)
  }
  
  
  # timegroup labels or median years
  # (2013 as threshold here is somewhat arbitrary, not sure which years in full_soib_my)
  if (what %in% c("timegroup_lab", "timegroup_med")) {
    
    if (what == "timegroup_lab") {
      pre <- c("before 2000","2000-2006","2007-2010","2011-2012","2013")
      full <- full_soib_my[full_soib_my > 2013] |> as.character()
    } else if (what == "timegroup_med") {
      pre <- c(median_soib_hist_years, 2013)
      full <- full_soib_my[full_soib_my > 2013]
    }
    
    all <- c(pre, full)
    return(all)
  }
  
  
  # cutoff year for CAT
  # 2015 was cutoff in SoIB 2023. So 8 years for CAT.
  # We eventually want 10 years for CAT
  cat_years <- full_soib_my[full_soib_my >= 2015] |> 
    sort() |> 
    tail(10)
  
  if (what == "cat_years") return(cat_years)
  else if (what == "cat_start") return(min(cat_years))
  
  
  # extra years for IUCN projection
  if (what == "iucn_projection") {
    extra.years = seq(latest_soib_my, length.out = 7) + 1
    return(extra.years)
  }
  
}


# Status categories -----------------------------------------------------------------

get_soib_status_cats <- function(which = NULL) {
  
  cats <- list(
    trend = c("Rapid Decline", "Decline", "Insufficient Data",
              "Trend Inconclusive", "Stable", "Increase", "Rapid Increase"),
    range = c("Historical", "Very Restricted", "Restricted",
              "Moderate", "Large", "Very Large"),
    decline = c("Decline", "Rapid Decline"),
    uncertain = c("Insufficient Data", "Trend Inconclusive"),
    restricted = c("Historical", "Very Restricted", "Restricted"),
    
    # old categories
    trend_soib1 = c("Strong Decline", "Moderate Decline", "Data Deficient",
                    "Uncertain", "Stable", "Moderate Increase", "Strong Increase"),
    decline_soib1 = c("Moderate Decline", "Strong Decline"),
    uncertain_soib1 = c("Data Deficient", "Uncertain")
  )
  
  if (is.null(which)) {
    
    return(cats)
    
  } else {
    
    if (!which %in% names(cats)) {
      return(glue("Please select one of the following: {names(cats) %>% str_flatten_comma()}"))
    }
    
    return(cats %>% pluck(which))
  }
  
}


# convert eBird name to India Checklist name ----------------------------------------

specname_to_india_checklist <- function(spec_names, already_show = TRUE) {
  
  names_map <- read.csv("00_data/SoIB_mapping_2023.csv") %>% 
    distinct(eBird.English.Name.2023, India.Checklist.Common.Name)
  
  df_names <- data.frame(OLD = spec_names)
  
  # quit if already India Checklist name (and print if we want to see the message)
  if (all(df_names$OLD %in% names_map$India.Checklist.Common.Name)) {
    
    if (already_show == TRUE){
      print("Species name(s) already align with India Checklist.")
    }
    
    return(spec_names)
  }
  
  df_names <- df_names %>% 
    left_join(names_map, by = c("OLD" = "eBird.English.Name.2023")) %>% 
    rename(NEW = India.Checklist.Common.Name)
  
  if (any(is.na(df_names$NEW))) {
    print("Input species name is not valid eBird name")
    stop()
  }
  
  return(df_names$NEW)
  
}


# get mapping info for eBird species names --------------------------------

# species names change every year, which proves difficult when working on annual updates
# this function helps map these different names

ebird_tax_mapping <- function() {
  read.csv("00_data/eBird_taxonomy_mapping.csv")
}


# get column names for IUCN projection values -----------------------------

get_iucn_proj_cols <- function() {
  map(soib_year_info("iucn_projection"), ~ {
    c(glue("proj{.x}.lci"), glue("proj{.x}.mean"), glue("proj{.x}.rci"))
  }) %>% 
    list_c()
}


# update IUCN Status based on latest updated in mapping sheet ---------------------------------------

# input dataframe can be any mapping/main type object with list of species along with IUCN status
# mutates IUCN Status column based on latest Status updated in SoIB_mapping_2023.csv
# preserves column order in input data

# col_specname must be eBird checklist species names

get_latest_IUCN_status <- function(data, col_specname, col_iucn = NULL,
                                   path_mapping = "00_data/SoIB_mapping_2023.csv") {
  
  if (!(is.character(col_specname) & 
        (is.character(col_iucn)) | is.null(col_iucn))) {
    stop("Arguments col_specname and col_iucn can only be character values.")
  }
  
  require(tidyverse)
  
  # col_iucn is the name we want for newly mutated IUCN column
  # (not necessarily name of IUCN column in mapping sheet)
  col_newnames <- if (is.null(col_iucn)) {
    c(col_specname, "IUCN.Category") # default name in mapping sheet
  } else {
    c(col_specname, col_iucn)
  }
  
  col_order <- names(data)
  
  mapping <- read_csv(path_mapping) %>% 
    dplyr::select("eBird.English.Name.2023", "IUCN.Category") %>% 
    magrittr::set_colnames(col_newnames)
  
  data_upd <- data %>% 
    # if IUCN column already exists, remove it before join
    {if (!is.null(col_iucn)) {
      dplyr::select(., -all_of(col_iucn))
    } else {
      .
    }} %>% 
    left_join(mapping, by = col_specname) %>% 
    # if IUCN col existed, preserves exact order; else, will be new col after same old cols
    relocate(all_of(col_order))
  
  return(data_upd)
  
}

