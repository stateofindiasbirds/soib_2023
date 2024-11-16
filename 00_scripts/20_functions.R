require(tidyverse)
require(glue)


###
temp_priority_correction <- function(db) {
  
  mapping <- db %>% 
    filter(MASK == "none") %>% 
    distinct(India.Checklist.Common.Name, SoIB.Latest.Priority.Status)
  
  db %>% 
    dplyr::select(-SoIB.Latest.Priority.Status) %>% 
    left_join(mapping)
  
}
###


# function to read in all CSVs if exist ---------------------------------------------

read_fn <- function(file_path) {
  
  if (file.exists(file_path)) {
    
    read_csv(file_path, guess_max = Inf,
             col_types = "ccccccccccccccccccdddccccccccdcccddddddddddddddddddddddddddddddddddddcccc") 
    # if not specified, cols with many NAs read as logical
    
  } else {
    
    print(glue("Skipped reading {file_path}"))
    
    NULL
    
  }
  
}


# function to combine lower and upper CIs into a string, if exist -------------------

str_c_CI <- function(data, lower, upper, new_name) {
  
  dist_range <- if (
    is.character(data %>% pull({{ lower }})) | is.character(data %>% pull({{ upper }}))
    ) TRUE else FALSE

  data %>% 
    # for range size, we have already made it character with commas
    {if (dist_range == TRUE) {
      mutate(., 
             lower_round = {{ lower }},
             upper_round = {{ upper }})
    } else {
      mutate(., 
             lower_round = round({{ lower }}, 2),
             upper_round = round({{ upper }}, 2))
    }} %>% 
    mutate({{ new_name }} := case_when(
      
      !is.na({{ lower }}) & !is.na({{ upper }}) ~ glue("({lower_round}, {upper_round})"),
      TRUE ~ NA
      
    )) %>% 
    dplyr::select(-lower_round, -upper_round)

}


# abbreviated codes for each mask ---------------------------------------------------

join_mask_codes <- function(data) {
  
  require(readxl)
  
  states <- read_xlsx("00_data/Website Codes.xlsx", sheet = 1) %>% 
    rename(MASK = STATE)
  habs <- read_xlsx("00_data/Website Codes.xlsx", sheet = 2) %>% 
    mutate(MASK = case_when(HABITAT == "Woodland" ~ "woodland",
                            HABITAT == "Cropland" ~ "cropland",
                            HABITAT == "Open Natural Ecosystem" ~ "ONEland"),
           HABITAT = NULL)
  pa <- read_xlsx("00_data/Website Codes.xlsx", sheet = 3) %>% 
    mutate(MASK = "PA", ADMIN = NULL)
  
  
  codes <- data.frame(MASK = "none",
                      CODE = "in") %>% 
    bind_rows(habs, pa, states) %>% 
    rename(MASK.CODE = CODE) %>% 
    # codes need to be lowercase
    mutate(MASK.CODE = str_to_lower(MASK.CODE))
  
  
  # return dataframe with codes joined
  data %>% 
    left_join(codes, by = "MASK") %>% 
    # labels
    mutate(MASK.LABEL = case_when(
      MASK == "none" ~ "India",
      MASK == "cropland" ~ "Cropland",
      MASK == "woodland" ~ "Woodland",
      MASK == "ONEland" ~ "ONEs",
      MASK == "PA" ~ "PAs",
      TRUE ~ MASK
    ))
  
}


# # function to create specific HTML string -------------------------------------------
# 
# create_HTML_strings <- function(data) {
#   
#   data_new <- data %>% 
#     mutate(link1 = glue('"{URL_base}birds/{MASK.CODE}/{custom_url}/"'),
#            link2 = glue('"{URL_pre_uploads}{MASK}.png"'),
#            
#            a_href_a = glue("<a href={link1}>"),
#            span1_a = glue('<span class="soib_trends_img">'),
#            span1_img = glue("<img src={link2}>"),
#            span1_b = glue("</span>"),
#            span2 = glue('<span class="soib_trends_lbl">{MASK.LABEL}</span>'),
#            a_href_b = "</a>",
#            
#            HTML_string = glue("{a_href_a}{span1_a}{span1_img}{span1_b}{span2}{a_href_b}"))
#   
#   return(data_new$HTML_string)
#   
# }

# function to identify if current state is key to the species

# keystates object must exist in environment

is_curspec_key4state <- function(data) {
  
  key_db <- keystates %>% 
    distinct(ST_NM, India.Checklist.Common.Name) %>% 
    mutate(KEY = TRUE)
  
  data <- data %>% 
    left_join(get_metadata() %>% distinct(MASK, MASK.TYPE)) %>% 
    join_mask_codes() %>% 
    left_join(key_db, 
              by = c("MASK.LABEL" = "ST_NM", "India.Checklist.Common.Name")) %>% 
    complete(KEY, fill = list(KEY = FALSE)) %>% 
    mutate(KEY = case_when(MASK.TYPE == "state" ~ KEY,
                           TRUE ~ NA)) %>% 
    dplyr::select(-c(MASK.TYPE, MASK.CODE, MASK.LABEL))
  
}


# round our model estimate values to appropriate precision --------------------------

round_model_estimates <- function(db) {
  
  # Estimate 10.12345, SE 5.9876 --> Round estimate to zero or at most one decimal place.
  # Estimate 10.12345, SE 0.5432 --> Round estimate to one or at most two places.
  
  # We are going with one decimal place to be conservative.
  
  db %>% 
    mutate(across(c("longtermlci","longtermmean","longtermrci","currentslopelci",
                    "currentslopemean","currentsloperci","rangelci","rangemean","rangerci"),
                  ~ round(., 1)))
  
}
