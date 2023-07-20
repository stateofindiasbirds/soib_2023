require(tidyverse)
require(glue)



# function to read in all CSVs if exist ---------------------------------------------

read_fn <- function(file_path) {
  
  if (file.exists(file_path)) {
    
    read_csv(file_path) 
    
  } else {
    
    print(glue("Skipped reading {file_path}"))
    
    NULL
    
  }
  
}


# function to combine lower and upper CIs into a string, if exist -------------------

str_c_CI <- function(lower, upper) {
  
  lower <- round(lower, 2)
  upper <- round(upper, 2)
  
  str_CI <- ifelse(!anyNA(c(lower, upper)), glue("({lower},{upper})"), NA)
  
  return(str_CI)
  
}


# abbreviated codes for each mask ---------------------------------------------------

join_mask_codes <- function(data, mask_col) {
  
  mask_code <- data %>% 
    mutate(MASK.CODE = case_when(
      
      MASK == "none" ~ "in",
      MASK == "woodland" ~ "wo",
      MASK == "cropland" ~ "cr",
      MASK == "ONEland" ~ "on",
      MASK == "PA" ~ "pa",
      MASK == "Andaman and Nicobar Islands" ~ "an",
      MASK == "Andhra Pradesh" ~ "ap",
      MASK == "Arunachal Pradesh" ~ "ar",
      MASK == "Assam" ~ "as",
      MASK == "Bihar" ~ "br",
      MASK == "Chandigarh" ~ "ch",
      MASK == "Chhattisgarh" ~ "ct",
      MASK == "Dadra and Nagar Haveli" ~ "dn",
      MASK == "Daman and Diu" ~ "dd",
      MASK == "Delhi" ~ "dl",
      MASK == "Goa" ~ "ga",
      MASK == "Gujarat" ~ "gj",
      MASK == "Haryana" ~ "hr",
      MASK == "Himachal Pradesh" ~ "hp",
      MASK == "Jammu and Kashmir" ~ "jk",
      MASK == "Jharkhand" ~ "jh",
      MASK == "Karnataka" ~ "ka",
      MASK == "Kerala" ~ "kl",
      MASK == "Ladakh" ~ "la",
      MASK == "Lakshadweep" ~ "ld",
      MASK == "Madhya Pradesh" ~ "mp",
      MASK == "Maharashtra" ~ "mh",
      MASK == "Manipur" ~ "mn",
      MASK == "Meghalaya" ~ "ml",
      MASK == "Mizoram" ~ "mz",
      MASK == "Nagaland" ~ "nl",
      MASK == "Odisha" ~ "or",
      MASK == "Puducherry" ~ "py",
      MASK == "Punjab" ~ "pb",
      MASK == "Rajasthan" ~ "rj",
      MASK == "Sikkim" ~ "sk",
      MASK == "Tamil Nadu" ~ "tn",
      MASK == "Telangana" ~ "ts",
      MASK == "Tripura" ~ "tr",
      MASK == "Uttar Pradesh" ~ "up",
      MASK == "Uttarakhand" ~ "ul",
      MASK == "West Bengal" ~ "wb"
      
    ),
    MASK.LABEL = case_when(
      MASK == "none" ~ "India",
      MASK == "cropland" ~ "Cropland",
      MASK == "woodland" ~ "Woodland",
      MASK == "ONEland" ~ "ONEs",
      MASK == "PA" ~ "PAs",
      TRUE ~ MASK
    ),
    )
  
}


# function to create specific HTML string -------------------------------------------

create_HTML_strings <- function(data) {
  
  data_new <- data %>% 
    mutate(link1 = glue('"{URL_base}birds/{MASK.CODE}/{custom_url}/"'),
           link2 = glue('"{URL_pre_uploads}{MASK}.png"'),
           
           a_href_a = glue("<a href={link1}>"),
           span1_a = glue('<span class="soib_trends_img">'),
           span1_img = glue("<img src={link2}>"),
           span1_b = glue("</span>"),
           span2 = glue('<span class="soib_trends_lbl">{MASK.LABEL}</span>'),
           a_href_b = "</a>",
           
           HTML_string = glue("{a_href_a}{span1_a}{span1_img}{span1_b}{span2}{a_href_b}"))
  
  return(data_new$HTML_string)
  
}


# combine HTML strings based on mask  -----------------------------------------------

c_HTML_strings <- function(data, mask_type) {
  
  data_new <- data %>% 
    mutate(deleted = case_when(
      
      MASK.TYPE == mask_type ~ HTML_str_all %>% 
        str_split(pattern = ",") %>% 
        unlist() %>% 
        setdiff(HTML_str) %>% 
        str_flatten(collapse = ","),
      TRUE ~ HTML_str_all
      
    )) 
  
  return(data_new$deleted)
  
}