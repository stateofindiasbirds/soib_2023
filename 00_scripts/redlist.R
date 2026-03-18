library(tidyverse)
library(rlang)
library(glue)

# SoIB2 uses 2015 as the start for the current trend
MaxYear <- soib_year_info("latest_year")
MinYear <- soib_year_info("cat_start")
Years <- soib_year_info("cat_years")
TrendYears <- MaxYear - MinYear + 1
threegenperiod <- 2 * (TrendYears - 1) - 1

# for this use, only need Gen 10 onwards
gen10plus <- (MinYear + 10):max(soib_year_info("iucn_projection"))
gen10plus_cols <- get_iucn_proj_cols()[get_iucn_proj_cols() %>% 
                                         str_detect(glue("{str_flatten(gen10plus, '|')}"))]
# columns to use for upper limit of proj values
proj_upp_lim <- gen10plus_cols[gen10plus_cols %>% str_detect(".rci")]


# function to create projected_decline column from various years' proj values
# Added extra protection for generating columns that are not present
get_proj_decline_col <- function(gen) {

  # Use the defined projection range from your stub
  proj_years <- soib_year_info("iucn_projection")
  
  # Calculate potential projection year
  proj_year <- soib_year_info("cat_start") + floor(gen)
  
  # Cap it to available projection years (e.g., up to 2029)
  proj_year <- ifelse(proj_year > max(proj_years), max(proj_years), proj_year)
  
  proj_col_name <- glue("proj{proj_year}.rci")
  return(proj_col_name)
}


# Read latest BLI 3gen values
threegen <- read.csv("00_data/3genbli.csv") # No in repository. Contact paintedstork@gmail.com
soib <- read.csv(get_metadata("none")$SOIBMAIN.PATH)   # Available in rep. after SoIB2 runs

# Remove unwanted fields from freq
soib <- soib %>% 
  select ("India.Checklist.Common.Name", "India.Checklist.Scientific.Name",
          "BLI.Scientific.Name", "Current.Analysis", "IUCN.Category",
          "Selected.SoIB", "totalrange25km", "proprange25km.current",
          "mean5km", "ci5km", "longtermrci", "longtermmean", "longtermlci",
          "currentsloperci", "currentslopemean", "currentslopelci",
          gen10plus_cols, "SoIB.Latest.Current.Status", "SoIB.Latest.Priority.Status")

###################################################################
#  Find of which species is likely to meet some relevant thresholds
###################################################################


# Make a list of species for which soib data is available and declining
species <- soib %>% 
  filter(
    Current.Analysis == "X", #Current trends available
    currentsloperci < 0, # Declining rci, which is used as a bound in redlisting
    mean5km > 8, # Atleast 1/3rd of a grid covered on an average
    1.96 * abs(currentslopemean) > abs(currentsloperci-currentslopelci), 
    (SoIB.Latest.Current.Status %in% c("Stable", "Decline", "Rapid Decline"))
  ) %>%            
  select('BLI.Scientific.Name') %>%
  as.data.frame()



# Join with the species for which comparable trend data exists.
species <- inner_join (species, threegen, by = c("BLI.Scientific.Name" = "BLI"))
  
soib <- inner_join (soib, species,by = c("BLI.Scientific.Name" = "BLI.Scientific.Name"))

soib <- soib %>%
  mutate(
    GEN1 = floor(pmax(GEN, 3)),        # 3 years OR 1 generation
    GEN2 = floor(pmax(2 * GEN, 5)),    # 5 years OR 2 generations
    GEN3 = floor(pmax(3 * GEN, 10)),    # 10 years OR 3 generations (already used)
    valid1GEN = GEN1 <= threegenperiod,
    valid2GEN = GEN2 <= threegenperiod,
    valid3GEN = GEN3 <= threegenperiod
  )

soib <- soib %>% arrange(currentsloperci)

redlist <- soib %>%
  filter(GEN <= threegenperiod) %>%
  mutate(
    Decline1GEN = ifelse(valid1GEN,
                         round(100 * (1 - (1 + currentsloperci/100)^GEN1),0),
                         NA),
    
    Decline2GEN = ifelse(valid2GEN,
                         round(100 * (1 - (1 + currentsloperci/100)^GEN2),0),
                         NA),
    
    Decline3GEN = ifelse(valid3GEN,
                         round(100 * (1 - (1 + currentsloperci/100)^GEN3),0),
                         NA),    Years1GEN = GEN1,
    Years2GEN = GEN2,
    Years3GEN = GEN3,
    projected_mean_decline = ifelse(valid3GEN, Decline3GEN, NA),
    redlist_category = case_when (
      projected_mean_decline > 80 ~ "Critically Endangered",
      projected_mean_decline > 50 ~ "Endangered",
      projected_mean_decline > 30 ~ "Vulnerable",
      projected_mean_decline > 20 ~ "Near Threatened",
      projected_mean_decline > 0 ~ "Least Concern"
    ),    
    projected_mean_decline = paste0(projected_mean_decline,"%"),
    longtermmean = -(100 - longtermmean),
    longtermlci  = -(100 - longtermlci),
    longtermrci  = -(100 - longtermrci),
    longtermmean = ifelse(longtermmean == "NA","NA",
                          paste0(round(longtermmean,2),
                                 "% (",
                                 round(longtermrci,2),
                                 ", ",
                                 round(longtermlci,2),")")),
    proprange25km.current = round(100 * proprange25km.current,2),
    currentslopemean = paste0(round(currentslopemean,2),
                              "% (",
                              round(currentsloperci,2),
                              ", ",
                              round(currentslopelci,2),")"),
    mean5km = paste0(round (100*mean5km/25, 2),
                     "% (",
                     round(100*(mean5km-ci5km)/25,2),
                     ", ",
                     round(100*(mean5km+ci5km)/25,2) ,")"),
    ci5km = round (ci5km, 2)) %>% 
  # getting appropriate proj decline values
  mutate(projected_decline = get_proj_decline_col(GEN3)) %>% 
  rowwise() %>% 
  mutate(projected_decline = floor(cur_data()[[projected_decline]]))  %>% 
  ungroup() %>% 
  mutate(
    redlist_category1_exp = case_when(projected_decline > 80 ~ "Critically Endangered",
                                      projected_decline > 50 ~ "Endangered",
                                      projected_decline > 30 ~ "Vulnerable",
                                      projected_decline > 20 ~ "Near Threatened",
                                      projected_decline > 0  ~ "Least Concern",
                                      TRUE ~ NA_character_
                                      ),
    redlist_different = ifelse(redlist_category == IUCN.Category,"No","Yes"),
    redlist_proj_different = ifelse(redlist_category == redlist_category1_exp,"No","Yes"),
    criterionC_flag = case_when(
      valid1GEN & Decline1GEN >= 25 ~ "Critically Endangered",
      valid2GEN & Decline2GEN >= 20 ~ "Endangered",
      valid3GEN & Decline3GEN >= 10 ~ "Vulnerable",
      valid3GEN & Decline3GEN >= 8  ~ "Near Threatened",
      TRUE ~ "No"
    )
  ) %>% 
  select(
    "India.Checklist.Common.Name", "proprange25km.current", "mean5km",
    "longtermmean", "currentslopemean", "SoIB.Latest.Current.Status", "SoIB.Latest.Priority.Status", 
    GEN1, Decline1GEN, GEN2, Decline2GEN, GEN3, Decline3GEN,
    valid1GEN, valid2GEN, valid3GEN,
    "redlist_category", "IUCN.Category", "redlist_different",
    "redlist_category1_exp", "redlist_proj_different", "projected_decline",
    criterionC_flag
  )

#colnames(redlist) <- c (
#  "Species", "Range Coverage", "Mean Grid Coverage", "Long-term Decline",
#  "Current Annual Decline", "Current Status", "Conservation Priority",
#  "3GEN", "3GEN Decline", "Redlist Category Proposed", "Current Redlist",
#  "IUCN Different", "Red List using Exp projections", "Red List projections different",
#  "Projected Decline"
#)

colnames(redlist) <- c(
  "Species",
  "Range Coverage",
  "Mean Grid Coverage",
  "Long-term Decline",
  "Current Annual Decline",
  "Current Status",
  "Conservation Priority",
  
  "Years1GEN",
  "1GEN Decline",
  "Years2GEN",
  "2GEN Decline",
  "Years3GEN",
  "3GEN Decline",
  "1GEN valid",
  "2GEN valid",
  "3GEN valid",
  
  "Criteria A Redlist Category Proposed",
  "Current Redlist",
  "IUCN Different",
  "Red List using Exp projections",
  "Red List projections different",
  "Projected Decline",
  "Criterion C Redlist Category Proposed"
)

write.csv(redlist, "01_analyses_full/results/redlist.csv", row.names = FALSE)
