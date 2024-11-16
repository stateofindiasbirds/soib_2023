library(tidyverse)
library(rlang)

# SoIB2 uses 2015 as the start for the current trend
MaxYear <- soib_year_info("latest_year")
MinYear <- soib_year_info("cat_start")
Years <- soib_year_info("cat_years")
TrendYears <- MaxYear - MinYear + 1
threegenperiod <- 2 * (TrendYears - 1) + 100

# for this use, only need Gen 10 onwards
gen10plus <- (MinYear + 10):max(soib_year_info("iucn_projection"))
gen10plus_cols <- get_iucn_proj_cols()[get_iucn_proj_cols() %>% 
                                         str_detect(glue("{str_flatten(gen10plus, '|')}"))]
# columns to use for upper limit of proj values
proj_upp_lim <- gen10plus_cols[gen10plus_cols %>% str_detect(".rci")]


# function to create projected_decline column from various years' proj values
get_proj_decline_col <- function(data, gen) {
  proj_year <- MinYear + gen # relation between GEN and projected year
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
  select('India.Checklist.Common.Name', 'BLI.Scientific.Name') %>%
  as.data.frame()


# Filter species whose 3GEN data is shorter than the period of reliable data
threegen <- threegen %>%
  filter (GEN <= threegenperiod)

# Join with the species for which comparable trend data exists.
species <- inner_join (species, threegen, by = c("BLI.Scientific.Name" = "BLI"))
  
soib <- inner_join (soib, species,by = c("BLI.Scientific.Name" = "BLI.Scientific.Name"))

soib    <- soib[order(soib$currentsloperci),]

redlist <- soib %>%
  mutate(
    GEN = ifelse (GEN < 10, 10, GEN),
    projected_mean_decline =  round (100 * (1 - (1 + currentsloperci/100) ^ GEN),0),
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
  mutate(projected_decline = get_proj_decline_col(GEN)) %>% 
  rowwise() %>% 
  mutate(projected_decline = get(projected_decline) %>% floor()) %>% 
  ungroup() %>% 
  mutate(
    redlist_category1_exp = case_when(projected_decline > 80 ~ "Critically Endangered",
                                      projected_decline > 50 ~ "Endangered",
                                      projected_decline > 30 ~ "Vulnerable",
                                      projected_decline > 20 ~ "Near Threatened",
                                      projected_decline > 0  ~ "Least Concern"),
    redlist_different = ifelse(redlist_category == IUCN.Category,"No","Yes"),
    redlist_proj_different = ifelse(redlist_category == redlist_category1_exp,"No","Yes")
  ) %>% 
  select(
    "India.Checklist.Common.Name", "proprange25km.current", "mean5km",
    "longtermmean", "currentslopemean", "SoIB.Latest.Current.Status",
    "SoIB.Latest.Priority.Status", "GEN", "projected_mean_decline",
    "redlist_category", "IUCN.Category", "redlist_different",
    "redlist_category1_exp", "redlist_proj_different", "projected_decline"
  )

colnames(redlist) <- c (
  "Species", "Range Coverage", "Mean Grid Coverage", "Long-term Decline",
  "Current Annual Decline", "Current Status", "Conservation Priority",
  "3GEN", "3GEN Decline", "Redlist Category Proposed", "Current Redlist",
  "IUCN Different", "Red List using Exp projections", "Red List projections different",
  "Projected Decline"
)

write.csv(redlist, "01_analyses_full/results/redlist.csv", row.names = FALSE)
