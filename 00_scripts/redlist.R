library(tidyverse)
library(rlang)

# SoIB2 uses 2015 as the start for the current trend
MaxYear <- 2022
MinYear <- 2015
Years <- c(MinYear:MaxYear)
TrendYears <- MaxYear - MinYear + 1
threegenperiod <- 2 * (TrendYears - 1) + 100

# Read latest BLI 3gen values
threegen <- read.csv("00_data/3genbli.csv") # No in repository. Contact paintedstork@gmail.com
soib <- read.csv("01_analyses_full/results/SoIB_main.csv")   # Available in rep. after SoIB2 runs

# Remove unwanted fields from freq
soib <- soib %>% select ("India.Checklist.Common.Name",
                         "India.Checklist.Scientific.Name",
                         "BLI.Scientific.Name",
                         "Current.Analysis",
                         "IUCN.Category",
                         "Selected.SOIB",
                         "totalrange25km",
                         "proprange25km.current",
                         "mean5km",
                         "ci5km",
                         "longtermrci",
                         "longtermmean",
                         "longtermlci",
                         "currentsloperci",
                         "currentslopemean",
                         "currentslopelci",
                         "proj2025.lci",
                         "proj2025.mean",
                         "proj2025.rci",
                         "proj2026.lci",
                         "proj2026.mean",
                         "proj2026.rci",
                         "proj2027.lci",
                         "proj2027.mean",
                         "proj2027.rci",
                         "proj2028.lci",
                         "proj2028.mean",
                         "proj2028.rci",
                         "proj2029.lci",
                         "proj2029.mean",
                         "proj2029.rci",
                         "SOIBv2.Current.Status",
                         "SOIBv2.Priority.Status")
                         
                         
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
    (SOIBv2.Current.Status %in% c("Stable", "Decline", "Rapid Decline"))
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
            mutate( GEN = ifelse (GEN < 10, 10, GEN),
                    projected_mean_decline =  round (100 * (1 - (1 + currentsloperci/100) ^ GEN),0),
                    redlist_category = case_when (
                      projected_mean_decline > 80 ~ "Critically Endangered",
                      projected_mean_decline > 50 ~ "Endangered",
                      projected_mean_decline > 30 ~ "Vulnerable",
                      projected_mean_decline > 20 ~ "Near-threatened",
                      projected_mean_decline > 0 ~ "Least Concern"
                    ),    
                    projected_mean_decline = paste0(projected_mean_decline,"%"),
                    longtermmean = -(100 - longtermmean),
                    longtermlci  = -(100 - longtermlci),
                    longtermrci  = -(100 - longtermrci),
                    longtermmean = ifelse(longtermmean == "NA","NA",paste0(round(longtermmean,2),"% (",round(longtermrci,2),", ",round(longtermlci,2),")")),
                    proprange25km.current = round(100 * proprange25km.current,2),
                    currentslopemean = paste0(round(currentslopemean,2),"% (",round(currentsloperci,2),", ",round(currentslopelci,2),")"),
                    mean5km = paste0 ( round (100*mean5km/25, 2),"% (",round(100*(mean5km-ci5km)/25,2),", ",round(100*(mean5km+ci5km)/25,2) ,")"),
                    ci5km = round (ci5km, 2),
                    projected_decline = case_when(
                    GEN == 5  ~ 100 - `proj2025.rci`,
                    GEN == 6  ~ 100 - `proj2025.rci`,
                    GEN == 7  ~ 100 - `proj2025.rci`,
                    GEN == 8  ~ 100 - `proj2025.rci`,
                    GEN == 9  ~ 100 - `proj2025.rci`,
                    GEN == 10 ~ 100 - `proj2025.rci`,
                    GEN == 11 ~ 100 - `proj2026.rci`,
                    GEN == 12 ~ 100 - `proj2027.rci`,
                    GEN == 13 ~ 100 - `proj2028.rci`,
                    GEN == 14 ~ 100 - `proj2029.rci`
                    ), 
                    projected_decline = floor(projected_decline),
                    redlist_category1_exp = case_when (
                    projected_decline > 80 ~ "Critically Endangered",
                    projected_decline > 50 ~ "Endangered",
                    projected_decline > 30 ~ "Vulnerable",
                    projected_decline > 20 ~ "Near-threatened",
                    projected_decline > 0  ~ "Least Concern",
                  ),
                  redlist_different = ifelse(redlist_category == IUCN.Category,"No","Yes"),
                  redlist_proj_different = ifelse(redlist_category == redlist_category1_exp,"No","Yes")
                  ) %>% 
                        select ("India.Checklist.Common.Name.x",
                                "proprange25km.current",
                                "mean5km",
                                "longtermmean",
                                "currentslopemean",
                                "SOIBv2.Current.Status",
                                "SOIBv2.Priority.Status",
                                "GEN",
                                "projected_mean_decline",
                                "redlist_category",
                                "IUCN.Category",
                                "redlist_different",
                                "redlist_category1_exp",
                                "redlist_proj_different",
                                "projected_decline"
                                )

colnames(redlist) <- c ("Species",
                     "Range Coverage",
                     "Mean Grid Coverage",
                     "Long-term Decline",
                     "Current Annual Decline",
                     "Current Status",
                     "Conservation Priority",
                     "3GEN",
                     "3GEN Decline",
                     "Redlist Category Proposed",
                     "Current Redlist",
                     "IUCN Different",
                     "Redlist using Exp projections",
                     "Redlist projections different",
                     "Projected Decline"
                     )

write.csv(redlist, "01_analyses_full/results/redlist.csv", row.names = FALSE)
