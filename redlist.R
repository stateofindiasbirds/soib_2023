library(tidyverse)
library(rlang)

# SoIB2 uses 2015 as the start for the current trend
MaxYear <- 2022
MinYear <- 2015
Years <- c(MinYear:MaxYear)
TrendYears <- MaxYear - MinYear + 1

# Read latest BLI 3gen values
threegen <- read.csv("3genbli.csv")
soib <- read.csv("SoIB_main.csv")

# Remove unwanted fields from freq
soib <- soib %>% select ("India.Checklist.Common.Name",
                         "India.Checklist.Scientific.Name",
                         "BLI.Scientific.Name",
                         "Current.Analysis",
                         "Selected.SOIB",
                         "totalrange25km",
                         "proprange25km.current",
                         "mean5km",
                         "ci5km",
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
                         "SOIBv2.Current.Status",
                         "SOIBv2.Priority.Status")
                         
                         
###################################################################
#  Find of which species is likely to meet some relevant thresholds
###################################################################

Years <- c(MinYear:MaxYear)
TrendYears <- MaxYear - MinYear + 1
threegenperiod <- 2 * (TrendYears - 1) -1 

# Make a list of species for which soib data is available and declining
species <- soib %>% 
              filter ( Current.Analysis == "X", #Current trends available
                       currentsloperci < 0, # Declining rci, which is used as a bound in redlisting
                       mean5km > 10, # Atleast 40% of a grid covered on an average
                       2 * 1.96 * abs(currentslopemean) > abs(currentsloperci-currentslopelci), # Mean > 2 * SE
                       (SOIBv2.Current.Status == "Stable") | (SOIBv2.Current.Status == "Declining") | (SOIBv2.Current.Status == "Rapid Decline")) %>%            
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
            mutate( mean_decline =  round (100 * (1 - (1 + currentsloperci/100) ^ GEN),0),
                    redlist_category1 = case_when (
                      mean_decline > 80 ~ "Critically Endangered",
                      mean_decline > 50 ~ "Endangered",
                      mean_decline > 30 ~ "Vulnerable",
                      mean_decline > 20 ~ "Near-threatened"
                    ),            
                    proprange25km.current = round(100 * proprange25km.current,2),
                    currentslopemean = paste0(round(currentslopemean,2),"% (",round(currentsloperci,2),", ",round(currentslopelci,2),")"),
                    mean5km = paste0 ( round (100*mean5km/25, 2),"% (",round(100*(mean5km-ci5km)/25,2),", ",round(100*(mean5km+ci5km)/25,2) ,")"),
                    ci5km = round (ci5km, 2),
                    redlist_decline = case_when(
                    GEN == 5  ~ 100 - `proj2025.rci`,
                    GEN == 6  ~ 100 - `proj2025.rci`,
                    GEN == 7  ~ 100 - `proj2025.rci`,
                    GEN == 8  ~ 100 - `proj2025.rci`,
                    GEN == 9  ~ 100 - `proj2025.rci`,
                    GEN == 10 ~ 100 - `proj2025.rci`,
                    GEN == 11 ~ 100 - `proj2026.rci`,
                    GEN == 12 ~ 100 - `proj2027.rci`,
                    GEN == 13 ~ 100 - `proj2028.rci`
                  ), 
                  redlist_decline = floor(redlist_decline),
                  redlist_category2 = case_when (
                    redlist_decline > 80 ~ "Critically Endangered",
                    redlist_decline > 50 ~ "Endangered",
                    redlist_decline > 30 ~ "Vulnerable",
                    redlist_decline > 20 ~ "Near-threatened"
                  )) %>% 
        #              filter (redlist_decline > 20) %>%
                        select ("India.Checklist.Common.Name.x",
                                "proprange25km.current",
                                "mean5km",
                                "currentslopemean",
                                "SOIBv2.Current.Status",
                                "SOIBv2.Priority.Status",
                                "mean_decline",
                                "redlist_category1",
                                "redlist_decline",
                                "redlist_category2")

colnames(redlist) <- c ("Species",
                     "Range Coverage",
                     "Mean Grid Coverage",
                     "Annual Decline",
                     "Current Status",
                     "Conservation Priority",
                     "Mean Decline",
                     "Mininum Redlist Decline",
                     "Redlist Category1 Proposed",
                     "Redlist Category2 Proposed")

write.csv(redlist, "redlist.csv", row.names = FALSE)
