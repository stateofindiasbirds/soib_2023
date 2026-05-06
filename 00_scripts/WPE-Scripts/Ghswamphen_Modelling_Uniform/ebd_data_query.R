library(purrr)
library(glue)
library(DBI)

species_list <- read.csv("species_list.csv")$scientific_name
source("/home/subhasmitap/Documents/Codes/private.R")

walk(species_list, function(sp) {
  
qry <- glue::glue_sql("
    SELECT *
    FROM EBD
    WHERE \"SCIENTIFIC.NAME\" = {sp}
      AND \"OBSERVATION.DATE\" >= DATE '2020-01-01'
      AND \"ALL.SPECIES.REPORTED\" = TRUE
    ", .con = con)
    
data <- DBI::dbGetQuery(con, qry)
    
# skip if no data returned
if (nrow(data) == 0) {
    message("No data for: ", sp)
    return(NULL)
    }
    
# extract common name (first row)
common_name <- data$COMMON.NAME[1]
    
# make filename safe (remove spaces/special chars)
# safe_sp <- gsub("[^A-Za-z0-9_]", "_", sp)
safe_common <- gsub("[^A-Za-z0-9_]", "_", common_name)
# file_name <- paste0(safe_common,"_",safe_sp,".csv")
file_name <- paste0(safe_common,".csv")
write.csv(data, file_name, row.names = FALSE)
  })

