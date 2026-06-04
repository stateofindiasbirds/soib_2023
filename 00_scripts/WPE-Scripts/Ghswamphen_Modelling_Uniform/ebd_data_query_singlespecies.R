library(glue)
library(DBI)

source("/home/subhasmitap/Documents/Codes/private.R")

sp <- "Porphyrio poliocephalus"

qry <- glue::glue_sql("
  SELECT *
  FROM EBD
  WHERE \"SCIENTIFIC.NAME\" = {sp}
    AND \"OBSERVATION.DATE\" >= DATE '2020-01-01'
    AND \"ALL.SPECIES.REPORTED\" = TRUE
", .con = con)

data <- dbGetQuery(con, qry)

# extract common name
common_name <- data$COMMON.NAME[1]

# make safe filename
safe_common <- gsub("[^A-Za-z0-9_]", "_", common_name)

file_name <- paste0(safe_common, ".csv")

write.csv(data, file_name, row.names = FALSE)
