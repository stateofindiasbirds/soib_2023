library(dplyr)
library(DBI)
library(RPostgres)
library(glue)
library(here)


scriptpath <- "00_scripts/iucn/"
datapath   <- "00_scripts/iucn/"

source(here(scriptpath, "private.R"))

con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = dbname,
  host     = ipaddress,
  port     = portnumber,
  user     = username,
  password = password
)


grid_size <- 4
grid_limit <- 3000 / (2 * 2)   # = 750

message("AOO prefilter GRID threshold: ", grid_limit)

qry <- glue_sql("
WITH loc AS (
    SELECT
        \"LOCALITY.ID\",
        FLOOR((\"LATITUDE\" * 111.32) / {grid_size}) AS lat_grid,
        FLOOR((\"LONGITUDE\" * 111.32 * COS(RADIANS(\"LATITUDE\"))) / {grid_size}) AS lon_grid
    FROM \"LOCATION\"
    WHERE \"LATITUDE\" IS NOT NULL
      AND \"LONGITUDE\" IS NOT NULL
),

ebd_clean AS (
    SELECT
        e.\"SCIENTIFIC.NAME\",
        e.\"COMMON.NAME\",
        e.\"LOCALITY.ID\",
        l.lat_grid,
        l.lon_grid
    FROM EBD e
    INNER JOIN loc l
        ON e.\"LOCALITY.ID\" = l.\"LOCALITY.ID\"
    WHERE e.\"OBSERVATION.DATE\" >= DATE '2016-01-01'
      AND e.\"CATEGORY\" IN ('species', 'issf', 'domestic')
      AND (
            e.\"EXOTIC.CODE\" IS NULL
            OR e.\"EXOTIC.CODE\" NOT IN ('P','X')
          )
)

SELECT
    \"SCIENTIFIC.NAME\" AS scientific_name,
    \"COMMON.NAME\" AS english_name,
    COUNT(DISTINCT (lat_grid, lon_grid)) AS grid_count
FROM ebd_clean
GROUP BY \"SCIENTIFIC.NAME\", \"COMMON.NAME\"
", .con = con)

df <- dbGetQuery(con, qry)

filtered_species <- df %>%
  filter(grid_count < grid_limit) %>%
  arrange(grid_count)

message("Total species: ", nrow(df))
message("Passed AOO prefilter: ", nrow(filtered_species))

write.csv(filtered_species,
          file.path(datapath,"species_aoo_prefilter.csv"),
          row.names = FALSE)

filtered_species