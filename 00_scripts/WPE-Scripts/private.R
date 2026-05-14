library(DBI)
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "ebd",
  host     = "192.168.0.11",
  user     = "postgres",
  password = "a1b2c3d4e5",
  port     = 5432
)
