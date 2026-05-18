sum_district_max <- function(data, eu_row) {
  
  start_m <- match(eu_row$START_MONTH, month.abb)
  end_m   <- match(eu_row$END_MONTH, month.abb)
  
  # Step 1: seasonal month filter (handles wrap-around like Nov–Mar)
  filtered <- data %>%
    filter(
      COMMON.NAME == eu_row$COMMON.NAME,
      STATE.CODE == eu_row$STATE.CODE,
      SEASON_YEAR >= eu_row$START_YEAR,
      SEASON_YEAR <= eu_row$END_YEAR
    ) %>%
    filter(if (start_m <= end_m) {
      MONTH_NUM >= start_m & MONTH_NUM <= end_m
    } else {
      MONTH_NUM >= start_m | MONTH_NUM <= end_m
    })
  
  cat(
    "EU:", eu_row$EU,
    "| Species:", eu_row$COMMON.NAME,
    "| State:", eu_row$STATE.CODE,
    "| Rows after filter:", nrow(filtered),
    "\n"
  )
  
  # if no data pass the filter for a specific EU - guards against empty data
  if (nrow(filtered) == 0) {
    return(data.frame(
      EU = eu_row$EU,
      Species = eu_row$COMMON.NAME,
      State = eu_row$STATE.CODE,
      Start_month = eu_row$START_MONTH,
      End_month = eu_row$END_MONTH,
      MaxTotal = NA,
      PeakSeasonYear = NA
    ))
  }
  
  # Step 2: max per district per SEASON_YEAR
  district_season_max <- filtered %>%
    group_by(STATE.CODE, COUNTY.CODE, SEASON_YEAR) %>%
    summarise(MaxCount = max(OBSERVATION.COUNT, na.rm = TRUE), .groups = "drop")
  
  # Step 3: sum all district across state per SEASON_YEAR
  state_season_total <- district_season_max %>%
    group_by(STATE.CODE, SEASON_YEAR) %>%
    summarise(SeasonTotal = sum(MaxCount, na.rm = TRUE), .groups = "drop")
  print(nrow(state_season_total))
  print(state_season_total)
  
  # Step 4: max across SEASON_YEAR
  state_max <- state_season_total %>%
    slice_max(order_by = SeasonTotal, n = 1, with_ties = FALSE) %>%
    mutate(
      MaxTotal = SeasonTotal,
      PeakSeasonYear = SEASON_YEAR
    ) %>%
    select(MaxTotal, PeakSeasonYear)
  
cat("EU:", eu_row$EU, "Rows:", nrow(filtered), "\n") 

  # Step 5: clean output
  return(data.frame(
    EU = eu_row$EU,
    Species = eu_row$COMMON.NAME,
    State = eu_row$STATE.CODE,
    Start_month = eu_row$START_MONTH,
    End_month = eu_row$END_MONTH,
    Estimate_Min = state_max$MaxTotal,
    PeakSeasonYear = state_max$PeakSeasonYear
  ))
}
