library(readr)
library(dplyr)
library(glue)
source("00_scripts/iucn/config_iucn.R")

# Read CSV
species <- read_csv(nrloutputfile)

# Safe number formatting
format_num <- function(x) {
  if (is.na(x) || x == "" || is.null(x)) return("")
  suppressWarnings({
    num <- as.numeric(x)
    if (is.na(num)) return(as.character(x))
    prettyNum(num, big.mark = ",", scientific = FALSE)
  })
}

# Combine IUCN criteria in the required format
combine_criteria <- function(sp) {
  criteria <- c()
  
  for (crit in c("A","B","C","D")) {
    
    crit_level  <- sp[[paste0("Criteria", crit, "_Category")]]
    crit_string <- sp[[paste0("Criteria", crit, "_String")]]
    
    if (!is.na(crit_level) && nzchar(crit_level) &&
        !is.na(crit_string) && nzchar(crit_string)) {
      
      criteria <- c(criteria, paste0(crit_level, " ", crit_string))
    }
  }
  
  paste(criteria, collapse = "; ")
}

combine_criteria_old <- function(sp) {
  criteria <- c()
  for (crit in c("A","B","C","D")) {
    main_col <- paste0("MainCriteria", crit)
    sub_col <- paste0("SubCriteria", crit)
    crit_level_col <- paste0("Criteria", crit)
    
    main_val <- as.character(sp[[main_col]])
    sub_val  <- as.character(sp[[sub_col]])
    crit_level <- as.character(sp[[crit_level_col]])
    
    if (!is.na(crit_level) && crit_level != "" && !is.na(main_val) && main_val != "") {
      crit_text <- paste0(crit_level, " ", main_val)
      if (!is.na(sub_val) && sub_val != "") crit_text <- paste0(crit_text, sub_val)
      criteria <- c(criteria, crit_text)
    }
  }
  paste(criteria, collapse = "; ")
}

# Generate HTML for a species
generate_html_pretty <- function(sp) {
  html_lines <- c(
    "<!DOCTYPE html>",
    "<html lang='en'>",
    "<head>",
    "  <meta charset='UTF-8'>",
    "  <meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    glue("  <title>{sp$EnglishName} Card</title>"),
    "  <link href='https://fonts.googleapis.com/css2?family=Gandhi+Sans:wght@400;600&display=swap' rel='stylesheet'>",
    "  <style>",
    "    body { font-family: 'Gandhi Sans', 'Palatino Linotype', serif; background-color: #f4ece9; padding: 20px; color: #2c2c2c; }",
    "    .card { background-color: #fffaf7; border-radius: 12px; box-shadow: 0 4px 10px rgba(0,0,0,0.08); padding: 20px; max-width: 900px; margin: auto; border: 1px solid #d1bab5; }",
    "    .top-bar { background-color: #e15b3a; height: 48px; width: 100%; border-radius: 8px 8px 0 0; margin-bottom: 15px; position: relative; }",
    "    .species-header { position: absolute; top: 50%; transform: translateY(-50%); left: 24px; color: white; font-size: 22px; font-weight: 600; letter-spacing: 0.5px; }",
    "    .species-header i { color: #f4ece9; font-weight: 400; }",
    "    .columns { display: grid; grid-template-columns: 1fr 1fr; gap: 10px 30px; }",
    "    .section-title { font-weight: 600; margin-top: 15px; border-bottom: 2px solid #99566a; padding-bottom: 3px; color: #333; display: flex; justify-content: space-between; align-items: center; }",
    "    .criteria-label { font-weight: 600; font-size: 14px; color: #99566a; }",
    "    .data-row { display: flex; justify-content: space-between; border-radius: 6px; padding: 4px 8px; }",
    "    .data-row:nth-child(even) { background-color: #f4ece9; }",
    "    .label { font-weight: 600; color: #2f4f4f; }",
    "    .value { color: #333; }",
    "    .global-value { color: #2f7f73; font-weight: 600; }",
    "    table { width:100%; border-collapse: collapse; margin-top: 5px; border:1px solid #e2d4d0; }",
    "    th, td { border:1px solid #e2d4d0; text-align:center; padding:6px 8px; }",
    "    th { background-color:#e3f0e8; font-weight:600; color:#2f4f4f; }",
    "    td:first-child { text-align:left; }",
    "    tr:nth-child(even) { background-color:#f9f6f4; }",
    "    a { color:#34916e; text-decoration:none; font-weight:600; }",
    "    a:hover { text-decoration:underline; }",
    "    /* Force center alignment for SoIB table */",
    "    .soib-table td, .soib-table th { text-align: center !important; }",
    "  </style>",
    
    "  </style>",
    "</head>",
    "<body>",
    "  <div class='card'>",
    "    <div class='top-bar'>",
    glue("      <div class='species-header'>{sp$EnglishName} <i>{sp$ScientificName}</i></div>"),
    "    </div>",
    "    <div class='columns'>",
    
    "      <!-- Left Column -->",
    "      <div>",
    "        <div class='section-title'>Redlist</div>",
    glue("        <div class='data-row'><span class='label'>Regional (Default):</span> <span class='value'>{sp$RegionalRedlist}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Regional (Adjusted):</span> <span class='value'>{sp$AdjustedRegionalRedlist}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Global:</span> <span class='value'><a href='{sp$GlobalRedlistURL}' target='_blank'>{sp$GlobalRedlist}</a></span></div>"),
    glue("        <div class='data-row'><span class='label'>Migratory Status (India):</span> <span class='value'>{sp$MigratoryStatusIndia}</span></div>"),
    
    "        <div class='section-title'>IUCN Criteria Met</div>",
    glue("        <div class='data-row'><span class='value'>{combine_criteria(sp)}</span></div>"),
    
    "        <div class='section-title'>Range Size (sq. km.) <span class='criteria-label'>Criteria B</span></div>",
    glue("        <div class='data-row'><span class='label'>Extent of Occurrence (EOO):</span> <span class='value'>{format_num(sp$EOO)} (Max {format_num(sp$MaxEOO)})</span></div>"),
    glue("        <div class='data-row'><span class='label'>Area of Occupancy (AOO):</span> <span class='value'>{format_num(sp$MinAOO)} (Max {format_num(sp$MaxAOO)})</span></div>"),
    glue("        <div class='data-row'><span class='label'>Projected Decline in EOO (3 generations):</span> <span class='value'>{sp$DeclineEOO3GEN}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Actual Decline in EOO:</span> <span class='value'>{sp$ActualDeclinePercentage} ({sp$YearsActualDecline} years)</span></div>"),
    glue("        <div class='data-row'><span class='label'>EOO Change Year Band:</span> <span class='value'>{sp$EOOYearBandChange}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Global EOO:</span> <span class='global-value'>{format_num(sp$GlobalEOO)}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Global AOO:</span> <span class='global-value'>{format_num(sp$GlobalAOO)}</span></div>"),
    glue("        <div class='data-row'><span class='label'>% of Global Range:</span> <span class='value'>{sp$GlobalRangePercent}</span></div>"),
    glue("        <div class='data-row'><span class='label'>No. of Locations:</span> <span class='value'>{sp$Locations}</span></div>"),
    glue("        <div class='data-row'><span class='label'>No. of Subspecies:</span> <span class='value'>{sp$Subspecies}</span></div>"),
    "      </div>",
    
    "      <!-- Right Column -->",
    "      <div>",
    "        <div class='section-title'>Population Decline <span class='criteria-label'>Criteria A & C</span></div>",
    "        <table>",
    "          <tr><th style='text-align:left;'>Generations</th><th>3</th><th>2</th><th>1</th></tr>",
    "          <tr><td>Projected Decline %</td>",
    glue("            <td>{sp$Decline3GEN}</td>"),
    glue("            <td>{sp$Decline2GEN}</td>"),
    glue("            <td>{sp$Decline1GEN}</td>"),
    "          </tr>",
    "          <tr><td>No. of Years</td>",
    glue("            <td>{sp$Years3GEN}</td>"),
    glue("            <td>{sp$Years2GEN}</td>"),
    glue("            <td>{sp$Years1GEN}</td>"),
    "          </tr>",
    "        </table>",
    glue("        <div class='data-row'><span class='label'>Generation Length:</span> <span class='value'>{sp$GenerationLength}</span></div>"),
    glue("        <div class='data-row'><span class='label'>Actual Decline:</span> <span class='value'>{sp$ActualDeclinePercentage}% ({sp$YearsActualDecline} years, {sp$NewEOOStart}–{sp$NewEOOEnd})</span></div>"),
    glue("        <div class='data-row'><span class='label'>Global Population Trend:</span> <span class='global-value'>{sp$GlobalPopulationTrend}</span></div>"),
    
    "        <div class='section-title'>Population <span class='criteria-label'>Criteria C & D</span></div>",
    glue("        <div class='data-row'><span class='label'>Regional Population:</span> <span class='value'>{format_num(sp$TotalLikelyPop)} (Max {format_num(sp$TotalMaxPop)})</span></div>"),
    glue("        <div class='data-row'><span class='label'>Global Population:</span> <span class='value'>{format_num(sp$GlobalPopulation)}</span></div>"),
    glue("        <div class='data-row'><span class='label'>1% biogeographic population:</span> <span class='value'>{format_num(sp$BiogPop1Percent)}</span></div>"),
    
"        <div class='section-title'>SoIB</div>",
"        <table class='soib-table'>",
"          <tr><th>Priority</th><th>LTC</th><th>CAT</th></tr>",
    "          <tr>",
    glue("            <td>{sp$SoIBPriority}</td>"),
    glue("            <td>{sp$LTC}</td>"),
    glue("            <td>{sp$CAT}</td>"),
    "          </tr>",
    "        </table>",
    
    "        <div class='section-title'>Conventions & Legal</div>",
    glue("        <div class='data-row'><span class='label'>CMS:</span> <span class='value'>{sp$CMS}</span></div>"),
    glue("        <div class='data-row'><span class='label'>CITES:</span> <span class='value'>{sp$CITES}</span></div>"),
    glue("        <div class='data-row'><span class='label'>WLPA Schedule:</span> <span class='value'>{sp$Schedule}</span></div>"),
    "      </div>",
    "    </div>",
    "  </div>",
    "</body>",
    "</html>"
  )
  html_lines
}

# Generate HTML for each species
for(i in 1:nrow(species)) {
  html_content <- generate_html_pretty(species[i, ])
  file_name <- paste0(gsub(" ", "_", species$EnglishName[i]), ".html")
  file_name <- paste0(redlist_home_dir,"outputs\\",file_name)
  writeLines(html_content, con = file_name)
}
