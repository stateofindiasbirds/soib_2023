  library(readr)
  library(dplyr)
  library(glue)
  library(webshot2)
  
  source("00_scripts/iucn/config_iucn.R")
  resultspath <- "01_analyses_full/results"
  
  # Read CSV
  species <- read_csv(nrloutputfile, show_col_types = FALSE,
           col_types = cols(.default = col_character()))
#  species <- read_csv(nrloutputfile)
  
  na_blank <- function(x) {
    ifelse(is.na(x) | x == "", "", x)
  }
  # Combine IUCN criteria in the required format
  combine_criteria <- function(sp) {
    criteria_out <- c()
    
    for (crit in c("A","B","C","D")) {
      
      crit_level  <- sp[[paste0("Criteria", crit, "_Category")]]
      crit_string <- sp[[paste0("Criteria", crit, "_String")]]
      
      if (!is.na(crit_level) && nzchar(crit_level) &&
          !is.na(crit_string) && nzchar(crit_string)) {
        
        # split on "+"
        parts <- unlist(strsplit(crit_string, "\\+"))
        parts <- trimws(parts)
        
        if (length(parts) > 1) {
          first <- parts[1]
          
          # remove only leading "B" (or A/C/D depending on crit)
          rest <- sub(paste0("^", crit), "", parts[-1])
          
          combined <- paste0(first, "+", paste(rest, collapse = "+"))
        } else {
          combined <- parts
        }
        
        criteria_out <- c(criteria_out, paste0(crit_level, " ", combined))
      }
    }
    
    if (length(criteria_out) == 0) return(NA_character_)
    
    paste(criteria_out, collapse = "; ")
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
      "    .value.redlist-highlight { color: blue; font-weight: bold; }",
      "    .section-title { font-weight: 600; margin-top: 15px; border-bottom: 2px solid #99566a; padding-bottom: 3px; color: #333; display: flex; justify-content: space-between; align-items: center; }",
      "    .criteria-label { font-weight: 600; font-size: 14px; color: #99566a; }",
      "    .data-row { display: flex; justify-content: space-between; border-radius: 6px; padding: 4px 8px; }",
      "    .data-row:nth-child(even) { background-color: #f4ece9; }",
      "    .iucn-value { text-align: right; }",
      "    .label { font-weight: 400; color: #2f4f4f; }",
      "    .value { color: #333; }",
      "    .global-value { color: #2f7f73; font-weight: 600; }",
      "    table { width: 100%; border-collapse: collapse; margin-top: 5px; border: 1px solid #e2d4d0; }",
      "    th, td { border: 1px solid #e2d4d0; text-align: center; padding: 6px 8px; }",
      "    th { background-color: #e3f0e8; font-weight: 600; color: #2f4f4f; }",
      "    td:first-child { text-align: left; }",
      "    tr:nth-child(even) { background-color: #f9f6f4; }",
      "    a { color: #34916e; text-decoration: none; font-weight: 600; }",
      "    a:hover { text-decoration: underline; }",
      "    /* Force center alignment for SoIB table */",
      "    .soib-table td, .soib-table th { text-align: center !important; }",
      "  </style>",
      
      "</head>",
      "<body>",
      "  <div class='card'>",
      "    <div class='top-bar'>",
      glue("      <div class='species-header'>{sp$EnglishName} <i>{sp$ScientificName}</i></div>"),
      "    </div>",
      
      # ========================================================
      # TWO-COLUMN LAYOUT
      # ========================================================
      
      "    <div class='columns'>",
      
      # ========================================================
      # LEFT COLUMN
      # ========================================================
      
      "      <div>",
      
      # --------------------------------------------------------
      # REDLIST
      # --------------------------------------------------------
      
      "        <div class='section-title'>Redlist</div>",
      glue("        <div class='data-row'><span class='label'>Regional (Default):</span> <span class='value redlist-highlight'>{sp$RegionalRedlist}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Regional (Adjusted):</span> <span class='value'>{sp$AdjustedRegionalRedlist}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Global (BirdLife):</span> <span class='value'><a href='{sp$GlobalRedlistURL}' target='_blank'>{sp$GlobalRedlist}</a></span></div>"),
      glue("        <div class='data-row'><span class='label'>Migratory Status (India):</span> <span class='value'>{sp$MigratoryStatusIndia}</span></div>"),
      
      # --------------------------------------------------------
      # IUCN CRITERIA
      # --------------------------------------------------------
      
      "        <div class='section-title'>IUCN Criteria Met</div>",
      glue("        <div class='data-row'><span>Regional:</span><span class='iucn-value'>{combine_criteria(sp)}</span></div>"),
      glue("        <div class='data-row'><span>Global (BirdLife):</span><span class='iucn-value'>{sp$GlobalCriteriaString}</span></div>"),
      
      # --------------------------------------------------------
      # RANGE SIZE
      # --------------------------------------------------------
      
      "        <div class='section-title'>Range Size (sq. km.) <span class='criteria-label'>Criteria B</span></div>",
      glue("        <div class='data-row'><span class='label'>Extent of Occurrence (EOO):</span> <span class='value'>{sp$EOO} {na_blank(sp$MaxEOO)}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Area of Occupancy (AOO):</span> <span class='value'>{sp$MinAOO} {na_blank(sp$MaxAOO)}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Decline in EOO (%):</span> <span class='value'>{sp$DeclineEOO}</span></div>"),
      glue("        <div class='data-row'><span class='label'>EOO Change Year Band:</span> <span class='value'>{sp$EOOYearBandChange}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Global EOO (BirdLife):</span> <span class='global-value'>{sp$GlobalEOO}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Global AOO (BirdLife):</span> <span class='global-value'>{sp$GlobalAOO}</span></div>"),
      glue("        <div class='data-row'><span class='label'>% of Global Range (BirdLife):</span> <span class='value'>{sp$GlobalRangePercent}</span></div>"),
      glue("        <div class='data-row'><span class='label'>No. of Locations:</span> <span class='value'>{ifelse(is.na(sp$Locations), '> 10', ifelse(!is.na(sp$MinLocations) & !is.na(sp$MaxLocations), paste0(sp$Locations, ' (', sp$MinLocations, '–', sp$MaxLocations, ')'), sp$Locations))}</span></div>"),
      glue("        <div class='data-row'><span class='label'>No. of Subspecies (Synopsis):</span> <span class='value'>{sp$Subspecies}</span></div>"),
      
      # --------------------------------------------------------
      # STATE OF INDIA'S BIRDS
      # --------------------------------------------------------
      
      "        <div class='section-title'>State of India's Birds</div>",
      "        <table class='soib-table'>",
      "          <tr><th>Priority</th><th>Long-term Change</th><th>Current Annual Trend</th></tr>",
      "          <tr>",
      glue("            <td>{sp$SoIBPriority}</td>"),
      glue("            <td>{sp$LTC}</td>"),
      glue("            <td>{sp$CAT}</td>"),
      "          </tr>",
      "        </table>",
      
      
      "      </div>",
      
      # ========================================================
      # RIGHT COLUMN
      # ========================================================
      
      "      <div>",
      
      # --------------------------------------------------------
      # POPULATION DECLINE — CRITERIA C
      # --------------------------------------------------------
      
      glue("        <div class='section-title'>Population Decline {ifelse(is.na(sp$Decline3GEN_C1_Method), '', paste0('(', sp$Decline3GEN_C1_Method, ')'))} <span class='criteria-label'>Criteria A & C1</span></div>"),
      "        <table>",
      "          <tr><th style='text-align:left;'>Generations</th><th>1</th><th>2</th><th>3</th></tr>",
      "          <tr><th style='text-align:left;'>Decline %</th>",
      glue("            <td>{sp$Decline1GEN}</td>"),
      glue("            <td>{sp$Decline2GEN}</td>"),
      glue("            <td>{sp$Decline3GEN_C}</td>"),
      "          </tr>",
      "          <tr><th style='text-align:left;'>No. of Years</th>",
      glue("            <td>{sp$Years1GEN}</td>"),
      glue("            <td>{sp$Years2GEN}</td>"),
      glue("            <td>{sp$Years3GEN}</td>"),
      "          </tr>",
      "        </table>",      
      glue("        <div class='data-row'><span class='label'>Actual Trend (%):</span> <span class='value'>{sp$ActualDeclinePercentage_C1} {na_blank(sp$YearsActualDecline_C1)}</span></div>"),
      
      # --------------------------------------------------------
      # POPULATION DECLINE — CRITERIA A
      # --------------------------------------------------------
      
      "        <div class='section-title'>Population Decline (Inferred, or better) <span class='criteria-label'>Criteria A & C2</span></div>",
      "        <table>",
      "          <tr><th style='text-align:left;'>Decline (3 generations)</th><th>Upper CI</th><th>Mean</th><th>Lower CI</th></tr>",
      "          <tr>",
      "            <td>Projected Decline %</td>",
      glue("            <td>{sp$Decline3GEN_A}</td>"),
      glue("            <td>{sp$Decline3GEN_A_Mean}</td>"),
      glue("            <td>{sp$Decline3GEN_A_Lci}</td>"),
      "          </tr>",
      "        </table>",
      
      # --------------------------------------------------------
      # POPULATION TREND / CONTINUING DECLINE
      # --------------------------------------------------------
      
      glue("        <div class='data-row'><span class='label'>Generation Length (BirdLife):</span> <span class='value'>{sp$GenerationLength}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Actual Trend (%):</span> <span class='value'>{sp$ActualDeclinePercentage} {na_blank(sp$YearsActualDecline)}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Global Population Trend (BirdLife):</span> <span class='global-value'>{sp$GlobalPopulationTrend}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Continuing Decline (Regional):</span> <span class='value'>{sp$ContinuingDecline}</span></div>"),
      
      # --------------------------------------------------------
      # POPULATION
      # --------------------------------------------------------
      
      "        <div class='section-title'>Population (Mature Individuals) <span class='criteria-label'>Criteria C & D</span></div>",
      glue("        <div class='data-row'><span class='label'>Regional Population:</span> <span class='value'>{sp$TotalLikelyPop} {na_blank(sp$TotalMaxPop)}</span></div>"),
      glue("        <div class='data-row'><span class='label'>Global Population (BirdLife):</span> <span class='value'>{sp$GlobalPopulation}</span></div>"),
      glue("        <div class='data-row'><span class='label'>1% biogeographic population (Wetlands Intl.):</span> <span class='value'>{sp$BiogPop1Percent}</span></div>"),
      
      # --------------------------------------------------------
      # CONVENTIONS & LEGAL
      # --------------------------------------------------------
      
      "        <div class='section-title'>Conventions & Legal</div>",
      glue("        <div class='data-row'><span class='label'>CMS:</span> <span class='value'>{sp$CMS}</span></div>"),
      glue("        <div class='data-row'><span class='label'>CITES:</span> <span class='value'>{sp$CITES}</span></div>"),
      glue("        <div class='data-row'><span class='label'>WLPA Schedule:</span> <span class='value'>{sp$Schedule}</span></div>"),
      
      
      "      </div>",
      
      # ========================================================
      # CLOSE CARD / DOCUMENT
      # ========================================================
      
      "    </div>",
      "  </div>",
      "</body>",
      "</html>"
    )
    
    html_lines
  }

  # Generate HTML for each species
  #for(i in 1:nrow(species)) {
  #  html_content <- generate_html_pretty(species[i, ])
  #  file_name <- paste0(gsub(" ", "_", species$EnglishName[i]), ".html")
  #  file_name <- paste0(redlist_home_dir,"outputs\\",file_name)
  #  writeLines(html_content, con = file_name)
  #}
  
  filter_species <- c(
    "Ashambu Laughingthrush",
    "Banasura Laughingthrush",
    "Bank Myna",
    "Black-tailed Godwit",
    "Black-winged Kite",
    "Bugun Liocichla",
    "Common Babbler",
    "Common Kestrel",
    "Garganey",
    "Great Indian Bustard",
    "Himalayan Quail",
    "House Crow",
    "House Sparrow",
    "Indian Courser",
    "Indian Roller",
    "Indian Vulture",
    "Jerdon's Courser",
    "Large-billed Crow",
    "Lesser Florican",
    "Manipur Bush-Quail",
    "Mount Victoria Babax",
    "Narcondam Hornbill",
    "Nilgiri Laughingthrush",
    "Northern Pintail",
    "Siberian Crane",
    "Slender-billed Vulture",
    "White-bellied Heron",
    "White-eyed Buzzard",
    "White-rumped Vulture",
    "Yunnan Nuthatch"
  )  
  
  
  for(i in 1:nrow(species)) {
    
    if(!(species$EnglishName[i] %in% filter_species)) next;
    
    # Generate HTML
    html_content <- generate_html_pretty(species[i, ])
    
    # Temp HTML file (needed for screenshot)
    temp_html <- tempfile(fileext = ".html")
    writeLines(html_content, con = temp_html)
    
    # Output JPG file name
    output_file <- paste0(gsub(" ", "_", species$EnglishName[i]), ".jpg")
    output_file <- file.path(paste0(resultspath, "/outputs"), output_file)
    
    print(paste("Generating JPG for", species$EnglishName[i]))
    
    # Screenshot
    tryCatch({
      webshot2::webshot(
        temp_html,
        file = output_file,
        selector = "body",
        vwidth = 1024, #1088,
        vheight = 768, #1366,
        zoom = 2,
  #      cliprect = c(0, 0, 1024, 768), #1080, 1360),
        delay = 2  # replace with your config if needed
      )
    }, error = function(e) {
      print(paste("Card generation failed:", e$message))
    })
    # Optional: remove temp file
    unlink(temp_html)
  }
