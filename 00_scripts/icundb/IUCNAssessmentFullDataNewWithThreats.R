library(httr)
library(jsonlite)
library(dplyr)
library(readxl)

token <- "wqHFfTu6ahmRAjmia8reNPyCxyZLn6V6apFW"

# Read BLI.Scientific.Name.2025 from the India sheet of the xlsx file
xlsx_data <- read_excel("02_SoIB_2025_main_v0.xlsx", sheet = "India")

# Split binomial name into genus_name and species_name
# For trinomial names (e.g. "Genus species subspecies"), only the first two parts are used
species_list <- xlsx_data %>%
  filter(!is.na(`BLI.Scientific.Name.2025`)) %>%
  mutate(
    genus_name   = sub("^(\\S+).*",         "\\1", `BLI.Scientific.Name.2025`),
    species_name = sub("^\\S+\\s+(\\S+).*", "\\1", `BLI.Scientific.Name.2025`)
  ) %>%
  select(
    `English Name`,
    `Scientific Name`,
    `SoIB 2023 Priority Status`,
    `eBird.English.Name.2025`,
    `eBird.Scientific.Name.2025`,
    `BLI.Common.Name.2025`,
    `BLI.Scientific.Name.2025`,
    `India.Checklist.Common.Name.2025`,
    `India.Checklist.Scientific.Name.2025`,
    genus_name,
    species_name
  )

all_results    <- list()
failed_species <- list()

# ---------- HELPER: safe GET with retry on rate-limit ----------
safe_get <- function(url, query = NULL, token, max_retries = 2, wait_base = 10) {
  for (attempt in 1:max_retries) {
    res <- GET(
      url = url,
      query = query,
      add_headers(Authorization = paste("Bearer", token))
    )
    raw_text <- content(res, as = "text", encoding = "UTF-8")
    status   <- status_code(res)

    # Check for valid JSON before returning
    is_json <- tryCatch({ fromJSON(raw_text); TRUE }, error = function(e) FALSE)

    if (is_json && status == 200) {
      return(raw_text)
    }

    # Rate-limited or server busy — wait and retry
    wait_secs <- wait_base * attempt
    cat(sprintf("  [Attempt %d] HTTP %d — non-JSON response: %s\n  Waiting %ds before retry...\n",
                attempt, status, trimws(substr(raw_text, 1, 80)), wait_secs))
    Sys.sleep(wait_secs)
  }
  cat("  Giving up after", max_retries, "attempts. Skipping.\n")
  return(NULL)
}

for (i in 1:nrow(species_list)) {
  english_name                    <- as.character(species_list$`English Name`[i])
  scientific_name                 <- as.character(species_list$`Scientific Name`[i])
  soib_priority_status            <- as.character(species_list$`SoIB 2023 Priority Status`[i])
  ebird_english_name              <- as.character(species_list$`eBird.English.Name.2025`[i])
  ebird_scientific_name           <- as.character(species_list$`eBird.Scientific.Name.2025`[i])
  bli_common_name                 <- as.character(species_list$`BLI.Common.Name.2025`[i])
  bli_scientific_name             <- as.character(species_list$`BLI.Scientific.Name.2025`[i])
  india_checklist_common_name     <- as.character(species_list$`India.Checklist.Common.Name.2025`[i])
  india_checklist_scientific_name <- as.character(species_list$`India.Checklist.Scientific.Name.2025`[i])
  genus   <- species_list$genus_name[i]
  species <- species_list$species_name[i]

  cat("Processing:", genus, species, "\n")

  # ---------- FIRST API ----------
  raw1 <- safe_get(
    url   = "https://api.iucnredlist.org/api/v4/taxa/scientific_name",
    query = list(genus_name = genus, species_name = species),
    token = token
  )

  if (is.null(raw1)) {
    failed_species[[length(failed_species) + 1]] <- data.frame(
      genus_name   = as.character(genus),
      species_name = as.character(species),
      reason       = "API call failed after 2 attempts",
      stringsAsFactors = FALSE
    )
    next
  }

  json1 <- fromJSON(raw1, flatten = TRUE)

  if (length(json1$assessments$assessment_id) == 0) {
    cat("  No assessment found\n")
    next
  }

  assessment_id <- json1$assessments$assessment_id[1]
  Sys.sleep(1)

  # ---------- SECOND API ----------
  raw2 <- safe_get(
    url   = paste0("https://api.iucnredlist.org/api/v4/assessment/", assessment_id),
    token = token
  )

  if (is.null(raw2)) {
    failed_species[[length(failed_species) + 1]] <- data.frame(
      genus_name   = as.character(genus),
      species_name = as.character(species),
      reason       = "Assessment API call failed after 2 attempts",
      stringsAsFactors = FALSE
    )
    next
  }

  json2 <- fromJSON(raw2, flatten = TRUE)

  # ---------- SAFE JSON CONVERTER ----------
  safe_json <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    out <- toJSON(x, auto_unbox = TRUE)
    return(as.character(out))     # **************** FORCE CHARACTER
  }

  # ---------- EXTRACT FIELDS (always character) ----------
  taxon_object_json       <- safe_json(json2$taxon)
  population_trend        <- safe_json(json2$population_trend)
  red_list_category       <- safe_json(json2$red_list_category)
  supplementary_info_json <- safe_json(json2$supplementary_info)
  documentation           <- safe_json(json2$documentation)
  biogeographical_realms  <- safe_json(json2$biogeographical_realms)
  conservation_actions    <- safe_json(json2$conservation_actions)
  faos                    <- safe_json(json2$faos)
  habitats_json           <- safe_json(json2$habitats)
  locations               <- safe_json(json2$locations)
  researches              <- safe_json(json2$researches)
  use_and_trade_json      <- safe_json(json2$use_and_trade)
  threats                 <- safe_json(json2$threats)
  credits                 <- safe_json(json2$credits)
  errata                  <- safe_json(json2$errata)
  references              <- safe_json(json2$references)
  growth_forms            <- safe_json(json2$growth_forms)
  lmes                    <- safe_json(json2$lmes)
  scopes                  <- safe_json(json2$scopes)
  stresses                <- safe_json(json2$stresses)
  systems                 <- safe_json(json2$systems)

  # ---------- BUILD A 1-ROW LIST ----------
  row_list <- list(
    english_name                    = english_name,
    scientific_name                 = scientific_name,
    soib_2023_priority_status       = soib_priority_status,
    ebird_english_name_2025         = ebird_english_name,
    ebird_scientific_name_2025      = ebird_scientific_name,
    bli_common_name_2025            = bli_common_name,
    bli_scientific_name_2025        = bli_scientific_name,
    india_checklist_common_name_2025     = india_checklist_common_name,
    india_checklist_scientific_name_2025 = india_checklist_scientific_name,
    genus_name   = as.character(genus),
    species_name = as.character(species),
    assessment_id = as.character(assessment_id),
    taxon_object_json = taxon_object_json,
    population_trend = population_trend,
    red_list_category = red_list_category,
    supplementary_info_json = supplementary_info_json,
    documentation = documentation,
    biogeographical_realms = biogeographical_realms,
    conservation_actions = conservation_actions,
    faos = faos,
    habitats_json = habitats_json,
    locations = locations,
    researches = researches,
    use_and_trade_json = use_and_trade_json,
    threats = threats,
    credits = credits,
    errata = errata,
    references = references,
    growth_forms = growth_forms,
    lmes = lmes,
    scopes = scopes,
    stresses = stresses,
    systems = systems
  )

  # Add scalar top-level fields
  for (name in names(json2)) {
    value <- json2[[name]]
    if (!is.list(value)) {
      row_list[[name]] <- as.character(value)[1]
    }
  }

  # ---------- Convert to 1-row data frame (safe) ----------
  df_row <- as.data.frame(row_list, stringsAsFactors = FALSE)

  all_results[[length(all_results) + 1]] <- df_row
}

final_df <- bind_rows(all_results)

write.csv(final_df, "iucn_assessments_full_output.csv", row.names = FALSE)
cat("Saved CSV successfully!\n")

# ---------- SAVE FAILED SPECIES ----------
if (length(failed_species) > 0) {
  failed_df <- bind_rows(failed_species)
  write.csv(failed_df, "iucn_failed_species.csv", row.names = FALSE)
  cat(sprintf("Saved %d failed species to iucn_failed_species.csv\n", nrow(failed_df)))
} else {
  cat("No failed species.\n")
}
