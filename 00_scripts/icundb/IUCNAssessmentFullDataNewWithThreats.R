library(httr)
library(jsonlite)
library(dplyr)

token <- "wqHFfTu6ahmRAjmia8reNPyCxyZLn6V6apFW"

species_list <- read.csv("indianspecieslist.csv", stringsAsFactors = FALSE)

all_results <- list()

for (i in 1:nrow(species_list)) {
  genus <- species_list$genus_name[i]
  species <- species_list$species_name[i]
  
  cat("Processing:", genus, species, "\n")
  
  # ---------- FIRST API ----------
  res1 <- GET(
    url = "https://api.iucnredlist.org/api/v4/taxa/scientific_name",
    query = list(genus_name = genus, species_name = species),
    add_headers(Authorization = paste("Bearer", token))
  )
  
  json1 <- fromJSON(content(res1, as="text", encoding="UTF-8"), flatten = TRUE)
  
  if (length(json1$assessments$assessment_id) == 0) {
    cat("  No assessment found\n")
    next
  }
  
  assessment_id <- json1$assessments$assessment_id[1]
  Sys.sleep(1)
  
  # ---------- SECOND API ----------
  res2 <- GET(
    url = paste0("https://api.iucnredlist.org/api/v4/assessment/", assessment_id),
    add_headers(Authorization = paste("Bearer", token))
  )
  
  json2 <- fromJSON(content(res2, as="text", encoding="UTF-8"), flatten = TRUE)
  
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
    genus_name = as.character(genus),
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
