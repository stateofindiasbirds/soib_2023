
# aoo_excluder.R
#
# Identifies grids eligible for expert-filled absences.
# This file is sourced from aoo.R after config.R has been sourced.
#
# Exclusion rules:
# 1. Grids fully inside an individually configured region.
# 2. Grids spanning multiple regions, when all of the grid's
#    area inside India is covered by configured exclusion regions.
# 3. Grids clipped by India's national boundary, when the
#    portion inside India is fully covered by configured
#    exclusion regions.
#
# A grid is NOT excluded merely because it intersects an
# excluded region. All of its area inside India must be covered
# by the union of configured exclusion regions.

# ------------------------------------------------------------
# 1. Load state/district polygons
# ------------------------------------------------------------

region_path <- file.path(
  dataglobal,
  districtshape,
  paste0(districtshape, ".shp")
)

region_polygons <- sf::st_read(
  region_path,
  quiet = TRUE
) %>%
  dplyr::mutate(
    STATE_C = as.character(STATE_C),
    COUNTY_ = as.character(COUNTY_),
    REGION_CODE = COUNTY_
  ) %>%
  sf::st_make_valid()

# ------------------------------------------------------------
# 2. Read expert-absence configuration
# ------------------------------------------------------------

expert_absence_path <- file.path(
  dataglobal,
  expertabsencefile
)

expert_absence_config <- read.csv(
  expert_absence_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
) %>%
  dplyr::transmute(
    Species = trimws(as.character(Species)),
    Regions = trimws(as.character(Regions))
  ) %>%
  dplyr::filter(!is.na(Species), Species != "") %>%
  dplyr::mutate(
    Regions = dplyr::if_else(
      is.na(Regions),
      "",
      Regions
    )
  )

if (anyDuplicated(expert_absence_config$Species)) {
  stop(
    "Duplicate species in expert-absence configuration: ",
    paste(
      unique(
        expert_absence_config$Species[
          duplicated(expert_absence_config$Species)
        ]
      ),
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------
# 3. Load grid polygons: use 4 km and 8 km only
# ------------------------------------------------------------

in_grids <- readRDS(
  file.path(scriptpath, "in_grids.RDS")
)

grid_polygons <- dplyr::bind_rows(
  lapply(
    c("4", "8"),
    function(resolution) {
      
      if (!resolution %in% names(in_grids)) {
        stop(
          "Grid resolution not found in in_grids: ",
          resolution
        )
      }
      
      in_grids[[resolution]] %>%
        dplyr::mutate(
          GridResolution = as.numeric(resolution),
          GridID = as.character(GridID)
        )
    }
  )
)

if (is.na(sf::st_crs(grid_polygons))) {
  stop("Grid polygons have no defined CRS.")
}

if (is.na(sf::st_crs(region_polygons))) {
  stop("Region polygons have no defined CRS.")
}

if (
  sf::st_crs(grid_polygons) !=
  sf::st_crs(region_polygons)
) {
  grid_polygons <- sf::st_transform(
    grid_polygons,
    sf::st_crs(region_polygons)
  )
}

# ------------------------------------------------------------
# 3A. Construct the national boundary from region polygons
# ------------------------------------------------------------
#
# The union of all district polygons provides the India
# boundary used to clip grids for the exclusion test.
#
# IMPORTANT:
# This assumes the district shapefile covers the relevant
# Indian territory and is already clipped to India's boundary.

india_boundary <- sf::st_union(
  sf::st_geometry(region_polygons)
)

india_boundary <- sf::st_make_valid(
  india_boundary
)

# ------------------------------------------------------------
# 4. Resolve one state or district code to its polygon
# ------------------------------------------------------------

get_region_geometry <- function(code) {
  
  # A state code selects all districts belonging to that state.
  if (code %in% unique(region_polygons$STATE_C)) {
    
    selected_regions <- region_polygons %>%
      dplyr::filter(STATE_C == code)
    
    # A district code selects only that district.
  } else if (code %in% unique(region_polygons$COUNTY_)) {
    
    selected_regions <- region_polygons %>%
      dplyr::filter(COUNTY_ == code)
    
  } else {
    stop(
      "Unknown region code in expert-absence CSV: ",
      code
    )
  }
  
  if (nrow(selected_regions) == 0) {
    stop("No polygons found for region code: ", code)
  }
  
  sf::st_union(
    sf::st_geometry(
      sf::st_make_valid(selected_regions)
    )
  )
}

# ------------------------------------------------------------
# 5. Find grids fully covered by configured regions
# ------------------------------------------------------------
#
# The test is performed on the portion of each grid inside
# India, rather than on the original, potentially un-clipped
# grid polygon.
#
# This handles:
# - grids inside one configured region;
# - grids spanning multiple configured regions;
# - grids clipped by the national boundary.
#
# A grid extending into a non-configured region within India
# will NOT qualify.

get_grids_inside_geometry <- function(target_geometry) {
  
  # Restrict the test to grids intersecting the configured
  # exclusion regions.
  candidates <- sf::st_filter(
    grid_polygons,
    target_geometry,
    .predicate = sf::st_intersects
  )
  
  if (nrow(candidates) == 0) {
    return(
      data.frame(
        GridResolution = numeric(),
        GridID = character()
      )
    )
  }
  
  # Clip candidate grids to India's national boundary.
  # This removes the portion of a grid outside India from
  # the coverage test.
  candidates_inside_india <- sf::st_intersection(
    candidates,
    sf::st_sf(
      geometry = india_boundary
    )
  )
  
  # Remove empty geometries, if any.
  candidates_inside_india <- candidates_inside_india[
    !sf::st_is_empty(candidates_inside_india),
  ]
  
  if (nrow(candidates_inside_india) == 0) {
    return(
      data.frame(
        GridResolution = numeric(),
        GridID = character()
      )
    )
  }
  
  # The entire portion of each grid inside India must be
  # covered by the union of the configured exclusion regions.
  fully_covered <- lengths(
    sf::st_covered_by(
      candidates_inside_india,
      target_geometry
    )
  ) > 0
  
  candidates_inside_india %>%
    dplyr::filter(fully_covered) %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      GridResolution = as.numeric(GridResolution),
      GridID = as.character(GridID)
    ) %>%
    dplyr::distinct()
}

# ------------------------------------------------------------
# 6. Public helper called by aoo.R
# ------------------------------------------------------------

get_expert_absence_grids <- function() {
  
  output <- vector(
    "list",
    nrow(expert_absence_config)
  )
  
  for (i in seq_len(nrow(expert_absence_config))) {
    
    species <- expert_absence_config$Species[i]
    regions_text <- expert_absence_config$Regions[i]
    
    if (is.na(regions_text) || !nzchar(regions_text)) {
      next
    }
    
    region_codes <- strsplit(
      regions_text,
      ";",
      fixed = TRUE
    )[[1]]
    
    region_codes <- unique(trimws(region_codes))
    region_codes <- region_codes[nzchar(region_codes)]
    
    if (length(region_codes) == 0) {
      next
    }
    
    # Resolve every configured state/district code.
    region_geometries <- lapply(
      region_codes,
      get_region_geometry
    )
    
    # Combine all configured regions for this species.
    #
    # A grid spanning adjacent configured districts/states
    # can now qualify if their combined geometry covers the
    # grid's entire portion inside India.
    combined_geometry <- sf::st_union(
      do.call(c, region_geometries)
    )
    
    # Find all qualifying grids at 4 km and 8 km.
    species_grids <- get_grids_inside_geometry(
      combined_geometry
    )
    
    if (nrow(species_grids) > 0) {
      
      output[[i]] <- species_grids %>%
        dplyr::mutate(
          Species = species,
          ExpertAbsence = TRUE
        ) %>%
        dplyr::select(
          Species,
          GridResolution,
          GridID,
          ExpertAbsence
        )
    }
  }
  
  result <- dplyr::bind_rows(output) %>%
    dplyr::distinct(
      Species,
      GridResolution,
      GridID,
      .keep_all = TRUE
    )
  
  result
}
