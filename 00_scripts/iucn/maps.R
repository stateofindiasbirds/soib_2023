library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(purrr)

scriptpath <- "00_scripts/iucn/"
datapath   <- "00_data"

source(file.path(scriptpath, "config.R"))

test_species <- c (
  "Jerdon's Babbler",
  "Marsh Babbler",
  "Black-breasted Parrotbill",
  "Jerdon's Bushchat",
  "Slender-billed Babbler",
  "Finn's Weaver",
  "Nilgiri Pipit",
  "Swamp Grass Babbler",
  "Rufous-vented Grass Babbler")

sensitive_species <- c ("Sarus Crane",
                        "Finn's Weaver",
                        "Black-bellied Tern",
                        "Great Hornbill",
                        "Asian Houbara",
                        "Derbyan Parakeet",
                        "Sociable Lapwing",
                        "Great Indian Bustard",
                        "Saker Falcon",
                        "Lesser Florican",
                        "Green Avadavat",
                        "Yellow-breasted Bunting")

test_species <- sensitive_species
# -----------------------------
# LOAD DATA
# -----------------------------

EOO <- readRDS(file.path(scriptpath, "eoo.RDS")) 
EOO <- EOO %>% dplyr::filter (Species %in% test_species)
AOO <- readRDS(file.path(scriptpath, "aoo.RDS"))

# Master sf grids
in_grids <- readRDS(file.path(scriptpath, "in_grids.RDS"))

# India boundary
zip_file <- file.path(datapath, regionshape, paste0(regionshape, ".shp"))

india_map <- st_read(zip_file, quiet = TRUE)

# -----------------------------
# FUNCTION TO LOAD SPECIES GRID
# -----------------------------

load_species_grid_data <- function(sp)
{
  
  safe_name <- gsub("[^A-Za-z0-9_]", "_", sp)
  
  file <- file.path(scriptpath, "tmp", paste0(safe_name, ".rds"))
  
  if (!file.exists(file)) {
    return(NULL)
  }
  
  x <- readRDS(file)
  
  # Extract only grid_data objects
  grid_data_list <- x[names(x) == "grid_data"]
  
  if (length(grid_data_list) == 0) {
    return(NULL)
  }
  
  grid_data <- bind_rows(grid_data_list)
  
  return(grid_data)
}

# -----------------------------
# FUNCTION TO CREATE ONE MAP
# -----------------------------

plotMap <- function(sp,
                    resolution,
                    EOOGrids_sp,
                    AOO_sp,
                    india_map,
                    in_grids)
{
  
  # -----------------------------
  # LOAD MASTER GRID
  # -----------------------------
  
  grid_sf <- in_grids[[as.character(resolution)]]
  
  # -----------------------------
  # GET SPECIES GRID IDS
  # -----------------------------
  
  species_grid_ids <- EOOGrids_sp %>%
    dplyr::filter(GridResolution == resolution) %>%
    pull(GridID) %>%
    as.character()
  
  # -----------------------------
  # HANDLE EMPTY DATA
  # -----------------------------
  
  if (length(species_grid_ids) == 0) {
    
    return(
      ggplot() +
        theme_void() +
        ggtitle(paste0(resolution, " km\nNo data"))
    )
  }
  
  # -----------------------------
  # EXTRACT MATCHING POLYGONS
  # -----------------------------
  
  grid_data <- grid_sf %>%
    mutate(GridID = as.character(GridID)) %>%
    dplyr::filter(GridID %in% species_grid_ids)
  
  # -----------------------------
  # OCCUPANCY CATEGORIES
  # -----------------------------
  
  present_ids <- character(0)
  uncertain_ids <- character(0)
  
  if (!is.null(AOO_sp) && nrow(AOO_sp) > 0) {
    
    present_col <- paste0("GridIDs_P_", resolution, "km")
    uncertain_col <- paste0("GridIDs_U_", resolution, "km")
    
    if (present_col %in% names(AOO_sp)) {
      present_ids <- AOO_sp[[present_col]][[1]]
    }
    
    if (uncertain_col %in% names(AOO_sp)) {
      uncertain_ids <- AOO_sp[[uncertain_col]][[1]]
    }
  }
  
  if (is.null(present_ids)) {
    present_ids <- character(0)
  }
  
  if (is.null(uncertain_ids)) {
    uncertain_ids <- character(0)
  }
  
  present_ids <- as.character(present_ids)
  uncertain_ids <- as.character(uncertain_ids)
  
  # -----------------------------
  # ASSIGN CATEGORY
  # -----------------------------
  
  grid_data <- grid_data %>%
    mutate(
      Category = case_when(
        GridID %in% present_ids ~ "Present",
        GridID %in% uncertain_ids ~ "Uncertain",
        TRUE ~ "Absent"
      )
    )
  
  # -----------------------------
  # AUTO ZOOM
  # -----------------------------
  
  bbox <- st_bbox(grid_data)
  
  x_buffer <- (bbox$xmax - bbox$xmin) * 0.15
  y_buffer <- (bbox$ymax - bbox$ymin) * 0.15
  
  # Handle very tiny ranges
  if (x_buffer == 0) x_buffer <- 0.5
  if (y_buffer == 0) y_buffer <- 0.5
  
  xmin <- bbox$xmin - x_buffer
  xmax <- bbox$xmax + x_buffer
  
  ymin <- bbox$ymin - y_buffer
  ymax <- bbox$ymax + y_buffer
  
  # -----------------------------
  # CREATE MAP
  # -----------------------------
  
  # -----------------------------
  # CREATE MAP
  # -----------------------------
  
  plot <- ggplot() +
    
    # India outline
    geom_sf(
      data = india_map,
      fill = "grey97",
      color = "grey60",
      linewidth = 0.2
    ) +
    
    # Species grids
    geom_sf(
      data = grid_data,
      aes(fill = Category),
      color = "grey25",
      linewidth = 0.12,
      alpha = 0.9
    ) +
    
    # Better colours
    scale_fill_manual(
      values = c(
        Present = "#E69F00",     # orange
        Uncertain = "#F0E442",   # soft yellow
        Absent = "white"
      ),
      drop = FALSE,
      name = "Occupancy"
    ) +
    
    # Auto zoom
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      expand = FALSE
    ) +
    
    ggtitle(paste0(resolution, " km")) +
    
    theme_minimal() +
    
    theme(
      
      # Remove axes
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      
      # Remove grids
      panel.grid = element_blank(),
      
      # Legend
      legend.position = "right",
      legend.title = element_text(
        size = 9,
        face = "bold"
      ),
      legend.text = element_text(size = 8),
      
      # Titles
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      )
    )
  return(plot)
}

# -----------------------------
# FUNCTION TO CREATE FULL INDIA EOO MAP
# -----------------------------

plotEOO <- function(sp,
                    EOOGrids_sp,
                    india_map,
                    in_grids)
{
  
  # Use 8 km master grid for the EOO display
  resolution <- 8
  grid_sf <- in_grids[[as.character(resolution)]]
  
  # Get EOO grid IDs
  eoo_grid_ids <- EOOGrids_sp %>%
    dplyr::filter(GridResolution == resolution) %>%
    dplyr::pull(GridID) %>%
    as.character()
  
  # Extract matching EOO polygons
  eoo_data <- grid_sf %>%
    dplyr::mutate(GridID = as.character(GridID)) %>%
    dplyr::filter(GridID %in% eoo_grid_ids)
  
  # Plot full India with EOO highlighted
  plot <- ggplot() +
    
    geom_sf(
      data = india_map,
      fill = "grey97",
      color = "grey60",
      linewidth = 0.2
    ) +
    
    geom_sf(
      data = eoo_data,
      fill = "#E69F00",
      color = "grey25",
      linewidth = 0.12
    ) +
    
    # Full India extent; do not zoom to the species range
    coord_sf(expand = FALSE) +
    
    ggtitle("EOO") +
    
    theme_minimal() +
    
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 11
      ),
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      panel.background = element_rect(
        fill = "white",
        color = NA
      )
    )
  
  return(plot)
}

# -----------------------------
# FUNCTION TO CREATE 3 MAPS
# -----------------------------

# -----------------------------
# FUNCTION TO CREATE 3 MAPS
# -----------------------------

plot3Maps <- function(sp,
                      EOO_sp,
                      EOOGrids_sp,
                      AOO_sp,
                      india_map,
                      in_grids)
{
  
  # First map: full India EOO
  eoo_plot <- plotEOO(
    sp,
    EOOGrids_sp,
    india_map,
    in_grids
  )
  
  # Second and third maps: 8 km and 4 km occupancy
  resolutions <- c(8, 4)
  
  occupancy_plots <- lapply(
    resolutions,
    function(grid_size_km) {
      
      plotMap(
        sp,
        grid_size_km,
        EOOGrids_sp,
        AOO_sp,
        india_map,
        in_grids
      )
    }
  )
  
  # Combine all three maps in the required order
  plots <- c(list(eoo_plot), occupancy_plots)
  
  # Summary subtitle
  if (nrow(AOO_sp) > 0) {
    
    subtitle_text <- paste(
      "MinAOO:", round(AOO_sp$MinAOO[1], 0),
      "| MaxAOO:", round(AOO_sp$MaxAOO[1], 0),
      "| LikelyEOO:", round(EOO_sp$LikelyEOO[1], 0),
      "| EOO Start:", EOO_sp$EOOStartYear[1]
    )
    
  } else {
    
    subtitle_text <- paste(
      "AOO: not available",
      "| LikelyEOO:", round(EOO_sp$LikelyEOO[1], 0),
      "| EOO Start:", EOO_sp$EOOStartYear[1]
    )
  }
  
  combined_plot <- (
    
    wrap_plots(
      plots,
      ncol = 3,
      guides = "collect"
    ) &
      
      theme(
        legend.position = "right",
        legend.title = element_blank()
      )
    
  ) +
    
    plot_annotation(
      title = sp,
      subtitle = subtitle_text
    )
  
  return(combined_plot)
}

# -----------------------------
# SPECIES LIST
# -----------------------------

AOO <- AOO %>%
  dplyr::filter(MaxAOO < 30000)

species <- AOO$Species
species <- EOO$Species

# -----------------------------
# OUTPUT DIRECTORY
# -----------------------------

output_dir <- file.path(scriptpath, "maps")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -----------------------------
# CREATE MAPS
# -----------------------------

lapply(species, function(sp) {
  
  cat("Processing:", sp, "\n")
  
  # Species data
  EOO_sp <- EOO %>%
    dplyr::filter(Species == sp)
  
  AOO_sp <- AOO %>%
    dplyr::filter(Species == sp)
  
  # Load species grid file
  EOOGrids_sp <- load_species_grid_data(sp)
  
  # Skip missing
  if (nrow(EOO_sp) == 0 ||
      0 #is.null(EOOGrids_sp) 
      # nrow(AOO_sp) == 0
      ) {
    
    cat("Skipping:", sp, "- missing data\n")
    return(NULL)
  }
  
  # Create maps
  combined_plot <- plot3Maps(
    sp,
    EOO_sp,
    EOOGrids_sp,
    AOO_sp,
    india_map,
    in_grids
  )
  
  # Save
  ggsave(
    filename = file.path(output_dir, paste0(sp, ".jpg")),
    plot = combined_plot,
    width = 12,
    height = 4,
    dpi = 300
  )
  
  cat("Saved:", sp, "\n")
  
})