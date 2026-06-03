library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(purrr)

scriptpath <- "00_scripts/iucn/"
datapath   <- "00_data"

source(file.path(scriptpath, "config.R"))

# -----------------------------
# LOAD DATA
# -----------------------------

EOO <- readRDS(file.path(scriptpath, "eoo.RDS"))
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
    filter(GridResolution == resolution) %>%
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
    filter(GridID %in% species_grid_ids)
  
  # -----------------------------
  # OCCUPANCY CATEGORIES
  # -----------------------------
  
  present_ids <- AOO_sp[[paste0("GridIDs_P_", resolution, "km")]][[1]]
  
  uncertain_ids <- AOO_sp[[paste0("GridIDs_U_", resolution, "km")]][[1]]
  
  # Prevent NULL errors
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
# FUNCTION TO CREATE 3 MAPS
# -----------------------------

plot3Maps <- function(sp,
                      EOO_sp,
                      EOOGrids_sp,
                      AOO_sp,
                      india_map,
                      in_grids)
{
  
  plots <- lapply(
    aoo_grid_sizes_km,
    
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
  
  subtitle_text <- paste(
    "MinAOO:", round(AOO_sp$MinAOO, 0),
    "| MaxAOO:", round(AOO_sp$MaxAOO, 0),
    "| LikelyEOO:", round(EOO_sp$LikelyEOO, 0),
    "| EOO Start:", EOO_sp$EOOStartYear
  )
  
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
  filter(MaxAOO < 30000)

species <- AOO$Species

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
    filter(Species == sp)
  
  AOO_sp <- AOO %>%
    filter(Species == sp)
  
  # Load species grid file
  EOOGrids_sp <- load_species_grid_data(sp)
  
  # Skip missing
  if (nrow(EOO_sp) == 0 ||
      is.null(EOOGrids_sp) ||
      nrow(AOO_sp) == 0) {
    
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