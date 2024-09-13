library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
source("config.R")

plotMap <- function (sp, resolution, EOO, EOOGrids, AOO, kerala_map)
{

  eoo_map <- EOO$EOOMap[[1]]

  # Assuming EOOGrids$EOOGridMap[[1]] contains the grid geometries
  
  # Extract grid IDs from EOOGrids
  grid_data <- EOOGrids$EOOGridMap[[1]] %>%
    mutate(GridID = as.character(GridID))  # Ensure GridID is a character type for matching

  # Create a category column based on GridIDs
  grid_data <- grid_data %>%
    mutate(
      Category = case_when(
        GridID %in% AOO$GridIDs_P_8km[[1]] ~ "Present",
        GridID %in% AOO$GridIDs_U_8km[[1]] ~ "Uncertain",
        GridID %in% AOO$GridIDs_P_4km[[1]] ~ "Present",
        GridID %in% AOO$GridIDs_U_4km[[1]] ~ "Uncertain",
        GridID %in% AOO$GridIDs_P_2km[[1]] ~ "Present",
        GridID %in% AOO$GridIDs_U_2km[[1]] ~ "Uncertain",
        TRUE ~ "Absent"
      )
    )


# Assuming `kerala_map` is already defined


  plot <- ggplot(data = kerala_map) +
    geom_sf(fill = NA, color = "red", linetype = "dotted", size = 1) +  # Red dotted line for Kerala
    geom_sf(data = grid_data, aes(fill = Category, color = Category), size = 1, alpha = 0.6) +
    scale_fill_manual(values = c(
      "Present" = "orange",  # Fill yellow for GridIDs_P_8km
      "Uncertain" = "yellow",   # Fill cream for GridIDs_U_8km
      "Absent" = "white"                 # No fill for other grids
    )) +
    scale_color_manual(values = c(
      "Present" = "green",   # Line color green for GridIDs_P_8km
      "Uncertain" = "green",   # Line color green for GridIDs_U_8km
      "Absent" = "green"            # Line color green for other grids
    )) +
    theme_minimal()  # Minimal theme for a clean look
  
  return (plot)  
}


plot3Maps <- function (sp, EOO, EOOGrids, AOO, kerala_map)
{
  plots <- lapply(grid_sizes_km, function(grid_size_km) {
    plotMap(sp, grid_size_km, EOO, EOOGrids %>% filter (GridResolution == grid_size_km), AOO, kerala_map)
  })
  
  title_text <- sp
  
  # Add additional details for MinAOO, MaxAOO, etc.
  subtitle_text <- paste("MinAOO:", round(AOO$MinAOO,0), 
                         "MaxAOO:", round(AOO$MaxAOO,0), 
                         "LikelyEOO:", round(EOO$LikelyEOO,0), 
                         "EOO Start Year:", EOO$EOOStartYear)
  # Combine the three plots side by side using patchwork
  combined_plot <- wrap_plots(plots, ncol = 3) + 
    plot_annotation ( 
        title = title_text,  # Add species name below the plots
        subtitle = subtitle_text
    )
  
  return (combined_plot)
}



kerala_map <- st_read(paste0(dir,regionshape)) %>% filter (STATE_NAME == "Kerala")
sp <- "Brahminy Kite"
resolution <- 4

EOO <- readRDS ("eoo.RDS")
EOOGrids <- readRDS ("grid_maps.RDS")
AOO <- readRDS ("aoo.RDS")

species <- AOO$Species

species <- c (
  "Brahminy Kite",
  "White-browed Bulbul",
  "Yellow-browed Bulbul",
  "Malabar Gray Hornbill",
  "Lesser Coucal",
  "Oriental Scops-Owl",
  "Rufous-bellied Eagle",
  "Sanderling",
  "White-cheeked Barbet",
  "Malabar Barbet",
  "Bay-backed Shrike",
  "Jerdon's Baza",
  "Legge's Hawk-Eagle",
  "Lesser Fish-Eagle",
  "Spot-bellied Eagle-Owl",
  "Golden-headed Cisticola"
)


# Iterate over all species and save the plots
lapply(species, function(sp) {
  # Call plot3Maps for each species
  
  EOO_sp <- EOO %>% filter (Species == sp)
  EOOGrids_sp <- EOOGrids  %>% filter (Species == sp)
  AOO_sp <- AOO %>% filter (Species == sp)                      
  
  combined_plot <- plot3Maps(sp, EOO_sp, EOOGrids_sp, AOO_sp, kerala_map)

  # Save the combined plot as a JPG
  ggsave(paste0(".\\maps\\", sp,".jpg"), plot = combined_plot, width = 12, height = 4)  # Adjust width/height as needed
})
