#################### Estimate Count in empty cell i, using known neighbours
# data-informed spatial imputation model constrained by species range and eBird abundance
# Step1: Prepare rasters
library(terra)
library(dplyr)

# Load abundance raster
r_abund <- rast("purswa3_abundance_seasonal_mean_3km_2023.tif")

# India boundary
library(geodata)
india <- geodata::gadm(country = "IND", level = 0, path = tempdir())
india_proj <- project(india, crs(r_abund))

# Range map
range <- vect("purswa3_range_smooth_9km_2023.gpkg")
range_proj <- project(range, crs(r_abund))

# Clip range to India
range_india <- crop(range_proj, india_proj)
range_india <- mask(range_india, india_proj)

# Step2: Mask raster to valid prediction zone
# Mask abundance to range ∩ India
r_masked <- mask(crop(r_abund, range_india), range_india)

# Step3: Create observed count raster
# Empty raster
r_count <- rast(r_masked)
values(r_count) <- NA
# Fill observed values
r_count[ghswamphen_max$cell_id] <- ghswamphen_max$OBSERVATION.COUNT

# Step4: Identify empty vs known cells
# Cells inside valid region
valid_cells <- which(!is.na(values(r_masked)))
# Known cells
known_cells <- which(!is.na(values(r_count)))
# Empty cells (within range)
empty_cells <- setdiff(valid_cells, known_cells)

# Step5: Neighbor structure - Use 8-direction neighbors:
neighbors <- adjacent(r_masked,
                      cells = valid_cells,
                      directions = 8,
                      pairs = TRUE)

# Step6: Prediction function
predict_cell <- function(cell_id) {
  
  # Find neighbors of this cell
  neigh <- neighbors[neighbors[,1] == cell_id, 2]
  
  # Keep only neighbors with observed counts
  neigh <- neigh[!is.na(values(r_count)[neigh])]
  
  if (length(neigh) == 0) return(NA)
  
  # Extract values
  Cj <- values(r_count)[neigh]
  RAj <- values(r_masked)[neigh]
  RAi <- values(r_masked)[cell_id]
  
  # Avoid division by zero
  if (sum(RAj, na.rm = TRUE) == 0) return(NA)
  
  # Prediction formula
  Ci_hat <- RAi * (sum(Cj, na.rm = TRUE) / sum(RAj, na.rm = TRUE))
  
  return(Ci_hat)
}

# Step7: Apply prediction
pred_values <- sapply(empty_cells, predict_cell)

# Fill predictions into raster
r_pred <- r_count
r_pred[empty_cells] <- pred_values

# Step8: Final mask
r_pred <- mask(r_pred, range_india)

# Step9: Visualization
plot(r_pred,
     col = hcl.colors(50, "YlOrRd"),
     main = "Predicted Counts (Observed + Interpolated)")

plot(range_india, add = TRUE, border = "blue")
plot(india_proj, add = TRUE)