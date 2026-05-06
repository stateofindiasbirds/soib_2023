############################################################
# 0. LOAD LIBRARIES
############################################################
library(terra)
library(dplyr)
library(geodata)
library(FNN)   # for fast nearest neighbors

############################################################
# 1. LOAD EBIRD ABUNDANCE RASTER (3km)
############################################################
r_abund <- rast("purswa3_abundance_seasonal_mean_3km_2023.tif")

############################################################
# 2. LOAD INDIA BOUNDARY
############################################################
india <- geodata::gadm(country = "IND", level = 0, path = tempdir())
india_proj <- project(india, crs(r_abund))

############################################################
# 3. LOAD EBIRD RANGE MAP
############################################################
range <- vect("purswa3_range_smooth_9km_2023.gpkg")
range_proj <- project(range, crs(r_abund))

# Clip to India
range_india <- crop(range_proj, india_proj)
range_india <- mask(range_india, india_proj)

############################################################
# 4. MASK ABUNDANCE RASTER TO VALID REGION
############################################################
r_masked <- mask(crop(r_abund, range_india), range_india)

############################################################
# 5. CREATE OBSERVED COUNT RASTER
############################################################
r_count <- rast(r_masked)
values(r_count) <- NA

# Fill observed counts
r_count[ghswamphen_max$cell_id] <- ghswamphen_max$OBSERVATION.COUNT

############################################################
# 6. EXTRACT CELL COORDINATES
############################################################
coords_all <- xyFromCell(r_masked, 1:ncell(r_masked))

# Keep only valid cells (inside range)
valid_cells <- which(!is.na(values(r_masked)))
coords_valid <- coords_all[valid_cells, ]

############################################################
# 7. SPLIT KNOWN vs UNKNOWN CELLS
############################################################
known_cells <- which(!is.na(values(r_count)))
empty_cells <- setdiff(valid_cells, known_cells)

coords_known <- coords_all[known_cells, ]
coords_empty <- coords_all[empty_cells, ]

############################################################
# 8. EXTRACT VALUES FOR KNOWN CELLS
############################################################
C_known <- values(r_count)[known_cells]    # observed counts
RA_known <- values(r_masked)[known_cells] # relative abundance

############################################################
# 9. SET PARAMETERS
############################################################
k <- 10   # number of nearest neighbors

############################################################
# 10. FIND K-NEAREST NEIGHBORS
############################################################
nn <- get.knnx(data = coords_known,
               query = coords_empty,
               k = k)

nn_index <- nn$nn.index
nn_dist  <- nn$nn.dist

############################################################
# 11. PREDICTION FUNCTION
############################################################
predict_cell <- function(i) {
  
  # Neighbor indices
  neigh_idx <- nn_index[i, ]
  dist <- nn_dist[i, ]
  
  # Remove zero distance (rare but safe)
  dist[dist == 0] <- 1e-6
  
  # Distance weights
  w <- 1 / dist
  
  # Neighbor values
  Cj <- C_known[neigh_idx]
  RAj <- RA_known[neigh_idx]
  
  # Remove NA cases
  valid <- !is.na(Cj) & !is.na(RAj)
  
  if (sum(valid) < 3) return(NA)
  
  Cj <- Cj[valid]
  RAj <- RAj[valid]
  w  <- w[valid]
  
  # -------- LOCAL REGRESSION (weighted) --------
  model <- try(lm(Cj ~ RAj, weights = w), silent = TRUE)
  
  if (inherits(model, "try-error")) return(NA)
  
  RAi <- values(r_masked)[empty_cells[i]]
  
  pred <- predict(model, newdata = data.frame(RAj = RAi))
  
  return(max(pred, 0))  # avoid negative counts
}

############################################################
# 12. APPLY PREDICTION
############################################################
pred_values <- sapply(1:length(empty_cells), predict_cell)

############################################################
# 13. CREATE FINAL PREDICTED RASTER
############################################################
r_pred <- r_count

# Fill predictions
r_pred[empty_cells] <- pred_values

# Mask again to clean edges
r_pred <- mask(r_pred, range_india)

############################################################
# 14. VISUALIZE RESULT
############################################################
plot(r_pred,
     col = hcl.colors(50, "YlOrRd"),
     main = "Predicted Counts (Improved Model)")

plot(range_india, add = TRUE, border = "blue", lwd = 1)
plot(india_proj, add = TRUE, border = "black")