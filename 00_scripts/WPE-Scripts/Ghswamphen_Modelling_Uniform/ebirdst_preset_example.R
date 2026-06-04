library(fields)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)
library(ebirdst)
install.packages("pak")
pak::pak("ropensci/rnaturalearthhires")

# download example data, yellow-bellied sapsucker in michigan
ebirdst_download_status(species = "yebsap-example")

# load relative abundance raster stack with 52 layers, one for each week
abd <- load_raster("yebsap-example", resolution = "27km")

# load species specific mapping parameters
pars <- load_fac_map_parameters("yebsap-example")
# custom coordinate reference system
crs <- st_crs(pars$custom_projection)
# legend breaks
breaks <- pars$weekly_bins
# legend labels for top, middle, and bottom
labels <- pars$weekly_labels

# the date that each raster layer corresponds to is stored within the labels
weeks <- as.Date(names(abd))
print(weeks)

# select a week in the middle of the year
abd <- abd[[26]]

# project to species specific coordinates
# the nearest neighbor method preserves cell values across projections
abd_prj <- project(trim(abd), crs$wkt, method = "near")

# get reference data from the rnaturalearth package
# the example data currently shows only the US state of Michigan
wh_states <- ne_states(country = c("United States of America", "Canada"),
                       returnclass = "sf") |> 
  st_transform(crs = crs) |> 
  st_geometry()

# start plotting
par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))

# use raster bounding box to set the spatial extent for the plot
bb <- st_as_sfc(st_bbox(trim(abd_prj)))
plot(bb, col = "white", border = "white")
# add background reference data
plot(wh_states, col = "#cfcfcf", border = NA, add = TRUE)

# plot zeroes as light gray
plot(abd_prj, col = "#e6e6e6", maxpixels = ncell(abd_prj),
     axes = FALSE, legend = FALSE, add = TRUE)

# define color palette
pal <- ebirdst_palettes(length(breaks) - 1, type = "weekly")
# plot abundance
plot(abd_prj, col = pal, breaks = breaks, maxpixels = ncell(abd_prj),
     axes = FALSE, legend = FALSE, add = TRUE)

# state boundaries
plot(wh_states, add = TRUE, col = NA, border = "white", lwd = 1.5)

# legend
label_breaks <- seq(0, 1, length.out = length(breaks))
image.plot(zlim = c(0, 1), breaks = label_breaks, col = pal,
           smallplot = c(0.90, 0.93, 0.15, 0.85),
           legend.only = TRUE,
           axis.args = list(at = c(0, 0.5, 1), 
                            labels = round(labels, 2),
                            cex.axis = 0.9, lwd.ticks = 0))

