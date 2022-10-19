### joinmapvars ########################################

# adapted from Ashwin's "addmapvars" function to allow use within data preparation steps
# i.e., inputs and outputs are data objects in the environment, no writing of files involved

# data: main data object to which map vars will be added (needs to be sliced already!)
# admin = T if DISTRICT and ST_NM from shapefile required, else F
# grids = T if grid cells (four resolutions) from shapefile required, else F


#### maps.RData must already be loaded
#### column names here are all uppercase, unlike in Ashwin's function


joinmapvars = function(data, admin = T, grids = T){
  
  require(tidyverse)
  # require(data.table)
  require(sp)
  require(rgeos)
  
  # single object at group ID level (same group ID, same grid/district/state)
  temp0 <- data 

  
  ### add columns with DISTRICT and ST_NM to main data 
  
  if (admin == T) {
    
    temp = temp0 # separate object to prevent repeated slicing (intensive step)
    
    rownames(temp) = temp$GROUP.ID # only to setup adding the group.id column for the future left_join
    coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF
    proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    temp = sp::over(temp, districtmap) %>% # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
      dplyr::select(1, 2) %>% 
      rename(DISTRICT = dtname,
             ST_NM = stname) %>% 
      rownames_to_column("GROUP.ID") 
    
    data = left_join(temp, data)
    
  }
  
  ### add grid cell info (at four resolutions) to main data
  
  if (grids == T) {
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg1) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG1 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg2) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG2 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg3) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG3 = id)
    
    data = left_join(temp, data)
    
    
    temp = temp0
    
    rownames(temp) = temp$GROUP.ID
    coordinates(temp) = ~LONGITUDE + LATITUDE
    
    temp = sp::over(temp, gridmapg4) %>% 
      rownames_to_column("GROUP.ID") %>% 
      rename(GRIDG4 = id)
    
    data = left_join(temp, data)
  
  }
  
  return(data)
  
}



### creating maps with ggplot ########################################

# Indirectly referring to variables:
# https://ggplot2-book.org/programming.html#indirectly-referring-to-variables

# The key to calling a tidy evaluation function inside of another function is to 
# quote (with enquo()) and unquote (with !!).
# from https://www.tidyverse.org/blog/2018/07/ggplot2-tidy-evaluation/

gg_map <- function(data, datalong, datalat, sf = TRUE, facetvar, ncol = 2,
                   poly1, poly1long = long, poly1lat = lat,
                   poly2, poly2long = long, poly2lat = lat, poly2type = "grid",
                   mainvar, na_fill = "#CCCCCC",
                   title, subtitle, legend_title) {
  
  # if the data object provided is an sf object, it should also be used in the geom_sf() call
  if (sf == TRUE) {
    data_sf <- data
  }
  # if polygon to be plotted within India is grid, size should be bigger than for regions
  if (poly2type == "grid") {
    poly2size <- 0.2
  } else if (poly2type == "region") {
    poly2size <- 0.1
  }
  
  # "enquoting" the data-vars for tidyeval
  
  facetvar <- enquo(facetvar)
  mainvar <- enquo(mainvar)
  # na_fill <- enquo(na_fill)
  # title <- enquo(title)
  # subtitle <- enquo(subtitle)
  # legend_title <- enquo(legend_title)
  
  datalong <- enquo(datalong)
  datalat <- enquo(datalat)
  poly1long <- enquo(poly1long)
  poly1lat <- enquo(poly1lat)
  poly2long <- enquo(poly2long)
  poly2lat <- enquo(poly2lat)
  
  
  # plotting
  
  ggplot(data, aes(!!datalong, !!datalat)) +
    facet_wrap(vars(!!facetvar), ncol = ncol) +
    geom_polygon(data = poly1,
                 aes(!!poly1long, !!poly1lat, group = group),
                 colour = "black", fill = NA, size = 0.2) +
    # geom_polygon(data = poly2,
    #              aes(!!poly2long, !!poly2lat, group = group, fill = !!mainvar)) +
    {if (sf == TRUE) {
      geom_sf(data = data_sf, aes(geometry = geometry, fill = !!mainvar),
              size = poly2size)
    }} +
    scale_fill_viridis_c(na.value = na_fill, option = "inferno", label = scales::comma) +
    labs(title = title,
         subtitle = subtitle,
         fill = legend_title) +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
  
}
