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


