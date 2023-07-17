# creating new directory if it doesn't already exist
if (!dir.exists(cur_metadata$OCCU.PRES.PATHONLY)) {
  dir.create(cur_metadata$OCCU.PRES.PATHONLY, 
             recursive = T)
}


# calculation -------------------------------------------------------------

# <annotation_pending_AV> at several steps below

speciesforocc %>% 
  {walk2(.$eBird.English.Name.2022, .$status, ~ {
    
    tic(glue("Presence-based occupancy for {.x}"))
    
    # File names for individual files
    write_path <- cur_metadata %>%
      summarise(OCCU.PRES.PATH = glue("{OCCU.PRES.PATHONLY}{.x}_{.y}.csv")) %>%
      pull(OCCU.PRES.PATH)
    
    if (.y == "R") {
      datac <- data
    } 
    
    if (.y == "M") {
      datac <- data %>%
        filter(COMMON.NAME == .x) %>%
        distinct(month) %>% 
        left_join(data)
    } 
    
    if (.y == "MP") {
      datac <- data %>%
        filter(month %in% c(9:11, 3:5)) %>%
        filter(COMMON.NAME == .x) %>%
        distinct(month) %>% 
        left_join(data)
    } 
    
    if (.y == "MS") {
      datac <- data %>%
        filter(month %in% c(5:8)) %>%
        filter(COMMON.NAME == .x) %>%
        distinct(month) %>% 
        left_join(data)
    } 
    
    if (.y == "MW") {
      datac <- data %>%
        filter(month %in% c(11:12, 1:2)) %>%
        filter(COMMON.NAME == .x) %>%
        distinct(month) %>% 
        left_join(data)
    }
    
    f2 <- datac %>%
      filter(COMMON.NAME == .x) %>% 
      distinct(gridg1) %>% 
      mutate(presence = 1,
             status = .y, 
             COMMON.NAME = .x)
    
    toc()
    
    if (length(f2$COMMON.NAME) > 0) {
      write.csv(f2, file = write_path, row.names = FALSE)
    }
    
    gc()
    
  })}
