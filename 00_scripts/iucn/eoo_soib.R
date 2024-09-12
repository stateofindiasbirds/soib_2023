library (tidyverse)
library(dplyr)
library (lubridate)
library (data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(rgdal)
library(rgeos)
library(ggmap)
library(Rcpp)
library(sp)
library(adehabitatHR)




#reading ebird file with only species, lat, long and SoIB year columns
ebd_eoo <- readRDS("ebd_locs_2023.RDS")
head(ebd_eoo)
unique(ebd_eoo$SOIB_YEAR)
ebd_eoo <- ebd_eoo %>%
  mutate(yearcode = case_when(
    endsWith(SOIB_YEAR, "2006") ~ 1,
    endsWith(SOIB_YEAR, "2010") ~ 2,
    endsWith(SOIB_YEAR, "2012") ~ 3,
    endsWith(SOIB_YEAR, "2013") ~ 4,
    endsWith(SOIB_YEAR, "2014") ~ 5,
    endsWith(SOIB_YEAR, "2015") ~ 6,
    endsWith(SOIB_YEAR, "2016") ~ 7,
    endsWith(SOIB_YEAR, "2017") ~ 8,
    endsWith(SOIB_YEAR, "2018") ~ 9,
    endsWith(SOIB_YEAR, "2019") ~ 10,
    endsWith(SOIB_YEAR, "2020") ~ 11,
    endsWith(SOIB_YEAR, "2021") ~ 12,
    endsWith(SOIB_YEAR, "2022") ~ 13
    
  ))

fullmap=read.csv("SoIB_mapping_2023.csv")

#unique(fullmap$Migratory.Status.Within.India)

resmigsp = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Resident",
                                              "Resident (Extirpated)",
                                              "Uncertain",
                                              "Winter Migrant (Extirpated)",
                                              "Passage Migrant",
                                              "Passage Migrant & Localized Summer Migrant",
                                              "Summer Migrant & Passage Migrant",
                                              "Summer Migrant",
                                              "Winter Migrant",
                                              "Passage Migrant & Localized Winter Migrant"))%>%
  dplyr::select("eBird.English.Name.2022")

#listsp<-read.csv("specieslist_res_flag.csv")
species<-as.character(resmigsp$eBird.English.Name.2022)
#species<-as.character(c("Large Blue Flycatcher","Yellow-eyed Pigeon"))
head(species)
#head(listsp)
years<-c(unique(ebd_eoo$year))
#years<-c(unique(ebd_eoo$year))
years<-years[order(years)]
#speciesname=as.list(unique(ebd_eoo$COMMON.NAME))

calEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- ebd_eoo %>% filter (COMMON.NAME %in% species,
                                year >= years)
  if(nrow(data_f)>5){
    coordinates(data_f) <- c("LONGITUDE", "LATITUDE")
    proj4string(data_f) <- CRS("+init=epsg:4326")
    data_f<-spTransform(data_f, CRS("+init=epsg:32644"))
    mcpsp<-mcp(data_f[,1], percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df<-data.frame(Species=character(0), Year=integer(0), EOO=integer(0))

for (i in 1:length(species)){
  print(paste(i,species[i]))
  for (j in length(years):1)
  {
    yearEOO <- calEOOYear (species[i],years[j])
    df[nrow(df)+1,]$Species <- species[i]
    df[nrow(df),]$Year <- years[j]
    df[nrow(df),]$EOO <- yearEOO
    #df_all <- rbind (df_all, df)
  }
}

write.csv(df,"year_eoo_res&mig_2023.csv")


#################################################################################################

##filtering dataset for local migrants seasonal records####

locmigsp = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Local Migrant",
                                              "Resident & Altitudinal Migrant",
                                              "Resident & Summer Migrant",
                                              "Winter Migrant & Localized Summer Migrant",
                                              "Resident & Local Migrant",
                                              "Resident & Winter Migrant",
                                              "Summer Migrant & Localized Winter Migrant",
                                              "Localized Summer Migrant & Localized Winter Migrant",
                                              "Within-India Migrant & Winter Migrant",
                                              "Summer Migrant & Winter Migrant",
                                              "Resident & Localized Summer Migrant",
                                              "Within-India Migrant",
                                              "Local Migrant & Winter Migrant",
                                              "Resident & Within-India Migrant",
                                              "Local Migrant & Summer Migrant",
                                              "Altitudinal Migrant"))%>%
  dplyr::select("eBird.English.Name.2022")

ebd_locmig_2023<-ebd_eoo %>% filter(COMMON.NAME %in% locmigsp$eBird.English.Name.2022)
head(ebd_locmig_2023)
ebd_locmig_sum = ebd_locmig_2023 %>% filter(day > 145 & day <= 215)
ebd_locmig_win = ebd_locmig_2023 %>% filter(day <= 60 | day > 325)
ebd_locmig_pas = ebd_locmig_2023 %>% filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) # If necessary

##################################################################################################

###EOO calculation for local migrants' summer and winter ranges###########

#######Summer ranges##########

eoo_locmig_sum <- locmig_sum %>%
  mutate(yearcode = case_when(
    endsWith(SOIB_YEAR, "2006") ~ 1,
    endsWith(SOIB_YEAR, "2010") ~ 2,
    endsWith(SOIB_YEAR, "2012") ~ 3,
    endsWith(SOIB_YEAR, "2013") ~ 4,
    endsWith(SOIB_YEAR, "2014") ~ 5,
    endsWith(SOIB_YEAR, "2015") ~ 6,
    endsWith(SOIB_YEAR, "2016") ~ 7,
    endsWith(SOIB_YEAR, "2017") ~ 8,
    endsWith(SOIB_YEAR, "2018") ~ 9,
    endsWith(SOIB_YEAR, "2019") ~ 10,
    endsWith(SOIB_YEAR, "2020") ~ 11,
    endsWith(SOIB_YEAR, "2021") ~ 12,
    endsWith(SOIB_YEAR, "2022") ~ 13
    
  ))

#fullmap=read.csv("SoIB_mapping_2023.csv")


species<-as.character(locmigsp$eBird.English.Name.2022)
#species<-as.character(c("Large Blue Flycatcher","Yellow-eyed Pigeon"))
head(species)
#head(listsp)
years<-c(unique(eoo_locmig_sum$year))
#years<-c(unique(ebd_eoo$year))
years<-years[order(years)]
#speciesname=as.list(unique(ebd_eoo$COMMON.NAME))

calEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- eoo_locmig_sum %>% filter (COMMON.NAME %in% species,
                                       year >= years)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    coordinates(data_f) <- c("LONGITUDE", "LATITUDE")
    proj4string(data_f) <- CRS("+init=epsg:4326")
    data_f<-spTransform(data_f, CRS("+init=epsg:32644"))
    mcpsp<-mcp(data_f[,1], percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df<-data.frame(Species=character(0), Year=integer(0), EOO=integer(0))

for (i in 1:length(species)){
  #if(i==6){next}
  #if(i==7){next}
  print(paste(i,species[i]))
  for (j in length(years):1)
  {
    yearEOO <- calEOOYear (species[i],years[j])
    df[nrow(df)+1,]$Species <- species[i]
    df[nrow(df),]$Year <- years[j]
    df[nrow(df),]$EOO <- yearEOO
    #df_all <- rbind (df_all, df)
  }
}

write.csv(df,"year_eoo_locmig_sum_2023.csv")


#######Winter ranges##########

eoo_locmig_win <- locmig_win %>%
  mutate(yearcode = case_when(
    endsWith(SOIB_YEAR, "2006") ~ 1,
    endsWith(SOIB_YEAR, "2010") ~ 2,
    endsWith(SOIB_YEAR, "2012") ~ 3,
    endsWith(SOIB_YEAR, "2013") ~ 4,
    endsWith(SOIB_YEAR, "2014") ~ 5,
    endsWith(SOIB_YEAR, "2015") ~ 6,
    endsWith(SOIB_YEAR, "2016") ~ 7,
    endsWith(SOIB_YEAR, "2017") ~ 8,
    endsWith(SOIB_YEAR, "2018") ~ 9,
    endsWith(SOIB_YEAR, "2019") ~ 10,
    endsWith(SOIB_YEAR, "2020") ~ 11,
    endsWith(SOIB_YEAR, "2021") ~ 12,
    endsWith(SOIB_YEAR, "2022") ~ 13
    
  ))

#fullmap=read.csv("SoIB_mapping_2023.csv")


#species<-as.character(locmigsp$eBird.English.Name.2022)
#species<-as.character(c("Large Blue Flycatcher","Yellow-eyed Pigeon"))
head(species)
#head(listsp)
years<-c(unique(eoo_locmig_win$year))
#years<-c(unique(ebd_eoo$year))
years<-years[order(years)]
#speciesname=as.list(unique(ebd_eoo$COMMON.NAME))

calEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- eoo_locmig_win %>% filter (COMMON.NAME %in% species,
                                       year >= years)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    coordinates(data_f) <- c("LONGITUDE", "LATITUDE")
    proj4string(data_f) <- CRS("+init=epsg:4326")
    data_f<-spTransform(data_f, CRS("+init=epsg:32644"))
    mcpsp<-mcp(data_f[,1], percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df<-data.frame(Species=character(0), Year=integer(0), EOO=integer(0))

for (i in 1:length(species)){
  #if(i==6){next}
  #if(i==7){next}
  print(paste(i,species[i]))
  for (j in length(years):1)
  {
    yearEOO <- calEOOYear (species[i],years[j])
    df[nrow(df)+1,]$Species <- species[i]
    df[nrow(df),]$Year <- years[j]
    df[nrow(df),]$EOO <- yearEOO
    #df_all <- rbind (df_all, df)
  }
}

write.csv(df,"year_eoo_locmig_win_2023.csv")
############################################################################
###########################################################################

###Determining the start year for every species to get the EOO value for reporting###

###Determining the start year for the Resident species###

Ryear=read.csv("year_eoo_res&mig_2023.csv",header=T)

Ryear1= Ryear%>%
  group_by(Species)%>%
  mutate(diff = (EOO - lag(EOO, default = first(EOO))))

Ryear2= Ryear1%>%
  group_by(Species)%>%
  mutate(propD = round(diff / lag(EOO, default = first(EOO)),3))

Ryear2[is.na(Ryear2)] = 0

x<-readRDS("ebd_locs_2023.RDS")

Residentsp = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Resident",
                                              "Resident (Extirpated)",
                                              "Uncertain"))%>%
  dplyr::select("eBird.English.Name.2022")


colnames(Residentsp)[1]<-"Common.Name"

df1=data.frame(Species=character(658), Start.Year=integer(658), EOO=integer(658))
for ( i in 1:nrow(Residentsp)){
  species <- Residentsp[i,]
  print(species)
  df <- Ryear2 %>% filter (Species==species)
  for (j in 1:23){
    if(df$EOO[j]==0 & df$EOO[23]==0 | nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[23]))<20){
      df1$Species[i] <- df$Species[1]
      df1$Start.Year[i] <- paste0("NA")
      df1$EOO[i] <- paste0("NA")
    }
    else{
      if(df$EOO[j]==0|nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[j]))<20){
        next
      }
      else{
        if(j<22&df$propD[j]==0&df$propD[j+1]==0&df$propD[j+2]==0){
          df1$Species[i] <- df$Species[j]
          df1$Start.Year[i] <- df$Year[j]
          df1$EOO[i] <- df$EOO[j]
          break
        }
        else{
          df1$Species[i] <- df$Species[23]
          df1$Start.Year[i] <- df$Year[23]
          df1$EOO[i] <- df$EOO[23]
        }
      }
    }
  }
}

write.csv(df1,"Resident_EOO_2023.csv")

############# MAX EOO calculation for Resident species ###########
datapts<-ebd_eoo
#ResEOO=read.csv("Resident_EOO_2023.csv", header =T) # to be followed if independently run after calculating start years
ResEOO=df1 ## can be followed if being run right after previous code
sp.list<-ResEOO$Species
st.year<-ResEOO$Start.Year

datapts$EFFORT.DISTANCE.KM[is.na(datapts$EFFORT.DISTANCE.KM)]<-10 #converting all NAs to buffer of 10 kms
datapts$EFFORT.DISTANCE.KM[datapts$EFFORT.DISTANCE.KM>10]<-10 #converting all records with more than 10 kms effort to 10 kms

# Converts kilometer to latitude
KM2LAT <- function (x) { return (x/110.574)}
# Converts kilometer to longitude. It changes with latitude = y
KM2LON <- function (x, y) { return (x/(111.320 * cos (y * 3.14/180)))}

# It finds N points on a circle of Radius kilometres
# given the center as LATITUDE and LONGITUDE
# Return value is a dataframe of coordinates

findNpointsOnaCircle <- function (LATITUDE, LONGITUDE, Radius, N)
{
  arcangle <- 360 / N
  aseq <- seq(1:N) %>% as.numeric() * arcangle * 3.14/180
  cpoints <- cbind(aseq, aseq) %>% as.data.frame()
  colnames(cpoints) <- c("LATITUDE", "LONGITUDE")
  cpoints$LATITUDE  <- KM2LAT(Radius) * cos (cpoints$LATITUDE) + LATITUDE
  cpoints$LONGITUDE <- KM2LON(Radius, LATITUDE) * sin (cpoints$LONGITUDE) + LONGITUDE
  return (cpoints)
}


calmaxEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- datapts %>% filter (COMMON.NAME %in% species,
                                year >= years)
  data_f <- na.omit(data_f)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    z<-findNpointsOnaCircle(data_f$LATITUDE, data_f$LONGITUDE, data_f$EFFORT.DISTANCE.KM, nrow(data_f)*360)
    coordinates(z)<-c("LONGITUDE", "LATITUDE")
    proj4string(z) <- CRS("+init=epsg:4326")
    z<-spTransform(z, CRS("+init=epsg:32644"))
    mcpsp<-mcp(z, percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df_max<-data.frame(Species=character(0), Year=integer(0), maxEOO=integer(0))

for (i in 1:length(sp.list)){
  print(paste(i,sp.list[i]))
  maxyearEOO <- calmaxEOOYear(sp.list[i], st.year[i])
  df_max[nrow(df_max)+1,]$Species <- sp.list[i]
  df_max[nrow(df_max),]$Year <- st.year[i]
  df_max[nrow(df_max),]$maxEOO <- maxyearEOO
}

#write.csv(df_max,"resident_maxeoo.csv")

Res_eoo=merge(ResEOO,df_max,by=c("Species"))
write.csv(Res_eoo,"Resident_EOO&Max_2023.csv")

##################################################################################################
###Determining the start year for the Winter Migrant Species###

WinterMigrantsp = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Winter Migrant (Extirpated)",
                                              "Passage Migrant",
                                              "Passage Migrant & Localized Summer Migrant",
                                              "Summer Migrant & Passage Migrant",
                                              "Summer Migrant",
                                              "Winter Migrant",
                                              "Passage Migrant & Localized Winter Migrant"))%>%
  dplyr::select("eBird.English.Name.2022")

#x<-readRDS("ebd_locs_2023.RDS") ##Already covered in previous iteration for resident species. But can be used if winter migrants are independently run

colnames(WinterMigrantsp)[1]<-"Common.Name"

df1=data.frame(Species=character(271), Start.Year=integer(271), EOO=integer(271))
for ( i in 1:nrow(WinterMigrantsp)){
  species <- WinterMigrantsp[i,]
  print(species)
  df <- Ryear2 %>% filter (Species==species)
  for (j in 1:23){
    if(df$EOO[j]==0 & df$EOO[23]==0 | nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[23]))<20){
      df1$Species[i] <- df$Species[1]
      df1$Start.Year[i] <- paste0("NA")
      df1$EOO[i] <- paste0("NA")
    }
    else{
      if(df$EOO[j]==0|nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[j]))<20){
        next
      }
      else{
        if(j<22&df$propD[j]==0&df$propD[j+1]==0&df$propD[j+2]==0){
          df1$Species[i] <- df$Species[j]
          df1$Start.Year[i] <- df$Year[j]
          df1$EOO[i] <- df$EOO[j]
          break
        }
        else{
          df1$Species[i] <- df$Species[23]
          df1$Start.Year[i] <- df$Year[23]
          df1$EOO[i] <- df$EOO[23]
        }
      }
    }
  }
}

write.csv(df1,"WinterMig_EOO_2023.csv")

############# MAX EOO calculation for Winter Migrant species ###########
datapts<-ebd_eoo
WinEOO=read.csv("WinterMig_EOO_2023.csv", header =T)
#WinEOO=df1 ## can be followed if being run right after previous code
sp.list<-WinEOO$Species
st.year<-WinEOO$Start.Year

datapts$EFFORT.DISTANCE.KM[is.na(datapts$EFFORT.DISTANCE.KM)]<-10 #converting all NAs to buffer of 10 kms
datapts$EFFORT.DISTANCE.KM[datapts$EFFORT.DISTANCE.KM>10]<-10 #converting all records with more than 10 kms effort to 10 kms

# Converts kilometer to latitude
KM2LAT <- function (x) { return (x/110.574)}
# Converts kilometer to longitude. It changes with latitude = y
KM2LON <- function (x, y) { return (x/(111.320 * cos (y * 3.14/180)))}

# It finds N points on a circle of Radius kilometres
# given the center as LATITUDE and LONGITUDE
# Return value is a dataframe of coordinates

findNpointsOnaCircle <- function (LATITUDE, LONGITUDE, Radius, N)
{
  arcangle <- 360 / N
  aseq <- seq(1:N) %>% as.numeric() * arcangle * 3.14/180
  cpoints <- cbind(aseq, aseq) %>% as.data.frame()
  colnames(cpoints) <- c("LATITUDE", "LONGITUDE")
  cpoints$LATITUDE  <- KM2LAT(Radius) * cos (cpoints$LATITUDE) + LATITUDE
  cpoints$LONGITUDE <- KM2LON(Radius, LATITUDE) * sin (cpoints$LONGITUDE) + LONGITUDE
  return (cpoints)
}


calmaxEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- datapts %>% filter (COMMON.NAME %in% species,
                                year >= years)
  data_f <- na.omit(data_f)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    z<-findNpointsOnaCircle(data_f$LATITUDE, data_f$LONGITUDE, data_f$EFFORT.DISTANCE.KM, nrow(data_f)*360)
    coordinates(z)<-c("LONGITUDE", "LATITUDE")
    proj4string(z) <- CRS("+init=epsg:4326")
    z<-spTransform(z, CRS("+init=epsg:32644"))
    mcpsp<-mcp(z, percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df_max<-data.frame(Species=character(0), Year=integer(0), maxEOO=integer(0))

for (i in 1:length(sp.list)){
  print(paste(i,sp.list[i]))
  maxyearEOO <- calmaxEOOYear(sp.list[i], st.year[i])
  df_max[nrow(df_max)+1,]$Species <- sp.list[i]
  df_max[nrow(df_max),]$Year <- st.year[i]
  df_max[nrow(df_max),]$maxEOO <- maxyearEOO
}

#write.csv(df_max,"WinterMig_maxeoo.csv")
#wintmig_maxeoo=read.csv("WinterMig_maxeoo.csv",header=T)
WinterMIg_eoo=merge(WinEOO,df_max,by=c("Species"))
#WinterMIg_eoo=merge(WinEOO,wintmig_maxeoo,by=c("Species"))

write.csv(WinterMIg_eoo,"WinterMig_EOO&Max_2023.csv")

##########################################################################################################
###Determining the start year for the Local Migrants Summer###

Ryear=read.csv("year_eoo_locmig_sum_2023.csv",header=T)

Ryear1= Ryear%>%
  group_by(Species)%>%
  mutate(diff = (EOO - lag(EOO, default = first(EOO))))

Ryear2= Ryear1%>%
  group_by(Species)%>%
  mutate(propD = round(diff / lag(EOO, default = first(EOO)),3))

Ryear2[is.na(Ryear2)] = 0

x<-ebd_locmig_sum

LocalMigrantsp = locmigsp
colnames(LocalMigrantsp)[1]<-"Common.Name"

df1=data.frame(Species=character(426), Start.Year=integer(426), EOO=integer(426))
for ( i in 1:nrow(LocalMigrantsp)){
  species <- LocalMigrantsp[i,]
  print(species)
  df <- Ryear2 %>% filter (Species==species)
  for (j in 1:23){
    if(df$EOO[j]==0 & df$EOO[23]==0 | nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[23]))<20){
      df1$Species[i] <- df$Species[1]
      df1$Start.Year[i] <- paste0("NA")
      df1$EOO[i] <- paste0("NA")
    }
    else{
      if(df$EOO[j]==0|nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[j]))<20){
        next
      }
      else{
        if(j<22&df$propD[j]==0&df$propD[j+1]==0&df$propD[j+2]==0){
          df1$Species[i] <- df$Species[j]
          df1$Start.Year[i] <- df$Year[j]
          df1$EOO[i] <- df$EOO[j]
          break
        }
        else{
          df1$Species[i] <- df$Species[23]
          df1$Start.Year[i] <- df$Year[23]
          df1$EOO[i] <- df$EOO[23]
        }
      }
    }
  }
}

write.csv(df1,"locmigrant_sum_EOO_2023.csv")

############# MAX EOO calculation for Local migrant species Summer ###########
datapts<-ebd_locmig_sum
LocSumEOO=read.csv("locmigrant_sum_EOO_2023.csv", header =T)
#LocSumEOO=df1 ## can be followed if being run right after previous code
sp.list<-LocSumEOO$Species
st.year<-LocSumEOO$Start.Year

datapts$EFFORT.DISTANCE.KM[is.na(datapts$EFFORT.DISTANCE.KM)]<-10 #converting all NAs to buffer of 10 kms
datapts$EFFORT.DISTANCE.KM[datapts$EFFORT.DISTANCE.KM>10]<-10 #converting all records with more than 10 kms effort to 10 kms

# Converts kilometer to latitude
KM2LAT <- function (x) { return (x/110.574)}
# Converts kilometer to longitude. It changes with latitude = y
KM2LON <- function (x, y) { return (x/(111.320 * cos (y * 3.14/180)))}

# It finds N points on a circle of Radius kilometres
# given the center as LATITUDE and LONGITUDE
# Return value is a dataframe of coordinates

findNpointsOnaCircle <- function (LATITUDE, LONGITUDE, Radius, N)
{
  arcangle <- 360 / N
  aseq <- seq(1:N) %>% as.numeric() * arcangle * 3.14/180
  cpoints <- cbind(aseq, aseq) %>% as.data.frame()
  colnames(cpoints) <- c("LATITUDE", "LONGITUDE")
  cpoints$LATITUDE  <- KM2LAT(Radius) * cos (cpoints$LATITUDE) + LATITUDE
  cpoints$LONGITUDE <- KM2LON(Radius, LATITUDE) * sin (cpoints$LONGITUDE) + LONGITUDE
  return (cpoints)
}


calmaxEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- datapts %>% filter (COMMON.NAME %in% species,
                                year >= years)
  data_f <- na.omit(data_f)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    z<-findNpointsOnaCircle(data_f$LATITUDE, data_f$LONGITUDE, data_f$EFFORT.DISTANCE.KM, nrow(data_f)*360)
    coordinates(z)<-c("LONGITUDE", "LATITUDE")
    proj4string(z) <- CRS("+init=epsg:4326")
    z<-spTransform(z, CRS("+init=epsg:32644"))
    mcpsp<-mcp(z, percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df_max<-data.frame(Species=character(0), Year=integer(0), maxEOO=integer(0))

for (i in 1:length(sp.list)){
  print(paste(i,sp.list[i]))
  maxyearEOO <- calmaxEOOYear(sp.list[i], st.year[i])
  df_max[nrow(df_max)+1,]$Species <- sp.list[i]
  df_max[nrow(df_max),]$Year <- st.year[i]
  df_max[nrow(df_max),]$maxEOO <- maxyearEOO
}

#write.csv(df_max,"LocMigSum_maxeoo.csv")

LocMig_Sum_eoo=merge(LocSumEOO,df_max,by=c("Species"))

#write.csv(LocMig_Sum_eoo,"LocMig_Sum_eoo&max_2023.csv") ##If need to be saved separately

##########################################################################################################
###Determining the start year for the Local Migrants Winter###

Ryear=read.csv("year_eoo_locmig_win_2023.csv",header=T)

Ryear1= Ryear%>%
  group_by(Species)%>%
  mutate(diff = (EOO - lag(EOO, default = first(EOO))))

Ryear2= Ryear1%>%
  group_by(Species)%>%
  mutate(propD = round(diff / lag(EOO, default = first(EOO)),3))

Ryear2[is.na(Ryear2)] = 0

x<-ebd_locmig_win

#LocalMigrantsp = locmigsp %>% colnames(locmigsp)[1]<-"Common.Name"

df1=data.frame(Species=character(426), Start.Year=integer(426), EOO=integer(426))
for ( i in 1:nrow(LocalMigrantsp)){
  species <- LocalMigrantsp[i,]
  print(species)
  df <- Ryear2 %>% filter (Species==species)
  for (j in 1:23){
    if(df$EOO[j]==0 & df$EOO[23]==0 | nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[23]))<20){
      df1$Species[i] <- df$Species[1]
      df1$Start.Year[i] <- paste0("NA")
      df1$EOO[i] <- paste0("NA")
    }
    else{
      if(df$EOO[j]==0|nrow(x %>% filter (COMMON.NAME == species,year>=df$Year[j]))<20){
        next
      }
      else{
        if(j<22&df$propD[j]==0&df$propD[j+1]==0&df$propD[j+2]==0){
          df1$Species[i] <- df$Species[j]
          df1$Start.Year[i] <- df$Year[j]
          df1$EOO[i] <- df$EOO[j]
          break
        }
        else{
          df1$Species[i] <- df$Species[23]
          df1$Start.Year[i] <- df$Year[23]
          df1$EOO[i] <- df$EOO[23]
        }
      }
    }
  }
}

write.csv(df1,"locmigrant_win_EOO_2023.csv")

############# MAX EOO calculation for Local migrant species Winter ###########
datapts<-ebd_locmig_win
LocWinEOO=read.csv("locmigrant_win_EOO_2023.csv", header =T)
#LocSumEOO=df1 ## can be followed if being run right after previous code
sp.list<-LocWinEOO$Species
st.year<-LocWinEOO$Start.Year

datapts$EFFORT.DISTANCE.KM[is.na(datapts$EFFORT.DISTANCE.KM)]<-10 #converting all NAs to buffer of 10 kms
datapts$EFFORT.DISTANCE.KM[datapts$EFFORT.DISTANCE.KM>10]<-10 #converting all records with more than 10 kms effort to 10 kms

# Converts kilometer to latitude
KM2LAT <- function (x) { return (x/110.574)}
# Converts kilometer to longitude. It changes with latitude = y
KM2LON <- function (x, y) { return (x/(111.320 * cos (y * 3.14/180)))}

# It finds N points on a circle of Radius kilometres
# given the center as LATITUDE and LONGITUDE
# Return value is a dataframe of coordinates

findNpointsOnaCircle <- function (LATITUDE, LONGITUDE, Radius, N)
{
  arcangle <- 360 / N
  aseq <- seq(1:N) %>% as.numeric() * arcangle * 3.14/180
  cpoints <- cbind(aseq, aseq) %>% as.data.frame()
  colnames(cpoints) <- c("LATITUDE", "LONGITUDE")
  cpoints$LATITUDE  <- KM2LAT(Radius) * cos (cpoints$LATITUDE) + LATITUDE
  cpoints$LONGITUDE <- KM2LON(Radius, LATITUDE) * sin (cpoints$LONGITUDE) + LONGITUDE
  return (cpoints)
}


calmaxEOOYear <- function (species, years){
  # Get all locations for a species reported after/incl a year
  data_f <- datapts %>% filter (COMMON.NAME %in% species,
                                year >= years)
  data_f <- na.omit(data_f)
  if(nrow(data_f)>5){
    #data_f <- data_f %>% select(LATITUDE, LONGTIUDE)
    z<-findNpointsOnaCircle(data_f$LATITUDE, data_f$LONGITUDE, data_f$EFFORT.DISTANCE.KM, nrow(data_f)*360)
    coordinates(z)<-c("LONGITUDE", "LATITUDE")
    proj4string(z) <- CRS("+init=epsg:4326")
    z<-spTransform(z, CRS("+init=epsg:32644"))
    mcpsp<-mcp(z, percent=100, unin = ("m"),unout = ("km2"))
    return (as.data.frame(mcpsp)$area)
  } else{return(0)}
}

df_max<-data.frame(Species=character(0), Year=integer(0), maxEOO=integer(0))

for (i in 1:length(sp.list)){
  print(paste(i,sp.list[i]))
  maxyearEOO <- calmaxEOOYear(sp.list[i], st.year[i])
  df_max[nrow(df_max)+1,]$Species <- sp.list[i]
  df_max[nrow(df_max),]$Year <- st.year[i]
  df_max[nrow(df_max),]$maxEOO <- maxyearEOO
}

#write.csv(df_max,"LocMigWin_maxeoo.csv")

LocMig_Win_eoo=merge(LocWinEOO,df_max,by=c("Species"))

#write.csv(LocMig_Win_eoo,"LocMig_Win_eoo&max_2023.csv") ##If need to be saved separately

LocMig_maxeoo=merge(LocMig_Sum_eoo,LocMig_Win_eoo,by=c("X","Species"))
write.csv(LocMig_maxeoo,"Localmig_maxeoo_2023.csv")

#######################################################################################
#####Merge Summer and Winter files to compare####
LocalMig_eoo_sum=read.csv("locmigrant_sum_EOO_2023.csv")
LocalMig_eoo_win=read.csv("locmigrant_win_EOO_2023.csv")

colnames(LocalMig_eoo_sum)[4] = "EOO_summer"
colnames(LocalMig_eoo_win)[4] = "EOO_winter"

LocalMig_eoo=merge(LocalMig_eoo_sum,LocalMig_eoo_win,by=c("X","Species"))
write.csv(LocalMig_eoo,"Localmig_eoo_2023.csv")


