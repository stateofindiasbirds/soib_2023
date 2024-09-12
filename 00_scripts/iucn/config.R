# To pick the ebd file. May need configuration to pick the RDS directly and transform the column names
CurMonth <- 9
CurYear <- 2022
dir <- "..\\data\\"
unzip <- 0
ebdfile <- paste0("ebd_IN-KL_rel",month.abb[CurMonth],"-",CurYear)


#Lists that have effort.distance are greater than this value (in km) will not be considered for AOO calculations
MaxDistanceThresholdforAOO <- 4  # Since we are calculating at 4x4 grid, any list about 4km will not have the necessary precision. 

#Grids with checklist less than or equal this value will not used for calculating the effort theshold for detecting the species
MinChecklistCount <- 5

#This value determines the confidence needed to define the threshold number of lists based on the detection frequency of that species in a grid
EffortThesholdWithinGridValue <- 80

#This is the precentile value of the threshold number across grids which is considered enough to detect the species in a grid
EffortThesholdAcrossGridsPercentile <- 80

# Number of years to check if EOO is constant before deciding start EOO year
consistent_eoo_limit <- 5

# Which all grid sizes need to be calculated
grid_sizes_km <- c(2, 4, 8)

# No records before 2000 used in EOO calculation
lastYearforEOOCalculation <- 2000

# MAximum distance to be used for EOO Calculation
MaxChecklistDistanceforEOO <- 10
