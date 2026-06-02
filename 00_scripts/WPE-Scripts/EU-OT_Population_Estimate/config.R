##########################################################
# Data and Mapping files - User-specific configuration
##########################################################

datapath <- "D:/NCF - Bird Mon/SoIB - Wetland Population Estimate/Data/EU-OT_Population_Estimate"

# EU and OT google sheet file
eut_and_ot <- "https://docs.google.com/spreadsheets/d/1pNW5qFOGug1igvMLBS3-101bVYg_iLvpdMNs8suDQrg"

# eBird data
ebird <- file.path(datapath, "ebird_data.csv")

# Sensitive species data from eBird
sensitive <- file.path(datapath, "ebd_sensitive_relAug-2025_IN.txt")

# AWC 2005 to 2025 data - from Apoorva
awc <- file.path(datapath, "AWC 2005-25_copy.xlsx")

# AWC 2026 raw data
awc26 <- file.path(datapath, "project-1051-report-20260203_0420.txt")

# Taxonomy mapping file
taxonomy_mapping <- file.path(datapath, "Taxonomy Mapping Table.csv")

# CAF Species List - AWC names
CAF_species_list <- file.path(datapath, "CAF_species_list_BLI_names.csv")

# CAF Species List - eBird names
CAF_species_list_ebird_names <- file.path(datapath, "CAF_species_list_ebird_names.csv")

# District level Shapefile for India
district_shapefile <- file.path(datapath, "maps_sf.RDATA")

# eBird State and District Codes
all_region_codes <- file.path(datapath, "Region Codes.csv")