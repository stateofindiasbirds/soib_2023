latestYear <- 2024 # Obtain this from a configuration 

#Input Files
parentfolder <- "test/iucn/"
popDeclinefile  <- paste0(parentfolder, "00_data/populationdeclines.csv")
threegenfile    <- "00_data/3genbli.csv"
soibredlistfile <- paste0(parentfolder, "01_analyses_full/results/redlist.csv")
eooaoofile <- paste0(parentfolder, "01_analyses_full/results/eooaoo.csv")
soibmainfile <- get_metadata("none")$SOIBMAIN.PATH
continuingdeclineexfile <- paste0(parentfolder, "01_analyses_full/results/continuingdeclineext.csv")
severelyfragmentedfile <- paste0(parentfolder, "01_analyses_full/results/severelyfragmented.csv")
nooflocationsfile <- paste0(parentfolder, "01_analyses_full/results/nooflocations.csv")
extremefluctationsfile <- paste0(parentfolder, "01_analyses_full/results/extremefluctuations.csv")
populationsfile <- paste0(parentfolder, "00_data/populations.csv")
plausiblethreatfile <- paste0(parentfolder, "01_analyses_full/results/plausiblethreat.csv")

assessmentsflattenedfile <- "00_data/IUCN_assessments_Flattened_With SoIB Names.csv"
subpopulationsfile <- "00_data/species_with_subspecies_count.csv"
raritiesfile <- "00_data/rarities_nrl.csv" 
percentrangefile <- "00_data/percent_species_ranges_India.csv"

criteriaAResultsfile <- paste0(parentfolder, "01_analyses_full/results/criteriaA_results.csv")
criteriaBResultsfile <- paste0(parentfolder, "01_analyses_full/results/criteriaB_results.csv")
criteriaCResultsfile <- paste0(parentfolder, "01_analyses_full/results/criteriaC_results.csv")
criteriaDResultsfile <- paste0(parentfolder, "01_analyses_full/results/criteriaD_results.csv")

nrloutputfile <- paste0(parentfolder, "01_analyses_full/results/species_nrl.csv")

nrlspecieslistfile <- "00_data/species_nrl.csv"

redlist_home_dir <- "test\\iucn\\"