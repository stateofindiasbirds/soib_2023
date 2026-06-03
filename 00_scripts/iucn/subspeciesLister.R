# Read data
specieslist <- read.csv("00_data\\species.txt")
subspecieslist <- read.csv("00_data\\subspecies.txt")

# Extract species names from subspecies list (first two words)
subspecieslist$Species <- sapply(strsplit(subspecieslist$Subspecies, " "), function(x) paste(x[1:2], collapse = " "))

# Count number of subspecies for each species
sub_count <- aggregate(Subspecies ~ Species, data = subspecieslist, FUN = length)

# Merge with species list
merged <- merge(specieslist, sub_count, by = "Species", all.x = TRUE)

# Replace NAs (monotypic species) with 1
merged$Subspecies[is.na(merged$Subspecies)] <- 1

# Rename columns for clarity
colnames(merged) <- c("Species", "No_of_Subspecies")

# View result
head(merged)

# Optionally save to CSV
write.csv(merged, "..\\..\\00_data\\species_with_subspecies_count.csv", row.names = FALSE)
