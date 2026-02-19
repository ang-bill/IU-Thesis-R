# 1. Install and load the necessary packages
# install.packages("rtry")
# install.packages("dplyr")
library(rtry)
library(dplyr)

basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_folder <- "TRY"
output_folder <- "output"
#try_dir <- file.path(basepath, try_folder)
#output_dir <- file.path(basepath, output_folder)

try_file <- file.path(basepath, try_folder, "47369_16022026235353/47369.txt")
output_file <- file.path(basepath, output_folder, "47369.csv")

# 2. Import the raw TRY data text file
# rtry_import automatically handles the complex encoding of TRY datasets
try_raw <- rtry_import(try_file)

# 3. Filter for your specific species
# We use AccSpeciesName to ensure we capture the accepted scientific name
species_filtered <- rtry_select_row(try_raw, AccSpeciesName == "Anacamptis morio")

# 4. Filter for the "Dispersal syndrome" trait
# Dispersal traits usually have specific TraitIDs in TRY (e.g., TraitID 28)
dispersal_data <- rtry_select_row(species_filtered, TraitName == "Dispersal syndrome")

# 5. Export to CSV for your Python pipeline
# This allows you to load it seamlessly into pandas later
rtry_export(dispersal_data, output_file)