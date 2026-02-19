# Install and load necessary packages
# install.packages("stringr")
library(rtry)
library(dplyr)
library(stringr) # For easier text pattern matching

# 1. Define paths and import the data
basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_file <- file.path(basepath, "TRY", "47369_16022026235353", "47369.txt")
output_file <- file.path(basepath, "output", "47369_traits_summary.csv")

try_raw <- rtry_import(try_file)

# 2. Filter and Clean the Trait Data
dispersal_clean <- try_raw %>%
  # Filter for Dispersal syndrome
  filter(TraitID == 28) %>%
  # CRITICAL FIX: Ignore rows where DataName indicates a "note" rather than the trait value
  filter(!grepl("note", tolower(DataName))) %>%
  # Remove empty values
  filter(OrigValueStr != "")

# 3. Aggregate into Tabular Format (Wide Format)
species_traits_tabular <- dispersal_clean %>%
  # Group by ID and Name to ensure uniqueness
  group_by(AccSpeciesID, AccSpeciesName) %>%
  # Combine all unique observations into a single comma-separated string
  summarize(
    Dispersal_Details = paste(unique(na.omit(OrigValueStr)), collapse = ", "),
    .groups = "drop"
  ) %>%
  # 4. Categorize based on the aggregated details
  mutate(
    Dispersal_Category = case_when(
      str_detect(tolower(Dispersal_Details), "wind|meteorochor|anemochor") ~ "Anemochory",
      str_detect(tolower(Dispersal_Details), "animal|zoochor|epizoochor") ~ "Zoochory",
      str_detect(tolower(Dispersal_Details), "gravity|barochor|unassisted") ~ "Barochory",
      str_detect(tolower(Dispersal_Details), "water|hydrochor") ~ "Hydrochory",
      TRUE ~ "Unknown"
    )
  )

# View the result
print(species_traits_tabular)

# 5. Export for Python pipeline
rtry_export(species_traits_tabular, output_file)