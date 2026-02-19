# Load necessary packages
library(readr)
library(rtry)
library(dplyr)
library(tidyr) # Required for pivoting data into wide format

# 1. Define paths 
basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_file <- file.path(basepath, "TRY", "taxonlist", "try_dataset.txt")
dict_file <- file.path(basepath, "output", "taxonlist", "trait-dictionary.csv")
output_file <- file.path(basepath, "output", "taxonlist", "species_traitmatrix.csv")

# 2. Import raw TRY data and the mapped dictionary
try_raw <- rtry_import(try_file)
#mapped_dict <- read.csv(dict_file, stringsAsFactors = FALSE)
mapped_dict <- read_csv(dict_file)

# View the first few rows to confirm it loaded correctly
head(mapped_dict)

# 3. Join the dictionary to the raw dataset
try_standardized <- try_raw %>%
  # Merge the Clean_Category based on ID and original text
  left_join(
    mapped_dict %>% select(TraitID, OrigValueStr, Clean_Category), 
    by = c("TraitID", "OrigValueStr")
  ) %>%
  # Filter out irrelevant traits and unmapped/excluded values
  filter(!is.na(Clean_Category) & Clean_Category != "Exclude" & Clean_Category != "")

# 4. Aggregate and Pivot into Wide Format
species_trait_matrix <- try_standardized %>%
  # Group by species and trait
  group_by(AccSpeciesID, AccSpeciesName, TraitName) %>%
  # Aggregate details and categories for each species-trait combination
  summarize(
    Category = paste(unique(na.omit(Clean_Category)), collapse = ", "),
    Details = paste(unique(na.omit(OrigValueStr)), collapse = ", "),
    .groups = "drop"
  ) %>%
  # Pivot wider: Creates two columns per trait (e.g., 'Dispersal syndrome_Category', 'Dispersal syndrome_Details')
  pivot_wider(
    names_from = TraitName,
    values_from = c(Category, Details),
    names_glue = "{TraitName}_{.value}"
  )

# 5. Export for Python / Scikit-Learn
write.csv(species_trait_matrix, output_file, row.names = FALSE)
print("Standardised trait matrix successfully created!")