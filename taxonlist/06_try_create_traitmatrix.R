# Load necessary packages
library(rtry)
library(dplyr)
library(tidyr)
library(readr)
library(writexl)

# 1. Define paths 
basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_file <- file.path(basepath, "TRY", "taxonlist", "try_dataset.txt")
dict_file <- file.path(basepath, "output", "taxonlist", "traits_dictionary.csv")
output_file_csv <- file.path(basepath, "output", "taxonlist", "traitmatrix.csv")
output_file_excel <- file.path(basepath, "output", "taxonlist", "traitmatrix.xlsx")

# 2. Import raw TRY data and the mapped dictionary
try_raw <- rtry_import(try_file)
mapped_dict <- read_csv(dict_file)

# 3. Join dictionary to raw data to classify rows
# We use left_join. If it's a numeric trait, Clean_Category will be NA because it wasn't in the dict.
try_joined <- try_raw %>%
  left_join(
    mapped_dict %>% select(TraitID, OrigValueStr, Clean_Category), 
    by = c("TraitID", "OrigValueStr")
  )

# ==========================================
# TRACK A: CATEGORICAL TRAITS PROCESSING
# ==========================================
categorical_traits <- try_joined %>%
  # Filter out rows where TRY database is missing the TraitName
  filter(!is.na(TraitName) & TraitName != "") %>%
  
  # Keep only mapped categorical traits, drop "Exclude" and unmapped/numeric (NA)
  filter(!is.na(Clean_Category) & Clean_Category != "Exclude" & Clean_Category != "") %>%
  group_by(AccSpeciesID, AccSpeciesName, TraitName) %>%
  summarize(
    Category = paste(unique(na.omit(Clean_Category)), collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = TraitName,
    values_from = Category,
    names_glue = "{TraitName}_Category"
  )

# ==========================================
# TRACK B: NUMERIC TRAITS PROCESSING
# ==========================================
numeric_traits <- try_joined %>%
  # Identify numeric traits: they weren't in the dictionary (Clean_Category is NA) 
  # AND they have a standardized numeric value (StdValue)
  filter(is.na(Clean_Category) & !is.na(StdValue)) %>%
  group_by(AccSpeciesID, AccSpeciesName, TraitName) %>%
  summarize(
    # Aggregate continuous traits mathematically
    Mean_Value = mean(as.numeric(StdValue), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter out any NAs that might have occurred during numeric conversion
  filter(!is.na(Mean_Value)) %>%
  pivot_wider(
    names_from = TraitName,
    values_from = Mean_Value,
    names_glue = "{TraitName}_Mean"
  )

# ==========================================
# FINAL MERGE: Combine both tracks
# ==========================================
# Use a full join to ensure we keep species even if they lack one of the track types
final_trait_matrix <- full_join(
  categorical_traits, 
  numeric_traits, 
  by = c("AccSpeciesID", "AccSpeciesName")
)

# Export the ready-to-model matrix for your Python pipeline
write_csv(final_trait_matrix, output_file_csv)
write_xlsx(final_trait_matrix, output_file_excel)

print("Bifurcated trait matrix successfully created!")