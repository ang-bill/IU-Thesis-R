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
mapped_dict <- read_csv(dict_file, show_col_types = FALSE)

# 3. Join dictionary to raw data
try_joined <- try_raw %>%
  left_join(
    mapped_dict %>% select(TraitID, OrigValueStr, Clean_Category), 
    by = c("TraitID", "OrigValueStr")
  )

# ==========================================
# TRACK A: CATEGORICAL TRAITS PROCESSING
# ==========================================
categorical_traits <- try_joined %>%
  filter(!is.na(Clean_Category) & Clean_Category != "Exclude" & Clean_Category != "") %>%
  group_by(AccSpeciesID, AccSpeciesName, TraitName) %>%
  summarize(
    # The standardized LLM category
    Category = paste(unique(na.omit(Clean_Category)), collapse = ", "),
    # The raw distinct concatenated values
    Details = paste(unique(na.omit(OrigValueStr)), collapse = ", "),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = TraitName,
    values_from = c(Category, Details),
    names_glue = "{TraitName}_{.value}"
  )

# ==========================================
# TRACK B: NUMERIC TRAITS PROCESSING
# ==========================================
numeric_traits <- try_joined %>%
  # Filter out missing TraitNames (fixes the _Mean column artifact)
  filter(!is.na(TraitName) & TraitName != "") %>%
  filter(is.na(Clean_Category) & !is.na(StdValue)) %>%
  group_by(AccSpeciesID, AccSpeciesName, TraitName) %>%
  summarize(
    Mean = mean(as.numeric(StdValue), na.rm = TRUE),
    # Calculate standard deviation to assess variance
    StdDev = sd(as.numeric(StdValue), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Mean)) %>%
  pivot_wider(
    names_from = TraitName,
    values_from = c(Mean, StdDev),
    names_glue = "{TraitName}_{.value}"
  )

# ==========================================
# FINAL MERGE: Combine both tracks
# ==========================================
final_trait_matrix <- full_join(
  categorical_traits, 
  numeric_traits, 
  by = c("AccSpeciesID", "AccSpeciesName")
)

# Export the enhanced matrix
write_csv(final_trait_matrix, output_file_csv)
write_xlsx(final_trait_matrix, output_file_excel)

print("Enhanced trait matrix for species selection successfully created!")