# Load necessary packages
library(rtry)
library(dplyr)
library(stringr)

# 1. Define paths and import the data
basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_file <- file.path(basepath, "TRY", "47369_16022026235353", "47369.txt")

try_raw <- rtry_import(try_file)

# ==========================================
# STEP 1: Dispersal Table Refinement
# ==========================================
dispersal_clean <- try_raw %>%
  filter(TraitID == 28) %>%
  filter(!grepl("note", tolower(DataName))) %>%
  filter(OrigValueStr != "")

species_traits_tabular <- dispersal_clean %>%
  group_by(AccSpeciesID, AccSpeciesName) %>%
  summarize(
    # Rename Details column
    Dispersal_Det = paste(unique(na.omit(OrigValueStr)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    # Rename Category column
    Dispersal_Cat = case_when(
      str_detect(tolower(Dispersal_Det), "wind|meteorochor|anemochor") ~ "Anemochory",
      str_detect(tolower(Dispersal_Det), "animal|zoochor|epizoochor") ~ "Zoochory",
      str_detect(tolower(Dispersal_Det), "gravity|barochor|unassisted") ~ "Barochory",
      str_detect(tolower(Dispersal_Det), "water|hydrochor") ~ "Hydrochory",
      TRUE ~ "Unknown"
    )
  ) %>%
  # Select and reorder columns: Category before Details
  select(AccSpeciesID, AccSpeciesName, Dispersal_Cat, Dispersal_Det)

# Export the refined table
rtry_export(species_traits_tabular, file.path(basepath, "output", "47369_dispersal.csv"))

# ==========================================
# STEP 2: Dictionary Extraction for Comprehensive Trait List
# ==========================================
# Define your full list of target Trait IDs
target_trait_ids <- c(
  2577, 862, 153, 889, 3684, 773, 3867, 3868, 193, 890, 3127, 891, 28, 892, 392, 
  233, 893, 1263, 237, 232, 894, 235, 231, 236, 2065, 2321, 1809, 3614, 605, 585, 
  586, 2817, 1104, 2946, 927, 12, 3435, 132, 131, 3438, 3432, 3429, 341, 1026, 18, 
  3107, 1015, 364, 567, 3361, 3010, 3106, 1016, 3942, 3119, 3118, 3503, 3039, 59, 
  813, 621, 356, 329, 358, 357, 4025, 1085, 1093, 33, 3482, 3833, 1099, 3986, 2944, 
  3636, 3041, 26, 3660, 3586, 1101, 3634, 3635, 3510, 112, 95, 3123, 596, 34, 353, 
  1102, 234, 27, 1268, 3836, 350, 678, 2945, 3372, 351, 1103, 3603, 336, 138, 1105, 
  1106, 2810, 96, 1110, 4082, 3000, 3373, 1112, 3045, 97, 1107, 349, 610, 3585, 98, 
  1108, 3043, 3832, 66, 238, 1109, 3834, 3835, 3042, 239, 3044, 3810, 3811, 3947, 
  1111, 2809, 2807, 2808, 159, 359, 3620, 3948, 611, 3621, 3624, 1187, 3068, 3989
)

unique_traits_dictionary <- try_raw %>%
  # 1. Filter only for the traits of interest
  filter(TraitID %in% target_trait_ids) %>%
  # 2. Exclude missing values or metadata-only rows without actual values
  filter(!is.na(TraitID) & OrigValueStr != "") %>%
  # 3. Select identifying columns for semantic mapping
  select(TraitID, TraitName, OrigValueStr) %>%
  # 4. Extract only unique combinations
  distinct() %>%
  # 5. Sort for readability
  arrange(TraitName, OrigValueStr) %>%
  # 6. Add an empty column intended for LLM manual mapping
  mutate(Clean_Category = "")

# Export the dictionary template
rtry_export(unique_traits_dictionary, file.path(basepath, "output", "traits_dictionary_template.csv"))

print(paste("Dictionary extracted with", nrow(unique_traits_dictionary), "unique trait expressions."))