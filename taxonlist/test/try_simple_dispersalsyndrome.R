library(rtry)
library(dplyr)

basepath <- "C:/drive_j/eigene/studium/Data_Science/Master-Thesis/Datasources" 
try_folder <- "TRY"
output_folder <- "output"
#try_dir <- file.path(basepath, try_folder)
#output_dir <- file.path(basepath, output_folder)

try_file <- file.path(basepath, try_folder, "47369_16022026235353/47369.txt")
output_file <- file.path(basepath, output_folder, "47369.csv")

# 1. IMPORT: Leverage rtry to handle the TRY long-table format
try_data <- rtry_import(try_file)

# 2. EXPLORE: (Optional) Inspect traits and ancillary data
try_explore <- rtry_explore(try_data, DataID, DataName, TraitID, TraitName, sortBy = TraitID)

# 3. EXCLUDE: Leverage rtry's relational awareness
# Exclude entire observations if they are explicitly marked as "juvenile" or "saplings"
try_data_clean <- rtry_exclude(try_data, 
                               (DataID %in% 413) & (OrigValueStr %in% c("juvenile", "saplings")), 
                               baseOn = ObservationID)

# Define a custom function to calculate the statistical mode
get_mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 4. SUMMARIZE & SCORE: Leverage dplyr for custom statistical logic
species_sensitivity <- try_data_clean %>%
  # Filter for Dispersal syndrome (TraitID 28)
  filter(TraitID == 28 & OrigValueStr != "") %>%
  group_by(AccSpeciesName) %>%
  summarize(
    Primary_Dispersal = get_mode(tolower(OrigValueStr)),
    Observation_Count = n()
  ) %>%
  mutate(
    # Assign sensitivity based on dispersal capacity across anthropogenic barriers
    Fragmentation_Sensitivity = case_when(
      grepl("wind|meteorochor", Primary_Dispersal) ~ "Low",
      grepl("animal|zoochor", Primary_Dispersal) ~ "Medium",
      grepl("gravity|barochor|unassisted", Primary_Dispersal) ~ "High",
      TRUE ~ "Unknown"
    )
  )

# View the final scored dataframe
print(species_sensitivity)

rtry_export(species_dispersal_summary, output_file)