setwd('C:/Users/SSAPKOTA/OneDrive - CIMMYT/DEWAS/Data_from_Gerald/Shishir/R/R')
library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
library(stringdist)
library(tidyr)  # For complete() function

# Import survey data
irrigated_survey <- st_read('./input/SurveyData/survey_vect_irrigated.shp') %>%
  mutate(Irrigation = "Irrigated") %>% # Add Irrigation column
  mutate(Source = "Survey")

rainfed_survey <- st_read('./input/SurveyData/survey_vect_rainfed.shp') %>%
  mutate(Irrigation = "Rainfed") %>%  # Add Irrigation column
  mutate(Source = "Survey")

# Bind survey data
survey_data <- rbind(irrigated_survey, rainfed_survey)

# Filter for Nepal
survey_data_np <- survey_data %>% filter(Country == 'Nepal')
head(survey_data_np)

# Extract Latitude and Longitude for survey_data_np
survey_data_np <- survey_data_np %>%
  mutate(Latitude = st_coordinates(geometry)[,2],  # Extract Y coordinate (Latitude)
         Longitude = st_coordinates(geometry)[,1]) # Extract X coordinate (Longitude)


# Import DNA fingerprinting data
dna_fp_irrigated <- st_read('./input/DNAfingerprinting/DNA_fp_I_Np.shp') %>%
  mutate(Irrigation = "Irrigated")%>%
  mutate(Source = 'dna')

dna_fp_rainfed <- st_read('./input/DNAfingerprinting/DNA_fp_R_Np.shp') %>%
  mutate(Irrigation = "Rainfed")%>%
  mutate(Source = 'dna')

# Bind DNA fingerprinting data
dna_fp_data <- rbind(dna_fp_irrigated, dna_fp_rainfed)
head(dna_fp_data)

# Import varietal data
varietal_np_irrigated <- st_read('./input/Varietal data/varietal_irrigated.shp') %>%
  mutate(Irrigation = "Irrigated")%>%
  mutate(Source = 'varietal')

varietal_np_rainfed <- st_read('./input/Varietal data/varietal_rainfed.shp') %>%
  mutate(Irrigation = "Rainfed")%>%
  mutate(Source = 'varietal')

# Bind varietal data
varietal_data <- rbind(varietal_np_irrigated, varietal_np_rainfed)

# Display first few rows
head(varietal_data)

##########################################################################
##########################################################################

# Standardize column names
dna_fp_data_selected <- dna_fp_data %>%
  rename(Cultivar = X.VARIETY.N)%>%    # Rename to match other datasets
  select(Cultivar, Irrigation,Source, Latitude, Longitude, geometry)

# Select common columns for merging
survey_data_np_selected <- survey_data_np %>%
  select(Cultivar, Irrigation,Source, Latitude, Longitude, geometry)

varietal_data_selected <- varietal_data %>%
  select(Edited.Var, Irrigation,Source, Latitude, Longitude, geometry)%>%
  rename(Cultivar = Edited.Var)

# Merge all datasets
merged_data <- rbind(survey_data_np_selected, dna_fp_data_selected, varietal_data_selected)

# Display first few rows
head(merged_data)


# clean the cultivar names to remove the spelling mistakes 

# Clean cultivar names
merged_data <- merged_data %>%
  mutate(Cultivar_cleaned = tolower(Cultivar),  # Convert to lowercase
         Cultivar_cleaned = str_trim(Cultivar_cleaned),  # Remove extra spaces
         Cultivar_cleaned = str_replace_all(Cultivar_cleaned, "[[:punct:]]", ""),  # Remove punctuation
         Cultivar_cleaned = str_replace_all(Cultivar_cleaned, "\\s+", " "))  # Normalize spaces

# Print unique cleaned names
unique(merged_data$Cultivar_cleaned)

library(stringdist)
library(fuzzyjoin)

# Define an expanded reference list of correct cultivar names
reference_list <- c(
  "bhrikuti", "nl 297", "hd 2967", "vijaya", "gautam", "unknown", "vijay", "wk 1204", "nl 971", 
  "annapurna 1", "zinc gahu 1", "zinc gahu 2", "borlaug 2020", "bheri ganga", "him ganga", 
  "khumal shakti", "awnless", "sworgadwari", "banganga", "dhaulagiri"
)

# Fuzzy matching to standardize names
merged_data <- merged_data %>%
  stringdist_left_join(data.frame(Cultivar_cleaned = reference_list), 
                       by = "Cultivar_cleaned", 
                       method = "jw", 
                       max_dist = 0.15) %>%
  mutate(Cultivar_final = ifelse(!is.na(Cultivar_cleaned.y), Cultivar_cleaned.y, Cultivar_cleaned.x)) %>%
  select(-Cultivar_cleaned.x, -Cultivar_cleaned.y)

# Print final unique names after standardization
unique(merged_data$Cultivar_final)




#############################################################################
######################## Visalize the dataset based on Cultivar_final_cleaned #############
#############################################################################


# Count the number of occurrences of each Cultivar_final for irrigated and rainfed conditions
Cultivar_final_counts <- merged_data %>%
  group_by(Source, Cultivar_final) %>%
  summarise(Count = n(), .groups = "drop")

# Get top 10 Cultivar_finals for irrigated and rainfed separately
top10_survey <- Cultivar_final_counts %>%
  filter(Source == "Survey") %>%
  arrange(desc(Count)) %>%
  head(10)

top10_dna <- Cultivar_final_counts %>%
  filter(Source == "dna") %>%
  arrange(desc(Count)) %>%
  head(10)

top10_vareital <- Cultivar_final_counts %>%
  filter(Source == "varietal") %>%
  arrange(desc(Count)) %>%
  head(10)

# Plot histogram for irrigated
p1 <- ggplot(top10_survey, aes(x = reorder(Cultivar_final, Count), y = Count, fill = Cultivar_final)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Most Widely Used Cultivar_finals in Nepal from Survey data",
       x = "Cultivar_final", y = "Count") +
  theme_minimal()

# Plot histogram for rainfed
p2 <- ggplot(top10_dna, aes(x = reorder(Cultivar_final, Count), y = Count, fill = Cultivar_final)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Most Widely Used Cultivar_finals in Nepal from DNA data",
       x = "Cultivar_final", y = "Count") +
  theme_minimal()

# Plot histogram for rainfed
p3 <- ggplot(top10_vareital, aes(x = reorder(Cultivar_final, Count), y = Count, fill = Cultivar_final)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Most Widely Used Cultivar_finals in Nepal from variety data",
       x = "Cultivar_final", y = "Count") +
  theme_minimal()

# Print both plots
print(p1)
print(p2)
print(p3)

# Read Nepal boundary shapefile
Nepal_boundary <- st_read('./output_static/Nepal_country.shp')

# Convert merged_data to sf object
nepal_data <- st_as_sf(merged_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Extract admin level-2 and level-3 boundaries
admin_level_2 <- Nepal_boundary %>% filter(!is.na(NAME_2))
admin_level_3 <- Nepal_boundary %>% filter(!is.na(NAME_3))

# Ensure CRS consistency
nepal_data <- st_transform(nepal_data, st_crs(Nepal_boundary))

# Spatially join cultivar data with administrative boundaries
nepal_data_level_2 <- st_join(nepal_data, admin_level_2, join = st_intersects)
nepal_data_level_3 <- st_join(nepal_data, admin_level_3, join = st_intersects)

# Identify top 10 most frequent Cultivar_final values
top_10_cultivars <- nepal_data %>%
  count(Cultivar_final, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(Cultivar_final)

# Function to calculate distribution (ensuring missing cultivars are filled with 0%)
calculate_distribution <- function(data, admin_col, admin_data) {
  distribution <- data %>%
    filter(Cultivar_final %in% top_10_cultivars) %>%
    count(!!sym(admin_col), Cultivar_final) %>%
    group_by(!!sym(admin_col)) %>%
    mutate(Percentage = (n / sum(n)) * 100) %>%
    ungroup()
  
  # Ensure all cultivars exist for every admin region, filling missing ones with 0
  complete_distribution <- admin_data %>%
    select(!!sym(admin_col)) %>%
    distinct() %>%
    crossing(Cultivar_final = top_10_cultivars) %>%  # Create all combinations
    left_join(distribution, by = c(admin_col, "Cultivar_final")) %>%
    mutate(Percentage = replace_na(Percentage, 0))  # Fill missing values with 0%
  
  return(complete_distribution)
}

# Compute distribution with missing values filled
level_2_distribution <- calculate_distribution(nepal_data_level_2, "NAME_2", admin_level_2)
level_3_distribution <- calculate_distribution(nepal_data_level_3, "NAME_3", admin_level_3)

# Convert back to sf for merging
level_2_top10_sf <- admin_level_2 %>%
  left_join(st_drop_geometry(level_2_distribution), by = "NAME_2")

level_3_top10_sf <- admin_level_3 %>%
  left_join(st_drop_geometry(level_3_distribution), by = "NAME_3")

# Plot Level-2 Percentage Distribution
ggplot(level_2_top10_sf) +
  geom_sf(aes(fill = Percentage)) +
  facet_wrap(~Cultivar_final) +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Top 10 Cultivar_finals (Level-2)",
       fill = "Percentage")

# Plot Level-3 Percentage Distribution
ggplot(level_3_top10_sf) +
  geom_sf(aes(fill = Percentage)) +
  facet_wrap(~Cultivar_final) +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Top 10 Cultivar_finals (Level-3)",
       fill = "Percentage")

# View results
head(level_2_distribution)
head(level_3_distribution)


