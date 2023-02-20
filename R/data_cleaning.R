# Cleaning the raw data into a suitable format

library(tidyverse)
library(arrow)


# Raw data taken from the RLS dropbox folder
# Using Method 1 (M1) data only
# Starting with Australia data only


# Observation level data (survey_id, species_name, size)
if(!file.exists("data/cleaned/obs_data_m1_aus.parquet")){
  read_csv("data/raw/ep_m1_ALL.csv") %>% 
    filter(country == "Australia") %>% 
    filter(method == 1) %>% 
    filter(size_class > 0) %>% 
    group_by(
      survey_id, 
      species_name,
      size_class
    ) %>% 
    summarise(total = sum(total),
              .groups = "drop") %>% 
    uncount(weights = total) %>% 
    filter(str_detect(species_name, "[A-Z]{1}[a-z]+\\s[a-z]+")) %>% 
    filter(!str_detect(species_name, "\\.")) %>% 
    write_parquet("data/cleaned/obs_data_m1_aus.parquet")
}

# survey-level data (survey_id, site_code, lat, lon etc.)
if(!file.exists("data/cleaned/survey_list_m1_aus.parquet")){
  read_csv("data/raw/ep_m1_ALL.csv") %>% 
    filter(country == "Australia") %>% 
    select(
      survey_id,
      site_code,
      ecoregion, 
      latitude, 
      longitude, 
      depth, 
      survey_date
    )%>% 
    write_parquet("data/cleaned/survey_list_m1_aus.parquet")
  
}

