library(dplyr)   
library(readr)   
library(purrr)   

# List of countries
countries <- c("CZ", "ES", "SE")

# Selected years for analysis
years <- c(2005, 2010, 2014, 2018, 2023)  

# Function to load and merge all necessary files for one year, for a given country
load_and_merge_year <- function(year, country_code) {
  
  #extract last two digits of the year, used to match the file naming convention (e.g., "23" for 2023)
  year_suffix <- substr(as.character(year), 3, 4)
  
  # Construct paths to the 4 files for that year 
  path_year <- file.path(paste0("./data/_Cross_2004-2023_full_set/", country_code), as.character(year))
  file_d <- file.path(path_year, paste0("UDB_c", country_code, year_suffix, "D.csv"))  
  file_h <- file.path(path_year, paste0("UDB_c", country_code, year_suffix, "H.csv"))  
  file_p <- file.path(path_year, paste0("UDB_c", country_code, year_suffix, "P.csv"))  
  file_r <- file.path(path_year, paste0("UDB_c", country_code, year_suffix, "R.csv"))  
  
  d <- read_csv(file_d, show_col_types = FALSE)
  h <- read_csv(file_h, show_col_types = FALSE)
  p <- read_csv(file_p, show_col_types = FALSE)
  r <- read_csv(file_r, show_col_types = FALSE)
  
  if (!"PE040" %in% names(p)) p$PE040 <- NA
  if (!"PE041" %in% names(p)) p$PE041 <- NA
  
  # Merge R and P by personal ID 
  merged_person <- inner_join(r, p, by = c("RB030" = "PB030"))
  
  merged_person <- merged_person %>%
    mutate(
      personal_income = rowSums(across(any_of(c("PY010G", "PY050G", "PY080G", "PY100G", "PY110G")), ~replace_na(., 0)))
    )
  
  # create household ID
  merged_person <- merged_person %>%
    mutate(hhid = as.numeric(substring(RB030, 1, nchar(RB030) - 2))) %>%
    relocate(hhid, .after = RB030)
  
  # Merge with household data (H) by household ID
  merged_with_household <- left_join(merged_person, h, by = c("hhid" = "HB030"))
  
  # Merge with demographic data (D) by household ID
  merged_full <- left_join(merged_with_household, d, by = c("hhid" = "DB030"))
  
  # Filter for only the selected country
  merged_full <- merged_full %>%
    filter(DB020 == country_code) %>%
    mutate(year = year, country = country_code)
  
  # Add age and recoded education category
  merged_full <- merged_full %>%
    mutate(
      age = year - PB140,
      edu_cat = case_when(
        year < 2014 & PE040 %in% c(0,1,2) ~ "Low",
        year < 2014 & PE040 %in% c(3,4) ~ "Medium",
        year < 2014 & PE040 %in% c(5,6) ~ "High",
        year >= 2014 & year < 2021 & PE040 %in% c(000,100,200) ~ "Low",
        year >= 2014 & year < 2021 & PE040 >= 300 & PE040 <= 490 ~ "Medium",
        year >= 2014 & year < 2021 & PE040 >= 500 & PE040 <= 800 ~ "High",
        year >= 2021 & PE041 %in% c(000,100,200) ~ "Low",
        year >= 2021 & PE041 >= 300 & PE041 <= 490 ~ "Medium",
        year >= 2021 & PE041 >= 500 & PE041 <= 800 ~ "High",
        TRUE ~ NA_character_
      ),
      edu_cat = factor(edu_cat, levels = c("Low", "Medium", "High"), ordered = TRUE)
    )
  
  
  return(merged_full)
}

for (country_code in countries) {
  
  all_data_list <- map(years, ~load_and_merge_year(.x, country_code))
  all_data_combined <- bind_rows(all_data_list)
  
  save(all_data_combined, file = paste0("./data/all_data_combined_", country_code, ".Rdata"))
}
