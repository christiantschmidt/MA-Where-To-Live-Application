# Libraries --------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(zipcodeR)
library(ggmap)

# City -------------------------------------------------------------------------
# Correct City names
City_Join_df <- read.csv('SFHP_GB_2023_Clean.csv',check.names = FALSE) %>%
  select('City' := 'City/Town') %>%
  mutate(City = ifelse(grepl('Attleboro', City), 'Attleborough', City)) %>%
  distinct()

duplicates <- City_Join_df %>%
  group_by(City) %>%
  filter(n() > 1) %>%
  ungroup()

write.csv(City_Join_df,'2nd_Sprint_Data/City_Join_df.csv')

# Confirm data from zipcodeR matches the City names from BM.com
zip_data <- zipcodeR::zip_code_db %>%
  filter(state == "MA",
         !str_detect(zipcode_type,"Box"),
         !str_detect(zipcode_type,"Unique")) %>%
  rename(City = major_city, Zipcode = zipcode) %>%
  distinct(City, .keep_all = TRUE) %>%
  mutate(
    City = case_when(
      City == 'Attleboro' ~ 'Attleborough',
      City == 'Foxboro' ~ 'Foxborough',
      City == 'Manchester' ~ 'Manchester-by-the-Sea',
      City == 'Middleboro' ~ 'Middleborough',
      City == 'Tyngsboro' ~ 'Tyngsborough',
      City == 'South Hamilton' ~ 'Hamilton',
      City == 'South Easton' ~ 'Easton',
      TRUE ~ City),
    county = gsub(' County','',county)) %>%
  select(Zipcode,
         City,
         County = 'county',
         State = 'state',
         City_Latitude = 'lat',
         City_Longitude = 'lng')

foo <- zip_data %>% filter(str_detect(City,"Easton|Hamilton"))

# Census -----------------------------------------------------------------------
## Connection ------------------------------------------------------------------
census_api_key("2c4eac93c762294b7ecfb9ed2dd56f72dff0a3e8", install = TRUE)
readRenviron("~/.Renviron")
## Variables -------------------------------------------------------------------
variables <- load_variables(2022, "acs5")
write.csv(variables,'2nd_Sprint_Data/census_variables.csv')
## Data Table ------------------------------------------------------------------
### Key Data Points
# B25065_001 = Estimate!!Aggregate gross rent
# B01003_001 = Estimate!!Total - Total Population
# B19127_001 = Estimate!!Aggregate family income in the past 12 months 
# (in 2022 inflation-adjusted dollars)

# Retrieve ACS data for all towns in Massachusetts
census_ma_stats <- get_acs(
  geography = "county subdivision",
  state = "25",
  variables = c("B01003_001","B19113_001"),
  geometry = FALSE,
  year = 2022
) %>%
  select(-moe) %>%
  pivot_wider(.,names_from = variable, values_from = estimate) %>%
  rename("Total_Population" = `B01003_001`,
         "Median_Family_Income" = B19113_001) %>%
  separate(NAME, into = c("City", "County", "State"), sep = ", ") %>%
  mutate(City = gsub("\\s*( city| town| Town city)\\s*",
                     "",
                     City,
                     ignore.case = TRUE),  # Remove specified words
         City = ifelse(City == 'Attleboro', 'Attleborough', City),
         County = gsub("\\s.*", "", County),
         State = "MA") %>%  # Keep only the first part before the first space
  filter(!is.na(Median_Family_Income)) %>%
  left_join(get_acs(
    geography = "county subdivision",
    state = "25",
    variables = "B01003_001",
    geometry = TRUE,
    year = 2022) %>% select(GEOID, geometry, Year = year),
    by = 'GEOID')

get_census_data <- function(years = c(2022, 2021, 2020, 2019)) {
  data_list <- lapply(years, function(year) {
    census_data <- get_acs(
      geography = "county subdivision",
      state = "25",
      variables = c("B01003_001", "B19113_001"),
      geometry = FALSE,
      year = year
    ) %>%
      select(-moe) %>%
      pivot_wider(names_from = variable, values_from = estimate) %>%
      rename("Total_Population" = `B01003_001`,
             "Median_Family_Income" = B19113_001) %>%
      separate(NAME, into = c("City", "County", "State"), sep = ", ") %>%
      mutate(
        City = gsub("\\s*( city| town| Town city)\\s*", "", City, ignore.case = TRUE),
        City = ifelse(City == 'Attleboro', 'Attleborough', City),
        County = gsub("\\s.*", "", County),
        State = "MA",
        Year = year
      ) %>%
      filter(!is.na(Median_Family_Income))
    
    geo_data <- get_acs(
      geography = "county subdivision",
      state = "25",
      variables = "B01003_001",
      geometry = TRUE,
      year = year
    ) %>%
      select(GEOID, geometry) %>%
      mutate(Year = year)
    
    full_data <- left_join(census_data, geo_data, by = c("GEOID", "Year"))
    return(full_data)
  })
  
  combined_data <- bind_rows(data_list)
  return(combined_data)
}

census_ma_stats <- get_census_data()

# Save Census Data
write.csv(census_ma_stats,'2nd_Sprint_Data/Census_Data.csv')

# Education --------------------------------------------------------------------
## District Info ---------------------------------------------------------------
# Fix Manchester-by-the- Sea city name
District_Info <- read.csv('2nd_Sprint_Data/PublicSchoolDistrict.csv', check.names = FALSE) %>%
  rename_with(~ "District Name", 1) %>%
  rename(`City` = `Town`,
         `Zipcode` = `Zip`,
         `District Code` = `Org Code`,
         `District Type` = `Org Type`) %>%
  mutate(City = ifelse(City == 'Manchester', 'Manchester-by-the-Sea',City),
         City = gsub('Attleboro','Attleborough',City),
         `District Name` = gsub('Attleboro','Attleborough',`District Name`),
         Zipcode = paste0(0,Zipcode),
         geocode_address = ifelse(`Address 2` == '', paste(`Address 1`,
                                                          City,
                                                          State,
                                                          Zipcode,
                                                          sep = ", "),
                                 paste(`Address 1`,
                                       `Address 2`,
                                       City,
                                       State,
                                       Zipcode,
                                       sep = ", "))) %>%
  mutate_geocode(geocode_address)

write.csv(District_Info,'2nd_Sprint_Data/PublicSchoolDistrict_Clean.csv')

# Decided to remove NextGenMCAS in lieu of SAT scores

Attendance <- read.csv('2nd_Sprint_Data/Attendance.csv',header = TRUE, skip = 1, check.names = FALSE) %>%
  select(`District Name`,`District Code`,`Attendance Rate`)

Gradrates <- read.csv('2nd_Sprint_Data/Gradrates.csv', skip = 1, check.names = FALSE) %>%
  select(`District Name`,`District Code`,`Graduation Rate` = `% Graduated`)

Dropout <- read.csv('2nd_Sprint_Data/Dropout.csv', skip = 1, check.names = FALSE) %>%
  select(`District Name`,
         `District Code`,
         `Dropout Rate`  = `% Dropout All Grades`)

#NextGenMCAS <- read.csv('2nd_Sprint_Data/NextGenMCAS.csv', skip = 1, check.names = FALSE)

SAT <- read.csv('2nd_Sprint_Data/SAT_Performance.csv', skip = 1, check.names = FALSE) %>%
  rename(`SAT Reading Writing Score` = `Reading / Writing`,
         `SAT Math Score` = Math) %>%
  select(-Writing, -`Tests Taken`)

Teacher <- read.csv('2nd_Sprint_Data/TeacherData.csv', skip = 1, check.names = FALSE) %>%
  mutate(`Student / Teacher Ratio` = as.numeric(gsub(" to 1","",(str_trim(`Student / Teacher Ratio`))))) %>%
  select(`District Name`, 
         `District Code`,
         `Student Teacher Ratio`  = `Student / Teacher Ratio`,
         `Percent of Teachers Licensed` = `% of Teachers Licensed`,
         `Percent of Experienced Teachers`)

# Home Prices ------------------------------------------------------------------
SFHP_2011 <- read.csv('SFHP_GB_2021_Clean.csv',check.names = FALSE) %>%
  select(-`2021 Median Price`,
         -`2020 Median Price`,
         -`2016 Median Price`,
         -`2021 DOM`,
         -`2020 DOM`,
         -`One-Year Change`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2011 Median Price`,
         `Ten-Year % Change in Price` = `10-Year % Change In Price`,
         `Five-Year % Change in Price`= `Five-Year % Change In Price`) %>%
  select(City, `Median Price`, everything()) %>%
  mutate(across(-c(1:2), ~ as.integer(NA)),
         Year = 2011,
         `Days on Market` = as.integer(NA)) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2016 <- read.csv('SFHP_GB_2021_Clean.csv',check.names = FALSE) %>%
  select(-`2021 Median Price`,
         -`2020 Median Price`,
         -`2011 Median Price`,
         -`2021 DOM`,
         -`2020 DOM`,
         -`One-Year Change`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2016 Median Price`,
         `Ten-Year % Change in Price` = `10-Year % Change In Price`,
         `Five-Year % Change in Price`= `Five-Year % Change In Price`) %>%
  select(City, `Median Price`, everything()) %>%
  mutate(across(-c(1:2), ~ as.integer(NA)),
         Year = 2016,
         `Days on Market` = as.integer(NA)) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2018 <- read.csv('SFHP_GB_2020_Clean.csv',check.names = FALSE) %>%
  select(-`2019 Median Price`,
         -`2019 DOM`,
         -`Unnamed: 0`,
         -`One-Year Change`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2018 Median Price`,
         `Days on Market` = `2018 DOM`) %>%
  select(City, `Median Price`, `Days on Market`, everything()) %>%
  mutate(across(-c(1:3), ~ as.integer(NA)),
         Year = 2018,
         `Days on Market` = as.integer(`Days on Market`))  %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2019 <- read.csv('SFHP_GB_2020_Clean.csv',check.names = FALSE) %>%
  select(-`2018 Median Price`,
         -`2018 DOM`,
         -`Unnamed: 0`,
         -`One-Year Change`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2019 Median Price`,
         `Days on Market` = `2019 DOM`) %>%
  mutate(Year = 2019,
         `Days on Market` = as.integer(`Days on Market`),
         `One-Year % Change in Price` = as.integer(str_remove(`One-Year % Change in Price`, "%$")),
         `Five-Year % Change in Price` = as.integer(str_remove(`Five-Year % Change in Price`, "%$")),
         `Ten-Year % Change in Price` = as.integer(str_remove(`Ten-Year % Change in Price`, "%$"))) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2020 <- read.csv('SFHP_GB_2021_Clean.csv',check.names = FALSE) %>%
  select(-`2021 Median Price`,
         -`2016 Median Price`,
         -`2011 Median Price`,
         -`2021 DOM`,
         -`One-Year Change`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2020 Median Price`,
         `Days on Market` = `2020 DOM`,
         `Ten-Year % Change in Price` = `10-Year % Change In Price`,
         `Five-Year % Change in Price`= `Five-Year % Change In Price`) %>%
  select(City, `Median Price`, `Days on Market`, everything()) %>%
  mutate(across(-c(1:3), ~ as.integer(NA)),
         Year = 2020)  %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2021 <- read.csv('SFHP_GB_2021_Clean.csv',check.names = FALSE) %>%
  select(-`2020 Median Price`,
         -`2016 Median Price`,
         -`2011 Median Price`,
         -`2020 DOM`,
         -`One-Year Change`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2021 Median Price`,
         `Days on Market` = `2021 DOM`,
         `Ten-Year % Change in Price` = `10-Year % Change In Price`,
         `Five-Year % Change in Price` = `Five-Year % Change In Price`) %>%
  mutate(Year = 2021,
         `One-Year % Change in Price` = as.integer(str_remove(`One-Year % Change in Price`, "%$")),
         `Five-Year % Change in Price` = as.integer(str_remove(`Five-Year % Change in Price`, "%$")),
         `Ten-Year % Change in Price` = as.integer(str_remove(`Ten-Year % Change in Price`, "%$"))) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2022 <- read.csv('SFHP_GB_2022_Clean.csv',check.names = FALSE) %>%
  select(-`2021 Median Price`,
         -`2021 Days on Market`,
         -`One-Year Change`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2022 Median Price`,
         `Days on Market` = `2022 Days on Market`,
         `Ten-Year % Change in Price` = `10-Year % Change in Price`) %>%
  mutate(Year = 2022,
         `One-Year % Change in Price` = as.integer(str_remove(`One-Year % Change in Price`, "%$")),
         `Five-Year % Change in Price` = as.integer(str_remove(`Five-Year % Change in Price`, "%$")),
         `Ten-Year % Change in Price` = as.integer(str_remove(`Ten-Year % Change in Price`, "%$"))) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

SFHP_2023 <- read.csv('SFHP_GB_2023_Clean.csv',check.names = FALSE) %>%
  select(-`2022 Median Price`,
         -`2022 Days on Market`,
         -`One-Year % Change Days on Market`,
         -`Unnamed: 0`) %>%
  rename(City = `City/Town`,
         `Median Price` = `2023 Median Price`,
         `Days on Market` = `2023 Days on Market`) %>%
  mutate(Year = 2023,
         `One-Year % Change in Price` = as.integer(str_remove(`One-Year % Change in Price`, "%$")),
         `Five-Year % Change in Price` = as.integer(str_remove(`Five-Year % Change in Price`, "%$")),
         `Ten-Year % Change in Price` = as.integer(str_remove(`Ten-Year % Change in Price`, "%$"))) %>%
  select(City, Year, `Median Price`, `Days on Market`, everything())

sapply(SFHP_2011, typeof)
sapply(SFHP_2011, typeof)
sapply(SFHP_2016, typeof)
sapply(SFHP_2018, typeof)
sapply(SFHP_2019, typeof)
sapply(SFHP_2020, typeof)
sapply(SFHP_2021, typeof)
sapply(SFHP_2022, typeof)
sapply(SFHP_2023, typeof)


colnames(SFHP_2011)
colnames(SFHP_2016)
colnames(SFHP_2018)
colnames(SFHP_2019)
colnames(SFHP_2020)
colnames(SFHP_2021)
colnames(SFHP_2022)
colnames(SFHP_2023)

Single_Home_Prices_df  <- bind_rows(SFHP_2011,
                                    SFHP_2016,
                                    SFHP_2018,
                                    SFHP_2019,
                                    SFHP_2020,
                                    SFHP_2021,
                                    SFHP_2022,
                                    SFHP_2023) %>%
  mutate(`Median Price` = as.integer(gsub("\\$|,", "", `Median Price`)),
         City = gsub('Attleboro','Attleborough',City))

write.csv(Single_Home_Prices_df,'2nd_Sprint_Data/SFHP_Data.csv')
# Crime ------------------------------------------------------------------------
Number_of_Actual_Offenses_df <- read.csv('Number_of_Actual_Offenses_df.csv',check.names = FALSE) %>%
  select(Location,
         `All Summary Offenses`,
         `Criminal Homicide`,
         `Forcible Rape Total`,
         `Robbery Total`,
         `Assault Total`,
         `Aggravated Assault Total`,
         `Burglary Total`,
         `Larceny - Theft Total`,
         `Year`) %>%
  rename_with(~paste0("Pop ", .), -1) %>%
  rename(Year = "Pop Year")

Summary_Offense_Rate_per_1000_df <- read.csv('Summary_Offense_Rate_per_1000.csv',check.names = FALSE) %>%
  select(Location,
         `All Summary Offenses`,
         `Criminal Homicide`,
         `Forcible Rape Total`,
         `Robbery Total`,
         `Assault Total`,
         `Aggravated Assault Total`,
         `Burglary Total`,
         `Larceny - Theft Total`,
         `Year`) %>%
  rename_with(~paste0("Per 1K - ", .), -1) %>%
  rename(Year = "Per 1K - Year")

Crime_df <- City_Join_df %>%
  left_join(.,Number_of_Actual_Offenses_df %>% rename('City' = 'Location'), by = 'City') %>%
  left_join(.,Summary_Offense_Rate_per_1000_df  %>% rename('City' = 'Location'), by = c('City','Year')) %>%
  select(City, Year, everything()) %>%
  mutate(across(everything(), ~ifelse(is.na(.) | str_trim(.) == '',0,.))) %>%
  filter(!City == 'Attleborough') %>%
  bind_rows(data.frame(
    City = rep("Attleborough", 4),
    Year = c(2019, 2020, 2021, 2022))) %>%
  arrange(City, Year)

# Final Tables -----------------------------------------------------------------
## Education Fact table --------------------------------------------------------

Education_Facts <- Attendance %>%
  left_join(.,Gradrates, by = c('District Name', 'District Code')) %>%
  left_join(.,Dropout, by = c('District Name', 'District Code')) %>%
  #left_join(.,NextGenMCAS, by = c('District Name', 'District Code')) %>%
  left_join(.,SAT, by = c('District Name', 'District Code')) %>%
  left_join(.,Teacher, by = c('District Name', 'District Code')) %>%
  left_join(.,District_Info %>% 
              select(`District Name`, `District Code`, City),
            by = c('District Name', 'District Code')) %>%
  right_join(.,City_Join_df, by = 'City')

## Education Dims table --------------------------------------------------------
Education_Dims <- District_Info %>%
  select(`District Code`,
         `District Name`,
         `District Type`,
         Grade,
         geocode_address,
         School_Longitude = lon,
         School_Latitude = lat)

## City Fact table -------------------------------------------------------------

City_Facts <- Single_Home_Prices_df %>%
  left_join(.,census_ma_stats %>% 
              select(City,Year,Total_Population,Median_Family_Income),
            by = c('City','Year')) %>%
  left_join(Crime_df, by = c('City','Year')) %>%
  arrange('City','Year')

## City Dims table -------------------------------------------------------------

City_Dims <- City_Join_df %>%
  left_join(., zip_data, by = 'City') %>%
  left_join(., census_ma_stats %>% select(City, geometry), by = 'City')

write.csv(Education_Facts,'version_2/Education_Facts.csv')
write.csv(Education_Dims,'version_2/Education_Dims.csv')
write.csv(City_Facts,'version_2/City_Facts.csv')
write.csv(City_Dims,'version_2/City_Dims.csv')
