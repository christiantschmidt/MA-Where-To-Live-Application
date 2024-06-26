# Libraries --------------------------------------------------------------------
library(tidyverse)
library(openxlsx)

# First thing is to build out the join tables and to finalize the clean version.
# This will be the entire Education database that will be used to highlight certain information.
# I will also make sure that this database will be used in the interactive chart and that
# No information that is not relevant will be listed

mass_row <- tibble(City = 'Massachusetts')

City_Join_df <- read.csv('SFHP_GB_2023_Clean.csv',check.names = FALSE) %>%
  select('City' := 'City/Town') %>%
  bind_rows(., mass_row)

# Now join the dataframes
# I decided to no longer include Attendance as the data no longer has meaning for me
District_Info <- read.csv('Data/search.xls.csv',check.names = FALSE) %>%
  select(`District Name` = `ï»¿Org Name`, Town)

# Attendance_df <- read.csv('Attendance_df.csv',check.names = FALSE) %>%
#   left_join(District_Info %>% rename(Town = `District Name`, City = Town), by = 'Town') %>%
#   mutate(City = ifelse(is.na(City), Town, City)) %>%
#   select(City, everything(), -Town,-`Town Code`) %>%
#   rename('Total Attendance' := 'Total') %>%
#   mutate(City = if_else(City == "State", "Massachusetts", City)) %>%
#   group_by(City) %>%
#   summarise_all(sum)

# Education --------------------------------------------------------------------
Enrollment_df <- read.csv('Enrollment_df.csv',check.names = FALSE) %>%
  left_join(District_Info, by = 'District Name') %>%
  rename('City' := 'Town',
         'Total Enrollment' := 'Total') %>%
  select(-`District Code`, -`District Name`, -`Total Enrollment`) %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City)) %>%
  mutate(across(-c(`End of School Year`, `City`), as.integer)) %>%
  group_by(`City`,`End of School Year`) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(`Total Enrollment` = rowSums(across(!c(`City`, `End of School Year`))))

# Graduation Rate is weird, lets fix it by accounting for District

Graduation_Rate_df <- read.csv('Graduation_Rate_df.csv',check.names = FALSE) %>%
  filter(grepl('District', `District Name`)) %>%
  left_join(District_Info, by = 'District Name') %>%
  rbind(., read.csv('Graduation_Rate_df.csv',check.names = FALSE) %>%
          filter(!grepl('District', `District Name`)) %>%
          left_join(District_Info, by = 'District Name')) %>%
  arrange(`District Name`) %>%
  select(-`District Code`, -`District Name`) %>%
  rename('City' := 'Town') %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City),
         `# in Cohort` = as.integer(gsub(",","",`# in Cohort`))) %>%
  filter(!is.na(City)) %>%
  mutate(across(-c(`End of School Year`, City, `# in Cohort`), ~ .x / 100),
         across(-c(`End of School Year`, City, `# in Cohort`), ~ .x * `# in Cohort`)) %>%
  group_by(`End of School Year`, City) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(-c(`End of School Year`, City, `# in Cohort`), ~ .x / `# in Cohort`)) %>%
  arrange(`City`)

Teacher_df <- read.csv('Teacher_df.csv',check.names = FALSE) %>%
  filter(grepl('District', `District Name`)) %>%
  left_join(District_Info, by = 'District Name') %>%
  rbind(., read.csv('Teacher_df.csv',check.names = FALSE) %>%
          filter(!grepl('District', `District Name`)) %>%
          left_join(District_Info, by = 'District Name')) %>%
  arrange(`District Name`) %>%
  select(-`District Code`, -`District Name`) %>%
  rename('City' := 'Town') %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City)) %>%
  select(`End of School Year`, `City`, `Total # of Teachers (FTE)`,`% of Teachers Licensed`,`Student / Teacher Ratio`) %>%
  mutate(`Student / Teacher Ratio` = as.double(gsub(" to 1","",`Student / Teacher Ratio`), na.rm = TRUE),
         `Total # of Teachers (FTE)` = as.double(gsub(",","",`Total # of Teachers (FTE)`)),
         across(-c(`End of School Year`, City, `Total # of Teachers (FTE)`, `Student / Teacher Ratio`), ~ .x / 100),
         across(-c(`End of School Year`, City, `Total # of Teachers (FTE)`), ~ .x * `Total # of Teachers (FTE)`)) %>%
  group_by(`End of School Year`, City) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(-c(`End of School Year`, City, `Total # of Teachers (FTE)`), ~ .x / `Total # of Teachers (FTE)`))
         
Education_df <- City_Join_df %>%
  left_join(.,Enrollment_df, by = 'City') %>%
  left_join(.,Graduation_Rate_df, by = c('City','End of School Year')) %>%
  left_join(.,Teacher_df, by = c('City','End of School Year'))

# Crime --------------------------------------------------------------------
# Now do criminal data and only select the total aggregation of main crimes
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
  rename('Location' := 'City') %>%
  left_join(.,Number_of_Actual_Offenses_df, by = 'Location') %>%
  left_join(.,Summary_Offense_Rate_per_1000_df, by = c('Location','Year')) %>%
  select(Location, Year, everything())

# Population --------------------------------------------------------------------
# Do a quick table on population
Population_df <- City_Join_df %>%
  rename('Location' := 'City') %>%
  left_join(.,read.csv('Population_Estimate.csv',check.names = FALSE), by ='Location')

# Weather --------------------------------------------------------------------
# Also download weather to put in the final excel worksheet
Weather_df <- read.csv('weather.csv',check.names = FALSE)

# Single Family Housing Prices --------------------------------------------------------------------
# Finally finish the Single Family Home Prices table
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
  mutate(`Median Price` = as.integer(gsub("\\$|,", "", `Median Price`)))

# Workbook ---------------------------------------------------------------------
# Finally save the data frames to be used in the Shiny App
# Create a workbook
wb <- createWorkbook()

# Add sheets to the workbook
addWorksheet(wb, "Education")
addWorksheet(wb, "Crime")
addWorksheet(wb, "Population")
addWorksheet(wb, "Weather")
addWorksheet(wb, "Single Family Home Prices")

# Write data frames to the respective sheets
writeData(wb, "Education", Education_df)
writeData(wb, "Crime", Crime_df)
writeData(wb, "Population", Population_df)
writeData(wb, "Weather", Weather_df)
writeData(wb, "Single Family Home Prices", Single_Home_Prices_df)

# Save the workbook
saveWorkbook(wb, "Final Data.xlsx", overwrite = TRUE)

