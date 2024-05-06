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
Attendance_df <- read.csv('Attendance_df.csv',check.names = FALSE) %>%
  rename('City' := 'Town',
         'Total Attendance' := 'Total'
         ) %>%
  select(-`Town Code`) %>%
  mutate(City = if_else(City == "State", "Massachusetts", City))

Enrollment_df <- read.csv('Enrollment_df.csv',check.names = FALSE) %>%
  rename('City' := 'District Name',
         'Total Enrollment' := 'Total') %>%
  select(-`District Code`) %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City))

Graduation_Rate_df <- read.csv('Graduation_Rate_df.csv',check.names = FALSE) %>%
  rename('City' := 'District Name') %>%
  select(-`District Code`) %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City))

Teacher_df <- read.csv('Teacher_df.csv',check.names = FALSE) %>%
  rename('City' := 'District Name') %>%
  select(-`District Code`) %>%
  mutate(City = if_else(City == "State Totals", "Massachusetts", City))

Education_df <- City_Join_df %>%
  left_join(.,Enrollment_df, by = 'City') %>%
  left_join(.,Graduation_Rate_df, by = c('City','End of School Year')) %>%
  left_join(.,Teacher_df, by = c('City','End of School Year'))

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

# Do a quick table on population
Population_df <- City_Join_df %>%
  rename('Location' := 'City') %>%
  left_join(.,read.csv('Population_Estimate.csv',check.names = FALSE), by ='Location')

# Also download weather to put in the final excel worksheet
Weather_df <- read.csv('weather.csv',check.names = FALSE)

# Finally finish the Single Family Home Prices table

Single_Home_Prices_df <- City_Join_df %>%
  left_join(.,
            read.csv('SFHP_GB_2020_Clean.csv',check.names = FALSE) %>%
              select(City = `City/Town`,
                     `2018` = `2018 Median Price`,
                     `2019` = `2019 Median Price`),
            by = 'City') %>%
  left_join(.,read.csv('SFHP_GB_2021_Clean.csv',check.names = FALSE) %>%
              select(City = `City/Town`,
                     `2020` = `2020 Median Price`),
            by = 'City') %>%
  left_join(.,read.csv('SFHP_GB_2022_Clean.csv',check.names = FALSE) %>%
              select(City = `City/Town`,
                     `2021` = `2021 Median Price`),
            by = 'City') %>%
  left_join(.,read.csv('SFHP_GB_2023_Clean.csv',check.names = FALSE) %>%
              select(City = `City/Town`,
                     `2022` = `2022 Median Price`,
                     `2023` = `2023 Median Price`),
            by = 'City')


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

