library(shiny)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(bs4Dash)
library(waiter)
library(tidyverse)
library(shinythemes)
library(openxlsx)


#Chart testing
Education = read.xlsx('Final Data.xlsx', sheet = 1, check.names = FALSE)
Crime = read.xlsx('Final Data.xlsx', sheet = 2, check.names = FALSE)
Population = read.xlsx('Final Data.xlsx', sheet = 3, check.names = FALSE)
Weather = read.xlsx('Final Data.xlsx', sheet = 4, check.names = FALSE)
SFHP = read.xlsx('Final Data.xlsx', sheet = 5, check.names = FALSE)

colnames(Education) <- gsub("\\.", " ", colnames(Education))
colnames(Crime) <- gsub("\\.", " ", colnames(Crime))
colnames(Population) <- gsub("\\.", " ", colnames(Population))
colnames(Weather) <- gsub("\\.", " ", colnames(Weather))
colnames(SFHP) <- gsub("\\.", " ", colnames(SFHP))

plot <- plot_ly(SFHP, x = ~Year, y = ~`Median Price`, color = ~City, type = 'scatter', mode = 'lines+markers') %>%
  layout(title = "Median Price Over Years by City",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Median Price"),
         legend = list(title = "City")) %>%
  layout(hovermode = "x unified")

plot

SFHP %>% select(City) %>%
  distinct(City) %>%
  pull(.,City)
# Single Family Home Price Rankings
SFHP_Rankings <- SFHP %>%
  filter(Year == 2023) %>%
  select(City, `Median Price`) %>%
  mutate(`Median Price` = rank(`Median Price`, na.last = "keep",ties.method = "max")) %>%
  arrange(`Median Price`)

cutoff_points_median_price <- SFHP_Rankings %>%
  summarise(cutoff = quantile(`Median Price`, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE))

SFHP_Rankings <- SFHP_Rankings %>%
  mutate(`Median Price Grade` = cut(`Median Price`, breaks = cutoff_points_median_price$cutoff, labels = c("A", "B", "C", "D", "F"), include.lowest = TRUE))

# Most recent Crime Rankings 
Crime_Rankings <- Crime %>% 
  rename(City = Location) %>%
  filter(Year == 2022) %>%
  select(City, matches("Per 1K")) %>%
  mutate_all(~replace_na(., 0)) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "max"))) %>%
  arrange(`Per 1K - All Summary Offenses`)

Crime %>% filter(Year == 2022) %>% arrange(`Per 1K - Criminal Homicide`) 

crime_weights <- tibble(`Per 1K - All Summary Offenses` = 3,
                        `Per 1K - Criminal Homicide` = 3,
                        `Per 1K - Forcible Rape Total` = 3,
                        `Per 1K - Robbery Total` = 2,           
                        `Per 1K - Assault Total` = 2,
                        `Per 1K - Aggravated Assault Total` = 2, 
                        `Per 1K - Burglary Total` = 1,         
                        `Per 1K - Larceny - Theft Total` = 1)
weights <- as.vector(t(weights))
Crime_Rankings <- Crime_Rankings %>% 
  mutate(`Final Score` = rowSums(select(., -City) * weights)) %>%
  arrange(`Final Score`)

cutoff_points_crime_score <- Crime_Rankings %>%
  summarise(cutoff = quantile(`Final Score`, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE))

# Assign grades based on cutoff points of Median Price
Crime_Rankings <- Crime_Rankings %>%
  mutate(`Crime Grade` = cut(`Final Score`, breaks = cutoff_points_crime_score$cutoff, labels = c("A", "B", "C", "D", "F"), include.lowest = TRUE))


# Education Rankings for year 2023
Education_Rankings <- Education %>%
  filter(Year == 2023) %>%
  select(City, `% Dropped Out`, `% Graduated`,`Student / Teacher Ratio`) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "max")))
# Create a max value to reverse rankings of % Dropped Out
max_value <- max(Education_Rankings$`% Dropped Out`, na.rm = TRUE)
# Final Education Rankings
Education_Rankings <- Education_Rankings %>%
  mutate(`% Dropped Out` = max_value + 1 - `% Dropped Out`) %>%
  arrange(`% Dropped Out`)




# Create Weights
crime_weights <- tibble(`Per 1K - All Summary Offenses` = 3,
                  `Per 1K - Criminal Homicide` = 3,
                  `Per 1K - Forcible Rape Total` = 3,
                  `Per 1K - Robbery Total` = 2,           
                  `Per 1K - Assault Total` = 2,
                  `Per 1K - Aggravated Assault Total` = 2, 
                  `Per 1K - Burglary Total` = 1,         
                  `Per 1K - Larceny - Theft Total` = 1)
weights <- as.vector(t(weights))
Combined_Rankings <- SFHP_Rankings %>%
  left_join(Education_Rankings, by = "City") %>%
  left_join(Crime_Rankings, by = "City")
Combined_Rankings <- Combined_Rankings %>% mutate_all(~replace_na(., 0))
# Calculate the final score
Combined_Rankings <- Combined_Rankings %>% 
  mutate(`Final Score` = rowSums(select(., -City) * weights)) %>%
  arrange(`Final Score`)
# Convert 0 back to NA
Combined_Rankings[Combined_Rankings == 0] <- NA
# Create Grades
cutoff_points <- quantile(Combined_Rankings$`Final Score`, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)
# Assign grades based on cutoff points
Combined_Rankings <- Combined_Rankings %>%
  mutate(Grade = cut(`Final Score`, breaks = cutoff_points, labels = c("A", "B", "C", "D", "F"), include.lowest = TRUE))


# Find bottom 5 values
bottom_5 <- Crime %>% arrange(`Per 1K - Burglary Total`) %>% slice_head(n = 5)
