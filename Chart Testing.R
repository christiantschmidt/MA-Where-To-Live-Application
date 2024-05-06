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

selected_city <- filter(SFHP, City == c('Abington','Acton')) %>%
  pivot_longer(cols = -City, names_to = "Year", values_to = "Price") %>%
  mutate(Year = as.numeric(gsub("`", "", Year)))

fig <- plot_ly(selected_city, x = ~Year, y = ~Price, type = 'scatter', mode = 'lines+markers')

fig

fig <- plot_ly(x = c(0,1,2), y = c(3,7,9), type = 'bar')
fig

library(plotly)

x <- c(1:100)
random_y <- rnorm(100, mean = 0)
data <- data.frame(x, random_y)

fig <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')

fig


