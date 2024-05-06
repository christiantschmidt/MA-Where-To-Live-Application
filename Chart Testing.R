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

selected_city <- SFHP %>%
  pivot_longer(cols = -City, names_to = "Year", values_to = "Price") %>%
  mutate(Year = as.numeric(gsub("`", "", Year)))


fig

fig <- plot_ly(x = c(0,1,2), y = c(3,7,9), type = 'bar')
fig

library(plotly)

plot_ly()
plot_ly(x = 1:10, y = 1:10)
d <- diamonds[sample(nrow(diamonds), 1000), ]
plot_ly(d, x = ~carat, y = ~price, text = ~paste("Clarity: ", clarity),
        mode = "markers", color = ~carat, size = ~carat, marker = list(line = list(color = "black")))
