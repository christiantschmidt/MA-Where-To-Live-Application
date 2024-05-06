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


