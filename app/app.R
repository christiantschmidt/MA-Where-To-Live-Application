# Libraries --------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bs4Dash)
library(waiter)
library(tidyverse)
library(shinythemes)
library(openxlsx)

# Read Data --------------------------------------------------------------------
Education = read.xlsx('Final Data.xlsx', sheet = 1, check.names = FALSE)
Crime = read.xlsx('Final Data.xlsx', sheet = 2, check.names = FALSE)
Population = read.xlsx('Final Data.xlsx', sheet = 3, check.names = FALSE)
Weather = read.xlsx('Final Data.xlsx', sheet = 4, check.names = FALSE)
SFHP = read.xlsx('Final Data.xlsx', sheet = 5, check.names = FALSE)

# UI ---------------------------------------------------------------------------
ui <- dashboardPage(
  title = 'MA Where to Live Application',
  ## Header --------------------------------------------------------------------
  header = dashboardHeader(
    skin = 'dark'
    ),
  ## Sidebar -------------------------------------------------------------------
  sidebar = dashboardSidebar(
    width = 150,
    elevation = 4,
    sidebarMenu(
      id = 'tabs',
      # menuItem(text = 'Map', tabName = 'map',
      #          icon = icon('map', lib = 'font-awesome')),
      menuItem(text = 'Compare', tabName = 'compare',
               icon = icon('equals', lib = 'font-awesome')))),
  ## Body ----------------------------------------------------------------------
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'compare',
              fluidRow(
                box(width = 6),
                box(width = 6)
                )))))

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
}



# Run the application 
shinyApp(ui = ui, server = server)
