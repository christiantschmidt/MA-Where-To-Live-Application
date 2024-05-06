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

Cities <- SFHP %>% select(City) %>%
  distinct(City) %>%
  arrange(City) %>%
  pull(.,City)

# menuItem(text = 'Map', tabName = 'map',
#          icon = icon('map', lib = 'font-awesome')),
#icon = icon('equals', lib = 'font-awesome'

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
      menuItem(text = 'Compare', tabName = 'compare'))),
  ## Body ----------------------------------------------------------------------
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'compare',
              fluidRow(
                box(title = 'Select First Citiy to Compare',
                    width = 6,
                    pickerInput(inputId = 'first_city',
                                label = 'First City',
                                choices = Cities,
                                #selected = Cities[1],
                                multiple = FALSE,
                                options = list(`live-search` = TRUE,
                                               `virtual-scroll` = TRUE))),
                box(title = 'Select Second City to Compare',
                    width = 6,
                    pickerInput(inputId = 'second_city',
                                label = 'Second City',
                                choices = Cities,
                                #selected = Cities[1],
                                multiple = FALSE,
                                options = list(`live-search` = TRUE,
                                               `virtual-scroll` = TRUE)))
                ),
              fluidRow(
                box(title = 'Charts',
                    width = 12,
                    )
              )))))

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
}



# Run the application 
shinyApp(ui = ui, server = server)
