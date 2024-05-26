# Libraries --------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bs4Dash)
library(waiter)
library(tidyverse)
library(shinythemes)
library(openxlsx)
library(plotly)
library(DT)
library(formattable)

# UI ---------------------------------------------------------------------------
ui <- dashboardPage(
  title = 'MA Where to Live Project',
  dark = NULL, # TRUE/FALSE
  help = NULL, # TRUE/FALSE
  #fullscreen = TRUE,
  ## Header --------------------------------------------------------------------
  header = dashboardHeader(
    title <- dashboardBrand(
      title = "Christian Schmidt",
      color = "lightblue",
      href = "https://www.linkedin.com/in/c-t-schmidt/",
      image = "https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png"
    ),
    skin = "lightblue"
  ),
  ## Sidebar -------------------------------------------------------------------
  sidebar = dashboardSidebar(
    id = 'sidebar',
    width = 150,
    elevation = 4,
    minified = FALSE,
    collapsed = FALSE,
    sidebarMenu(
      id = 'tabs',
      menuItem(
        text = 'Project Outline',
        tabName = 'project_outline',
        icon = icon('r-project', lib = 'font-awesome', verify_fa = FALSE)
      ),
      menuItem(
        text = 'Compare',
        tabName = 'compare',
        icon = icon('equals', lib = 'font-awesome', verify_fa = FALSE)
      ),
      menuItem(
        text = 'City Map',
        tabName = 'city_map',
        icon = icon('map', lib = 'font-awesome', verify_fa = FALSE)
      ),
      menuItem(
        text = 'School Map',
        tabName = 'school_map',
        icon = icon('map', lib = 'font-awesome', verify_fa = FALSE)
      )
    )
  ),
  ## Controlbar ----------------------------------------------------------------
  controlbar = dashboardControlbar(),
  ## Footer --------------------------------------------------------------------
  footer = dashboardFooter(
    left = paste0('| Date: ', Sys.Date(), ' | Time: ', 
                  format(Sys.time(), "%I:%M:%S %p"),' |'),
    right = paste0('| By: Christian Schmidt | c.schmidt131@gmail.com |')),
  ## Body ----------------------------------------------------------------------
  body = dashboardBody()
)


