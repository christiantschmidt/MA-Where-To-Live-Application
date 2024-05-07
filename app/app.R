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

# Read Data --------------------------------------------------------------------
Education = read.xlsx('Final Data.xlsx', sheet = 1, check.names = FALSE)
Crime = read.xlsx('Final Data.xlsx', sheet = 2, check.names = FALSE)
Population = read.xlsx('Final Data.xlsx', sheet = 3, check.names = FALSE)
Weather = read.xlsx('Final Data.xlsx', sheet = 4, check.names = FALSE)
SFHP = read.xlsx('Final Data.xlsx', sheet = 5, check.names = FALSE)

# Fix weird spacing issue in files
colnames(Education) <- gsub("\\.", " ", colnames(Education))
colnames(Education)[2:16] <- paste0("Grade ",colnames(Education)[2:16], " Enrollment")
colnames(Crime) <- gsub("\\.", " ", colnames(Crime))
colnames(Population) <- gsub("\\.", " ", colnames(Population))
colnames(Weather) <- gsub("\\.", " ", colnames(Weather))
colnames(SFHP) <- gsub("\\.", " ", colnames(SFHP))
# Make final changes to the data to work with dataframes and charts

Cities <- SFHP %>% select(City) %>%
  distinct(City) %>%
  arrange(City) %>%
  pull(.,City)

Education <- Education %>% 
  select(City, Year = `End of School Year`, everything())

Crime <- Crime %>%
  rename(`City` = `Location`)

y_axis_sfhp <- SFHP %>% colnames(.)
y_axis_sfhp <- y_axis_sfhp[3:length(y_axis_sfhp)]

y_axis_education <- Education %>% colnames(.)
y_axis_education <- y_axis_education[3:length(y_axis_education)]

y_axis_crime <- Crime %>% colnames(.)
y_axis_crime <- y_axis_crime[3:length(y_axis_crime )]

# Create Ranking Tables
Crime_Rankings <- Crime %>% 
  filter(Year == 2022) %>%
  select(City, matches("Per 1K")) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "min"))) %>%
  arrange(`Per 1K - All Summary Offenses`)

Education_Rankings <- Education %>%
  filter(Year == 2023) %>%
  select(City, `% Dropped Out`, `% Graduated`,`Student / Teacher Ratio`) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "min")))

max_value <- max(Education_Rankings$`% Dropped Out`, na.rm = TRUE)

Education_Rankings <- Education_Rankings %>%
  mutate(`% Dropped Out` = max_value + 1 - `% Dropped Out`) %>%
  arrange(`% Dropped Out`)

SFHP_Rankings <- SFHP %>%
  filter(Year == 2023) %>%
  select(City, `Median Price`) %>%
  mutate(`Median Price` = rank(`Median Price`, na.last = "keep",ties.method = "min")) %>%
  arrange(`Median Price`)

Combined_Rankings <- SFHP_Rankings %>%
  left_join(Education_Rankings, by = c("City")) %>%
  left_join(Crime_Rankings, by = c("City"))

# menuItem(text = 'Map', tabName = 'map',
#          icon = icon('map', lib = 'font-awesome')),
#icon = icon('equals', lib = 'font-awesome'
# Functions --------------------------------------------------------------------

compare_charts <- function(first_city, second_city, y_axis, data, dtick) {
  filtered_data <- data %>%
    filter(City %in% c(first_city, second_city))
  
  plot <- plot_ly(filtered_data,
                  x = ~Year,
                  y = ~get(y_axis),
                  color = ~City,
                  type = 'scatter',
                  mode = 'lines+markers') %>%
    layout(title = paste(y_axis, "Over Years"),
           xaxis = list(title = "Year",
                        tickmode = 'linear',
                        dtick = dtick),
           yaxis = list(title = y_axis),
           showlegend = TRUE,
           legend = list(title = "City")) %>%
    layout(hovermode = "x unified")
  
  return(plot)
}


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
      menuItem(text = 'Compare', tabName = 'compare',
               icon = icon('equals', lib = 'font-awesome')),
      menuItem(text = 'Rankings', tabName = 'rankings',
               icon = icon('star', lib = 'font-awesome')))),
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
                                multiple = FALSE,
                                options = list(`live-search` = TRUE,
                                               `virtual-scroll` = TRUE))),
                box(title = 'Select Second City to Compare',
                    width = 6,
                    pickerInput(inputId = 'second_city',
                                label = 'Second City',
                                choices = Cities,
                                selected = Cities[2],
                                multiple = FALSE,
                                options = list(`live-search` = TRUE,
                                               `virtual-scroll` = TRUE)))),
              fluidRow(
                box(title = 'Single Family Homes',
                    width = 12,
                    pickerInput(inputId = 'y_axis_sfhp',
                                label = 'Y-axis',
                                choices = y_axis_sfhp),
                    htmlOutput('sfhp_text'),
                    br(),
                    plotlyOutput("sfhp_plot"))),
              fluidRow(
                box(title = 'Education',
                    width = 12,
                    pickerInput(inputId = 'y_axis_education',
                                label = 'Y-axis',
                                choices = y_axis_education),                    
                    htmlOutput('education_text'),
                    br(),
                    plotlyOutput("education_plot"))),
              fluidRow(
                box(title = 'Crime',
                    width = 12,
                    pickerInput(inputId = 'y_axis_crime',
                                label = 'Y-axis',
                                choices = y_axis_crime),                    
                    htmlOutput('crime_text'),
                    br(),
                    plotlyOutput("crime_plot")))
              
              ),
      tabItem(tabName = 'rankings')
      )))

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$sfhp_plot <- renderPlotly({
    compare_charts(input$first_city,
                   input$second_city,
                   input$y_axis_sfhp,
                   SFHP,
                   1)
  })
  
  output$sfhp_text <- renderText({
    "* Data collected from 4 articles from BostonMagazine.com.<br>
    ** Median Price data collected from 2011, 2016, 2018-2023"
  })
  
  output$education_plot <- renderPlotly({
    compare_charts(input$first_city,
                   input$second_city,
                   input$y_axis_education,
                   Education,
                   1)
  })
  
  output$education_text <- renderText({
    "* Data collected from education datasets on mass.gov"
  })
  
  output$crime_plot <- renderPlotly({
    compare_charts(input$first_city,
                   input$second_city,
                   input$y_axis_crime,
                   Crime,
                   1)
  })
  
  output$crime_text <- renderText({
    "* Data collected from crime datasets on mass.gov"
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
