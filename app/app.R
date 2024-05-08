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

# Read Data --------------------------------------------------------------------
Education = read.xlsx('Final Data.xlsx', sheet = 1, check.names = FALSE)
Crime = read.xlsx('Final Data.xlsx', sheet = 2, check.names = FALSE)
Population = read.xlsx('Final Data.xlsx', sheet = 3, check.names = FALSE)
Weather = read.xlsx('Final Data.xlsx', sheet = 4, check.names = FALSE)
SFHP = read.xlsx('Final Data.xlsx', sheet = 5, check.names = FALSE)

# Fix weird spacing issue in files
colnames(Education) <- gsub("\\.", " ", colnames(Education))
colnames(Education)[3:16] <- paste0("Grade ",colnames(Education)[3:16], " Enrollment")
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

# Create Ranking Tables --------------------------------------------------------
# Crime Rankings Table
Crime_Rankings <- Crime %>% 
  filter(Year == 2022) %>%
  select(City, matches("Per 1K")) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "min"))) %>%
  arrange(`Per 1K - All Summary Offenses`)%>%
  mutate(across(everything(), ~replace(., is.na(.), 0)),
         across(-City, ~ntile(., 5), .names = "{.col} Quartile"))

# Apply the case_when() separately
Crime_Rankings <- Crime_Rankings %>%
  mutate(across(contains("Quartile"), ~ {
    case_when(
      . == 1 ~ "A",
      . == 2 ~ "B",
      . == 3 ~ "C",
      . == 4 ~ "D",
      . == 5 ~ "F",
      TRUE ~ NA_character_)
  })) %>%
  mutate(across(contains("Quartile"), factor)) %>%
  rename_with(~gsub("Quartile$", "Grade", .x), contains("Quartile"))

# Education Ranking Table
Education_Rankings <- Education %>%
  filter(Year == 2023) %>%
  select(City, `% Dropped Out`, `% Graduated`,`Student / Teacher Ratio`) %>%
  mutate(across(-City, ~ rank(., na.last = "keep",ties.method = "min"))) 

max_value <- max(Education_Rankings$`% Graduated`, na.rm = TRUE)

Education_Rankings <- Education_Rankings %>%
  mutate(`% Graduated` = max_value + 1 - `% Graduated`) %>%
  arrange(`% Graduated`)

cutoff_points_education <- Education_Rankings %>%
  summarise(across(-`City`, ~quantile(., probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE)))

Education_Rankings <- Education_Rankings %>%
  mutate(across(c(`% Dropped Out`, `% Graduated`, `Student / Teacher Ratio`),
                ~ cut(., breaks = cutoff_points_education[, cur_column()], 
                      labels = c("A", "B", "C", "D", "F"), include.lowest = TRUE),
                .names = "{.col} Grade"))


# Rankings for SFHP
SFHP_Rankings <- SFHP %>%
  filter(Year == 2023) %>%
  select(City, `Median Price`) %>%
  mutate(`Median Price` = rank(`Median Price`, na.last = "keep",ties.method = "min")) %>%
  arrange(`Median Price`)

cutoff_points_median_price <- SFHP_Rankings %>%
  summarise(cutoff = quantile(`Median Price`, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE))

SFHP_Rankings <- SFHP_Rankings %>%
  mutate(`Median Price Grade` = cut(`Median Price`, breaks = cutoff_points_median_price$cutoff, labels = c("A", "B", "C", "D", "F"), include.lowest = TRUE))

Combined_Rankings <- SFHP_Rankings %>%
  left_join(Education_Rankings, by = c("City")) %>%
  left_join(Crime_Rankings, by = c("City")) %>%
  select(City, contains('Grade'))

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
  ## Header --------------------------------------------------------------------
  header = dashboardHeader(disable = TRUE,
                           title = '| Table of Contents |'),
  ## Sidebar -------------------------------------------------------------------
  sidebar = dashboardSidebar(
    width = 150,
    elevation = 5,
    sidebarMenu(
      id = 'tabs',
      menuItem(text = 'Project Outline', tabName = 'project_outline',
               icon = icon('r-project', lib = 'font-awesome')),
      menuItem(text = 'Compare', tabName = 'compare',
               icon = icon('equals', lib = 'font-awesome')),
      menuItem(text = 'Rankings', tabName = 'rankings',
               icon = icon('star', lib = 'font-awesome'))),
    style = "background-color: #add8e6;"), # Light blue sidebar background),
  ## Body ----------------------------------------------------------------------
  body = dashboardBody(
    tabItems(
      tabItem(tabName = 'project_outline',
              htmlOutput('project_outline_text')),
      tabItem(tabName = 'compare',
              fluidRow(
                box(title = 'Select First City to Compare',
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
                    width = 4,
                    pickerInput(inputId = 'y_axis_sfhp',
                                label = 'Y-axis',
                                choices = y_axis_sfhp),
                    htmlOutput('sfhp_text'),
                    br(),
                    plotlyOutput("sfhp_plot")),
                box(title = 'Education',
                    width = 4,
                    pickerInput(inputId = 'y_axis_education',
                                label = 'Y-axis',
                                choices = y_axis_education),                    
                    htmlOutput('education_text'),
                    br(),
                    plotlyOutput("education_plot")),
                box(title = 'Crime',
                    width = 4,
                    pickerInput(inputId = 'y_axis_crime',
                                label = 'Y-axis',
                                choices = y_axis_crime),                    
                    htmlOutput('crime_text'),
                    br(),
                    plotlyOutput("crime_plot")))),
      tabItem(tabName = 'rankings',
              fluidRow(
                box(title = 'Rankings',
                    width = 12,
                    htmlOutput('rankings_text'),
                    DTOutput('rankings_table'))))
    )),
  footer = dashboardFooter(
    right = paste0('Time: ',Sys.time(), ' | By: Christian Schmidt | c.schmidt131@gmail.com')))

# Server -----------------------------------------------------------------------
server <- function(input, output, session) {
  
  output$project_outline_text <- renderText({
    '<h1>MA Where to Live Project</h1>
    <h5>What is this project about?</h5>
    <p>The MA Where to Live Project originated from a table discovered on <a href="https://www.bostonmagazine.com/property/boston-single-family-home-prices-by-town-in-2023/">BostonMagazine.com</a>. 
    <br>This table depicted average home prices across the Greater Boston Area.</p>
    <p>Given Massachusetts\' reputation for high housing costs, the project aims to assist potential homeowners in assessing whether these prices are justified.
    <br> we are considering factors like crime rates and education quality to determine our rankings.</p>
    <p>To achieve this, data is sourced from various tables on BostonMagazine.com, providing historical single-family home pricing data.
    <br> Additionally, data available on mass.gov is utilized to enrich the analysis.</p>
    <p>It\'s crucial to emphasize that the project focuses solely on cities within Massachusetts, comparing and contrasting their characteristics.</p>
        
    
    <h5>Here\'s what\'s happening in this script:</h5>
    <p>First, we load various libraries.
    <br>These are like toolkits that contain pre-built functions for different tasks.
    <br>Some examples are creating interactive web applications (<code>shiny</code>) or working with data (<code>tidyverse</code>, <code>plotly</code>, <code>DT</code>, etc.).</p>
    <p>Next, we read in data from an Excel file (<code>\'Final Data.xlsx\'</code>) into different data frames (<code>Education</code>, <code>Crime</code>, <code>Population</code>, <code>Weather</code>, <code>SFHP</code>).
    <br> Each data frame represents different aspects of living conditions in different cities.</p>
    <p>Then, there\'s some data cleaning and manipulation.
    <br> For example, fixing column names and selecting specific columns for analysis.</p>
    <p>We also create ranking tables for crime, education, and single-family home prices (<code>SFHP</code>).
    <br>These tables rank cities based on certain criteria, like crime rates or median home prices.</p>
    <p>The UI part defines the layout of our web application.
    <br>It includes tabs for different sections: project outline, comparison, and rankings.
    <br>Each tab has its own content, like inputs for selecting cities to compare or plots showing trends over time.</p>
    <p>In the server part, we define functions to generate plots based on user inputs and render text explaining the data sources or providing context for the rankings.</p>
    <p>Finally, we run the Shiny application, combining the UI and server logic to create an interactive tool for exploring data and helping people make informed decisions about where to live in Massachusetts.</p>
    '
    
  })
  
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
    "* Data collected from education datasets on mass.gov<br>
    ** Data is aggregated based on location of school and not by district"
  })
  
  output$crime_plot <- renderPlotly({
    compare_charts(input$first_city,
                   input$second_city,
                   input$y_axis_crime,
                   Crime,
                   1)
  })
  
  output$crime_text <- renderText({
    "* Data collected from crime datasets on mass.gov<br>
     ** Data does no include School police and certain distinct precincts."
  })
  
  output$rankings_text <- renderText({
  "* A rankings means the City is in the top 20% in the Greater Boston Area.<br>
   ** F rankings means the City is in the bottom 20% in the Greater Boston Area. <br>
   *** These rankings are determined in comparison to each other, this is not based on statewide averages or nationwide averages.<br>"
  })
  
  output$rankings_table <- renderDT({
    # Define color tile formatting for all columns except "City"
    color_formats <- setNames(rep(list(color_tile('#57dc8f', '#dc6464')), ncol(Combined_Rankings) - 1), 
                              names(Combined_Rankings)[-which(names(Combined_Rankings) == "City")])
    
    # Apply color tile formatting to the data
    formattable(Combined_Rankings, color_formats) %>%
      as.datatable()
  })
  
}
  




# Run the application 
shinyApp(ui = ui, server = server)
