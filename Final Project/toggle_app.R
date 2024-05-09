library(shiny)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(sf)
library(shinydashboard)

setwd("~/Desktop/App")

# Set your census API key
census_api_key("f55a180f51955b1f67d3a703d629105d8f24eb71")

# Fetch and prepare the data outside of the Shiny server function
Variables_ALL <- load_variables(2020, "acs5")

acs_data <- get_acs(
  geography = "state", #geographic level of data to retrieve(so in this case state level)
  variables = list(    #Specifying the variables to retrieve from ACS
    median_age = "B01002_001",           #Variable for median age
    median_earnings = "B20002_001",      #Variable for median earnings
    cost_of_living_index = "B19013_001"  #Variable for cost of living
  ),
  year = 2020,     #the year of ACS data to retrieve
  geometry = TRUE  #geographic boundaries 
) |> 
  
  #Mutate and manipulate the data
  #Added a new column called Variable to the dataset (acs_data) to conditionally assign values 
  mutate(Variable = case_when(variable == "median_age" ~ "Median Age",
                              variable == "cost_of_living_index" ~ "Cost of Living Index",
                              variable == "median_earnings" ~ "Median Earnings")) |> 
  #columns to exclude
  dplyr::select(-moe, -GEOID, -variable) |> 
  #transforming the dataset from a long format to a wide format
  pivot_wider(names_from = Variable, values_from = estimate) |> 
  #filtering data the continental US states only
  filter(!NAME %in% c("Alaska", "Puerto Rico", "Hawaii"))

  
state_df <- data.frame(NAME=state.name,state_abv=state.abb)
acs_data <- acs_data |>
  left_join(state_df,by="NAME")


home_page <- tabItem(tabName = "Home",
                     h1('Where to Move?', style='color: white;position:fixed;text-align:center;width:100%;'),
                     img(src='homepage.png', width='100%'))

map_page <- tabItem(tabName = "Map",
                    leafletOutput("map"), 
                    tags$head(
                      tags$style(HTML("
          .content-wrapper {
            background-color: transparent !important;
          }
          .main-sidebar {
            background-color: transparent !important;
            border-right: 0px !important;
          }
        "))
                    ))


# Define UI
ui <- dashboardPage(
  dashboardHeader(title='This is my App'),
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon=icon('home')),
      menuItem("Map", tabName = "Map", icon=icon('map')),
      menuItem("Charts", tabName = "Charts", icon=icon('line-chart')),
      menuItem("More Options", tabName = "More Options")
    )
  ),
  dashboardBody(
    tabItems(
      home_page,
      map_page
    )
    
  )
)

# Define Server logic
server <- function(input, output) {
  output$map <- renderLeaflet({      #the output object named "map" in the UI will be generated here
    leaflet(data = acs_data) %>%     #creating a leaflet map
      addTiles() %>%                 #providing the underlying map imagery 
      addPolygons(                   #adding geographic boundaries 
        fillColor = ~colorNumeric(palette = "viridis", domain = acs_data$`Median Age`)(`Median Age`),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~paste(state_abv), # Add state initials as labels
        labelOptions = labelOptions(direction = "center", noHide = TRUE, textOnly = TRUE),
        popup = ~paste("State:", NAME, "<br>",
                       "Median Age:", `Median Age`, "<br>",
                       "Median Earnings:", `Median Earnings`, "<br>",
                       "Cost of Living Index:", `Cost of Living Index`)
      ) %>%
      addLegend("bottomright",    #adding map color 
                pal = colorNumeric(palette = "viridis", domain = acs_data$`Median Age`),
                values = ~`Median Age`,
                title = "Median Age",
                opacity = 0.7)
  })
}


# Run the application
shinyApp(ui, server)


