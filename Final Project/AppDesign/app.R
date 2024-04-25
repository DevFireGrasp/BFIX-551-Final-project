if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(leaflet)) install.packages("leaflet"); require(leaflet) #leaflet library for interactive maps
if (!require(sf)) install.packages("sf"); require(sf) #sf library for spatial data manipulation
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)
if (!require(shiny)) install.packages("shiny"); require(shiny)
if (!require(tidycensus)) install.packages("shiny"); require(tidycensus) #for accessing US Census Bureau data
if (!require(shinydashboard)) install.packages("shinydashboard"); require(shinydashboard) #for accessing US Census Bureau data

# Set your census API key
census_api_key("f55a180f51955b1f67d3a703d629105d8f24eb71")

# Fetch and prepare the data outside of the Shiny server function
Variables_ALL <- load_variables(2020,"acs5")

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


#################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "US Census Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("USMap", tabName = "usmap", icon = icon("map")),
      menuItem("Dataset", tabName = "dashboard", icon = icon("home")),
      menuItem("Documentation", tabName = "settings", icon = icon("gear"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "usmap",
              h2("Interactive Map with Census Data")),

      #leafletOutput("map"), #ADDING MAP

      tabItem(tabName = "dashboard",
              h2("Data Section")),
      tabItem(tabName = "settings",
              h2("Documentation Section"))
    )
  )
)
# Define Server logic
server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
