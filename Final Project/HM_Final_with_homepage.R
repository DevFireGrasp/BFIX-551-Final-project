if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(leaflet)) install.packages("leaflet"); require(leaflet) #leaflet library for interactive maps
if (!require(sf)) install.packages("sf"); require(sf) #sf library for spatial data manipulation
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)
if (!require(shiny)) install.packages("shiny"); require(shiny)
if (!require(tidycensus)) install.packages("shiny"); require(tidycensus) #for accessing US Census Bureau data
if (!require(shinyjs)) install.packages("shinyjs")
if (!require(shinydashboardPlus)) install.packages("shinydashboardPlus")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(plotly)) install.packages("plotly")

#Setting working directory 
setwd("~/Desktop/App")

#loading data stored in acs_data.Rdata
load('acs_data.Rdata')

#setting the homepage
home_page <- tabItem(tabName = "Home",
                     tags$div(
                       style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; background: linear-gradient(to bottom right, steelblue, steelblue);",
                       h1("Welcome to Our Interactive Census Data Explorer", style = "color: white;"),
                       img(src = 'homepage.png', width = '130%'),
                       p("Are you looking to make informed decisions about where to live based on the current state of the economy? Look no further! Our interactive web page provides you with a powerful tool to explore the latest US Census Data from the American Community Survey (ACS).", style = "color:white;")
                     )
)

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




#setting the map tab



# Define UI
ui <- dashboardPage(
  dashboardHeader(title=span('Dashboard')),
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
  
  acs_data <- reactive({
    all_data[[as.character(input$year)]]
  })
  
  output$map <- renderLeaflet({      #the output object named "map" in the UI will be generated here
    leaflet(data = acs_data()) %>%     #creating a leaflet map
      addTiles() %>%                 #providing the underlying map imagery 
      addPolygons(                   #adding geographic boundaries 
        fillColor = ~colorNumeric(palette = "viridis", domain = acs_data()$`Median Age`)(`Median Age`),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~paste(state_abv), # Add state initials as labels
        labelOptions = labelOptions(direction = "center", noHide = TRUE, textOnly = TRUE),
        popup = ~paste("State:", NAME, "<br>",
                       "Population:", `Population`, "<br>",
                       "Median Age:", `Median Age`, "<br>",
                       "Median Earnings:", `Median Earnings`)
                       
      ) %>%
      addLegend("bottomright",    #adding map color 
                pal = colorNumeric(palette = "viridis", domain = acs_data()$`Median Age`),
                values = ~`Median Age`,
                title = "Median Age",
                opacity = 0.7)
  })
}


# Run the application
shinyApp(ui = ui, server = server)