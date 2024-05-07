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
                     h1('', style='color: white;position:fixed;text-align:center;width:100%;'),
                     img(src='homepage.png', width='100%'),
                     p("Explore US Census Data (ACS) with Interactive Map and Charts", style="color:white;"))


#setting the map tab
map_page <- tabItem(tabName = "Map",
                    selectInput("year", "Year:", 2018:2022),
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
  dashboardHeader(title='Welcome'),
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