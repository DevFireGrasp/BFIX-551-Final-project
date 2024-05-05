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
#Variables_ALL <- load_variables(2020,"acs5")

acs_data2022 <- get_acs(
  geography = "state", #geographic level of data to retrieve(so in this case state level)
  variables = list(    #Specifying the variables to retrieve from ACS
    median_age = "B01002_001",           #Variable for median age
    median_earnings = "B20002_001",      #Variable for median earnings
    cost_of_living_index = "B19013_001"  #Variable for cost of living
    
    
  ),
  year = 2022,     #the year of ACS data to retrieve
  geometry = TRUE  #geographic boundaries 
) 

#   #Mutate and manipulate the data
#   #Added a new column called Variable to the dataset (acs_data) to conditionally assign values 
#   mutate(Variable = case_when(variable == "median_age" ~ "Median Age",
#                               variable == "cost_of_living_index" ~ "Cost of Living Index",
#                               variable == "median_earnings" ~ "Median Earnings")) |> 
#   #columns to exclude
#   dplyr::select(-moe, -GEOID, -variable) |> 
#   #transforming the dataset from a long format to a wide format
#   pivot_wider(names_from = Variable, values_from = estimate) |> 
#   #filtering data the continental US states only
#   filter(!NAME %in% c("Alaska", "Puerto Rico", "Hawaii"))|>
#   mutate(year=2018)
#   
# asc_data2020
#I ran the each data-set to download the acs_data between 2018-2022
#acs_data<-rbind(acs_data2022, acs_data2021, acs_data2020,acs_data2019,acs_data2018)
#write_csv(acs_data, "Acsdata.csv")
#acs_data<-read_csv("Acsdata.csv")(no geo)

# asc_data2020

#acs_data<-rbind(acs_data2022, acs_data2021, acs_data2020,acs_data2019,acs_data2018)


#write_csv(acs_data, "Acsdata.csv")
#write_sf(acs_data,"acs_data.geojson")
acs_data<-read_sf("acs_data.geojson")
acs_data2<-acs_data %>% mutate(Cost_of_living_Gap=`Median Earnings`-`Cost of Living Index`)

#Linear
reg<-lm(Cost_of_living_Gap~year+NAME,data=acs_data2)
plot_data<-acs_data2 %>% 
  as.data.frame()%>% 
  select(NAME, Cost_of_living_Gap, year) %>% 
  mutate(type="Historic")
#2023, 2024, 2025,2026,2027
pred_data<-data.frame(NAME=rep(unique(plot_data$NAME),5),
                      year=c(rep(2023,49), rep(2024,49), rep(2025,49),rep(2026,49), rep(2027,49)))
pred_data$Cost_of_living_Gap<-predict(reg,newdata=pred_data)
pred_data$type<-"Prediction"
plot_data_full<-rbind(plot_data,pred_data)

#################################################################################################

ui <- dashboardPage(skin = "purple",  #Change to change theme, Dashboard theme is different from shiny themes, dashboard has blue, black, purple, green, red yellow,
                    dashboardHeader(title = "US Census Data"),
                    dashboardSidebar(
                      
                      
                      sidebarMenu(
                        menuItem("Home", tabName = "Home", icon=icon('home')),
                        menuItem("Search", icon = icon("search"), #For Each new Slider or Search func, it needs to be added as a new menuSubItem
                                 menuSubItem(
                                   selectInput(
                                     inputId =  "date_from", 
                                     label = "Select time period:", 
                                     choices = 2018:2022,
                                     selected = 2020
                                   )
                                 ),
                                 menuSubItem( 
                                   selectInput(
                                     inputId="variable_name",
                                     label="Variable",
                                     choices=c("Median Earnings","Cost of Living Index", "Median Age"),
                                     selected= "Cost of Living Index"
                                   )
                                 ),
                                 #menuSubItem( 
                                  # selectInput("removeStates", "Which States to not consider:",
                                   #            choices = c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Guam', 'Hawaii', "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                    #           multiple = TRUE
                                   #)
                                 #),
                                 
                                 menuSubItem( 
                                   selectizeInput("modelStates", "Select Three States for Model:",
                                                  choices = c('Alabama', 'Alaska','Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Guam', 'Hawaii', "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                                  multiple = TRUE, selected = "Maryland",
                                                  options=list(maxItems=3)
                                   )
                                 )
                        ),
                        
                        
                        menuItem("USMap", tabName = "usmap", icon = icon("map")),
                        menuItem("Dataset", tabName = "dataset", icon = icon("home")),
                        menuItem("Download Personal Report", tabName = "print", icon = icon("print"))
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                h1('Where to Move?', style='color: white;position:fixed;text-align:center;width:100%;'),
                                img(src='homepage.png', width='100%')),
                        
                        tabItem(tabName = "usmap",
                                list(
                                  h2("Interactive Map with Census Data"),
                                  leafletOutput("map") ),
                                h2("Gap Between Cost of Living and Median Earnings"),
                                fluidRow(
                                  column(width=6,plotOutput("plot1")),
                                  column(width=6,plotOutput("plot2"))
                                )
                        ),
                        
                        
                        tabItem(tabName = "dataset",
                                h2("Data Section"),
                                tableOutput("chart")),
                        tabItem(tabName = "print",
                                h2("Print Section"),
                                textInput("name", "Please enter your Name", placeholder = "Jane Doe"),
                                checkboxInput("hasMap", "Would you like the map included?", FALSE),
                                checkboxInput("hasTable", "Would you like the table included?", FALSE),
                                downloadButton("downloadData", "Download Data")
                        )
                      )
                    )
)
# Define Server logic
server <- function(input, output) {
  
  #chosen_data <- reactive({get(input$date_from)})
  chosen_data <- reactive({
    
    
    acs_data |>filter(year==input$date_from)
    
  })
  #output$chart <- renderTable(acs_data) Working on this
  
  #server function for the shiny app
  output$map <- renderLeaflet({      #the output object named "map" in the UI will be generated here
    leaflet(data = chosen_data()) %>%     #creating a leaflet map
      addTiles() %>%                 #providing the underlying map imagery 
      addPolygons(                   #adding geographic boundaries 
        fillColor = ~colorNumeric(palette = "viridis", domain = chosen_data()[[input$variable_name]])(chosen_data()[[input$variable_name]]),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        popup = ~paste("State:", NAME, "<br>",
                       "Median Age:", `Median Age`, "<br>",
                       "Median Earnings:", `Median Earnings`, "<br>",
                       "Cost of Living Index:", `Cost of Living Index`)
      ) %>%
      addLegend("bottomright",    #adding map color 
                pal = colorNumeric(palette = "viridis", domain =chosen_data()[[input$variable_name]]),
                values = ~chosen_data()[[input$variable_name]],
                title = input$variable_name,
                opacity = 0.7)
    
  })
  output$plot1<-renderPlot({
    plot_data_full %>% filter(NAME%in%input$modelStates) %>% 
      ggplot(aes(x=year, y=Cost_of_living_Gap, color=NAME,linetype=type))+
      geom_point()+
      geom_smooth(method="lm",se=FALSE)+
      labs(x="Year",y="Gap Between Cost of Living and Median Earnings",fill="State")+
      scale_y_reverse()+
      scale_x_continuous(breaks=2018:2027)
  })
  output$plot2<-renderPlot({
    ggplot(acs_data2, aes(x=Cost_of_living_Gap, fill=as.factor(year)))+
      geom_histogram(color="white")+
      labs(x="Gap Between Cost of Living and Median Earnings",y="Amount of States", fill="Year")+
      facet_wrap(~year)
  }
  
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
    },
    content = function(file) {
     
      write.csv(chosen_data, file)
    }
  )
  
  
  
  
}





shinyApp(ui = ui, server = server)