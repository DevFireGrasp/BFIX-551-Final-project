
if (!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if (!require(leaflet)) install.packages("leaflet"); require(leaflet) #leaflet library for interactive maps
if (!require(sf)) install.packages("sf"); require(sf) #sf library for spatial data manipulation
if (!require(ggplot2)) install.packages("ggplot2"); require(ggplot2)
if (!require(shiny)) install.packages("shiny"); require(shiny)
if (!require(tidycensus)) install.packages("tidycensus"); require(tidycensus) #for accessing US Census Bureau data
if (!require(shinydashboard)) install.packages("shinydashboard"); require(shinydashboard) #for accessing US Census Bureau data
if (!require(dplyr )) install.packages("dplyr "); require(dplyr )
if (!require(shinyjs )) install.packages("shinyjs"); require(shinyjs)
if (!require(gif )) install.packages("gif"); require(gif)
#Data 
acs_data<-read_sf("acs_data3.geojson")
acs_data2<-acs_data %>% mutate(Cost_of_living_Gap=`Median Earnings`-`Cost of Living Index`)



#Linear and Histogram 
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


#Print Input
jsCode <- 'shinyjs.winprint = function(){

window.print();
}'
#################################################################################################

#Home Page 
home_page <- tabItem(tabName = "Home",
                     tags$div(
                       style = "position: absolute; top: 52%; left: 58%; transform: translate(-50%, -50%); text-align: center; background: linear-gradient(to bottom right, steelblue, steelblue); width: 78%; height: 77%;",
                       h2("Welcome to Our Interactive Census Data Explorer", style = "color: white;"),
                       h4("Where To Live in this Economy?", style="color:white;"),
                       #read.gif(url("https://giphy.com/clips/parksandrec-parks-and-recreation-rec-peacock-tv-16sySqkP1ObuleZfAG")),
                       br(),
                       img(src="giphy.gif", align='center', height='150px', width='100px'),
                       br(),
                       img(src ='homepage.png', width = '130%'),
                       br(),
                       h5("Are you looking to make informed decisions about where to live based on the current state of the economy? Look no further! Our interactive web page provides you with a powerful tool to explore the latest US Census Data from the American Community Survey (ACS).", style = "color:white; width='50px';"),
                       h4("How to Navigate Search Functions :",style="color:white;"),
                       h6("STEP 1: Utilize the 'Select time period' dropdown inorder to view yearly changes between 2018-2022",style="color:white;"),
                       h6(" STEP 2: Simultaneously select a 'Varaible' from the dropdown to provide geographical analysis of the following",style="color:white;"),
                       h6("*Cost of Living",style="color:white;"),
                       h6("*Median Earnings",style="color:white;"),
                       h6("*Median Age",style="color:white;"),
                       h6("*Migration Patterns between Ages 20-30",style="color:white;"),
                       h6("STEP3: Choose up to three states MAX to compare cost of living and earningss gap for historical and predictive years up 2027",style="color:white;" ),
                       h6("STEP4: Review Map + Graphs, Ranks of Top 10 Expenses, and Print out analysis",style="color:white;"),
                       h6("Note: There are two print functions one on the Rank Tab and another on the top left corner of the Map Tab", style="color:white;")
                       
          
                     )
)

#Variables, Maps, Year Interactive 

ui <- dashboardPage(skin = "blue",  #Change to change theme, Dashboard theme is different from shiny themes, dashboard has blue, black, purple, green, red yellow,
                    
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
                                     choices=c("Median Earnings","Cost of Living Index", "Median Age", "Age Population 20-24"="Migration Age20-24", "Age Population 25-30"="Migration Age25-30"),
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
                        #menuItem("Dataset", tabName = "dataset", icon = icon("home")), Removed dataset tab, already have a lot of visualization of the data
                        menuItem("Ranking and Personal Report", tabName = "print", icon = icon("print"))
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        home_page,
        
                        tabItem(tabName = "usmap",
                                list(
                                  useShinyjs(),extendShinyjs(text = jsCode, functions =  #print function
                                                               c("winprint")),
                                  actionButton("print", "PRINT"),
                                  h2("Interactive Map with Census Data"),
                                  leafletOutput("map") ),
                                h2("Gap Between Cost of Living and Median Earnings"),
                                fluidRow(
                                  column(width=6,plotOutput("plot1")),
                                  column(width=6,plotOutput("plot2"))
                                )
                        ),
                        
                        
                        
                        
                        ##Download page and button
                        tabItem(tabName = "print",
                                h2("Ranking and Print Section"),
                                 tableOutput("preview"),
                                 selectInput("datasetYR", "Pick a Year", 2018:2022, selected = 2020),
                                 checkboxInput("all", "Print All States?", FALSE),
                                 downloadButton("download", "Download .csv")
                        )
                      )
                    )
)
# Define Server logic
server <- function(input, output) {
  
  observeEvent(input$print, {
    
    js$winprint()
  }) #print function
  
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
  
  #TESTING
   output$preview<-renderTable({
     table_data <- plot_data<-acs_data2 %>% #Grabs the same data we used for the charts on map page
       as.data.frame()%>% 
       select(NAME, Cost_of_living_Gap, year) %>% filter(year==input$datasetYR)   #filters for year, state (NAME), and COL - pay
     table_data$Cost_of_living_Gap <- abs(table_data$Cost_of_living_Gap)           #COL - pay is negative so got the absolute
     table_data <- table_data[order(table_data$Cost_of_living_Gap, decreasing = TRUE),]       #order by COL
     table_data$year <- as.integer(table_data$year)                                       #getting .00 after year, fixed
     table_data <- unique(table_data)                                                     #states showing more then once, so made only unique vars show
     table_data <- head(table_data, 10)                                                    #Make it a top 10 list
     table_data <- table_data %>% mutate(Ranking = as.integer(rank(-table_data$Cost_of_living_Gap) ) )   #Show a Rank column to look nice
  
  
  
   })
  
  
  
   data <- reactive({
   
   
     
     
     if(input$all == TRUE) #This is the same data as the renderTable above be is ALL 50 States
   {
       table_data <- plot_data<-acs_data2 %>% 
         as.data.frame()%>% 
         select(NAME, Cost_of_living_Gap, year) %>% filter(year==input$datasetYR)
       table_data$Cost_of_living_Gap <- abs(table_data$Cost_of_living_Gap)
       table_data <- table_data[order(table_data$Cost_of_living_Gap, decreasing = TRUE),]
       table_data$year <- as.integer(table_data$year)
       table_data <- unique(table_data)
       table_data <- table_data %>% mutate(Ranking = as.integer(rank(-table_data$Cost_of_living_Gap) ) )
     }
     else
     {
       #This is a 1to1 of the renderTable function above, but to input into a file.This is the top 10 Ranking
       table_data <- plot_data<-acs_data2 %>% 
         as.data.frame()%>% 
         select(NAME, Cost_of_living_Gap, year) %>% filter(year==input$datasetYR)
       table_data$Cost_of_living_Gap <- abs(table_data$Cost_of_living_Gap)
       table_data <- table_data[order(table_data$Cost_of_living_Gap, decreasing = TRUE),]
       table_data$year <- as.integer(table_data$year)
       table_data <- unique(table_data)
       table_data <- head(table_data, 10)
       table_data <- table_data %>% mutate(Ranking = as.integer(rank(-table_data$Cost_of_living_Gap) ) )
       }
  })
  
  
  #download function
  output$download <- downloadHandler(
    filename = function() {
      paste('CensusRankingACCESSED-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      vroom::vroom_write(data(), file)
    }
  )
  
  
  
  
}





runApp(shinyApp(ui, server), launch.browser = TRUE)


