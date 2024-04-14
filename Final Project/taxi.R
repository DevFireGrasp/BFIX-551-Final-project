library(shiny)
library(dplyr)
library(ggplot2)

taxi <- readr::read_csv("Taxi Datset.csv")
taxi$payment_type = as.character(taxi$payment_type)
taxi_s= taxi%>%
  select(trip_distance,fare_amount,tip_amount)


UI= fluidPage(
  sidebarLayout(sidebarPanel(
    varSelectInput("xvar", "X variable", taxi_s, selected = "trip_distance"),
    varSelectInput("yvar", "Y variable", taxi_s, selected = "fare_amount"),
    checkboxGroupInput(
      "payment_type", "Filter by payment type",
      choices = unique(taxi$payment_type), 
      selected = unique(taxi$payment_type)
    ),
    hr(),
    checkboxInput("by_payment_type", "Show payment type", TRUE)
  ),
  mainPanel(plotOutput("scatter")))
)

server = function(input, output, session){
  data = reactive({
    req(input$payment_type)
    taxi |> filter(payment_type %in% input$payment_type)
  })
  
  
  output$scatter= renderPlot({
    plt = ggplot(data(),aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_payment_type) aes(color = payment_type),
      geom_point()
    ) 
    plt
  }, res = 100)
}

shinyApp(UI, server)
