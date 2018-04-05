library(shiny)
library(readr)
library(ggplot2)
library(leaflet)
library(dplyr)
library(magrittr)
locations <- read_csv("data/locations_with_index.csv")
fore <- readRDS("data/forecast1.rds")
hist <- read_csv("data/historical1.csv")
qpal <- colorQuantile("Reds", locations$index, n = 3)


ui <- fluidPage(  #Define UI for application
   titlePanel("Happiness Index and Weather Forecasts in U.S. Cities"),
   sidebarPanel(
     selectInput("city","Select City:",choices = sort(locations$city)),
     selectInput("var","Select Variable:",choices = c("Minimum Temperature (F)","Maximum Temperature (F)","Precipitation (in.)")),
     checkboxGroupInput("days","Days Between Forecast and Recorded Value",
                        choices = c("0 days","1", "2","3","4","5"))
     ),
   mainPanel(
     tabsetPanel(type="tabs",
                 tabPanel("Visualizations"
                 ),
                 tabPanel("US Map",
                  leafletOutput("USmap")
                 ),
                 tabPanel("Correlation")
     )
   )
)

server <- function(input, output) {  #Define server for application
    output$USmap <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(lat = locations$latitude,lng = locations$longitude,
                         popup = paste0("<b>",locations$city,", ",locations$state,"</b>","</br>",
                          "Happ. Index: ",locations$index,"</br>",
                          "Happ. Ranking: ",locations$X1,"</br>",
                          "Num. of Tweets: ", locations$tweets),
                         stroke=F,radius=5,color = qpal(locations$index),
                         fillOpacity = 0.75) %>%
        addLegend(colors = unique(qpal(locations$index)),labels=quantile(locations$index)[c(1,3,5)])
    })
}

shinyApp(ui = ui, server = server) #runs the application 

