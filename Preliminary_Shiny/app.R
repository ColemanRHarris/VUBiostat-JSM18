library(shiny)
library(readr)
library(ggplot2)
locations <- read_csv("data/locations.csv")
fore <- readRDS("data/forecast1.rds")
hist <- read_csv("data/historical1.csv")


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
                 tabPanel("US Map"
                 ),
                 tabPanel("Visualizations"
                 ),
                 tabPanel("Correlation")
     )
   )
)

server <- function(input, output) {  #Define server for application

}

shinyApp(ui = ui, server = server) #runs the application 

