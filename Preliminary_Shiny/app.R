library(shiny)
library(readr)
library(ggplot2)
locations <- read_csv("data/locations.csv")
fore <- readRDS("data/forecast1.rds")
hist <- read_csv("data/historical1.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Imputation"),
   sidebarPanel(selectInput("city","Select City:",choices = locations$city)),
   mainPanel(plotOutput("plot1"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$plot1 <- renderPlot({
    sub <- hist[which(hist$AirPtCd == locations[which(locations$city==input$city),]$AirPtCd),]
    if(nrow(sub[which(sub$PrecipitationIn == "T"),])==0){
      ggplot(data=sub,aes(x=Date,y=PrecipitationIn)) + geom_point() + 
        scale_y_discrete(breaks=NULL)
    }
    else{
      sub[which(sub$PrecipitationIn == "T"),'Col'] <- "Trace"
      sub[which(sub$PrecipitationIn != "T"),'Col'] <- "Measured"
      sub[which(sub$PrecipitationIn == "T"),]$PrecipitationIn <- 0.5
      ggplot(data=sub,aes(x=Date,y=PrecipitationIn)) + geom_point(aes(color=Col)) + 
        scale_y_discrete(breaks=NULL)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

