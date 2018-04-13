#fix rank to ordered locations
#add note about size (log Hi)

library(shiny)
library(readr)
library(ggplot2)
library(leaflet)
library(dplyr)
library(magrittr)
locations <- read_csv("data/locations_with_index.csv"); locations <- locations[,-c(1)]
fore <- readRDS("data/forecast1.rds")
hist <- read_csv("data/historical1.csv")
mns <- read_csv("data/variable_means.csv")
qpal <- colorQuantile("viridis", locations$index, n = 3)


ui <- fluidPage(  #Define UI for application
   titlePanel("Happiness Index and Weather Forecasts in U.S. Cities"),
   sidebarPanel(
     selectInput("city","Select City:",choices = sort(paste(locations$city,", ",locations$state,sep=''))),
     selectInput("var","Select Variable:",choices = c("Minimum Temperature (F)","Maximum Temperature (F)"
                                                      #,"Precipitation (in.)"
                                                      )),
     radioButtons("days","Days Between Forecast and Recorded Value",
                        choices = c(0,1,2,3,4,5))
     ),
   mainPanel(
     tabsetPanel(type="tabs",
                 tabPanel("Visualizations",
                  plotOutput("histVals"),
                  plotOutput("maxhist"),
                  plotOutput("hhist")
                 ),
                 tabPanel("US Map",
                  leafletOutput("USmap")
                 ),
                 tabPanel("Correlation")
     )
   )
)

server <- function(input, output) {  #Define server for application
    subsetHist <- reactive({
      l <- locations[which(stringr::str_detect(input$city,locations$city)),]
      s <- state.abb[which(state.name == substr(input$city,grepRaw(",",input$city)+2,nchar(input$city)))]
      l <- l[which(stringr::str_detect(s,l$state_abb)),]
      hist[which(hist$AirPtCd == l$AirPtCd),]
    })

    subsetFore <- reactive({
      if(input$var == "Minimum Temperature (F)"){
        v <- "Min_TemperatureF"} else if(input$var == "Maximum Temperature (F)"){
          v <- "Max_TemperatureF"} 
      # else{
      #       v <- "PrecipitationIn"
      #     }
      l <- locations[which(stringr::str_detect(input$city,locations$city)),]
      s <- state.abb[which(state.name == substr(input$city,grepRaw(",",input$city)+2,nchar(input$city)))]
      l <- l[which(stringr::str_detect(s,l$state_abb)),]
      f <- fore[which(fore$airport_code == l$AirPtCd),]
      # f <- subset(fore,fore$airport_code == locations[which(stringr::str_detect(input$city,locations$city) & stringr::str_detect(input$city,locations$state_abb)),]$AirPtCd)
      f <- subset(f,f$delta_t == as.numeric(input$days) & f$forecast_variable == v)
      f
    })

    output$histVals <- renderPlot({
      s <- subsetHist()
      f <- subsetFore()
      plot(0:10,0:10,main=nrow(s))
      plot(as.Date(s$Date),unlist(s[,v]),xaxt='n',xlab="Historical Date", pch=16,
           ylab=paste0("Historical Value of ", input$var),main=paste0("Accuracy of ",input$var, " Forecasts for ",input$city))
      par(new=T)
      plot(as.Date(f$predicted_date),f$predicted_value,col="coral",xlab='',ylab='',yaxt='n',type="l",lwd=3)
    })
  
    output$USmap <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(lat = locations$latitude,lng = locations$longitude,
                         popup = paste0("<b>",locations$city,", ",locations$state,"</b>","</br>",
                          "Happ. Index: ",locations$index,"</br>",
                          "Happ. Ranking: ",row.names(locations),"</br>",
                          "Num. of Tweets: ", locations$tweets),
                         label = paste0(locations$city,", ",locations$state),
                         stroke=F,radius=log(locations$tweets),color = qpal(locations$index),
                         fillOpacity = 0.75) %>%
        addLegend(title = "Happiness Index",colors = unique(qpal(locations$index)),labels=quantile(locations$index)[c(5,3,1)])
    })
    
    output$hhist <- renderPlot({
      hist(locations$index,breaks=20,col="lightgoldenrodyellow",xlim=c(5.8,6.21),
           freq=F,ylab="Number of Cities",xlab="Happiness Index",
           main="Distribution of Cities' Happiness Index")
      abline(v=locations[which(stringr::str_detect(input$city,locations$city) & stringr::str_detect(input$city,locations$state)),]$index,col="firebrick",lwd=5)
      text(x = locations[which(stringr::str_detect(input$city,locations$city) & stringr::str_detect(input$city,locations$state)),]$index,
           y = 6,input$city,pos=4,col = "firebrick",cex=0.8)
    })
    
    output$maxhist <- renderPlot({
      hist(unlist(mns[,input$var]),breaks=20,col="lightblue",
           xlab=paste0("Mean ",input$var),main=paste0("Distribution of Mean ",input$var))
      m <- mns[which(locations[which(stringr::str_detect(input$city,locations$city) & stringr::str_detect(input$city,locations$state)),]$AirPtCd == mns$AirPtCd),input$var]
      abline(v=m,col="firebrick",lwd=5)
      text(x = m,y = 10,input$city,pos=4,col = "firebrick",cex=0.8)
    })
}

shinyApp(ui = ui, server = server) #runs the application 

