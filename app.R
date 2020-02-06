library(shiny)
library(shinydashboard)
library(leaflet)

data = read.csv("proj_1_raw.csv", header=TRUE, dec=',')

str(data)

data$lat <- as.numeric(as.character(data$lat))
data$lon <- as.numeric(as.character(data$lon))

ui <- dashboardPage(
  dashboardHeader(title = "CS 424 - Project 1"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE
                   
  ),
  dashboardBody(
    leafletOutput("map")
    
  )
)


server <- function(input, output) { 
  output$map <- renderLeaflet(
    {
      
      leaflet(data = data[1,], options=leafletOptions(preferCanvas = TRUE)) %>% 
        addTiles(options=tileOptions(
          updateWhenIdle=FALSE
        )) %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   lng=data$lon,
                   lat=data$lat,
                   popup=paste("Picked up: ", data$tags)) %>%
        # addCircleMarkers(radius= 2,
        #                  opacity = 100,
        #                  lng=data$lon,
        #                  lat=data$lat) %>%
        # 
        # addMarkers(lng=-87.81282, lat=41.86555) %>%
        setView(lng=-87.81, lat=41.87, zoom=10)
        # median: lat = 41.87, lon = -87.81
    }
  )
  }

shinyApp(ui, server)