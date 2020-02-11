library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
library(lubridate)
library(stringr)


data = read.csv("proj_1_raw.csv", header=TRUE, dec=',')

str(data)

# Making the longitude and latitude usable
data$lat <- as.numeric(as.character(data$lat))
data$lon <- as.numeric(as.character(data$lon))
data$tags <- as.character(data$tags)

# convert usernames from litterati -> user(number)
data$username <- gsub("litterati", "user", data$username)

# changing blank tags into "untagged"
data$tags[nchar(data$tags) == 0] <- "untagged"

# changing the values into cleaner times
data$litterTimestamp <- as.character(data$litterTimestamp) # convert to char
dt_utc <- mdy_hm(data$litterTimestamp)
data$new_date <- as_date(dt_utc)
# day of the week
data$dotw <- wday(data$new_date, label=TRUE, abbr=FALSE)
data.dotw <- as.data.frame(table(data$dotw))

# hour
data$hour <- hour(dt_utc)
data.hour <- as.data.frame(table(data$hour))
order.hour <- order(data.hour$Freq, decreasing=TRUE)
data.hour <- data.hour[order.hour, ]
colnames(data.hour) <- c("Hour", "Count")
data.hour$Hour <- as.numeric(data.hour$Hour)
# adding the Hours with zero litter picked up
zero_hours <- c(6,8,9,11,12)
zero_hours.df <- data.frame(Hour = zero_hours, Count = 0)
data.hour <- rbind(data.hour, zero_hours.df)

# Only points within Forest Park area
sub <- subset(data, lat > 41.85589 & lat < 41.889 & lon < -87.8 & lon > -87.8398)
data <- sub


username_table <- table(sort(data$username))
sort(username_table, decreasing=TRUE)
# only keep values within:
#     Lat: (41, 42)
#     Lon: (-87, -88)
# target number of values is: 11913 as opposed to 12646
# data[['full_tags']]
nrow(data)


# for the top 10 usernames
# for the table
data.username <- as.data.frame(table(data$username))
order.username <- order(data.username$Freq,  decreasing=TRUE)
data.username <- data.username[order.username, ]
colnames(data.username) <- c("Usernames", "Count")


##  cleaning up the tags
##  parsing for each tag
##  without this bootleg way, the table was spitting out garbage with freq_tags
# uniq_tags <- unique(data$tags)
# uniq_list <- as.list(uniq_tags)
# uniq_string <- paste(uniq_list, collapse=' ')
# uniq_string <- gsub(" ", ",", uniq_string)
# uniq_v <- strsplit(uniq_string, ",")
# # converting tags into dataframe
# freq_tags <- as.data.frame(table(uniq_v))
# order.tag <- order(freq_tags$Freq, decreasing=TRUE)
# freq_tags <- freq_tags[order.tag, ]
# colnames(freq_tags) <- c("Tag", "Count")
# write.csv(freq_tags, "tag.csv")
data.tag = read.csv("tag.csv", header=TRUE, dec=",")
data.tag$X <- NULL
order.tag <- order(data.tag$Count, decreasing=TRUE)
data.tag <- data.tag[order.tag, ]

# for the days of the week
# bar chart
data.dotw <- as.data.frame(table(data$dotw))
colnames(data.dotw) <- c("dotw", "Count")

# time of day (tod)
data$tod[data$hour >= 0 & data$hour <= 6] <- "night"
data$tod[data$hour >= 7 & data$hour <= 12] <- "morning"
data$tod[data$hour >= 13 & data$hour <= 18] <- "afternoon"
data$tod[data$hour >= 19 & data$hour <= 24] <- "evening"

# getting month
data$month <- month(data$new_date)

# Defining the menu items
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "CS 424 - Project 1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      menuItem("", tabName="Empty", icon=NULL),
      
      selectInput("Username", "Select a user:", data.username[,1], selected="julieta"),
      selectInput("Tag", "Select a tag:", data.tag[, 1], selected = 'plastic'),
      selectInput("tod", "Select time of day:", c('night', 'morning', 'afternoon', 'evening'), selected="afternoon"),
      selectInput("month", "Select month:",  c(1:12), selected=9)
      )
    
  ),
  dashboardBody(
    fluidRow(
      column(2, 
             fluidPage(
               box(title="Total count",  solidHeader=TRUE, status="primary", width=12,
                   textOutput("text"))
             ),
             fluidRow(
               box(title="Top 10 Pickers", solidHeader=TRUE, status="primary", width=12,
                   dataTableOutput("topTenTable", height=100))
             ),
             fluidRow(
               box(title="Top 10 Tags", solidHeader = TRUE, status="primary", width=12,
                   dataTableOutput("topTenTags", height=100))
             )
        ),
      column(6,
             fluidRow(
               box(title="Litter by type", solidHeader=TRUE, status="primary", width=12, 
                   plotOutput("histTag", height=200))
             ),
             fluidRow(
               box(title="Litter Collection by Hour", solidHeader=TRUE, status="primary", width=12,
                   plotOutput("histHour", height=200))
             ),
             fluidRow(
               box(title="Litter by Calendar Day", solidHeader=TRUE, status="primary", width=12,
                   plotOutput("histDay", height=300))
             )
      ),
      column(4,
             fluidRow(
               box(title="About", solidHeader=TRUE,  status="primary", width=12,
                   htmlOutput("about", height=200))
             ),
             fluidRow(
               box(title="Litter by Day of the Week", solidHeader=TRUE, status="primary", width=12,
                   plotOutput("hist_dotw", height=200))
             ),
             fluidRow(
               box(title="Map", solidHeader = TRUE, status="primary", width=12, 
                   leafletOutput("map", height=400)
               )
             )
      )
    ),
    
  )
)


server <- function(input, output) { 
  # tagReactive <- reactive({subset(data, data[data$tags %in% c(input$Tag), ])})
  tagReactive <- reactive({subset(data, str_detect(data$tags, input$Tag))})
  # temp.usernames[str_detect(temp.usernames, 'litterati')]
  usernameReactive <- reactive({subset(data, data$username == input$Username)})
  combinedReactive <- reactive({subset(data, str_detect(data$tags, input$Tag) & data$username == input$Username)})
  allCombinedReactive <- reactive({subset(data, str_detect(data$tags, input$Tag) & data$username == input$Username & data$tod == input$tod & data$month == input$month)})
  
  output$about <- renderUI({
    str0 <- paste("- Dashboard made by Brandon Graver.")
    str1 <- paste("- Libraries used: Shiny, Shinydashboard, leaflet, DT, ggplot2, lubridate, stringr.")
    str2 <- paste("- Data comes from https://www.litterati.org/.")
    HTML(paste(str0, str1, str2, sep='<br>'))
  })
  
  output$map <- renderLeaflet(
    {
      # 
      map_df <- allCombinedReactive()
      leaflet(data=map_df, options=leafletOptions()) %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions(),
                   lng=map_df$lon,
                   lat=map_df$lat,
                   popup=paste("Picked up: ", map_df$tags, "<br>",
                               "Username: ", map_df$username, "<br>",
                               "ID: ", map_df$user_id, "<br>",
                               "Date:", map_df$new_date, "<br>",
                               "Time:", map_df$new_time, "<br>",
                               "(Lat, Lon): (", map_df$lat, ", ", map_df$lon, ")")
        ) %>%
        setView(lng=-87.81, lat=41.87, zoom=11)
      # median: lat = 41.87, lon = -87.81
      
    }
  )
  
  output$text <- renderText({
    text_df <- allCombinedReactive()
    paste(NROW(text_df), " pieces of garbage")
  })
  
  output$topTenTable <- DT::renderDataTable(
    DT::datatable(
      {
        data.username
      },
      options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
    )
  )
  
  output$topTenTags <- DT::renderDataTable(
    DT::datatable(
      {
        # freq_tags
        data.tag 
      },
      options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
    )
  )
  
  # output$summary <- DT::renderDataTable(
  #   DT::datatable(
  #     {
  #       
  #     }
  #     options = list(searching=FALSE, pageLength=10, lengthChange=FALSE,  rownames=FALSE)
  #   )
  # )
  
  output$histDay <- renderPlot({
    temp <- combinedReactive()
    temp.date <- as.data.frame(table(temp$new_date))
    colnames(temp.date) <- c("Day", "Count")
    ggplot(temp.date, aes(x=Day, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Tags", y="Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
  output$hist_dotw <- renderPlot({
    # data.dotw
    temp <- combinedReactive()
    temp.dotw <- as.data.frame(table(temp$dotw))
    colnames(temp.dotw) <- c("dotw", "Count")
    # ggplot(data.dotw, aes(x=dotw, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Days of the Week", y="Count")
    ggplot(temp.dotw, aes(x=dotw, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Days of the Week", y="Count")
  })
  
  output$histTag <- renderPlot({
    # data.tag <- data.tag[1:10,]
    # temp.tag <- combinedReactive()
    # temp.tag <- temp.tag[1:10, ]
    temp.tag <- usernameReactive()
    
    uniq_tags <- unique(temp.tag$tags)
    uniq_list <- as.list(uniq_tags)
    uniq_string <- paste(uniq_list, collapse=' ')
    uniq_string <- gsub(" ", ",", uniq_string)
    uniq_v <- strsplit(uniq_string, ",")
    # converting tags into dataframe
    freq_tags <- as.data.frame(table(uniq_v))
    order.tag <- order(freq_tags$Freq, decreasing=TRUE)
    freq_tags <- freq_tags[order.tag, ]
    colnames(freq_tags) <- c("Tag", "Count")
    as.character(freq_tags[1])
    temp.tag <- freq_tags[1:10, ]
    
    
    ggplot(temp.tag, aes(x=Tag, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Tags", y="Count") 
  })
  
  output$histHour <- renderPlot({
    # data.hour
    temp <- combinedReactive()
    temp.hour <- as.data.frame(table(temp$hour))
    colnames(temp.hour) <- c("Hour", "Count")
    # zero_hours <- c(6,8,9,11,12)
    # zero_hours.df <- data.frame(Hour = zero_hours, Count = 0)
    # temp.hour <- rbind(temp.hour, zero_hours.df)
    
    # ggplot(data.hour, aes(x=Hour, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Hour", y="Count") + ylim(0,2000)
    ggplot(temp.hour, aes(x=Hour, y=Count)) + geom_bar(stat="identity", fill="#0072B2") + labs(x="Hour", y="Count") + ylim(0,2000)  })
}

shinyApp(ui, server)