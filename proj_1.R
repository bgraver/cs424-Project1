library(leaflet)
library(shiny)

# runExample("01_hello")

# setwd('School Folders/cs424/project_1')
# dir()

data = read.csv("proj_1_raw.csv", header=TRUE, dec=',')

str(data)

data$lat <- as.numeric(as.character(data$lat))
data$lon <- as.numeric(as.character(data$lon))

# data <- data[complete.cases(data[,5]), ]
# data <- data[rowSums(is.na(data[, 5:6])) == 0, ]

str(data)

summary(data[,5:6])

# median: lat = 41.87, lon = -87.81
# 