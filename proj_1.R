library(leaflet)
library(shiny)
library(lubridate)
library(DT)
library(ggplot2)
library(stringr)

# runExample("01_hello")
# setwd('School Folders/cs424/project_1')
# dir()

# write.csv(data, "fin_data.csv")


data = read.csv("proj_1_raw.csv", header=TRUE, dec=',')
# data.tag = read.csv("tag.csv", header=TRUE, dec=",")


str(data)

data$lat <- as.numeric(as.character(data$lat))
data$lon <- as.numeric(as.character(data$lon))
data$tags <- as.character(data$tags)
gsub("litterati", "user", temp.usernames)


# convert usernames from litterati -> user(number)
# # freq_tags[freq_tags$Tag %in% c("paper"), ]
# temp.usernames <- data$username
# temp.usernames <- as.character(temp.usernames)
# # temp.usernames[str_detect(temp.usernames, 'litterati')]
# # gsub("litterati", "user", temp.usernames)


# data$tags <- sub(is.empty, "untagged", data$tags) # convert to char
data$tags[nchar(data$tags) == 0] <- "untagged"

data$litterTimestamp <- as.character(data$litterTimestamp) # convert to char

# str(data)
# new_time <- strsplit(data$litterTimeStamp, " ")
# all time changes
data$new_time <- as.character(0) 
data$new_date <- as.character(0)

dt_utc <- mdy_hm(data$litterTimestamp)
data$new_date <- as_date(dt_utc)

# dotw = day of the week
data$dotw <- wday(data$new_date, label=TRUE, abbr=FALSE)
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

sub <- subset(data, lat > 41.85589 & lat < 41.889 & lon < -87.814 & lon > -87.839)
str(sub)
nrow(sub)

data.username <- as.data.frame(table(data$username))
order.username <- order(data.username$Freq,  decreasing=TRUE)
data.username <- data.username[order.username, ]
colnames(data.username) <- c("Usernames", "Count")
data.username

# ggplot2 bar chart by weekday
data.dotw <- as.data.frame(table(data$dotw))
# order.dotw <- order(data.dotw$Freq, decreasing=TRUE)
# data.dotw <- data.dotw[order.dotw, ]
colnames(data.dotw) <- c("dotw", "Count")
data.dotw

ggplot(data, aes(x=dotw, y="Count")) + geom_bar(stat="identity")
# g <- ggplot(data, aes(dotw))
# g + geom_bar(fill="#0072B2")

# cleaning up the tags
# parsing for each tag
uniq_tags <- unique(data$tags)
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
head(freq_tags)
write.csv(freq_tags, "tag.csv")

data.tag = as.data.frame(read.csv("tag.csv", header=TRUE, dec=","))
data.tag$X <- NULL
# str_detect(uniq_tags[[1000]], "piece")
# str_detect(uniq_tags[[1000]], "untagged")

# freq_tags[freq_tags$Tag %in% c("paper"), ]
# ggplot2 for unique tags
ggplot(data.tag, aes(x=Tag, y=Count)) + geom_bar(stat="identity")

# data[data$username %in% c("julieta"), ]
# head(data[data$tags %in% c("plastic"), ])


# chart by day 
data.date <- as.data.frame(table(data$new_date))
colnames(data.date) <- c("Day", "Count")
ggplot(data.date, aes(x=Day, y=Count)) + geom_bar(stat="identity")


# time of day (tod)
data$tod[data$hour >= 0 & data$hour <= 6] <- "night"
data$tod[data$hour >= 7 & data$hour <= 12] <- "morning"
data$tod[data$hour >= 13 & data$hour <= 18] <- "afternoon"
data$tod[data$hour >= 19 & data$hour <= 24] <- "evening"

# getting month
data$month <- month(data$new_date)