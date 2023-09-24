# Data Analysis Using Uber Dataset 

library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)
library(scales)
library(readr)
library(ggmap)



apr = read.csv("Uber_Data/uber-raw-data-apr14.csv")
may = read.csv("Uber_Data/uber-raw-data-may14.csv")
jun = read.csv("Uber_Data/uber-raw-data-jun14.csv")
jul = read.csv("Uber_Data/uber-raw-data-jul14.csv")
aug = read.csv("Uber_Data/uber-raw-data-aug14.csv")
sep = read.csv("Uber_Data/uber-raw-data-sep14.csv")
jan_june = read.csv("Uber_Data/uber-raw-data-janjune-15.csv")



uber_data <- rbind(apr,may,jun,jul,aug,sep)

#Binding Jan_June to Uber Data



dim(uber_data)
head(uber_data)

# Convert to Standard R Format And Create Time Column 
uber_data$Date.Time <- as.POSIXct(uber_data$Date.Time, format="%m/%d/%Y %H:%M:%S")
uber_data$Time <- format(uber_data$Date.Time, format="%H:%M:%S")


uber_data$dayofweek <- factor(wday(uber_data$Date.Time, label = TRUE))
uber_data$day <- factor(day(uber_data$Date.Time))
uber_data$month <- factor(month(uber_data$Date.Time, label=TRUE))
uber_data$year <- factor(year(uber_data$Date.Time))


uber_data$hour <- factor(hour(hms(uber_data$Time)))
uber_data$minute <- factor(minute(hms(uber_data$Time)))
uber_data$second <- factor(second(hms(uber_data$Time)))



#Plotting Trips Per Hour
hourly <- uber_data %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 

  
# Uber Trips Per Hour
ggplot(hourly, aes(hour, Total)) +
  geom_bar(stat = "identity",
           color = '#0000FF',
           fill =  '#ADD8E6') +
  ggtitle("Uber Trips every Hour", subtitle = "April 2014 to September 2014") +
  theme(legend.position = 'none') +
  scale_y_continuous(labels = scales::comma)

# Uber Trips Per Hour w/ Months
colors = c("#e84d79",'#f45a65','#fa6c51','#f8813e','#f0972b','#e2ad1e')

month_hour <- uber_data %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Uber Trips per Hour with Months",
          subtitle = "April 2014 - Sep 2014") +
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = colors)
  


#Group by Months

month_colors = c("#CBC3E3","#ADD8E6","#ffff00","#ffa500","#98FB98","#FF6961")
  
month_group <- uber_data %>%
  group_by(month) %>%
  dplyr::summarize(Total = n())


ggplot(month_group , aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = month_colors)


#Day of the Week
day_month_data <- uber_data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())

ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) +
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") +
  ggtitle("Trips by Day & Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)")


