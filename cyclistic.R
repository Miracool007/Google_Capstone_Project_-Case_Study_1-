#Loading the packages
library(tidyverse)
library(data.table)

#importing the datasets
capstone_1 <- read.csv("Capstone Data 202101.csv")
capstone_2 <- read.csv("Capstone Data 202102.csv")
capstone_3 <- read.csv("Capstone Data 202103.csv")

#merging the datasets
cyclistic <- rbind(capstone_1, capstone_2, capstone_3)

View(cyclistic)

#removing null values
cyclistic <- cyclistic%>%
  distinct()%>%
  drop_na()

#creating a column to check the difference in time
cyclistic$ride_length <- difftime(cyclistic$ended_at, cyclistic$started_at, 
                                  units = "mins")

cyclistic$date <- as.Date(cyclistic$started_at) #converting the column to a date data-type

cyclistic$day_of_week <- wday(cyclistic$started_at) #creating a column for days of the week

cyclistic$day_of_week <- format(as.Date(cyclistic$date), "%A")#imputing the days of the week

cyclistic$month <- format(as.Date(cyclistic$date), "%m")#creating a column for month

cyclistic$day <- format(as.Date(cyclistic$date), "%d") #creating a column for day

cyclistic$year <- format(as.Date(cyclistic$date), "%Y") #creating a column for year

cyclistic$time <- format(as.Date(cyclistic$date), "%H:%M:%S") #creating a column for time

#imputing the months
cyclistic <- cyclistic%>%
  mutate(months = case_when(month == "01" ~ "January",
                            month == "02" ~ "February",
                            month == "03" ~ "March"))

cyclistic <- na.omit(cyclistic)#removing null values
cyclistic <- distinct(cyclistic)#removing duplicate values

#removing values from the ride_length column that have decimal values
cyclistic <- cyclistic[!(cyclistic$ride_length <=0),]

#deleting columns that aren't needed
cyclistic <- cyclistic%>%
  select(-c(ride_id,start_station_id, end_station_id, start_lat, 
            end_lat, start_lng, end_lng))

#rounding the ride_length column to one decimal place
cyclistic$ride_length <- round(cyclistic$ride_length, digits = 1)

#removing columns that aren't needed
cyclistic <- cyclistic%>%
  select(-c(started_at, ended_at, start_station_name, end_station_name))

#exporting dataset for visualisation
write.csv(cyclistic, "cyclistic.csv")

