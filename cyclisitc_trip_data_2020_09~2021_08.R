#Clearing objects from the workspace
rm(list=ls())

#Loading libraries
library(readxl)
library(tidyverse)
library(skimr)
library(chron)
library(lubridate)
library(dplyr)
library(scales)

#Preparing the data - loading excel files
data_202009 <- read_excel("processed_data_xls/trip_data_2020_09.xlsx")

data_202010 <- read_excel("processed_data_xls/trip_data_2020_10.xlsx")

data_202011 <- read_excel("processed_data_xls/trip_data_2020_11.xlsx")

data_202012 <- read_excel("processed_data_xls/trip_data_2020_12.xlsx")

data_202101 <- read_excel("processed_data_xls/trip_data_2021_01.xlsx")

data_202102 <- read_excel("processed_data_xls/trip_data_2021_02.xlsx")

data_202103 <- read_excel("processed_data_xls/trip_data_2021_03.xlsx")

data_202104 <- read_excel("processed_data_xls/trip_data_2021_04.xlsx")

data_202105 <- read_excel("processed_data_xls/trip_data_2021_05.xlsx")

data_202106 <- read_excel("processed_data_xls/trip_data_2021_06.xlsx")

data_202107 <- read_excel("processed_data_xls/trip_data_2021_07.xlsx")

data_202108 <- read_excel("processed_data_xls/trip_data_2021_08.xlsx")

#Combining all data into one dataset
data_all <- rbind(data_202009, data_202010, data_202011, data_202012, data_202101, data_202102,
                  data_202103, data_202104, data_202105, data_202106, data_202107, data_202108)

#Producing a csv file for the combined data
write.csv(data_all, "combined_data_trips_202009_to_202108.csv")

#Reading the csv file 
data_all_months <- read.csv("combined_data_trips_202009_to_202108.csv")

#
skim(data_all_months)
head(data_all_months)
glimpse(data_all_months)
as_tibble(data_all_months)

#Cleaning and Manipulation
##Turning the datetime format of ride length into only time 
data_all_months$ride_length <- format(as.POSIXct(data_all_months$ride_length), format = "%H:%M:%S")

##Explore - 
less_than_five <- sum(hms(data_all_months$ride_length) <  hms("00:00:05"), na.rm=TRUE)

less_than_five / nrow(data_all_months)

#####0.2% of observations are less than 5 sec###

##Filter observations
data_all_months <- filter(data_all_months, hms(ride_length) > hms("00:00:05"))

#*data_all_months$ride_length2 <- chron(times = data_all_months$ride_length)

#The number of members and casual users

n <- data_all_months %>% 
  count(member_casual)

class(n$n)

n <- mutate(n, perc = n/sum(n))
n <- mutate(n, percentage = percent(perc))

ggplot(n, aes(x="", y=perc, fill=member_casual)) +
  geom_col() +
  geom_text(aes(label = percentage), position = position_stack(vjust= 0.5), size = 13, color = "black") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = NULL,title = "Proportion of the Number of Casual Users and Members") +
  theme_void(base_size = 13) +
  theme(title = element_text(size=14), legend.text= element_text(size=14), plot.title = element_text(hjust = 0.5, color = "#666666"))


#The average of ride length 
mean(times(data_all_months$ride_length))

mean_lng <- data_all_months %>% 
  group_by(member_casual) %>% 
  summarise(mean_ride_length = mean(times(ride_length)))


mean_lng <- mutate(mean_lng, period_to_seconds(hms(mean_ride_length))) 

names(mean_lng)[names(mean_lng) == 'period_to_seconds(hms(mean_ride_length))'] <- "in_sec"


ggplot(mean_lng,aes(x=member_casual, y=in_sec, fill=member_casual))+
  geom_col(position="dodge") +
  labs(x=NULL, y= "mean ride length (seconds)", fill=NULL, title = "Mean Ride Length: Casual vs Member") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), title = element_text(size=14), legend.text= element_text(size=14), plot.title = element_text(hjust = 0.5, color = "#666666"))


#



#Average ride length by members and casual users, by the day of the week
mean_lng_m <- data_all_months %>% 
  group_by(member_casual,day_of_the_week) %>% 
  summarise(mean_ride_length = mean(times(ride_length))) 


mean_lng_m <- mutate(mean_lng_m, period_to_seconds(hms(mean_ride_length))) 

names(mean_lng_m)[names(mean_lng_m) == 'period_to_seconds(hms(mean_ride_length))'] <- "in_sec"


ggplot(data=mean_lng_m, aes(fill=member_casual)) +
  geom_col(mapping = aes(x=day_of_the_week, y=in_sec), position = "dodge")

#Plotting graphs 





#Plotting the number of member and casual by the day of the week
ggplot(data=data_all_months) +
  geom_bar(mapping = aes(x=day_of_the_week, fill=member_casual),  position = "dodge")

