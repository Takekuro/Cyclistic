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

#Turning the datetime format of ride length into only time 
data_all_months$ride_length <- format(as.POSIXct(data_all_months$ride_length), format = "%H:%M:%S")

#Explore - 
less_than_five <- sum(hms(data_all_months$ride_length) <  hms("00:00:05"), na.rm=TRUE)

less_than_five / nrow(data_all_months)

##0.2% of observations are less than 5 sec##

#Filter observations
data_all_months <- filter(data_all_months, hms(ride_length) > hms("00:00:05"))

#The number of members and casual users

n <- data_all_months %>% 
  count(member_casual)

n <- mutate(n, prop = n/sum(n))
n <- mutate(n, percentage = percent(prop))

ggplot(n, aes(x="", y=prop, fill=member_casual)) +
  geom_col() +
  geom_text(aes(label = percentage), position = position_stack(vjust= 0.5), size = 13, color = "black") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = NULL,title = "Percentage of the Number of Casual Users and Members") +
  theme_void(base_size = 13) +
  theme(title = element_text(size=13), legend.text= element_text(size=14), plot.title = element_text(hjust = 0.5, color = "#666666"))

#The average of ride length 
mean(times(data_all_months$ride_length))

mean_lng <- data_all_months %>% 
  group_by(member_casual) %>% 
  summarise(mean_ride_length = mean(times(ride_length)))


mean_lng <- mutate(mean_lng, in_sec = period_to_seconds(hms(mean_ride_length))) 

(2680000-2221000)/2221000

ggplot(mean_lng,aes(x=member_casual, y=in_sec, fill=member_casual))+
  geom_col(position="dodge") +
  labs(x=NULL, y= "mean ride length (seconds)", fill=NULL, title = "Mean Ride Length: Casual vs Member") +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), title = element_text(size=14), legend.text= element_text(size=14), plot.title = element_text(hjust = 0.5, color = "#666666"))

#median
median(times(data_all_months$ride_length))

median_lng <- data_all_months %>% 
  group_by(member_casual) %>% 
  summarise(median_ride_lng = median(times(ride_length)))

median_lng <- mutate(median_lng, in_sec = period_to_seconds(hms(median_ride_lng))) 

#Freqpoly Count of ride length

data_all_months <- mutate(data_all_months, in_sec = period_to_seconds(hms(ride_length))) 

ggplot(data_all_months, aes(in_sec, color = member_casual)) +
  geom_freqpoly(binwidth = 50, size = 1) +
  xlim(0, 10000) +
  labs(x = "ride length", y = "count", title = "Total Count of Ride Length of All Rides", 
       color = NULL, fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))


#Average ride length by members and casual users, by the day of the week
mean_lng_d <- data_all_months %>% 
  group_by(member_casual,day_of_the_week) %>% 
  summarise(mean_ride_length = mean(times(ride_length))) 


mean_lng_d <- mutate(mean_lng_d, period_to_seconds(hms(mean_ride_length))) 

names(mean_lng_d)[names(mean_lng_d) == 'period_to_seconds(hms(mean_ride_length))'] <- "in_sec"


ggplot(data=mean_lng_d, aes(fill=member_casual)) +
  geom_col(mapping = aes(x=day_of_the_week, y=in_sec), position = "dodge") +
  labs(x="Sun to Sat", y="seconds", fill=NULL, title="Mean Ride Length (by Day of the Week)")+
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

#Plotting the number of member and casual by the day of the week

total_n_d <- data_all_months %>% 
  group_by(member_casual,day_of_the_week) %>% 
  count()

ggplot(data=data_all_months) +
  geom_bar(mapping = aes(x=day_of_the_week, fill=member_casual),  position = "dodge") +
  labs(x="Sun to Sat", y=NULL, fill=NULL, title="Number of Casual Users and Members (by Day of the Week)") +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

#How many casual users and members are using the bikes for over 30 min?

over_30m <- sum(hms(data_all_months$ride_length) > hms("00:30:00"), na.rm=TRUE)

over_30m / nrow(data_all_months)

over_30m_d <- data_all_months %>% 
  group_by(member_casual, day_of_the_week) %>% 
  summarise(over_30m = sum(hms(ride_length) > hms("00:30:00"), na.rm=TRUE))

ggplot(over_30m_d) +
  geom_col(mapping = aes(x=day_of_the_week, y=over_30m, fill=member_casual), position = "dodge") +
  labs(x="Sun to Sat", y=NULL, fill=NULL, title="Number of Casual Users and Members Over 30 minutes (by Day of the Week)") +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

#How many casual users and members are using the bikes for under 10 min?

below_10m <- sum(hms(data_all_months$ride_length) < hms("00:10:00"), na.rm=TRUE)

below_10m / nrow(data_all_months)

count <- summarise(sum(hms(data_all_months$ride_length) < hms("00:10:00"), na.rm=TRUE))

below_10m_d <-data_all_months %>% 
  group_by(member_casual, day_of_the_week) %>% 
  summarise(below_10m = sum(hms(ride_length) < hms("00:10:00"), na.rm=TRUE))

ggplot(below_10m_d) +
  geom_col(mapping = aes(x=day_of_the_week, y=below_10m, fill=member_casual), position = "dodge") +
  labs(x="Sun to Sat", y=NULL, fill=NULL, title="Number of Casual Users and Members below 10 minutes (by Day of the Week)") +
  theme(plot.title = element_text(hjust = 0.5, color = "#666666"))

#Casual
a <- below_10m_d$below_10m[1] + below_10m_d$below_10m[7]
b <- over_30m_d$over_30m[1] + over_30m_d$over_30m[7]
c <- sum(over_30m_d$over_30m[1:7])
c2 <- sum(below_10m_d$below_10m[1:7])

#Members
d <- below_10m_d$below_10m[8] + below_10m_d$below_10m[14]
e <- over_30m_d$over_30m[8] + over_30m_d$over_30m[14]
f <- sum(over_30m_d$over_30m[8:14])
f2 <- sum(below_10m_d$below_10m[8:14])

casual_rows <- sum(data_all_months$member_casual == "casual")
member_rows <- sum(data_all_months$member_casual == "member")

#percentage of long distance of all rides
c / casual_rows
f / member_rows

#percentage of short distance of all rides
c2 / casual_rows
f2 / member_rows

