#Importing necessary libraries   
library(tidyverse)
library(lubridate)
library(ggplot2)

#Reading datasets of Bike trips in 2019 over 4 quarters
df1=read_csv("D:/Capstone dataset/Divvy_Trips_2019_Q1.csv")
df2=read_csv("D:/Capstone dataset/Divvy_Trips_2019_Q2.csv")
df3=read_csv("D:/Capstone dataset/Divvy_Trips_2019_Q3.csv")
df4=read_csv("D:/Capstone dataset/Divvy_Trips_2019_Q4.csv")
colnames(df2)
colnames(df3)
colnames(df4)
colnames(df1)

#Making data consistent across all 4 datasets
df2 <- df2 %>% 
  rename(
    trip_id = "01 - Rental Details Rental ID"
          ,bikeid = "01 - Rental Details Bike ID" 
          ,start_time = "01 - Rental Details Local Start Time"  
          ,end_time = "01 - Rental Details Local End Time"  
          ,tripduration=  "01 - Rental Details Duration In Seconds Uncapped"
          ,from_station_name = "03 - Rental Start Station Name" 
          ,from_station_id = "03 - Rental Start Station ID"
          ,to_station_name = "02 - Rental End Station Name" 
          ,to_station_id = "02 - Rental End Station ID"
          ,usertype = "User Type"
          ,gender="Member Gender"
          ,birthyear= "05 - Member Details Member Birthday Year"
          )
df1 <-  mutate(df1, trip_id = as.character(trip_id)
                   ,bikeid = as.character(bikeid))
df2 <-  mutate(df2, trip_id = as.character(trip_id)
               ,bikeid = as.character(bikeid))
df3 <-  mutate(df3, trip_id = as.character(trip_id)
               ,bikeid = as.character(bikeid))
df4 <-  mutate(df4, trip_id = as.character(trip_id)
               ,bikeid = as.character(bikeid))

#Merging the dataset together
all_trips <- bind_rows(df1,df2,df3,df4)
all_trips <- read_csv("D:/Capstone dataset/all_trips.txt")
colnames(all_trips)
dim(all_trips)
summary(all_trips)

#Data cleaning
all_trips$date <- as.Date(all_trips$start_time)
all_trips$time <- format(as_datetime(all_trips$start_time),"%H")
all_trips$month <- format(as.Date(all_trips$start_time),"%b")
all_trips$day <- format(as.Date(all_trips$start_time),"%d")
all_trips$day_of_week <- format(as.Date(all_trips$start_time),"%A")
str(all_trips)

#selecting only relevant features
df <- select(all_trips,-c(start_time,end_time,bikeid,from_station_id,
                          to_station_id,birthyear))

#COMPARE
#trip duration with usertype
aggregate(df$tripduration~df$usertype,FUN = mean)
aggregate(df$tripduration~df$usertype,FUN = length)

#trip duration with gender
aggregate(df$tripduration~df$gender,FUN = mean)
aggregate(df$tripduration~df$gender,FUN = length)

#trip duration with day_week
aggregate(df$tripduration~df$day_of_week,FUN = mean)
aggregate(df$tripduration~df$day_of_week,FUN = length)

#fixing the order
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday",
            "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(df$tripduration~df$day_of_week,FUN = mean)
aggregate(df$tripduration~df$day_of_week,FUN = length)

#trip duration with month
df$month <- ordered(df$month, levels=c("Jan","Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec"))
aggregate(df$tripduration~df$month,FUN = mean)
aggregate(df$tripduration~df$month,FUN = length)

#ANALYSIS
# ridership data by usertype and weekday
graph_1 <- df %>%
  group_by(usertype,day_of_week) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(tripduration)) %>% 
  arrange(usertype,day_of_week) 
ggplot(data = graph_1, aes(x=day_of_week,y=number_of_rides,fill = usertype)) +
  geom_col(position = "dodge")+labs(title = "Mean trip duration by usertype and weekday")
ggplot(data = graph_1, aes(x=day_of_week,y=avg_duration,fill = usertype)) +
  geom_col(position = "dodge")+labs(title = "Number of trips by usertype and weekday")

# ridership data by gender and weekday
graph_2 <- df %>%
  drop_na(gender) %>% 
  group_by(gender,day_of_week) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(tripduration)) %>% 
  arrange(gender,day_of_week) 
ggplot(data = graph_2, aes(x=day_of_week,y=number_of_rides,fill = gender)) +
  geom_col(position = "dodge")+labs(title = "Number of trips by gender and weekday")
ggplot(data = graph_2, aes(x=day_of_week,y=avg_duration,fill = gender)) +
  geom_col(position = "dodge")+labs(title = "Mean trip duration by gender and weekday")


#ridership data by gender and usertype
graph_3 <- df %>%
  drop_na(gender) %>% 
  group_by(gender,usertype) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(tripduration)) %>% 
  arrange(gender,usertype) 
ggplot(data = graph_3, aes(x=usertype,y=number_of_rides,fill = gender)) +
  geom_col(position = "dodge")+labs(title = "Number of trips by gender and usertype")
ggplot(data = graph_3, aes(x=usertype, y=avg_duration,fill = gender)) +
  geom_col(position = "dodge")+labs(title = "Mean trip duration by gender and usertype")

#ridership data by week days and months
df$month <- ordered(df$month, levels=c("Jan","Feb", "Mar", "Apr", "May", "Jun",
                                       "Jul","Aug","Sep","Oct","Nov","Dec"))
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday", "Monday", "Tuesday",
                                  "Wednesday", "Thursday", "Friday", "Saturday"))
graph_4 <- df %>%
  group_by(day_of_week,month,usertype) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(tripduration)) %>% 
  arrange(day_of_week,month) 
ggplot(data = graph_4, aes(x=day_of_week,y=number_of_rides, fill = usertype)) +
  geom_col(position = "dodge")+
  facet_wrap(~month)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = "Number of trips by months and days of week")

#ridership data by week days and time
df$day_of_week <- ordered(df$day_of_week, levels=c("Sunday",
                                                   "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
graph_5 <- df %>%
  group_by(day_of_week,time) %>% 
  summarise(number_of_rides = n(), avg_duration=mean(tripduration)) %>% 
  arrange(day_of_week,time) 
ggplot(data = graph_5, aes(x=day_of_week,y=number_of_rides, fill = day_of_week)) +
  geom_col(position = "dodge")+
  facet_wrap(~time)+
  theme(axis.text.x=element_blank())+
  labs(title = "Number of trips by time and days of week")
