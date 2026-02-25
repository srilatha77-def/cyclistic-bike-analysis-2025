 # install.packages("tidyverse")
 # install.packages("lubridate")
#  install.packages("ggplot2")
 library(tidyverse)
 library(lubridate)
 library(ggplot2)
 library(dplyr)
 jan <- read_csv("raw_data/202501-divvy-tripdata.csv")
 feb <- read_csv("raw_data/202502-divvy-tripdata.csv")
 mar <- read_csv("raw_data/202503-divvy-tripdata.csv")
 apr <- read_csv("raw_data/202504-divvy-tripdata.csv")
 may <- read_csv("raw_data/202505-divvy-tripdata.csv")
 jun <- read_csv("raw_data/202506-divvy-tripdata.csv")
 jul <- read_csv("raw_data/202507-divvy-tripdata.csv")
 aug <- read_csv("raw_data/202508-divvy-tripdata.csv")
 sep <- read_csv("raw_data/202509-divvy-tripdata.csv")
 oct <- read_csv("raw_data/202510-divvy-tripdata.csv")
 nov <- read_csv("raw_data/202511-divvy-tripdata.csv")
 dec <- read_csv("raw_data/202512-divvy-tripdata.csv")
 

 all_trips <- bind_rows(jan, feb, mar, apr, may, jun,
                                           jul, aug, sep, oct, nov, dec)
 dim(all_trips)
 colnames(all_trips)
 str(all_trips)
 head(all_trips)
 all_trips$started_at <- ymd_hms(all_trips$started_at)
 all_trips$ended_at <- ymd_hms(all_trips$ended_at)
 all_trips <- all_trips %>%
   mutate(
     ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
     day_of_week = wday(started_at, label = TRUE)
   )
 
 all_trips <- all_trips %>%
   filter(ride_length > 0)
 dim(all_trips)
 colnames(all_trips)
 str(all_trips)
 head(all_trips)
 summary(all_trips$ride_length)
 table(all_trips$member_casual)
 all_trips$day_of_week <- ordered(all_trips$day_of_week,
                                  levels = c("Sunday","Monday","Tuesday",
                                             "Wednesday","Thursday",
                                             "Friday","Saturday"))
 mean(all_trips$ride_length, na.rm = TRUE)
 median(all_trips$ride_length, na.rm = TRUE)
 max(all_trips$ride_length, na.rm = TRUE)
 min(all_trips$ride_length, na.rm = TRUE)
 
 aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = mean)
 aggregate(all_trips$ride_length ~ all_trips$member_casual, FUN = median)
 aggregate(ride_length ~ member_casual, data = all_trips, FUN = max)
 
 class(all_trips$ride_length)
 all_trips$day_of_week <- format(all_trips$started_at, "%A")
 all_trips$day_of_week <- ordered(all_trips$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 aggregate(ride_length ~ member_casual + day_of_week, data = all_trips, FUN = mean, na.rm = TRUE)
 aggregate(all_trips$ride_length ~ all_trips$member_casual + all_trips$day_of_week, FUN = length)
 ggplot(data = all_trips) +
       geom_bar(mapping = aes(x = day_of_week, fill = member_casual))
 ggsave("rides_by_day.png", width = 8, height = 5)
 
 all_trips %>%
   group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(),
             average_duration = mean(ride_length)) %>%
   arrange(member_casual, day_of_week) %>%
   ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
   geom_col(position = "dodge")

 ggsave("avg_duration_by_day.png", width = 8, height = 5)
 
 
 
 
 all_trips$month <- month(all_trips$started_at, label = TRUE)
 
 ggplot(all_trips, aes(x = month, fill = member_casual)) +
   geom_bar(position = "dodge") +
   labs(title = "Total Rides by Month",
        x = "Month",
        y = "Number of Rides",
        fill = "Rider Type") + theme_minimal()

 ggsave("total rides by month.png", width = 8, height = 5)
 
 all_trips$hour <- hour(all_trips$started_at)
 
 ggplot(all_trips, aes(x = hour, fill = member_casual)) +
   geom_bar(position = "dodge") +
   labs(title = "Rides by Hour of Day",
        x = "Hour",
        y = "Number of Rides",
        fill = "Rider Type") +
   theme_minimal()
 ggsave("rides by hour of day.png", width = 8, height = 5)
 
 