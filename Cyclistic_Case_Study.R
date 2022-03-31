
############################################################ PREREQUISITES ############

library(tidyverse) # Helps wrangle data
library(lubridate) # Helps wrangle date attributes
library(ggplot2)   # Helps visualize data
library(dplyr)     # Distinct function
getwd()            # Displays your working directory
setwd("C:/Users/***/R code") 
#sets your working directory to simplify calls to data

############################################################ LOADING IN DATA ############

table_202103 <- read_csv("202103-divvy-tripdata.csv")
table_202104 <- read_csv("202104-divvy-tripdata.csv")
table_202105 <- read_csv("202105-divvy-tripdata.csv")
table_202106 <- read_csv("202106-divvy-tripdata.csv")
table_202107 <- read_csv("202107-divvy-tripdata.csv")
table_202108 <- read_csv("202108-divvy-tripdata.csv")
table_202109 <- read_csv("202109-divvy-tripdata.csv")
table_202110 <- read_csv("202110-divvy-tripdata.csv")
table_202111 <- read_csv("202111-divvy-tripdata.csv")
table_202112 <- read_csv("202112-divvy-tripdata.csv")
table_202201 <- read_csv("202201-divvy-tripdata.csv")
table_202202 <- read_csv("202202-divvy-tripdata.csv")

############################################################ WRANGLING DATA ############

#colnames(table_202103) # Checking if column names match in each dataframe ... they do
#colnames(table_202104)
#colnames(table_202105)
#colnames(table_202106)
#colnames(table_202107)
#colnames(table_202108)
#colnames(table_202109)
#colnames(table_202110)
#colnames(table_202111)
#colnames(table_202112)
#colnames(table_202201)
#colnames(table_202202)

#str(table_202103)     # Checking if data types are same in each column ... # Yes, but found NA's in some columns, will fix once df's are combined
#str(table_202104)     # Also found discrepancies in the start_station_id and end_station_id 
#str(table_202105)     # Will remove those columns and the start station and end station names as     
#str(table_202106)     # we can gather the same info from the latitude and longitude columns 
#str(table_202107)
#str(table_202108)
#str(table_202109)
#str(table_202110)
#str(table_202111)
#str(table_202112)
#str(table_202201)
#str(table_202202)

all_trips <- bind_rows(table_202103, # Combining data frames
                      table_202104,
                      table_202105,
                      table_202106,
                      table_202107,
                      table_202108,
                      table_202109,
                      table_202110,
                      table_202111,
                      table_202112,
                      table_202201,
                      table_202202) 

#str(all_trips)                      # Checking combined data frames
#summary(all_trips) 

#table(all_trips$member_casual)      # Making sure it only contains "member" or "casual" 
#table(all_trips$rideable_type)      # Contains "classic_bike", "docked_bike", or "electric_bike"

############################################################ CLEANING DATA ############
                                                                                                                
all_trips_1 <- all_trips %>%                                                              # MADE NEW DATA FRAME TO BACKUP ORIGINAL DATA
 # select(-c(start_station_name, start_station_id, end_station_name, end_station_id)) %>%  # removed specified columns since we can use longitude and latitude for start loc and end loc
  distinct() %>%                                                                          # removed any rows which are complete duplicates
  drop_na()                                                                               # removed rows which have NA's

all_trips_1$ride_length_min <- (difftime(all_trips_1$ended_at, all_trips_1$started_at)) / 60      # Calculated ride length using 'difftime' and made a new column for it
all_trips_1$ride_length_min <- as.numeric(all_trips_1$ride_length_min)                            # Converted ride length to fully numeric bc when 'difftime' is used the class results in 'difftime'
all_trips_1 <- all_trips_1 %>% filter(ride_length_min > 1, ride_length_min < 1440)                # Filtered to only include rides over 1 minute and under 1 day (users who ride for a whole day maybe ubereat deliver?)
all_trips_1 <- all_trips_1 %>% filter(rideable_type != "docked_bike") 

all_trips_1$date <- as.Date(all_trips_1$started_at)                # Adding date in  default format yyyy-mm-dd
all_trips_1$month <- format(as.Date(all_trips_1$date), "%m")       # Adding month column
all_trips_1$day <- format(as.Date(all_trips_1$date), "%d")         # Adding day column
all_trips_1$year <- format(as.Date(all_trips_1$date), "%Y")        # Adding year column
all_trips_1$day_of_week <- format(as.Date(all_trips_1$date), "%A") # Adding day of the week column
all_trips_1$day_of_week <- ordered(all_trips_1$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) # Putting days of the week in order since they were not


############################################################ DESCRIPTIVE ANALYSIS ############

summary(all_trips_1$ride_length_min)                                            # Mean, median, max, min of ride length in minutes

aggregate(all_trips_1$ride_length_min ~ all_trips_1$member_casual, FUN = summary)   # Comparing ride length statistics between members and casual riders

aggregate(all_trips_1$ride_length_min ~ all_trips_1$member_casual + all_trips_1$day_of_week, FUN = mean) ## Comparing ride length statistics between members and casual riders by day of week

aggregate(all_trips_1$ride_id ~ all_trips_1$date, FUN = length)

## Analyze ridership data by type and weekday ##

all_trips_1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%                            # creates weekday field using wday()
  group_by(member_casual, weekday) %>%                                            # groups by usertype and weekday
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>%	    # calculates the number of rides and average duration, calculates the average duration
  arrange(member_casual, weekday)								                                  # sorts

## Visualize the number of rides by rider type ##

all_trips_1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

## Visualize average duration ##

all_trips_1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length_min)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

############################################################ EXPORTING FILES FOR FURTHER ANALYSIS ############

## Getting Average Ride Length Per Weekday ##

avg_ride_length_per_weekday <- aggregate(all_trips_1$ride_length_min ~ all_trips_1$member_casual + all_trips_1$day_of_week, FUN = mean) # Makes data frame comparing avg ride length per day of week                                                                                                                                                                      # for members and casuals (to use for tableau)
## Getting Counts of Rides Per Period ##

num_of_rides_per_weekday <- all_trips_1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, weekday)

num_of_rides_per_date <- all_trips_1 %>%
  group_by(member_casual, date) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, date) 

## Finding Number of Trips per Avg Station Coordinates ##

avg_start_coor_per_station_id <- all_trips_1 %>% 
  group_by(start_station_id, member_casual) %>% 
  summarise(avg_start_lat = mean(start_lat), avg_start_lng = mean(start_lng),number_of_rides = n()) %>% 
  arrange(desc(number_of_rides))

avg_end_coor_per_station_id <- all_trips_1 %>% 
  group_by(end_station_id, member_casual) %>% 
  summarise(avg_end_lat = mean(end_lat), avg_end_lng = mean(end_lng),number_of_rides = n()) %>% 
  arrange(desc(number_of_rides))

## Creating Path Id for Each Trip ##

all_trips_1$path_id <- paste0(all_trips_1$start_station_id, "_", all_trips_1$end_station_id)

## Creating DF for Origins ##

count_rides_per_path_id_origin <- all_trips_1 %>% 
  mutate(location = start_station_id) %>% 
  group_by(path_id, member_casual, location) %>% 
  summarise(latitude = mean(start_lat), longitude = mean(start_lng), number_of_rides = n()) %>% 
  mutate(origin_destination = "origin") %>% 
  arrange(desc(number_of_rides))

## Creating DF for Destinations ##

count_rides_per_path_id_destination <- all_trips_1 %>% 
  mutate(location = start_station_id) %>% 
  group_by(path_id, member_casual, location) %>% 
  summarise(latitude = mean(end_lat), longitude = mean(end_lng), number_of_rides = n()) %>% 
  mutate(origin_destination = "destination") %>% 
  arrange(desc(number_of_rides))

## Combining The Rows Into One DF ##

count_rides_per_path_id <- bind_rows(count_rides_per_path_id_origin, count_rides_per_path_id_destination) # Combining data frames

## Rounding Latitudes and Longitudes ##
                   
count_rides_per_path_id$latitude <- formatC(count_rides_per_path_id$latitude, digits = 3, format = "f")   # Rounding coordinate to 3 decimal places
count_rides_per_path_id$longitude <- formatC(count_rides_per_path_id$longitude, digits = 3, format = "f")


## Writing CSVs of Final Data ##

write.csv(avg_ride_length_per_weekday, file = "C:/Users/***/R code/avg_ride_length_per_weekday.csv")
write.csv(num_of_rides_per_weekday, file = "C:/Users/***/R code/num_of_rides_per_weekday.csv")
write.csv(num_of_rides_per_date, file = "C:/Users/***/R code/num_of_rides_per_date.csv")
write.csv(avg_start_coor_per_station_id, file = "C:/Users/***/R code/start_coor_per_station.csv")
write.csv(avg_end_coor_per_station_id, file = "C:/Users/***/R code/end_coor_per_station.csv")
write.csv(count_rides_per_path_id, file = "C:/Users/***/R code/count_rides_per_path_id.csv")

write.csv(all_trips_1, file = "C:/Users/***/R code/all_trips_1.csv")





