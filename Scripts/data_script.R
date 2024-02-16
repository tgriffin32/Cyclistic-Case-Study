library(tidyverse)

# create data frame for each months data set
data_01_df <- read.csv("new_data/202301-divvy-tripdata.csv")
data_02_df <- read.csv("new_data/202302-divvy-tripdata.csv")
data_03_df <- read.csv("new_data/202303-divvy-tripdata.csv")
data_04_df <- read.csv("new_data/202304-divvy-tripdata.csv")
data_05_df <- read.csv("new_data/202305-divvy-tripdata.csv")
data_06_df <- read.csv("new_data/202306-divvy-tripdata.csv")
data_07_df <- read.csv("new_data/202307-divvy-tripdata.csv")
data_08_df <- read.csv("new_data/202308-divvy-tripdata.csv")
data_09_df <- read.csv("new_data/202309-divvy-tripdata.csv")
data_10_df <- read.csv("new_data/202310-divvy-tripdata.csv")
data_11_df <- read.csv("new_data/202311-divvy-tripdata.csv")
data_12_df <- read.csv("new_data/202312-divvy-tripdata.csv")

# merge data frames into one data set for the whole year
trip_data_df <- rbind(data_01_df, data_02_df, data_03_df, data_04_df, 
              data_05_df, data_06_df, data_07_df, data_08_df, data_09_df, 
              data_10_df, data_11_df, data_12_df)

rm(data_01_df, data_02_df, data_03_df, data_04_df, data_05_df, data_06_df, data_07_df, data_08_df, data_09_df, data_10_df, data_11_df, data_12_df)

# Make copy of original data set for cleaning and removing null values in records
trip_data_clean_df <- trip_data_df %>% 
  drop_na()

View(trip_data_clean_df)


library(lubridate)
library(hms)

trip_data_clean_df %>% 
  mutate(ride_time = as_hms(difftime(end_time, start_time))) %>% 
  group_by(user_type) %>% 
  summarise(max_time = mean(ride_time))

# Change type of start and end_time to POSIXct
# Makes it easier to work with date-time objects for manipulation and time calculations
trip_data_clean_df %>% 
  mutate(start_time = as.POSIXct(trip_data_clean_df$start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(end_time = as.POSIXct(trip_data_clean_df$end_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(day_of_week = weekdays(start_time)) %>% 
  mutate(ride_time = difftime(end_time, start_time)) %>% 
  filter(ride_time > 0)

# check that each station name has correct id

# checks which start_station have more than one unique ids and lists the ids associated with them
trip_data_clean_df %>% 
  group_by(start_station_name) %>% 
  summarise(num_unique_ids = n_distinct(start_station_id), id_list=paste(unique(start_station_id), collapse = ", ")) %>% 
  filter(num_unique_ids > 1)

# checks which end_stations have more than one unique id and lists ids associated with them
trip_data_clean_df %>% 
  group_by(end_station_name) %>% 
  summarise(num_unique_ids = n_distinct(end_station_id), id_list=paste(unique(end_station_id), collapse = ", ")) %>% 
  filter(num_unique_ids > 1) 
  

trip_data_clean_df %>% 
  mutate(start_time = as.POSIXct(trip_data_clean_df$start_time, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(end_time = as.POSIXct(trip_data_clean_df$end_time, format = "%Y-%m-%d %H:%M:%S")) %>%
  mutate(ride_time = end_time - start_time)


################### Analysis #####################################
trip_data_clean_df %>% 
  group_by(user_type) %>% 
  summarise(user_type_count=n(), percent_of_total_rides=n()/nrow(trip_data_clean_df) * 100)

# User type vs mean ride time
mean_ride_times <- trip_data_clean_df %>% 
  group_by(user_type) %>% 
  summarise(mean_ride_time_sec = mean(ride_time_sec), max_ride_time=max(ride_time_sec), 
            min_ride_time=min(ride_time_sec))

mean_ride_times

# Causal Rider: just over 20 min 30 sec average ride time
# Member Rider: just over 12 min average ride time
ggplot(data=mean_ride_times) + 
  geom_col(mapping=aes(x=user_type, y=mean_ride_time_sec,fill=user_type)) +
  geom_text(aes(x=user_type, y=mean_ride_time_sec, label=round(mean_ride_time_sec, 2)),vjust=1) +
  labs(title = "Average Ride Times by Usertype", x="Usertype",y="Avg. Ride Time(Sec)")


# Aggregate and organize data by week day counts for each type of rider
weekday_rides <- trip_data_clean_df %>% 
  mutate(day_of_week = wday(start_time, label=TRUE)) %>% 
  group_by(day_of_week, user_type) %>% 
  summarise(.groups="drop",ride_count=n()) %>% 
  arrange(day_of_week)

str(weekday_rides)

ggplot(data = weekday_rides) + 
  geom_col(mapping = aes(x=day_of_week, y=ride_count,fill=user_type),position = 'dodge') +
  labs(title = "Ride Count Per Day of Week", x="Day of Week",y="Number of Rides") +
  scale_y_continuous(labels = scales::comma)


# which months do rides occur the most
trip_data_clean_df %>% 
  mutate(month_of_ride = month(start_time, label=TRUE)) %>% 
  group_by(month_of_ride, user_type) %>% 
  summarise(rides_per_month = n()) %>% 
  arrange(month_of_ride) %>% 
  ggplot() + 
  geom_line(mapping=aes(x=month_of_ride,y=rides_per_month, 
                        color=user_type, group=user_type)) +
  labs(title="Rides per Month by Usertype",x="Month",y="Number of Rides") +
  scale_y_continuous(labels = scales::comma)


# Analysing rideable bike types per usertype
library(gridExtra)
# Calculating the number of rides and percent of total rides taken with each
# type of bike for casual riders
c_rideable <- trip_data_clean_df %>% 
  filter(user_type == "casual") %>% 
  filter(rideable_type != "docked_bike") %>% 
  group_by(rideable_type) %>% 
  summarise(num_rides = n()) %>% 
  ungroup() %>% 
  mutate(percent_total_rides = num_rides/sum(num_rides) * 100)

c_rideable

# Pie chart displaying percentages of each bike used for casual riders
casual_pie <- ggplot(data = c_rideable, aes(x = "", y = percent_total_rides, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = paste0(round(percent_total_rides, 1), "%")), 
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Rides by Rideable Type (Casual)",
       fill = "Rideable Type") +
  theme_void() +
  theme(legend.position = "right")
  

# Calculating the number of rides and percent of total rides taken with each
# type of bike for casual riders
m_rideable <- trip_data_clean_df %>% 
  filter(user_type == "member") %>% 
  group_by(rideable_type) %>% 
  summarise(num_rides = n()) %>% 
  ungroup() %>% 
  mutate(percent_total_rides = num_rides/sum(num_rides)*100)

# Pie chart displaying percentages of each bike used for member riders
member_pie <- ggplot(data = m_rideable, aes(x = "", y = percent_total_rides, fill = rideable_type)) +
  geom_bar(stat = "identity", width = 1) +
  geom_label(aes(label = paste0(round(percent_total_rides, 1), "%")), 
             position = position_stack(vjust = 0.5),show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Rides by Rideable Type (Member)",
       fill = "Rideable Type") +
  theme_void() +
  theme(legend.position = "right")


######## Identify the top 5 starting stations for each usertype

# selecting user_type, start_station_name, and rideable_type from and storing 
# in a data frame station_data
station_data <- trip_data_clean_df %>% 
  filter(rideable_type != "docked_bike") %>% 
  select(user_type, start_station_name, rideable_type) %>% 
  drop_na()

# Group data by user_type, start_station_name, and rideable type, calculate 
# number of rides for each combination
station_summary <- station_data %>% 
  filter(start_station_name != "") %>% 
  group_by(user_type, start_station_name, rideable_type) %>% 
  summarise(num_rides = n()) %>% 
  arrange(user_type, start_station_name)

# Transform data to a wide format to create separate columns for each rideable 
# bike type
station_summary_pivot <- station_summary %>%
  pivot_wider(names_from = rideable_type, values_from = num_rides, values_fill = 0)

# Calculate the total number of rides and how much number of rides with classic 
# and electric bike types for each combination or user_type and 
# start_station_name
station_summary_final <- station_summary_pivot %>%
  group_by(user_type, start_station_name) %>%
  summarise(num_rides = sum(classic_bike)+sum(electric_bike),
            num_rides_w_classic = sum(classic_bike),
            num_rides_w_electric = sum(electric_bike))

# Get the top 5 stations with the most num_rides for each user_type
top_5_stations <- station_summary_final %>%
  group_by(user_type) %>%
  top_n(5, num_rides) %>% 
  arrange(user_type, desc(num_rides))

top_5_stations


## Bar graph for Top 5 start stations by num rides (casual rider)
top_5_stations %>% 
  filter(user_type == "casual") %>% 
  ggplot(aes(x=start_station_name, y=num_rides,fill="All Stations")) + 
  geom_bar(stat = "identity",) +
  labs(title = "Top 5 Start Stations by Number of Rides (Casual)",
       x = "Start Station", y = "Total Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "none",plot.title = element_text(size = 12)) +
  scale_fill_manual(values = "brown")


# Bar graph showing distribution rideable types for each top 5 start station (casual)
top_5_stations %>% 
  filter(user_type == "casual") %>% 
  ggplot(aes(x = start_station_name)) +
  geom_col(aes(y = num_rides_w_classic, fill = "Classic"), stat = "identity", position = "dodge") +
  geom_col(aes(y = num_rides_w_electric, fill = "Electric"), stat = "identity", position = "dodge") +
  labs(title = "Distribution of Electric and Classic Bikes (Casual)",
       x = "Start Station",
       y = "Number of Rides",
       fill = "Rideable Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        plot.title = element_text(size=12)) 



## Bar graph for Top 5 start stations by num rides (member rider)
top_5_stations %>% 
  filter(user_type == "member") %>% 
  ggplot(aes(x=start_station_name, y=num_rides,fill="All Stations")) + 
  geom_bar(stat = "identity",) +
  labs(title = "Top 5 Start Stations by Number of Rides (Member)",
       x = "Start Station", y = "Total Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "none",plot.title = element_text(size = 12)) +
  scale_fill_manual(values = "brown")


# Bar graph showing distribution rideable types for each top 5 start station (member)
top_5_stations %>% 
  filter(user_type == "member") %>% 
  ggplot(aes(x = start_station_name)) +
  geom_col(aes(y = num_rides_w_classic, fill = "Classic"), stat = "identity", position = "dodge") +
  geom_col(aes(y = num_rides_w_electric, fill = "Electric"), stat = "identity", position = "dodge") +
  labs(title = "Distribution of Electric and Classic Bikes (Member)",
       x = "Start Station",
       y = "Number of Rides",
       fill = "Rideable Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
        plot.title = element_text(size=12)) 

