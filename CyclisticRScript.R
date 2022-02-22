# Load libraries
library(tidyverse)
library(lubridate)
library(hms)

# Import data
X202009_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202009-divvy-tripdata.csv")
X202010_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202010-divvy-tripdata.csv")
X202011_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202011-divvy-tripdata.csv")
X202012_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202012-divvy-tripdata.csv")
X202101_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202101-divvy-tripdata.csv")
X202102_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202102-divvy-tripdata.csv")
X202103_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202103-divvy-tripdata.csv")
X202104_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202104-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202105-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202106-divvy-tripdata.csv")
X202107_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202107-divvy-tripdata.csv")
X202108_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202108-divvy-tripdata.csv")
X202109_divvy_tripdata <- read_csv("Downloads/DataAnalysis/CapStone/CyclistFiles/CSV/202109-divvy-tripdata.csv")

# Data checking and exploration before combining all files
# Making sure the column names match. Could just skip and try to bind them to see if there is an error.
colnames(X202009_divvy_tripdata)
colnames(X202010_divvy_tripdata)
colnames(X202011_divvy_tripdata)
colnames(X202012_divvy_tripdata)
colnames(X202101_divvy_tripdata)
colnames(X202102_divvy_tripdata)
colnames(X202103_divvy_tripdata)
colnames(X202104_divvy_tripdata)
colnames(X202105_divvy_tripdata)
colnames(X202106_divvy_tripdata)
colnames(X202107_divvy_tripdata)
colnames(X202108_divvy_tripdata)
colnames(X202109_divvy_tripdata)

# Just looking at the data to see if data formats are the same
View(X202009_divvy_tripdata)
View(X202010_divvy_tripdata)
View(X202011_divvy_tripdata)
# Noticed "start_station_id" and "end_station_id" changed formats starting 202012
# Should make consistent throughout, or not worry about it if station names stay the same
View(X202012_divvy_tripdata)
View(X202101_divvy_tripdata)
View(X202102_divvy_tripdata)
View(X202103_divvy_tripdata)
View(X202104_divvy_tripdata)
View(X202105_divvy_tripdata)
View(X202106_divvy_tripdata)
View(X202107_divvy_tripdata)
View(X202108_divvy_tripdata)
View(X202109_divvy_tripdata)

# Change data type of station id columns (found from error when trying to combine data frames later)
X202009_dt <- transform(X202009_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
X202010_dt <- transform(X202010_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
X202011_dt <- transform(X202011_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

# Combine all files to one data frame
ALL_divvy_tripdata <- bind_rows(X202009_dt, X202010_dt, X202011_dt, X202012_divvy_tripdata, X202101_divvy_tripdata, X202102_divvy_tripdata, X202103_divvy_tripdata, X202104_divvy_tripdata, X202105_divvy_tripdata, X202106_divvy_tripdata, X202107_divvy_tripdata, X202108_divvy_tripdata, X202109_divvy_tripdata)

# Add some new columns calculating stuff (ride_length, day_of_week)
ALL_divvy_tripdata <- mutate(ALL_divvy_tripdata, ride_length_hms = as_hms(ended_at - started_at))
ALL_divvy_tripdata <- mutate(ALL_divvy_tripdata, day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

# Calculate some summaries
Casual_vs_member <- ALL_divvy_tripdata %>%
  group_by(member_casual) %>%
  summarize(total_ride_length = sum(ride_length_hms), mean_ride_length = mean(ride_length_hms), trip_count = n())

Mean_length_DOW_cas <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>%
  summarize(mean_ride_DOW_cas = mean(ride_length_hms), trip_count = n())

# Graphs to visualize data
# Casual member ride counts by week
week_count_casual <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(week = floor_date(started_at, "week")) %>%
  summarize(casual_trips_week = n())

week_count_casual %>%
  ggplot(mapping = aes(x = week, y = casual_trips_week)) + geom_line () + geom_smooth() +
  labs(title = "Casual Member Ride Counts (Sept. 2020 - Sept. 2021)", x = "Month", y = "Trip Count") +
  scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b")

# Popular stations for casual riders
popular_stations_casual <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(station = start_station_name) %>%
  summarize(trips = n()) %>%
  arrange(-trips) %>%
  slice(2:6)

popular_stations_casual %>%
  ggplot(aes(x = reorder(station, (-trips)), y = trips)) + geom_bar(stat = 'identity') +
  labs(title = "Most Popular Stations of Casual Riders", x = "Station", y = "Trip Count")
