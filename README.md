README
================
Michael Kinison
11/7/2021

## Cyclistic Bike-Share Case Study

At the completion of the Google Data Analytic Certificate program, there
is an opportunity to highlight the skills you’ve learned by doing a case
study. This is my step through explanation of the R script I wrote to
reach the insights and conclusions of the study. You can see the R
script listed in the files.

## Background Scenario

You are a junior data analyst working in the marketing analyst team at
Cyclistic, a bike-share company in Chicago. The director of marketing
believes the company’s future success depends on maximizing the number
of annual memberships. Therefore, your team wants to understand how
casual riders and annual members use Cyclistic bikes differently. From
these insights, your team will design a new marketing strategy to
convert casual riders into annual members. But first, Cyclistic
executives must approve your recommendations, so they must be backed up
with compelling data insights and professional data visualizations.

## Loading Librarys

I used 3 libraries for this study: tidyverse, lubridate, and hms

``` r
library(tidyverse)
library(lubridate)
library(hms)
```

## Importing Data

Monthly bike sharing data was provided and can be found at
<https://divvy-tripdata.s3.amazonaws.com/index.html>. After downloading
12 months of data, I imported each one into its own data frame.

``` r
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
```

## Checking Data

Before combining all of the separate data files into one data frame for
easier analysis, I just explored each file to check for consistencies,
missing values, data types, etc.

``` r
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
```

While viewing each individual file, I noticed “start_station_id” and
“end_station_id” changed formats starting 202012, but decided not to fix
it since I could just use “start_station_name” since it stayed
consistent.

``` r
View(X202009_divvy_tripdata)
View(X202010_divvy_tripdata)
View(X202011_divvy_tripdata)
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
```

When I tried to combine all the files, it threw an error because the
data type of the station ids changed from numeric to character that was
related to the way station ids changed. So, I changed the data type of
those columns and decided to save it to a new data frame name.

``` r
X202009_dt <- transform(X202009_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
X202010_dt <- transform(X202010_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
X202011_dt <- transform(X202011_divvy_tripdata, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
```

## Combine Data Files

Once everything was consistent and columns matched up, I combined them
together to make analysis easier.

``` r
ALL_divvy_tripdata <- bind_rows(X202009_dt, X202010_dt, X202011_dt, X202012_divvy_tripdata, X202101_divvy_tripdata, X202102_divvy_tripdata, X202103_divvy_tripdata, X202104_divvy_tripdata, X202105_divvy_tripdata, X202106_divvy_tripdata, X202107_divvy_tripdata, X202108_divvy_tripdata, X202109_divvy_tripdata)
```

## Analysis

Once I had my combined data frame, I added some columns to perform some
calculations for all riders: ride length, day of the week the trip was
taken. This information was used later to see how the rider members
differ.

``` r
ALL_divvy_tripdata <- mutate(ALL_divvy_tripdata, ride_length_hms = as_hms(ended_at - started_at))
ALL_divvy_tripdata <- mutate(ALL_divvy_tripdata, day_of_week = wday(started_at, label = TRUE, abbr = FALSE))
```

Since the main focus of the case study was to see how casual members
differ from annual membership riders, I did some comparison
calculations. I wanted to see which member type had the most total ride
length, what their average ride length was, and how many times each
member type rode a bike.

``` r
Casual_vs_member <- ALL_divvy_tripdata %>%
  group_by(member_casual) %>%
  summarize(total_ride_length = sum(ride_length_hms), mean_ride_length = mean(ride_length_hms), trip_count = n())
```

I also wanted to see which day of the week the riders rode the most for
marketing purposes. I filtered for the casual riders since this would be
the target group. Turns out the weekends were most popular for the
casual riders.

``` r
Mean_length_DOW_cas <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>%
  summarize(mean_ride_DOW_cas = mean(ride_length_hms), trip_count = n())
```

## Visualizations

Next, I started graphing data to easily see trends. I plotted the amount
of rides per week for casual riders through the whole year to see when a
marketing campaign would reach the most customers. No surprise that
there are more riders during the warmer months of summer versus the
colder months, especially in Chicago.

``` r
week_count_casual <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(week = floor_date(started_at, "week")) %>%
  summarize(casual_trips_week = n())

week_count_casual %>%
  ggplot(mapping = aes(x = week, y = casual_trips_week)) + geom_line () + geom_smooth() +
  labs(title = "Casual Member Ride Counts (Sept. 2020 - Sept. 2021)", x = "Month", y = "Trip Count") +
  scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b")
```

The other major part of the marketing campaign was figuring out which
stations were the most popular so those could be used for potential
geofencing marketing.

``` r
popular_stations_casual <- ALL_divvy_tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(station = start_station_name) %>%
  summarize(trips = n()) %>%
  arrange(-trips) %>%
  slice(2:6)

popular_stations_casual %>%
  ggplot(aes(x = reorder(station, (-trips)), y = trips)) + geom_bar(stat = 'identity') +
  labs(title = "Most Popular Stations of Casual Riders", x = "Station", y = "Trip Count")
```

## Conclusion

After I completed my analysis and had my visuals, I made a mock
presentation slide show that I would present to my stakeholders if this
were a real-world problem. You can view that powerpoint presentation in
the files above.
