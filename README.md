# Googles-Data-Analytics-Capstone

---
title: "Google's Data Analytics - Capstone Project"
output:
  pdf_document: default
  html_document: default
date: "2022-09-02"
---

## Data Preparation

###Install and Load necessary packages

```{r}
library(tidyverse)
library(janitor)
library(lubridate)
library(rmarkdown)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
```

###Import/Load past 12 months data

```{r}
df1 <- read.csv("may_2020.csv")
df2 <- read.csv("jun_2020.csv")
df3 <- read.csv("jul_2020.csv")
df4 <- read.csv("aug_2020.csv")
df5 <- read.csv("sep_2020.csv")
df6 <- read.csv("oct_2020.csv")
df7 <- read.csv("nov_2020.csv")
df8 <- read.csv("dec_2020.csv")
df9 <- read.csv("jan_2021.csv")
df10 <- read.csv("feb_2021.csv")
df11 <- read.csv("mar_2021.csv")
df12 <- read.csv("apr_2021.csv")
```

###Merge all 12 months data into year view data frame

```{r}
bike_rides <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)
```

### View newly created data frame

```{r}
summary(bike_rides)
```

##Data cleanup

###check members_casual column for any discrepancies in data

```{r}
table(bike_rides$member_casual)
```

```{r}
drop_na(bike_rides)

summary(bike_rides)

bike_rides <- na.omit(bike_rides)
```

##Format Data into weekdays, dates, months, seasons for further analysis of data

```{r}
bike_rides$date <- as.Date(bike_rides$started_at)

bike_rides$month <- format(as.Date(bike_rides$date), "%m")
bike_rides$day <- format(as.Date(bike_rides$date), "%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date), "%A")
```

Check data and create ride length column

```{r}
bike_rides <- bike_rides %>%
  mutate(across(c(started_at, ended_at), ymd_hms), 
         ride_length = difftime(ended_at,started_at))

bike_rides <- bike_rides[!(bike_rides$ride_length < 0),]
```

###create column for season spring, summer, fall, winter

```{r}
bike_rides <- bike_rides %>% 
  mutate(season = case_when(month == "03" ~ "Spring", month == "04" ~ "Spring", month == "05" ~ "Spring", month == "06"  ~ "Summer", month == "07"  ~ "Summer", month == "08"  ~ "Summer", month == "09" ~ "Fall", month == "10" ~ "Fall",  month == "11" ~ "Fall", month == "12" ~ "Winter", month == "01" ~ "Winter", month == "02" ~ "Winter"))
```

###check final data

```{r}
glimpse(bike_rides)
summary(bike_rides)
```

##Descriptive Analysis

```{r}
aggregate(bike_rides$ride_length ~ bike_rides$member_casual, FUN = mean)
aggregate(bike_rides$ride_length ~ bike_rides$member_casual, FUN = median)
aggregate(bike_rides$ride_length ~ bike_rides$member_casual, FUN = max)
aggregate(bike_rides$ride_length ~ bike_rides$member_casual, FUN = min)

```

##Data Visualization

###Weekdays Vs. Number of Rides

```{r}
bike_rides %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

```

! [Weekdays Vs number of rides] (E:\Self Learning\google Data Analytics\Capstone Project Google Data Analytics\Cyclist Data\CSV Files monthly/Weekdays Vs number of rides_Rplot.jpeg)

###Weekdays Vs. Average Duration

```{r}
bike_rides %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
```

[Weekdays Vs Average Duration] (E:\Self Learning\google Data Analytics\Capstone Project Google Data Analytics\Cyclist Data\CSV Files monthly/Weekdays Vs Average Duration_Rplot.jpeg)
