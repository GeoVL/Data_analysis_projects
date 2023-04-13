
# Case study: #1
# Data Analyst: George Vellis

# Analyzing several data sets collected by a fitness tracker device with the aim to identify insights and trends.
# This case study is the last course of the Google Data Analytics Professional Certificate program.
# Data collected by Mobius.


# Installing the packages

install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("here")

# Loading the packages

library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(janitor)
library(skimr)
library(here)

# Importing and renaming the .csv files
# daily data

daily_activity <- read_csv("Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_steps <- read_csv("Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
daily_sleep <- read_csv("Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")


#Weight

weight_info <- read_csv("Fitness Tracker Data/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

# Reviewing the headers of the daily data and weight data
head(daily_activity)
head(daily_sleep)
head(daily_steps)
head(weight_info)

# Cleaning the columns names

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
daily_steps <-clean_names(daily_steps)
weight_info <- clean_names(weight_info)

# Formatting the datatype

daily_activity$activity_date <- as.Date(daily_activity$activity_date, "%m/%d/%y")
daily_sleep$sleep_day <- as.Date(daily_sleep$sleep_day, "%m/%d/%y")
daily_steps$activity_day <- as.Date(daily_steps$activity_day, "%m/%d/%y")
weight_info$date <- as.(weight_info$date, "%m/%d/%y")

# Sorting by date

weight_info <-weight_info %>% 
  arrange(date)

daily_activity <- daily_activity %>% 
  arrange(activity_date)

daily_sleep <- daily_sleep %>% 
  arrange(sleep_day)

daily_steps <- daily_steps %>% 
  arrange(activity_day)


# Finding the unique participants

n_distinct(daily_activity$id)
n_distinct(daily_sleep$id)
n_distinct(daily_steps$id)
n_distinct(weight_info$id)

#Converting the minutes to hours in daily sleep data set

daily_sleep["hours_asleep"]<- round(daily_sleep$total_minutes_asleep / 60 , digits= 2)
daily_sleep["hours_in_bed"]<- round(daily_sleep$total_time_in_bed / 60 , digits= 2)


View(daily_sleep)

#Converting the activity and sedentary minutes to hours in daily activity data set

daily_activity["total_active_hours"] <- round((daily_activity$very_active_minutes + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes) / 60, digits=2)
daily_activity["sedentary_hours"] <- round((daily_activity$sedentary_minutes+daily_activity$sedentary_active_distance) / 60 , digits= 2)


View(daily_activity)

#Time of wearing the fitness tracker and time of the fitness tracker is off

daily_activity["wearing_hours"]<- daily_activity$total_active_hours + daily_activity$sedentary_hours
daily_activity["off_hours"] <-  24.00 - daily_activity$wearing_hours 


# Summaries and statistics

daily_activity_sum <- daily_activity %>% 
  select(total_active_hours, total_distance, calories, sedentary_hours, wearing_hours) %>% 
  summary(daily_activity)

weight_sum <- weight_info %>% 
  select(weight_kg,weight_pounds,bmi) %>% 
  summary(weight_info)

daily_sleep_sum <- daily_sleep %>% 
 select(hours_asleep, hours_in_bed) %>% 
  summary(daily_sleep)


View(daily_sleep_sum)

View(daily_activity_sum)

View(weight_sum)


#max, min and means of data sets 

daily_steps <- daily_activity %>% 
  group_by(id) %>% 
   summarize(mean_total_steps = mean(total_steps), max(total_steps), min(total_steps))
  
total_distance_km <- total_distance %>% 
  group_by(id) %>% 
  summarize(mean_distance_km = mean(total_distance), max(total_distance), min(total_distance))

mean_active_hours <- daily_activity %>% 
  group_by(id) %>% 
  summarize(mean_of_active_hours = mean(total_active_hours))

mean_of_weight_per_id <- weight_info %>% 
  group_by(id) %>% 
   summarize(mean_weigh_kg = mean(weight_kg))

data_of_daily_sleep <- daily_sleep %>% 
  group_by(id) %>% 
   summarize(mean_of_daily_sleep_hours = mean(hours_asleep), max(hours_asleep), min(hours_asleep))

wearing_hours_per_id <- daily_activity %>% 
  group_by(id) %>% 
   summarize(mean_wearing_hours = mean(wearing_hours), max(wearing_hours), min(wearing_hours))

no_usage_of_fittness_tracker <- daily_activity %>% 
  group_by(id) %>% 
  summarize(off_hours = mean(off_hours))

mean_of_sedentary_hours <- daily_activity %>% 
  group_by(id) %>% 
  summarize(mean_sedentary_hours= mean(sedentary_hours))

mean_calories <- daily_activity %>% 
  group_by(id) %>% 
  summarize(mean_calories = mean(calories))




View(daily_steps)  
View(total_distance_km)
View(mean_of_weight_per_id)
View(data_of_daily_sleep)
View(wearing_hours_per_id)
View(mean_of_sedentary_hours)
View(mean_calories)
View(no_usage_of_fittness_tracker)
View(mean_active_hours)

#Merging the data

library(purrr)

merged_data <- list(wearing_hours_per_id, daily_steps, total_distance_km, data_of_daily_sleep, mean_of_sedentary_hours, mean_calories, no_usage_of_fittness_tracker, mean_active_hours) %>% 
  reduce(inner_join, by = "id")

View(merged_data)

#Exporting and saving the new data set

save(merged_data, file = "R_foder_1")

write.csv(merged_data, file = "merged_data.csv")

#Visualization of the walking distance and the daily calories
library(ggplot2)

ggplot(data=merged_data) + 
  geom_point(mapping = aes(x=mean_distance_km, y= mean_calories), color="blue") +
  labs(x= "Distance (KM)", y= "Calories", title = "Walking and Calories", subtitle = "Of 33 fitness tracker users")

#Visualization of the walking distance and the daily calories per user

ggplot(data=merged_data) + 
  geom_point(mapping = aes(x=mean_distance_km, y= mean_calories), color="blue") +
  facet_wrap(~id)+
  labs(x= "Distance (KM)", y= "Calories", title = "Walking and Calories", subtitle = "Of 33 fitness tracker users")

# Calories burned and sedentary hours of the users

ggplot(data=merged_data) + 
  aes(x=mean_sedentary_hours, y= mean_calories)+
  geom_violin(color="yellow", fill= "orange") +
  labs(x="Sedentary hours", y= "Calories burned", title = "Sedentary hours and burned calories")


#Total Steps and Sedentary hours per ID

ggplot(data=merged_data) +
  aes(x =mean_total_steps , y= mean_sedentary_hours) +
  geom_point() + 
  geom_smooth()+
  labs(x = "Mean Steps Per Day", y= "Mean Sedentary Hours", title= "Comparison of Sendentary Hours and Total Steps per user")
  






