# Bellabeat Case Study with R by Hossameldin Saber.

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
#Import Data
activity <- read.csv("dailyActivity_merged.csv")
calories <- read.csv("hourlyCalories_merged.csv")
intensities <- read.csv("hourlyIntensities_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")

#Check Data
glimpse(activity)
head(activity)
#noticed problems with time stamp data, so run glimpse with each.

#Format Fix
#activity
glimpse(activity)
activity$ActivityDate <- mdy(activity$ActivityDate)
activity$date <- format(activity$ActivityDate, format = "%m/%d/%y")

#sleep
glimpse(sleep)
sleep$SleepDay <- mdy_hms(sleep$SleepDay)
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

#calories
glimpse(calories)
calories$ActivityHour <- mdy_hms(calories$ActivityHour)
calories$time <- format(calories$ActivityHour, format = "%H:%M:%S")
calories$date <- format(calories$ActivityHour, format = "%m/%d/%y")

#intensities
glimpse(intensities)
intensities$ActivityHour <- mdy_hms(intensities$ActivityHour)
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")

#weight
glimpse(weight)
weight$Date <- mdy_hms(weight$Date)
weight$time <- format(weight$Date, format = "%H:%M:%S")
weight$date <- format(weight$Date, format = "%m/%d/%y")

##Exploring & Summarizing Data, no. of participants (ID)
n_distinct(activity$Id)
n_distinct(calories$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(weight$Id)

#activity
activity %>% select(TotalSteps,TotalDistance,
                    SedentaryMinutes,Calories)%>%
  summary()
activity %>% select(VeryActiveMinutes,FairlyActiveMinutes,
                    LightlyActiveMinutes,SedentaryMinutes) %>% 
  summary()

#Average sedentary time is 991 minutes or 16 hours. Definitely needs to be reduced.
#Average total steps per day are 7638 which is less healthy.

#sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
#on average, participants sleep 1 time for 7 hours.

#weight
weight %>%
  select(WeightKg, BMI) %>%
  summary()

#Data Merge
glimpse(sleep)
glimpse(activity)
merged_data <- merge(activity, sleep, by = c("Id", "date"))

#Data Visualization

activity %>% 
  ggplot(aes(x=TotalSteps, y=Calories)) +
  geom_point() + geom_smooth() + theme_minimal()
  labs(title="Total Steps vs. Calories")
#+ve correlation; the more active the more calories we burn.

sleep %>% 
  ggplot(aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ geom_smooth() + theme_minimal() +
  labs(title="Total Minutes Asleep vs. Total Time in Bed")
#+ve linear relationship; the more we spend in bed the better sleep.

intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity)) %>% 
  ggplot(aes(x=time, y=mean_total_int)) +
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Average Total Intensity vs. Time")
#most activity happens between 5 pm and 7 pm.

merged_data %>% 
ggplot(aes(x=TotalMinutesAsleep,y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() + theme_minimal()+
  labs(title="Minutes Asleep vs. Sedentary Minutes")
#-ve correlation; the less sedentary life the better sleep.

