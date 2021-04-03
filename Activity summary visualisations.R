# visualising my running history

# packages
library(tidyverse)
library(lubridate)

# set working directory
setwd("~/GitHub/running")

# read in activity summary data
activities <- read.csv("~/GitHub/running/running-data-exports/Strava/activities.csv")

# create dataset focused on just runs
run_metrics <- activities %>%
  # select columns of interest
  select(Activity.ID, Activity.Date, Activity.Type, Elapsed.Time, Distance.1,                                                                             
         Moving.Time, Max.Speed, Elevation.Gain, Elevation.High, Max.Grade,                                                                                   
         Max.Cadence, Average.Cadence) %>%
  # rename columns because I don't like dots
  rename(ActivityID = Activity.ID, ActivityDate = Activity.Date, ActivityType = Activity.Type,
         ElapsedTime_s = Elapsed.Time, Distance_m = Distance.1, MovingTime_s = Moving.Time,
         MaxSpeed = Max.Speed, Elevation = Elevation.Gain, ElevationMax = Elevation.High,
         MaxGradient = Max.Grade, CadenceMax = Max.Cadence, CadenceAvg = Average.Cadence) %>%
  # tidy up date field and create new versions of distance and elapsed time
  mutate(ActivityDate = dmy_hms(ActivityDate),
         Distance_km = Distance_m/1000,
         ElapsedTime_m = ElapsedTime_s/60,
         MovingTime_m = MovingTime_s/60) %>%
  # filter to just runs
  filter(ActivityType == 'Run')

# plot distance of runs per week and year
ggplot(data = run_metrics, aes(x = week(ActivityDate), y = Distance_km)) +
  geom_col() + 
  facet_wrap(~ year(ActivityDate), ncol = 1)
  