# visualising my running history

##############
### Set up ###
##############

# packages
library(tidyverse) # obviously
library(lubridate) # dates
library(ggpubr) # arranging ggplots
library(wesanderson) # nice colours

# set working directory
setwd("~/GitHub/running")

# read in activity summary data
activities <- read.csv("~/GitHub/running/running-data-exports/Strava/activities.csv")

#################################
### Prepare data for analysis ###
#################################

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
         MovingTime_m = MovingTime_s/60,
         Pace_minskm = MovingTime_m/Distance_km,
         DaysSinceRun = ceiling(difftime(max(ActivityDate), ActivityDate, units = c("days"))),
         YearsSinceRun = as.integer(floor(DaysSinceRun)/365)) %>%
  # filter to just runs from 2016 onwards
  filter(ActivityType == 'Run', year(ActivityDate) >= 2016)


# annual run statistics
annual_stats <- run_metrics %>%
  mutate(Year = year(ActivityDate)) %>%
  group_by(Year) %>%
  summarise(Runs = n(), 
            TotalDistance = sum(Distance_km),
            AvgDistance = mean(Distance_km, na.rm = TRUE),
            MaxDistance = max(Distance_km),
            TotalMovingTime_hrs = sum(MovingTime_m)/60,
            AvgMovingTime = mean(MovingTime_m, na.rm = TRUE),
            TotalElevation = sum(Elevation),
            AvgElevation = mean(Elevation, na.rm = TRUE),
            AvgPace = (TotalMovingTime_hrs/60)/TotalDistance,
            AvgCadence = mean(CadenceAvg, na.rm = TRUE))


######################
### Visualisations ###
######################


### plot distance of runs per week by year
p1 <- ggplot(data = run_metrics, aes(x = week(ActivityDate), y = Distance_km, fill = as.factor(year(ActivityDate)))) +
  geom_col() +
  scale_fill_jco() + # ggsci palette
  # scale_fill_manual(values = wes_palette("Darjeeling2", n = 5)) + # wesanderson palette
  theme(legend.position = "none") +
  labs(x = "Week of the year", y = "Running Distance (km)", title = "Weekly running distance by year") +
  facet_wrap(~ year(ActivityDate), ncol = 2)
p1
  
### distribution of run distances
ggplot(data = run_metrics, aes(x = Distance_km)) +
  geom_histogram(binwidth = 0.25) + 
  labs(x = "Distance (km)", y = "Runs", title = "Run distances")

# add wrap by year
p2 <- ggplot(data = run_metrics, aes(x = Distance_km, fill = as.factor(year(ActivityDate)))) +
  geom_histogram(binwidth = 0.5) + 
  # lims(x = c(0, 30)) +
  theme(legend.position = "none") +
  labs(x = "Distance (km)", y = "Runs", title = "Run distances") +
  facet_wrap(~ year(ActivityDate), ncol = 2)


### distribution of run pace
ggplot(data = run_metrics, aes(x = Pace_minskm)) +
  geom_histogram(binwidth = 0.1) + 
  lims(x = c(3, 10)) + 
  labs(x = "Running pace (mins per km)", y = "Runs", title = "Run pace")

# add wrap by year
p3 <- ggplot(data = run_metrics, aes(x = Pace_minskm, group = year(ActivityDate), fill = as.factor(year(ActivityDate)))) +
  geom_density(alpha = 0.3) + 
  lims(x = c(3, 10)) +
  labs(x = "Running pace (mins per km)", y = "Runs", title = "Run pace", fill = "Year")

# try again as a boxplot
p4 <- ggplot(data = run_metrics, aes(x = year(ActivityDate), y = Pace_minskm, fill = as.factor(year(ActivityDate)))) +
  geom_boxplot() + 
  lims(y = c(3, 12)) +
  labs(x = "Year", y = "Running pace (mins per km)", title = "Run pace", fill = "Year")
p4

### add plots together ###

# This great article helped me with this bit
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

ggarrange(p1, p2, p3, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
