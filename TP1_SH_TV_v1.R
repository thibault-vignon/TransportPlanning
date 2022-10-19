##############################################################################################-
## TRANSPORT PLANNING ASSIGNMENT PART 1: DESCRIPTIVE ANALYSIS
# Summary: Descriptive analysis of trip data in Switzerland
#Author: Sanelma Heinonen and Thibault Vignon
#Date: October 2022
##############################################################################################-

# Set up ------------------------------------------------------------------

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it


# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("Library/Mobile Documents/com~apple~CloudDocs/Documents/ETH Sem 1/Transport Planning/","data_assignment.RData"))


# 2 Cleaning --------------------------------------------------------------

# 1. get rid of odd naming
trips <- trips %>% mutate(mode = str_remove(mode, "Mode::"), 
                          mode_agg = str_remove(mode_agg, "Mode::"), 
                          length = floor(length), 
                          duration = floor(duration))




# 3. select the legs of each trip that is the longest of all legs in the same trip

#Note: I did this using suggestion on moodle
#ie, choose the mode that had the combined longest length out of all the modes used in the trip
trips_mainmode <- trips %>%
  group_by(trip_id, mode_agg) %>%
  summarise(mode_total_length = sum(length), 
            mode_total_duration = sum(duration)) %>%
  filter(mode_total_length == max(mode_total_length)) %>%
  mutate(trip_mainmode = TRUE)

#note: for trip id 11861141, walk and tram both had the same distance. should probably just choose one of these? 
#note: when a trip has multiple legs that both use the mainmode, all of these legs will be marked as mainmode true


# left-join the trips_mainmode df to the existing df
#impute FALSE for trip_mainmode when not the mainmode
trips <- trips %>%
  left_join(trips_mainmode, by = c("trip_id", "mode_agg")) %>%
  mutate(trip_mainmode = ifelse(is.na(trip_mainmode), FALSE, trip_mainmode))

#adding variables for total length and duration of trip, so can view these for trip as whole even when focusing in on mainmode
trips <- trips %>% 
  group_by(trip_id) %>% 
  mutate(trip_length = sum(length), 
         trip_duration = sum(duration), 
         trip_avg_speed = trip_length/trip_duration)  #in meters per second

# join trips to participants 
trips <- trips %>%
  left_join(participants, by = c("participant_id"))


trips <- trips %>% mutate(date = round_date(started_at))

#create trips_aggregated df that contains only one line per trip for the mainmode 
trips_aggregated <- trips %>%
  filter(trip_mainmode == TRUE) %>%
  distinct(trip_id, .keep_all = TRUE)

#investigate data and filter out some unreasonable trips
#filter out some unreasonable trips
trips_aggregated <- trips_aggregated %>% filter(
  #total trip duration is less than 30 seconds
  trip_duration > 30 &
    #total trip length to be under 80 km - aim to remain within canton zurich
    trip_length < 80000 &
    #speed should be under 150 km/hr (42 meters per second)
  trip_avg_speed < 42
)

#note: there may be some other unreasonable trips, such as walk with fast speeds, but we will leave this for now


#notes: incomes probably overrepresented, since we targetted people who owned cars


# 3 travel behaviour ------------------------------------------------------

custom_theme <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, margin = margin(10, 10, 10, 10)), 
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 12, 
                                margin = margin(10, 10, 10, 10)))


# only look at main mode legs of the trips

#Note: since trips that have multiple legs using the main mode are all marked as main mode, I selected only distinct trip_ids 
#since i created variables for mode_total_length and mode_total duration, these will have the combined info for all segments completed with mainmode of transport


#day of week

#note: there are more weeks recorded for tue/wed/thu
trips_aggregated %>% group_by(weekday) %>% summarise(n = n_distinct(weeknr))

#setting levels so appear in logical order on plot
trips_aggregated$weekday = factor(trips_aggregated$weekday, 
                       levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#plotting number of trips by weekday
ggplot(trips_aggregated, 
       aes(x = weekday)) + 
  geom_bar() + 
  labs(y = "Total recorded trips",
       x = "Weekday",
       title = "Number of trips by day of week", 
       caption = "Source: One week of 2019 MOBIS data") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white") +
  custom_theme

#plotting avg trips per person per weekday
ggplot(trips_aggregated %>% 
         group_by(participant_id, weekday) %>% 
         summarise(n = n()) %>% 
         group_by(weekday) %>%
         summarise(avg = mean(n)), 
       aes(x = weekday, y = avg)) + 
  labs(y = "Average trips per person per weekday", 
       title = "Average trips per person per weekday by day of week") +
  geom_text(aes(label = round(avg, 1)), vjust = -0.5) +
  geom_col() +
  custom_theme



#number of trips related to household income

##Note: Should look into adding weights

#Income is a factor variable
#will show boxplot of distribution of total number of trips per person by household income 

#maybe should do average number of trips by day instead
#could plot weekday vs weekend day

ggplot(trips_aggregated %>% 
         #combine "prefer not to answer" and missing into one category -- is this best?
         mutate(income = ifelse(is.na(income), 99, income)) %>% 
         group_by(participant_id, income) %>%
         summarise(total_trips = n()),
       aes(x = factor(income), y = total_trips)) + 
  geom_boxplot() +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 99), 
                     labels = c("<4k", "4k - 8k", "8k - 12k", "12k - 16k", ">16k", "Unknown")) +
  labs(x = "Income (CHF)", 
       y = "Total weekly trips", 
       title = "Distribution of number of trips by household income", 
       caption = "Source: One week of 2019 MOBIS data") +
  custom_theme


#household size and income
#will look at per capita income



# rest: your turn ;-)


# 4 socio-demographics ----------------------------------------------------

# your turn ;-)



#plotting by time of day with peak colored - 
#note: code from class, should double check and customize
trips_aggregated %>%
  select(participant_id, started_at) %>%
  mutate(trip_h = hour(started_at), 
         trip_m = minute(started_at), 
         trip_int = case_when(between(trip_m, 0, 29) ~ as.numeric(trip_h), 
                             between(trip_m, 30, 59) ~ as.numeric(trip_h, "0.5"))) %>%
  group_by(trip_int) %>%
  count() %>%
  mutate(phase= case_when(between(trip_int, 7, 8.9) ~ "Morning", 
                          between(trip_int, 16, 18.4) ~"Evening", 
                          TRUE ~ "Rest")) %>%
  ggplot(aes(x = trip_int, y = n, fill = phase)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  theme_bw() +
  labs(x = "Time of Day")















