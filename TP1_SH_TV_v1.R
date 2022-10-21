##############################################################################################-
## TRANSPORT PLANNING ASSIGNMENT PART 1: DESCRIPTIVE ANALYSIS
# Summary: Descriptive analysis of trip data in Switzerland
#Author: Sanelma Heinonen and Thibault Vignon
#Date: October 2022
##############################################################################################-

# Set up ------------------------------------------------------------------

rm(list=ls()) 

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it

# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
#load(paste0("Library/Mobile Documents/com~apple~CloudDocs/Documents/ETH Sem 1/Transport Planning/","data_assignment.RData"))
load(paste0("C:/Users/Thibault Vignon/OneDrive/Desktop/ETHZ/Transport Planning Methods/","data_assignment.RData"))

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
  labs(y = "Average trips per person", 
       title = "Average trips per person by day of the week") +
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
         #combine "prefer not to answer" and missing into one category -- is this best? -- I don't think so
         #mutate(income = ifelse(is.na(income), 99, income)) %>% 
         drop_na(income) %>%
         group_by(participant_id, income) %>%
         summarise(total_trips = n()),
       aes(x = factor(income), y = total_trips)) + 
  geom_boxplot() +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 99), 
                     labels = c("<4k", "4k - 8k", "8k - 12k", "12k - 16k", ">16k", "Prefer not to say")) +
  labs(x = "Income (CHF)", 
       y = "Total weekly trips", 
       title = "Distribution of number of trips by household income", 
       caption = "Source: One week of 2019 MOBIS data") +
  custom_theme


#household size and income
#will look at per capita income

ggplot(trips_aggregated %>% 
         #combine "prefer not to answer" and missing into one category -- is this best? 
         #Thibault : I think it's better to get rid of na values entirely
         #mutate(income = ifelse(is.na(income), 99, income)) %>% 
         drop_na(income) %>%
         drop_na(household_size) %>%
         group_by(participant_id, income, household_size) %>%
         mutate(income= case_when(income == 1 ~ "<4k", 
                                  income == 2 ~"4k - 8k", 
                                  income == 3 ~"8k - 12k", 
                                  income == 4 ~"12k - 16k",
                                  income == 5 ~">16k", 
                                 TRUE ~ "Prefer not to say")) %>%
         summarise(total_trips = n()),
       aes(x = factor(household_size), y = total_trips, fill = income)) + 
  geom_boxplot() +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5), 
                   labels = c("1", "2", "3", "4", "5 or more")) +
  labs(x = "Household size", 
       y = "Total weekly trips", 
       title = "Number of trips by household income and size", 
       caption = "Source: One week of 2019 MOBIS data") +
  custom_theme


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
                          TRUE ~ "Other")) %>%
  ggplot(aes(x = trip_int, y = n, fill = phase)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  theme_bw() +
  labs(x = "Time of Day", y = "Total recorded trips")



# 4 socio-demographics ----------------------------------------------------


# Age distribution of participants

participants %>%
  select(participant_id, age) %>%
  drop_na(age) %>%
  group_by(age) %>%
  count() %>%
  ggplot(aes(x = age, y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_continuous(breaks = seq(18, 68, 5)) +
  labs(x = "Age", y = "Number of participants")


#... separated by sex

participants %>%
  select(participant_id, age, sex) %>%
  drop_na(age) %>%
  drop_na(sex) %>%
  group_by(age,sex) %>%
  count() %>%
  mutate(sex= case_when(sex == 1 ~ "Male", 
                          TRUE ~ "Female")) %>%
  mutate(age = case_when(between(age, 18, 25) ~ "18-25", 
                            between(age, 26, 40) ~ "26-40",
                            between(age, 41, 55) ~ "41-55",
                            TRUE ~ "56+")) %>%
  ggplot(aes(x = age, y = n, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Number of participants")


# Education level distribution

participants %>%
  drop_na(education) %>%
  group_by(education) %>%
  count() %>%
  mutate(education= case_when(education == 1 ~ "Only mandatory education", 
                              education == 2 ~ "Secondary education",
                              education == 3 ~ "Higher education",
                        TRUE ~ "Other")) %>%
  ggplot(aes(x = education, y = n)) +
  scale_x_discrete(limits = c("Only mandatory education", "Secondary education", "Higher education")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Education level", y = "Number of participants")


# .. by employment status

participants %>%
  drop_na(education) %>%
  drop_na(employment_1) %>%
  group_by(education, employment_1) %>%
  count() %>%
  mutate(education= case_when(education == 1 ~ "Only mandatory education", 
                              education == 2 ~ "Secondary education",
                              education == 3 ~ "Higher education",
                              TRUE ~ "Other")) %>%
  mutate(employment_1= case_when(employment_1 == 10 ~ "Employed", 
                              employment_1 == 20 ~ "Self-employed",
                              employment_1 == 30 ~ "Apprentice",
                              employment_1 == 40 ~ "Unemployed",
                              employment_1 == 50 ~ "Student",
                              employment_1 == 70 ~ "Retired",
                              TRUE ~ "Other")) %>%
  ggplot(aes(x = employment_1, y = n, fill = education)) +
  scale_x_discrete(limits = c("Apprentice", "Student", "Employed", "Self-employed", "Unemployed", "Retired", "Other")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Employment status", y = "Number of participants")


# Income distribution

participants %>%
  drop_na(income) %>%
  group_by(income) %>%
  count() %>%
  mutate(income= case_when(income == 1 ~ "<4k", 
                           income == 2 ~"4k - 8k", 
                           income == 3 ~"8k - 12k", 
                           income == 4 ~"12k - 16k",
                           income == 5 ~">16k", 
                           TRUE ~ "Prefer not to say")) %>%
  ggplot(aes(x = income, y = n)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Household income", y = "Number of participants")
