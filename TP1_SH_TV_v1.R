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
library(scales)

# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("data_assignment.RData"))
#load(paste0("C:/Users/Thibault Vignon/OneDrive/Desktop/ETHZ/Transport Planning Methods/","data_assignment.RData"))

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


trips <- trips %>% mutate(date = round_date(as.Date(started_at)), "day")

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
  theme(plot.title = element_text(hjust = 0.5, margin = margin(10, 10, 5, 10)), 
        plot.subtitle = element_text(hjust = 0.5, size = 11),
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
  labs(y = "Number trips",
       x = "Weekday",
       title = "Total number of trips by day of week in Canton Zurich", 
       subtitle = "Over total MOBIS Fall 2019 data",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white") +
  custom_theme

#since number of trips is a function of number of people tracked and the number of days tracked over, we plo
#plotting avg trips per person per weekday
ggplot(trips_aggregated %>% 
         group_by(participant_id, weekday) %>% 
         summarise(n = n()) %>% 
         group_by(weekday) %>%
         summarise(avg = mean(n)), 
       aes(x = weekday, y = avg)) + 
  labs(y = "Average trips per person", 
       title = "Average trips per person per day in Canton Zurich by day of week",
       subtitle = "MOBIS Fall 2019 data",
       x = "Weekday",
      caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  geom_col() +
  geom_text(aes(label = round(avg, 1)), vjust = 1.5, color = "white") +
  custom_theme



#number of trips related to household income

##Note: Should look into adding weights

#Income is a factor variable
#will show boxplot of distribution of total number of trips per person by household income 

#could plot weekday vs weekend day
#participants were tracked on varying numbers of days, so will do average daily trips
ggplot(trips_aggregated %>% 
         #mutate(income = ifelse(is.na(income), 99, income)) %>% 
         drop_na(income) %>%
         group_by(participant_id, income) %>%
         summarise(total_trips = n(), 
                   avg_daily_trips = total_trips/n_distinct(date)) %>%
         #filtered for only people who took more than 10 trips total
         filter(total_trips > 10),
       aes(x = factor(income), y = avg_daily_trips)) + 
  geom_boxplot() +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 99), 
                     labels = c("<4k", "4k - 8k", "8k - 12k", "12k - 16k", ">16k", "Prefer not to say")) +
  labs(x = "Monthly household income (CHF)", 
       y = "Average daily trips", 
       title = "Average number of daily trips by household income",
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' \n Removed those with missing income data and less than 10 total trips.") +
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
         summarise(total_trips = n(), 
                   avg_daily_trips = total_trips/n_distinct(date)) %>%
         filter(total_trips > 10),
       aes(x = factor(household_size), y = avg_daily_trips, fill = income)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(breaks = c(1, 2, 3, 4, 5), 
                   labels = c("1", "2", "3", "4", "5 or more")) +
  scale_y_continuous(limits = c(0, 10))+
  scale_fill_brewer(name = "Monthly household \nincome (CHF)", palette = "Set2")+
  labs(x = "Household size", 
       y = "Average daily trips", 
       title = "Average daily trips by household income and size", 
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' \n Removed those with missing income data and less than 10 total trips.") +
  custom_theme


ggplot(trips_aggregated %>% 
         #combine "prefer not to answer" and missing into one category -- is this best? 
         #Thibault : I think it's better to get rid of na values entirely
         #mutate(income = ifelse(is.na(income), 99, income)) %>% 
         drop_na(age) %>%
         group_by(participant_id, age, income) %>%
         summarise(total_trips = n(), 
                   avg_daily_trips = total_trips/n_distinct(date)) %>%
         filter(total_trips > 10),
       aes(x = age, y = avg_daily_trips)) + 
  geom_point() +
  scale_y_continuous(limits = c(0, 10))+
#  scale_fill_brewer(name = "Monthly household \nincome (CHF)")+
  labs(x = "Age", 
       y = "Average daily trips", 
       title = "Average daily trips by age", 
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' \n Removed those with missing age and less than 10 total trips.") +
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
  scale_fill_brewer(palette = "Set2") + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = seq(0, 24, 1)) +
  labs(x = "Time of day", 
       y = "Number of trips", 
       title = "Distribution of trips by time of day", 
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' ") +
  custom_theme +
  theme(legend.title = element_blank())


#edit

  
#### distance traveled
  
verticals <- trips_aggregated %>% 
  filter(!is.na(income)) %>%
  mutate(income = case_when(income == 1 ~ "<4k", 
                            income %in% c(2, 3, 4, 5) ~ ">4k",
                            income == 99 ~ "Prefer not to answer")) %>%
  group_by(income) %>% 
  summarise(med = median(trip_length)/1000) %>%
  mutate(x = c(4.2, 4.2, 2.2), 
         y = c(4000, 3700, 4000))


ggplot(trips_aggregated, aes(x = trip_length/1000)) + 
  geom_histogram(binwidth = 1) +
#  facet_grid(rows = vars(income))+
  scale_x_continuous(limits = c(0, 21))+
  geom_vline(data = verticals, 
             aes(xintercept = med, color = factor(income))) +
#  geom_text(data = verticals, 
 #           aes(x = x, y = y, label = round(med, 1), color = factor(income)), 
  #          show.legend = FALSE)+
#  scale_color_brewer(palette = "Set1", 
 #                    name = "Monthly household \nincome (CHF)") + 
  
  labs(x = "Trip distance (km)", 
       y = "Number of trips", 
       title = "Distribution of trip lengths", 
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' \n Removed those with missing income data. Not showing trips over 20km in length.") +
  custom_theme



ggplot(trips_aggregated %>% 
         filter(!is.na(income)) %>%
         mutate(income= case_when(income == 1 ~ "<4k", 
                                  income == 2 ~"4k - 8k", 
                                  income == 3 ~"8k - 12k", 
                                  income == 4 ~"12k - 16k",
                                  income == 5 ~">16k", 
                                  TRUE ~ "Prefer not to say")),
       aes(x = trip_length/1000, y= factor(income))) + 
  geom_boxplot(binwidth = 1, show.legend = FALSE) +

  scale_x_continuous(limits = c(0, 21))+

  scale_fill_brewer() + 
  
  labs(x = "Trip distance (km)", 
       y = "Monthly household income (CHF)", 
       title = "Distribution of trip lengths by household income", 
       subtitle = "Canton Zurich, 2019",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData' \n Removed those with missing income data. Not showing trips over 20km in length.") +
  custom_theme

#average trip length is around 7km
mean(trips_aggregated$trip_length)



### mode choice

#df with percentages by group
labels <- trips_aggregated %>%
  mutate(daytype = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"), 
         mode_agg = factor(mode_agg, levels = c("Walk", "PT", "Car", "Bicycle"))) %>%
  group_by(daytype, mode_agg) %>%
  summarise(n= n()) %>%
  group_by(daytype) %>% 
  mutate(perc = n/sum(n), 
         y = cumsum(perc)-0.5*perc)

ggplot(trips_aggregated %>%
         mutate(daytype = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")),
         aes(x = daytype, fill = mode_agg)) + 
  geom_bar(position = "fill") +
  geom_text(data = labels, 
            aes(x = daytype, y = y, label = percent(perc, accuracy = 1)))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = "Pastel2", name = "Mode")+

  labs(title = "Mode type by weekday vs weekend", 
       subtitle = "Canton Zurich, 2019", 
       x = "", 
       y = "Percent of trips", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'\nn = 18999 for weekday, n = 5381 for weekend.") +
  custom_theme


#### mode choice by income

labels <- trips_aggregated %>%
  filter(!is.na(income)) %>%
  mutate(daytype = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"), 
                          income = case_when(income == 1 ~ "<4k", 
                                             income == 2 ~"4k - 8k", 
                                             income == 3 ~"8k - 12k", 
                                             income == 4 ~"12k - 16k",
                                             income == 5 ~">16k", 
                                             TRUE ~ "Prefer not to say"), 
         mode_agg = factor(mode_agg, levels = c("Walk", "PT", "Car", "Bicycle"))) %>%
  group_by(daytype, income, mode_agg) %>%
  summarise(n= n()) %>%
  group_by(daytype, income) %>% 
  mutate(total = sum(n), 
         perc = n/total, 
         y = cumsum(perc) - 0.5*perc) 



ggplot(trips_aggregated %>%
         filter(!is.na(income)) %>%
         mutate(daytype = ifelse(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday"), 
                income = case_when(income == 1 ~ "<4k", 
                                                     income == 2 ~"4k - 8k", 
                                                     income == 3 ~"8k - 12k", 
                                                     income == 4 ~"12k - 16k",
                                                     income == 5 ~">16k", 
                                                     TRUE ~ "Prefer not to say")),
       aes(x = factor(income), fill = mode_agg)) + 
  geom_bar(position = "fill") +
  facet_grid(cols = vars(daytype)) +
  geom_text(data = labels, size = 3.5,
            aes(x = income, y = y, label = percent(perc, accuracy = 1)))+
  geom_text(data = labels, vjust = -0.5, size = 3.5, 
            aes(x = income, y = 1, label = total)) +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette = "Set2", name = "Mode")+
  
  labs(title = "Mode type by income bracket, weekday vs weekend", 
       subtitle = "Canton Zurich, 2019", 
       x = "Monthly household income (CHF)", 
       y = "Percent of trips", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'\nRploved those with missing income data.") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 20), 
        strip.text.x = element_text(size = 11))




=======
  labs(x = "Time of Day", y = "Total recorded trips")
>>>>>>> 76dffd8c8a5ae61356939a1a84a7512d32ee50b8



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
  labs(x = "Age", 
       y = "Number of participants", 
       title = "Age distribution of participants in MOBIS data", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme


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
  scale_fill_brewer(palette = "Set1", name = "Sex") +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Age",
       y = "Number of participants", 
       title = "Age and gender distribution of participants in MOBIS data", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme

#population pyramid

swiss_pop <-
  Switzerland_2018 %>%
  mutate(Male = M*-1, 
         Female = F) %>% 
  filter(! Age %in% c("0-4", "5-9", "10-14", "15-19", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>%
  select(-M, -F) %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "sex", values_to = "population") %>%
  mutate(pop = "Swiss population") %>%
  rename(age = Age)
 

participants %>%
  group_by(sex, age) %>%
  mutate(age = case_when(age <= 19 ~ "19",
                         age <= 24 ~ "20-24", 
                         age <= 29 ~ "25-29", 
                         age <= 34 ~ "30-34", 
                         age <= 40 ~ "35-39",
                         age <= 44 ~ "40-44", 
                         age <= 49 ~ "45-49", 
                         age <= 54 ~ "50-54", 
                         age <= 59 ~ "55-59", 
                         age <= 64 ~ "60-64", 
                         age > 64 ~ "65-66")) %>%
  filter(!is.na(age)) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  summarise(n=n()) %>%
  mutate(population = ifelse(sex=="Male", n*(-1),
                      n*1))%>%
  mutate(pop = "Participants in MOBIS dataset") %>%
  bind_rows(swiss_pop) %>%
  ggplot(aes(x = age, y = population, fill=sex)) + 
  facet_grid(cols = vars(pop), scales = "free_x") + 
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_brewer(palette  = "Set1") + 
  labs(x = "Age",
       y = "Count", 
       title = "Age and gender distribution", 
       caption = "Source: Fall 2019 MOBIS data from Transport Planning Methods 'HS22 data_assignment.RData'\nSwiss population data from 2018 United Nations Population Prospects") +
  custom_theme +
  theme(legend.title = element_blank()) + 
  theme(strip.text.x = element_text(size = 11))



Switzerland_2018 %>%
  mutate(Male = M*-1, 
         Female = F) %>% 
  filter(! Age %in% c("0-4", "5-9", "10-14", "15-19", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")) %>%
  select(-M, -F) %>%
  pivot_longer(cols = c("Male", "Female"), names_to = "sex", values_to = "population") %>%
  ggplot(aes(x = Age, y = population, fill=sex)) + 
  geom_bar(stat = "identity") +
  coord_flip()+
  scale_fill_brewer(palette  = "Set1") + 
  scale_y_continuous(labels = c(-200, 0, 200), breaks = c(-200000, 0, 200000))+
  labs(x = "Age",
       y = "Number of people (1000s)", 
       title = "Age and gender distribution of Swiss population", 
       caption = "Source: data from 2018 United Nations Population Prospects") +
  custom_theme +
  theme(legend.title = element_blank())



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

labels <- 
  participants %>%
  drop_na(education) %>%
  drop_na(employment_1) %>%
  #  group_by(education, employment_1) %>%
  # count() %>%
  mutate(employment_1= case_when(employment_1 == 10 ~ "Employed", 
                                 employment_1 == 20 ~ "Self-employed",
                                 employment_1 == 30 ~ "Apprentice",
                                 employment_1 == 40 ~ "Unemployed",
                                 employment_1 == 50 ~ "Student",
                                 employment_1 == 70 ~ "Retired",
                                 TRUE ~ "Other")) %>%
  group_by(employment_1) %>%
  summarise(n = n())
  
  
  
participants %>%
  drop_na(education) %>%
  drop_na(employment_1) %>%
#  group_by(education, employment_1) %>%
 # count() %>%
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
  mutate(education = factor(education, levels = c("Only mandatory education", "Secondary education", "Higher education") ))%>%
  ggplot(aes(x = employment_1)) +
  scale_x_discrete(limits = c("Employed", "Self-employed", "Student","Retired", "Unemployed", "Apprentice",  "Other")) +
  geom_bar(aes(fill = education), position = "fill") +
  scale_fill_brewer(name = "Education", palette = "Set1", trans = "reverse")+
  scale_y_continuous(labels = scales::percent)+
  geom_text(data = labels, 
            aes(y = 1, x = employment_1, label = n), 
            vjust = -0.5)+
  labs(x = "Employment status",
       y = "Percent of participants", 
       title = "Distribution of employment status and education for MOBIS participants",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme+
  theme(axis.text.x = element_text(angle = 20))


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
  labs(x = "Monthly household income (CHF)", y = "Number of participants", 
       title = "Income distribution of MOBIS participants",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme




#each person only recorded on maximum of seven days
#however, lots of people who only recorded on less than 7 days - maybe worth rescaling these?
trips_aggregated %>% group_by(participant_id) %>% 
  summarise(n_days = n_distinct(day(started_at))) %>%
  group_by(n_days) %>%
  summarise(n = n())

#lists that contain the participants who have trip data from a Saturday, Sunday, and Friday
participants_w_sat_data <- (trips_aggregated %>% filter(weekday == "Sat"))$participant_id
participants_w_sun_data <-(trips_aggregated %>% filter(weekday == "Sun"))$participant_id
participants_w_fri_data <- (trips_aggregated %>% filter(weekday == "Fri"))$participant_id

#df with the number of distinct days of the workweek (excluding Friday), that we recorded data per participant
n_weekdays_per_participant <- trips_aggregated %>%
  filter(weekday %in% c("Mon", "Tue", "Wed", "Thu")) %>%
  group_by(participant_id) %>%
  summarise(n_weekdays = n_distinct(weekday), 
            n_weekday_trips = n(), 
            avg_daily_trips_weekday = n_weekday_trips/n_weekdays)

#weekly trips per participant dataframe: includes info on the total weekly trips a participant took, as well as which days they were taken on 
weekly_trips_per_participant <- trips_aggregated %>% 
  group_by(participant_id) %>% 
  summarise(n_trips = n(),
            n_days = n_distinct(day(started_at)), 
            saturday_data = participant_id %in% participants_w_sat_data, 
            sunday_data = participant_id %in% participants_w_sun_data, 
            friday_data = participant_id %in% participants_w_fri_data) %>%
  distinct() %>%
  left_join(n_weekdays_per_participant, by = c("participant_id")) %>%
  replace_na(list(n_weekdays = 0))

#for analysis on predicting weekly trips for an individual, we will only use individuals for which we can get "decent" data for a week of trips
#since Fridays, Saturdays, and Sundays tend to look different, we will only use data from people who have these three days
#for workday trips, we will include individuals who have at least 2 workdays, and impute the other one or two days if needed as the average of the others
#we will exclude individuals who have data from one or zero workdays

weekly_trips_participant_clean <- weekly_trips_per_participant %>%
  filter(saturday_data, sunday_data, friday_data, n_weekday_trips > 1) %>%
  select(-saturday_data, -sunday_data, -friday_data) %>%
  mutate(n_trips_imputed = n_trips + (4 - n_weekdays)*avg_daily_trips_weekday)

#left with 453 people - hopefully enough for analysis

ggplot(weekly_trips_participant_clean, aes(x = n_trips_imputed)) + 
  geom_histogram()
#data looks relatively normal








test <- trips_aggregated %>% 
  group_by(participant_id) %>%
  mutate(n_days = n_distinct(day(started_at))) %>%
  filter(n_days == 7) %>%
  summarise(n_trips = n_distinct(trip_id))

ggplot(test, aes(x = n_trips)) + 
  geom_histogram()


cba <- data.frame(project = c("Tunnel", "Tax"), 
                  x = c(1, 2),
                  ratio = c(2.2, 0.27), 
                  min_costs = c(1.1, 0.05), 
                  max_costs = c(4.4, 0.5), 
                  min_discount = c(1.02, 0.27), 
                  max_discount = c(2.78, 0.27), 
                  min_scc = c(2.19, 0.22),
                  max_scc = c(2.22, 0.4),
                  min_tt = c(2.2, 0.27), 
                  max_tt = c(3.2, 0.27)
                
                  )


ggplot(cba, aes(x = x, y = ratio)) + 
  geom_col() + 
  geom_errorbar(aes(, 
                    ymin=min_costs, ymax=max_costs, color = "Benefit-cost ratio\nunder 50% to 200%  \ncost uncertainty"), width=0.1, 
                position=position_dodge(-10), size = 0.8) +
#  geom_errorbar(aes(x = x -0.1, ymin=min_discount, ymax=max_discount, color = "Discount rate"), width=.1,
#                position=position_dodge()) +
#  geom_errorbar(aes(x = x +0.1, ymin=min_scc, ymax=max_scc, color = "Social cost of carbon"), width=.1,
#                position=position_dodge()) +
#  geom_errorbar(aes(x = x + 0.3, ymin=min_tt, ymax=max_tt, color = "Value of travel time"), width=.1,
#                position=position_dodge()) +
  labs(x = "Project", y = "Benefit-cost ratio", 
       title = "Benefit-cost ratios with cost uncertainty ranges", 
       subtitle = "For proposed policies in canton Zürich") +
  scale_x_continuous(labels = c("Rail tunnel", "Congestion tax"), breaks = c(1, 2)) +
  scale_color_brewer(palette = "Set2" )+
  custom_theme + 
  theme(legend.title = element_blank(),
        legend.position = c(0.85,0.9), 
        legend.background = element_rect(color = "grey"))




ggplot(cba, aes(x = x, y = ratio)) + 
  geom_col() + 
  geom_errorbar(aes(x = x-0.3, 
                      ymin=min_costs, ymax=max_costs, color = "Cost"), width=0.1, 
                position=position_dodge(-10)) +
  geom_errorbar(aes(x = x -0.1, ymin=min_discount, ymax=max_discount, color = "Discount rate"), width=.1,
                position=position_dodge()) +
  geom_errorbar(aes(x = x +0.1, ymin=min_scc, ymax=max_scc, color = "Social cost of carbon"), width=.1,
                position=position_dodge()) +
  geom_errorbar(aes(x = x + 0.3, ymin=min_tt, ymax=max_tt, color = "Value of travel time"), width=.1,
                position=position_dodge()) +
  labs(x = "Project", y = "Benefit-cost ratio", 
       title = "Benefit-cost ratios with uncertainty ranges", 
       subtitle = "For proposed policies in canton Zürich", 
       caption = "Cost uncertainty: 50% to 200% of estimated costs\nDiscount rate uncertainty: 1.5% to 7% (US federal)\nSCC uncertainty: 50 CHF/ton (US federal) to 185 CHF/ton (Rennert et al. 2022)\nVTT uncertainty: 2009 Swiss norm to updated Swiss norm") +
  scale_x_continuous(labels = c("Rail tunnel", "Congestion tax"), breaks = c(1, 2)) +
  scale_color_brewer(palette = "Set2")+
  custom_theme + 
  theme(legend.title = element_blank(), 
        plot.caption = element_text(hjust = 0))




