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
         trip_duration = sum(duration))

# join trips to participants 
trips <- trips %>%
  left_join(participants, by = c("participant_id"))



# 3 travel behaviour ------------------------------------------------------

# only look at main mode legs of the trips

#Note: since trips that have multiple legs using the main mode are all marked as main mode, I selected only distinct trip_ids 
#since i created variables for mode_total_length and mode_total duration, these will have the combined info for all segments completed with mainmode of transport
trips <- trips %>%
  filter(trip_mainmode == TRUE) %>%
  distinct(trip_id, .keep_all = TRUE)

#day of week

#setting levels so appear in logical order on plot
trips$weekday = factor(trips$weekday, 
                       levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

#plotting number of trips by weekday
ggplot(trips, 
       aes(x = weekday)) + 
  geom_bar() + 
  labs(y = "# recorded trips") +
  theme_bw()

#number of trips related to household income

#Income is a factor variable
#will show boxplot of distribution of total number of trips per person by household income 

ggplot(trips %>% 
         group_by(participant_id, income) %>%
         summarise(total_trips = n()),
       aes(x = factor(income), y = total_trips)) + 
  geom_boxplot() + 
  theme_bw()



# rest: your turn ;-)


# 4 socio-demographics ----------------------------------------------------

# your turn ;-)


















