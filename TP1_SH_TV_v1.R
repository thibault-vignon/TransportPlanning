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
  summarise(mode_total = sum(length)) %>%
  filter(mode_total == max(mode_total)) %>%
  mutate(trip_mainmode = TRUE)

#note: for trip id 11861141, walk and tram both had the same distance. should probably just choose one of these? 
#note: when a trip has multiple legs that both use the mainmode, all of these legs will be marked as mainmode true


# left-join the trips_mainmode df to the existing df
#impute FALSE for trip_mainmode when not the mainmode
trips <- trips %>%
  left_join(trips_mainmode, by = c("trip_id", "mode_agg")) %>%
  mutate(trip_mainmode = ifelse(is.na(trip_mainmode), FALSE, trip_mainmode))




# 3 travel behaviour ------------------------------------------------------

# only look at main mode legs of the trips

#Note: since trips that have multiple legs using the main mode are all marked as main mode, should probably
       #aggregate or filter in another way before filtering trip_mainmode = TRUE
trips <- trips %>%
  filter(??? == TRUE)

# join the datasets 
trips <- trips %>%
  left_join(participants %>% select(???), by = c(???))

# rest: your turn ;-)


# 4 socio-demographics ----------------------------------------------------

# your turn ;-)


















