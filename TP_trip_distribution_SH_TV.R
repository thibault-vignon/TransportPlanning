##############################################################################################-
# SKELETON PART 2
##############################################################################################-

# Set up ------------------------------------------------------------------

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it
require("AER") # package for vif test
require("stargazer") # package to turn your regression results into Latex-coded table
require("xtable")

# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("data_assignment.RData"))



# 2 Cleaning - same data cleaning as before -------------------------

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

#want started in and ended in to reflect trip as a whole, rather than the main mode segment
temp <- trips %>% group_by(trip_id) %>% 
  arrange(started_at) %>% 
  summarise(trip_started_in=first(start_bzname), trip_ended_in=last(end_bzname))

#create trips_aggregated df that contains only one line per trip for the mainmode 
trips_aggregated <- trips %>%
  filter(trip_mainmode == TRUE) %>%
  distinct(trip_id, .keep_all = TRUE) %>%
  left_join(temp, by = c("trip_id"))

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



# 3 Trip distribution -----------------------------------------------------

## Free tutorial on loops in R
## https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE




# create and look at the matrix of trips 
od <- trips_aggregated %>%
  select(participant_id, start_bzname, end_bzname) %>% #reduce the size of the dataframe by only taking what you need
  group_by(start_bzname, end_bzname) %>% # group/split data frame into little ones, iterate through those
  count() %>% ungroup() %>% # for each little group, count trips
  pivot_wider(id_cols = start_bzname, names_from = end_bzname, values_from = n) %>% # "open" the two column dataframe into a matrix
  mutate(across(.cols = -start_bzname, .fns = ~ replace_na(., 0))) # replace "NA" with "0"




# Heatmaps on observed data
od = data.frame(subset(od, select = -c(start_bzname)))

rownames(od) <- colnames(od)

print(xtable(od, type = "latex"), file = "od.tex")

trip_dist <- data.frame(od) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")


# ggplot2 combo
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2
#https://stackoverflow.com/questions/67010741/single-option-in-scale-fill-stepsn-changes-color-rendering-in-legend
trip_dist %>%
  mutate(nr_trips = floor(nr_trips)) %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(breaks = c(0,50,100,250,500,1000,2500,5000),
                    colours = rev(heat.colors(7, alpha = 0.8)),
                    values = scales::rescale(c(0,50,100,250,500,1000,2500,5000)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "# Trips")


trip_dist<-t(apply(od,1, function(x) x/sum(x)))

trip_dist <- data.frame(trip_dist) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")

trip_dist %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(breaks = c(0,0.1,0.2,0.3,0.4,0.5),
                    colours = rev(heat.colors(7, alpha = 0.8)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "Fraction of trips from origin")





# create and look at the matrix of trips 
od1 <- trips_aggregated %>%
  select(participant_id, trip_started_in, trip_ended_in) %>% #reduce the size of the dataframe by only taking what you need
  group_by(trip_started_in, trip_ended_in) %>% # group/split data frame into little ones, iterate through those
  count() %>% ungroup() %>% # for each little group, count trips
  pivot_wider(id_cols = trip_started_in, names_from = trip_ended_in, values_from = n) %>% # "open" the two column dataframe into a matrix
  mutate(across(.cols = -trip_started_in, .fns = ~ replace_na(., 0))) # replace "NA" with "0"




# Same heatmaps but with all trip instead of main mode
od1 = data.frame(subset(od1, select = -c(trip_started_in)))

rownames(od1) <- colnames(od1)

print(xtable(od, type = "latex"), file = "od1.tex")

trip_dist <- data.frame(od1) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")


# ggplot2 combo
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2
#https://stackoverflow.com/questions/67010741/single-option-in-scale-fill-stepsn-changes-color-rendering-in-legend
trip_dist %>%
  mutate(nr_trips = floor(nr_trips)) %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(breaks = c(0,50,100,250,500,1000,2500,5000),
                    colours = rev(heat.colors(7, alpha = 0.8)),
                    values = scales::rescale(c(0,50,100,250,500,1000,2500,5000)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "# Trips")


trip_dist<-t(apply(od1,1, function(x) x/sum(x)))

trip_dist <- data.frame(trip_dist) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")

trip_dist %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(breaks = c(0,0.1,0.2,0.3,0.4,0.5),
                    colours = rev(heat.colors(7, alpha = 0.8)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "Fraction of trips from origin")





# create a little zones dataset
zones <- trips_aggregated %>%
  ungroup() %>% 
  select(zone_nr = start_bznr, zone_name = start_bzname) %>% # reducing size of dataset by selecting only what we want AND renaming variables
  distinct() %>% # only keep unique zones, no repeats
  arrange(zone_name) # sort zones by name, alphabetically 
  

# 4.2 Step 1
# creating a dataframe that counts and displays the number of trips begin in each zone, as in our dataset (not a model! Just descriptives!)
production <- trips_aggregated %>% 
  group_by(start_bzname) %>%
  summarise(production = n()) %>%
  rename(zone_name = start_bzname) %>%
  left_join(zones, by = c("zone_name")) %>%
  relocate(zone_nr, .after = "zone_name")

#4.2 Step 2
# creating a dataframe that counts and displays the number of trips that end in each zone, as in our dataset (not a model! Just descriptives!)
attraction <- trips_aggregated %>%
  group_by(end_bzname) %>%
  summarise(attraction = n()) %>%
  rename(zone_name = end_bzname) %>%
  left_join(zones, by = c("zone_name")) %>%
  relocate(zone_nr, .after = "zone_name")

# check that we don't have trips without an end or without a begining
sum(production$production) == sum(attraction$attraction)


#4.2 Step 3
#Calculating Generalized Costs between the zones in terms of travel time
tt <- trips_aggregated %>%
  select(trip_id, duration = duration, start_bzname, end_bzname) %>%
  mutate(duration = duration/60) %>%
  group_by(start_bzname, end_bzname) %>%
  summarise(generalizedTravelCost = 1/(round(mean(duration), 2))^2) %>% # this treatment of travel time is from 4.2 step 3! 
  ungroup() %>%
  pivot_wider(id_cols = start_bzname, names_from = end_bzname, values_from = generalizedTravelCost) %>%
  mutate(across(.cols = -start_bzname, .fns = ~ replace_na(., 0))) %>% select(-start_bzname) %>%
  as.data.frame()

str(tt)
zones$zone_name

# iterative proportional fitting IPF aka the Furness Method 
ipf <- function(zones_df,outgoing,incoming,gcosts_df){
  
  # number of zones
  nr_z = nrow(zones_df) 
  
  # balancing factors vectors
  alpha_origin = rep(0, nr_z)
  alpha_destination = rep(0, nr_z)
  
  # define the minimal error for the balancing factors
  eps = 0.000001 
  
  # row and column totals 
  rowTotals = rep(0, nr_z)
  colTotals = rep(0, nr_z)
  
  # iteration
  nr_it = 0 
  
  # matrix with flows
  trip_dist = gcosts_df
  #colnames(trip_dist) <- seq(1,nr_z)
  
  # Compute the flows, nested for loop
  while ((any(abs(1 - alpha_destination) > eps)) | (any(abs(1 - alpha_origin) > eps))) {
    
    nr_it <- nr_it + 1
    
    # Adjust by origin (rows)
    rowTotals <- apply(trip_dist, 1, sum)
    for (z in 1:nr_z){
      alpha_origin[z] <- outgoing[z]/rowTotals[z]
    }
    trip_dist <- sweep(trip_dist, 1, alpha_origin, "*")
    
    # Adjust by destination (columns)
    colTotals <- apply(trip_dist, 2, sum)
    for (z in 1:nr_z){
      alpha_destination[z] <- incoming[z]/colTotals[z]
    }
    trip_dist <- sweep(trip_dist, 2, alpha_destination, "*")
  }
  
  distr <- list("trip_dist" =trip_dist, "nr_it" = nr_it, "alpha_origin" = alpha_origin, "alpha_destination" = alpha_destination)
  return(distr)
}

debug(ipf) #run this (aka ctrl-enter, as normal) to enter debug mode
undebug(ipf)
distribution <- ipf(zones, production$production, attraction$attraction, tt)

distribution$trip_dist
distribution$nr_it
distribution$alpha_origin
distribution$alpha_destination

rownames(distribution$trip_dist) <- colnames(distribution$trip_dist)

distribution$trip_dist <- distribution$trip_dist %>% mutate_all(funs(round(., 0)))

print(xtable(distribution$trip_dist, type = "latex"), file = "odmodel.tex")

trip_dist <- data.frame(distribution$trip_dist[1:12,1:12]) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")


# ggplot2 combo
#https://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2
#https://stackoverflow.com/questions/67010741/single-option-in-scale-fill-stepsn-changes-color-rendering-in-legend
trip_dist %>%
  mutate(nr_trips = floor(nr_trips)) %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(breaks = c(0,50,100,250,500,1000,2500,5000),
                    colours = rev(heat.colors(7, alpha = 0.8)),
                    values = scales::rescale(c(0,50,100,250,500,1000,2500,5000)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "# Trips")



trip_dist<-t(apply(distribution$trip_dist,1, function(x) x/sum(x)))

trip_dist <- data.frame(trip_dist) %>%
  rownames_to_column() %>% as_tibble() %>%
  rename(Origin = rowname) %>%
  pivot_longer(cols = -Origin, names_to = "Destination", values_to = "nr_trips")

trip_dist %>%
  ggplot(aes(x = Destination, y = Origin, fill = nr_trips)) +
  geom_tile() +
  scale_x_discrete(position = "top") +
  scale_fill_stepsn(colours = rev(heat.colors(7, alpha = 0.8)),
                    show.limits = T) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1)) +
  labs(fill = "Fraction of trips from origin")

