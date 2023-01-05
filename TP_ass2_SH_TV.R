##############################################################################################-
## TRANSPORT PLANNING ASSIGNMENT PART 1: DESCRIPTIVE ANALYSIS
# Summary: Descriptive analysis of trip data in Switzerland
#Author: Sanelma Heinonen and Thibault Vignon
#Date: October 2022
##############################################################################################-

# Set up ------------------------------------------------------------------

rm(list = ls())

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it
library(scales)
library(car)
library(stargazer)
require("xtable")

#custom theme for ggplot
custom_theme <- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, margin = margin(10, 10, 5, 10)), 
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12, 
                                  margin = margin(10, 10, 10, 10)))

# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("data_assignment.RData"))
#load(paste0("C:/Users/Thibault Vignon/OneDrive/Desktop/ETHZ/Transport Planning Methods/","data_assignment.RData"))

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


#notes: incomes probably overrepresented, since we targetted people who owned cars

############# Part 1: Trip Production - Regression


###Step 1: impute missing data
#each person only recorded on maximum of seven days
#however, lots of people who only recorded on less than 7 days
#we will impute missing weekdays for those who have at least two days measured out of Mon/Tues/Wed/Thus
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

ggplot(weekly_trips_participant_clean, 
       aes(x = log(n_trips_imputed))) + 
  geom_histogram() +
  labs(title = "Distribution of log(weekly trips) in Canton Zurich", 
       x = "log(n weekly trips)",
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme

#data looks relatively normal, not too bad

#join imputed number of weekly trips to participant data
#will be NA for those who did not have enough data to impute from
participants <- participants %>% left_join(weekly_trips_participant_clean %>% 
                                             select(n_trips_imputed) %>%
                                             rename(weekly_trips_imputed = n_trips_imputed), 
                                           by = c("participant_id"))

#new data frame for only those who have an imputed # weekly trips
participants_trips <- participants %>% 
  filter(!is.na(weekly_trips_imputed)) %>%
  #creating a variable for public transport pass that is true if any of the particular passes are true
  mutate(has_pt_pass = (has_pt_pass_ga | has_pt_pass_halffare | has_pt_pass_regional))




### Part 2: Formulate a hypothesis

#pairwise plots
library(GGally)
ggpairs(data = participants_trips[,c(3, 4, 5, 6, 7, 44, 45)] %>%
          mutate(household_size = factor(household_size), 
                 income = factor(income), 
                 education = factor(education), 
                 sex = factor(sex)))


#sex looks pretty similar
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(sex))) +
  geom_boxplot()

#some income brackets look different
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(income))) +
  geom_boxplot() +
  labs(title = "Number of weekly trips by income", 
       x = "Number weekly trips", 
       y = "Income category", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme

#some education looks different
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(education))) +
  geom_boxplot()

#no clear relationship with age
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = age)) +
  geom_point()

#some household brackets look slightly different
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(household_size))) +
  geom_boxplot() +
  labs(title = "Number of weekly trips by household size", 
       x = "Number weekly trips", 
       y = "Houshold size", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme


#public transport pass looks like it plays a role
ggplot(participants_trips %>%
         filter(!is.na(has_pt_pass)),
       aes(x = weekly_trips_imputed, y = factor(has_pt_pass))) +
  geom_boxplot() + 
  labs(title = "Number of weekly trips for those with vs without a public transit travel pass", 
       x = "Number weekly trips", 
       y = "Has public travel travel pass (regional, GA, or half tax)", 
       caption = "Source: Fall 2019 MOBIS from Transport Planning Methods 'HS22 data_assignment.RData'") +
  custom_theme


#car access looks like it plays a role - but turns out almost everyone in the survey owns a car.. .maybe sample size too small
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(own_car))) +
  geom_boxplot()

#sample sizes get quite small looking at freq_driver_own_car so probably best to leave out
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(freq_driver_own_car))) +
  geom_boxplot()
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(work_type))) +
  geom_boxplot()


### Step 3: Construct dummy variables - this will happen within lm calls with factor() function

participants_trips <- participants_trips %>%
  filter(!is.na(income)) %>%
  mutate(income = factor(income), 
         education = factor(education), 
         sex = factor(sex), 
         household_size = factor(household_size), 
         work_type = factor(work_type)) 

#to get larger sample sizes, group some of the housoehold size bins and incomes
participants_trips <- participants_trips %>% mutate(
  household_size = case_when(
    household_size == 1 ~ "1", 
    household_size %in% c(2, 3) ~ "2-3", 
    household_size %in% c(4, 5) ~ "4-5"
  ), 
  income = case_when(
    income == 1 ~ "Low", 
    income == 99 ~ "Undisclosed", 
    TRUE ~ "High"
  ), 
  income = factor(income, levels = c("Low", "High", "Undisclosed"))
)

### Step 4:

#Don't think any transformations would be needed, as data looks 

#unfortunately, not seeing any obvious relationships for any of these...

#having a public transport has an interaction with income
#those with a public transport pass tend to take more trips
summary(lm(data = participants_trips, 
           weekly_trips_imputed ~ 
            # factor(has_pt_pass)))
             factor(income) * factor(has_pt_pass)))


summary(lm(data = participants_trips, 
           weekly_trips_imputed ~ 
        #     age + 
             income+
        #     factor(household_size) + 
        #     factor(income) * factor(household_size) +
        #     factor(sex)+
         #    factor(own_car)+
        #    factor(has_ebike)+
          +has_pt_pass*income+
            has_pt_pass))   #public transport pass se
           #  factor(education)))

#chosing to use log of weekly trips in order to better satisfy assumptions
model <- lm(data = participants_trips, 
        log(weekly_trips_imputed) ~ 
          income +
     #     age+
          household_size+
          has_pt_pass)

summary(model)

#chose to leave out interaction effects due to relatively small sample size and no strong visual relationships when plotting between any of the variables
#model quite poor - tried many combinations
#having a public transport pass is the only variable that seems to be close to being significant. Also has a fair amount of leverage
#having undisclosed income also has a somewhat lower p-value, they seem to make on average fewer trips

#variables that appear to be at least sometimes somwhat significant: 
#public transport pass - can't get below 0.05 p-value, but can get below 0.1. 
#household size= 4 for some reason.
#some of the income categories (wish to not disclose) but can't find good explanation

### Step 5: Checking linear regression assumptions

#Linearity - line is close to horizontal so this assumptions holds

plot(model, which = 1)

#normality - looks pretty good
plot(model, which = 2)

#homoscedasticity (residuals have constant variance) - not too terrible
plot(model, 3)

#collinearity - vif values are all very close to 1, so we do not have collinearity
vif(model)

#autocorrelation of residuals -- these look low so it is fine
acf(resid(model))

### Part 6: Goodness of fit

#Goodness of fit is very low. r2 = 0.026, adjusted r2 = 0.012 
#need to explain difference between r2 and adjusted r2

### Part 7: Significant parameters

#not really - see comment above near model
#F-test on whole model is significant at 10% confidence level, but this is mainly from the intercept, which provides average for the whole group


### Part 7: apply model to new data set
#can do this if we have the same variables


### Part 8: What other types of models could we have used? 

#linear assumptions are actually held quite well, so linear model may be the most appropriate here
#other models include... ??

stargazer(model, single.row = TRUE, report = c("vcp*"), title = "Regression summary")

stargazer(model)

## 1. Calculate trips per week per person and merge it to participants (by now you should know how that works)
## 2. Use pairs() to make a scatterplot matrix in order to examine possible relationships between variables
## 3. Regression: fit <- lm(y ~ x1 + x2, data),
## 4. Regression output: summary(fit),
## 5. Visual assumption checks: plot(fit) gives 4 plots, plot(fit, which = c(1,2)) only gives Tukey Anscombe plot and QQ plot of residuals
## 6. Multicolinearity: Package AER: vif()
## 7. Use the stargazer package to create nice Latex tables of your model! stargazer()





test <- trips_aggregated %>% 
  group_by(participant_id) %>%
  mutate(n_days = n_distinct(day(started_at))) %>%
  filter(n_days == 7) %>%
  summarise(n_trips = n_distinct(trip_id))

ggplot(test, aes(x = n_trips)) + 
  geom_histogram()





###### Part 3: Mode choice modeling

library(apollo)

rm(list = ls())

#Since our alternatives data is based on mainmode computed by the taking the longest trip leg, we will redefine mainmode

load(paste0("data_assignment.RData"))

trips <- trips %>% mutate(mode = str_remove(mode, "Mode::"), 
                          mode_agg = str_remove(mode_agg, "Mode::"), 
                          length = floor(length), 
                          duration = floor(duration))


#trips_mainmode df now only includes trip legs with the mainmode, and defines mainmode as the longest leg in the trip
trips_mainmode <- trips %>% 
  group_by(trip_id) %>%
  slice(which.max(length))
  

#load in alternative mode choice data
load("data_assignment_modechoice.RData")

trips_total <- trips_mainmode %>% right_join(modechoicetrips, by = c("trip_id", "participant_id")) %>%
  arrange(participant_id)
trips_total <- trips_total %>%  left_join(participants, 
            by = c("participant_id" = "participant_id"))

trips_total <- trips_total %>%
  mutate(lowincome = case_when(income == 1 ~ 1, 
                               !is.na(income) ~ 0, 
                               TRUE ~ 0), 
         female = case_when(sex == 2 ~ 1, 
                            !is.na(sex) ~ 0, 
                            TRUE ~ 0))

#trips_total <- trips_total %>%
#  mutate(cost_pt = case_when(has_pt_pass_regional == 1 ~ 0,
#                             has_pt_pass_ga == 1 ~ 0, 
#                             TRUE ~ cost_pt))




### Apollo modeling

apollo_initialise()

database = trips_total


#set up core controls
apollo_control = list(
  modelName ="mode_choice",
  indivID ="participant_id",
  outputDirectory = "output",
  #weights = "weight_double",
  nCores = 1
)

#parameters that we are interested in the effect of
apollo_beta = c(
  asc_car = 0, 
  asc_bike = 0, 
  asc_pt = 0, 
  asc_walk = 0,
#  asc_shift_car_female = 0,
#  asc_shift_bike_female = 0,
#  asc_shift_pt_female = 0,
#  asc_shift_walk_female = 0,
  b_tt_car = 0, 
  b_tt_bike = 0, 
  b_tt_pt = 0, 
  b_tt_walk = 0,
#  b_distance_car = 0, 
# b_distance_bike = 0, 
#  b_distance_pt = 0, 
#  b_distance_walk = 0,
  b_freq_pt = 0,
  b_trans_nr_pt = 0,
  b_cost = 0, 
  b_cost_shift_lowincome = 0
)

#fix reference variables - not entirely sure what this is about? 
apollo_fixed = c("asc_car")

apollo_inputs = apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  #attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  #create list of probabilities
  P = list()
  
  #create sociodemographic shifts
  
#  asc_car_value = asc_car + asc_shift_car_female * female
#  asc_bike_value = asc_bike + asc_shift_bike_female * female
#  asc_pt_value = asc_pt + asc_shift_pt_female * female
#  asc_walk_value = asc_walk + asc_shift_walk_female * female
  
  b_cost_value = b_cost + b_cost_shift_lowincome * lowincome
  
  
  #list of utilities
  V = list()
  
  #using distance instead of travel time
#  V[["car"]] = asc_car + b_distance_car * distance_car + b_cost*cost_car
 # V[["bike"]] = asc_bike  + b_distance_bike * distance_bike
#  V[["pt"]] = asc_pt +  b_distance_pt * distance_pt +b_cost*cost_pt
#  V[["walk"]] = asc_walk + b_distance_walk * distance_walk 
  
  V[["car"]] = asc_car + b_tt_car * tt_car  + b_cost_value*cost_car
  V[["bike"]] = asc_bike + b_tt_bike * tt_bike 
  V[["pt"]] = asc_pt + b_tt_pt*tt_pt + b_cost_value*cost_pt + b_freq_pt * freq_pt + b_trans_nr_pt*trans_nr_pt
  V[["walk"]] = asc_walk + b_tt_walk*tt_walk 
  
  
  mnl_settings = list(
    
    #alternatives - assign a number that will be used for the alternative in the model
    alternatives = c(car=1, pt=2, bike=3, walk=4),
    #setting vector from which to pull the availability of alternatives
    avail = list(car=avail_car, pt = avail_pt, bike = avail_bike, walk = avail_walk),
    #vector that contains actual chosen option - should be from the numbers above
    choiceVar = choice,
    utilities = V)
  
  #compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  #Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  #Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
  
}




model = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

x <- apollo_modelOutput(model)

#calculating value of travel time

deltaMethod_settings=list(expression=c(VTT_car_min="b_tt_car/b_cost",
                                       VTT_pt_min = "b_tt_pt/b_cost"))
apollo_deltaMethod(model, deltaMethod_settings)

forecast = apollo_prediction(model,
                             apollo_probabilities,
                             apollo_inputs)


forecast


############ Want to apply mode choice model to trip distribution 
#### to predict modal split between zones

# will use trip_dist calculated in Trip Production file




### need to build a simpler model with sociodemographic characteristics. We also leave out the frequency of public transport since this was not significant
apollo_initialise()

database = trips_total


#set up core controls
apollo_control = list(
  modelName ="mode_choice",
  indivID = "participant_id",
  outputDirectory = "output",
  #weights = "weight_double",
  nCores = 1
)

#parameters that we are interested in the effect of
apollo_beta = c(
  asc_car = 0, 
  asc_bike = 0, 
  asc_pt = 0, 
  asc_walk = 0,
  b_tt_car = 0, 
  b_tt_bike = 0, 
  b_tt_pt = 0, 
  b_tt_walk = 0,
  b_trans_nr_pt = 0,
  b_cost = 0

)

#fix reference variables - not entirely sure what this is about? 
apollo_fixed = c("asc_car")

apollo_inputs = apollo_validateInputs()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  
  #attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  #create list of probabilities
  P = list()

  
  
  #list of utilities
  V = list()
  
  V[["car"]] = asc_car + b_tt_car * tt_car  + b_cost*cost_car
  V[["bike"]] = asc_bike + b_tt_bike * tt_bike 
  V[["pt"]] = asc_pt + b_tt_pt*tt_pt + b_cost*cost_pt  + b_trans_nr_pt*trans_nr_pt
  V[["walk"]] = asc_walk + b_tt_walk*tt_walk 
  
  
  mnl_settings = list(
    
    #alternatives - assign a number that will be used for the alternative in the model
    alternatives = c(car=1, pt=2, bike=3, walk=4),
    #setting vector from which to pull the availability of alternatives
    avail = list(car=avail_car, pt = avail_pt, bike = avail_bike, walk = avail_walk),
    #vector that contains actual chosen option - should be from the numbers above
    choiceVar = choice,
    utilities = V)
  
  #compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  #Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  #Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
  
  
}

model = apollo_estimate(apollo_beta,
                        apollo_fixed,
                        apollo_probabilities,
                        apollo_inputs)

 apollo_modelOutput(model)



#then we will use the model to get the predicted modal splits between each pair of zones
 
zones <- unique(trips_total$start_bzname.x) 

#create database that will end up having the probabilities of each mode between zones
probs <- data.frame(start_zone = 0, 
                    end_zone = rep(zones, length(zones)),
                    perc_car = 0, 
                    perc_pt = 0, 
                    perc_walk = 0, 
                    perc_bike = 0)


#loop through each combination of zones, and use the model to predict the probabilities of each mode for trips made between those two districts 
row = 1
for (i in 1:length(zones)){
  for (j in 1:length(zones)){
    database <- trips_total %>% filter(start_bzname.x == zones[i] & end_bzname.x == zones[j])
    apollo_inputs = apollo_validateInputs()
    forecast = apollo_prediction(model,
                                 apollo_probabilities,
                                 apollo_inputs)
    probs$start_zone[row] = zones[i]
    probs$end_zone[row] = zones[j]
    probs$perc_car[row] = mean(forecast$car)
    probs$perc_pt[row] = mean(forecast$pt)
    probs$perc_bike[row] = mean(forecast$bike)
    probs$perc_walk[row] = mean(forecast$walk)
    row = row + 1
  }
} 
  


#join modal probabilities to trip_dist data (from Trip Production.R) 

#note that some origin/destination pairs did not exist in the alternatives data, so we did not get estimations for these
#a more detailed analysis could impute values based on averages from the trips data, in order to compute trip distributions for these pairs

modal_split <- probs %>% left_join(trip_dist, 
                            by = c("start_zone" = "Origin", "end_zone" = "Destination")) %>%
  mutate(n_car = round(perc_car *nr_trips), 
         n_pt = round(perc_pt * nr_trips), 
         n_bike = round(perc_bike * nr_trips),
         n_walk = round(perc_walk * nr_trips))

modal_split_output <- modal_split %>%
  filter(!is.na(n_car)) %>%
  select(start_zone, end_zone, n_car, n_pt, n_walk, n_bike) %>%
  filter(start_zone == "Zürich") %>%
  arrange(end_zone)

xtable(modal_split_output, type = "latex", digits = 0)




test <- modechoicetrips %>% 
  group_by(start_bzname, end_bzname) %>% 
  summarise(n_trips = n(), 
            n_car_avail = sum(avail_car),
            tt_car = mean(tt_car, na.rm = TRUE), 
            cost_car = mean(cost_car, na.rm = TRUE),
            n_pt_avail = sum(avail_pt), 
            tt_pt = mean(tt_pt, na.rm = TRUE), 
            cost_pt = mean(cost_pt, na.rm = TRUE), 
            trans_nr_pt = mean(trans_nr_pt, na.rm = TRUE),
            n_bike_avail = sum(avail_bike), 
            tt_bike = mean(tt_bike, na.rm = TRUE), 
            n_walk_avail = sum(avail_walk), 
            tt_walk = mean(tt_walk, na.rm = TRUE))






########## Demand Calculations for Cost Benefit Analysis  ###############

# now we are interested in how demand changes if we apply the policy changes that we want to evaluate as a part of a CBA analysis





modechoicetrips %>% 
  filter((start_bzname == "Zürich" & end_bzname == "Bülach") | (start_bzname == "Bülach" & end_bzname == "Zürich")) %>%
  summarise(avg_dist_pt = mean(distance_pt, na.rm = TRUE), 
            avg_dist_car = mean(distance_car, na.rm = TRUE), 
            avg_dist_walk = mean(distance_walk, na.rm = TRUE), 
            avg_dist_bike = mean(distance_bike, na.rm = TRUE), 
            avg_tt_car = mean(tt_car, na.rm = TRUE), 
            avg_tt_pt = mean(tt_pt, na.rm = TRUE), 
            avg_tt_walk = mean(tt_walk, na.rm = TRUE), 
            avg_tt_bike = mean(tt_bike, na.rm = TRUE))


modechoicetrips %>% 
  filter((start_bzname == "Zürich" & end_bzname != "Zürich") | (start_bzname != "Zïrich" & end_bzname == "Zürich")) %>%
  summarise(avg_dist_pt = mean(distance_pt, na.rm = TRUE), 
            avg_dist_car = mean(distance_car, na.rm = TRUE), 
            avg_dist_walk = mean(distance_walk, na.rm = TRUE), 
            avg_dist_bike = mean(distance_bike, na.rm = TRUE), 
            avg_cost_car = mean(cost_car, na.rm = TRUE),
            avg_tt_car = mean(tt_car, na.rm = TRUE), 
            avg_tt_pt = mean(tt_pt, na.rm = TRUE), 
            avg_tt_walk = mean(tt_walk, na.rm = TRUE), 
            avg_tt_bike = mean(tt_bike, na.rm = TRUE))



trips_aggregated %>% 
  filter((start_bzname == "Zürich" & end_bzname != "Zürich") | (start_bzname != "Zïrich" & end_bzname == "Zürich")) %>%
  group_by(mode_agg) %>%
  summarise(n = n())



