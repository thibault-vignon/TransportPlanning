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
library(scales)
library(car)
library(stargazer)

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

ggplot(weekly_trips_participant_clean, aes(x = n_trips_imputed)) + 
  geom_histogram()
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
  geom_boxplot()

#some education looks different
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(education))) +
  geom_boxplot()

#no clear relationship with age
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = age)) +
  geom_point()

#some household brackets look slightly different
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(household_size))) +
  geom_boxplot()

#public transport pass looks like it plays a role
ggplot(participants_trips, aes(x = weekly_trips_imputed, y = factor(has_pt_pass))) +
  geom_boxplot()

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
  mutate(income = factor(income), 
         education = factor(education), 
         sex = factor(age), 
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
          age+
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
  b_tt_car = 0, 
  b_tt_bike = 0, 
  b_tt_pt = 0, 
  b_tt_walk = 0
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
  
  V[["car"]] = asc_car + b_tt_car * tt_car
  V[["bike"]] = asc_bike + b_tt_bike * tt_bike
  V[["pt"]] = asc_pt + b_tt_pt*tt_pt
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



