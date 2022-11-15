##############################################################################################-
# SKELETON PART 2
##############################################################################################-

# Set up ------------------------------------------------------------------

require("lubridate") # great for dealing with dates if needed
require("tidyverse") # basek of great packages: e.g. dplyr which makes data wrangling more readable and easier
require("sf") # needed for plotting of spatial polygons and calculations with it
require("AER") # package for vif test
require("stargazer") # package to turn your regression results into Latex-coded table


# 1 Import data -----------------------------------------------------------

# use load and adjust the path to where you have saved the data file
load(paste0("data_assignment.RData"))


# 2 Trip production (regression) ------------------------------------------

## 1. Calculate trips per week per person and merge it to participants (by now you should know how that works)
## 2. Use pairs() to make a scatterplot matrix in order to examine possible relationships between variables
## 3. Regression: fit <- lm(y ~ x1 + x2, data),
## 4. Regression output: summary(fit),
## 5. Visual assumption checks: plot(fit) gives 4 plots, plot(fit, which = c(1,2)) only gives Tukey Anscombe plot and QQ plot of residuals
## 6. Multicolinearity: Package AER: vif()
## 7. Use the stargazer package to create nice Latex tables of your model! stargazer()


# 3 Trip distribution -----------------------------------------------------

## Free tutorial on loops in R
## https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r?utm_source=adwords_ppc&utm_campaignid=898687156&utm_adgroupid=48947256715&utm_device=c&utm_keyword=&utm_matchtype=b&utm_network=g&utm_adpostion=1t1&utm_creative=255798340456&utm_targetid=aud-390929969673:dsa-473406582635&utm_loc_interest_ms=&utm_loc_physical_ms=1003297&gclid=CjwKCAiAz7TfBRAKEiwAz8fKOCIL6X9eZMrF2fpvSKODUgK4BKzNtmeqpo3LgqQJTN6LOXmz3bwlZRoCHxsQAvD_BwE

## 1. You may use heatmap() as a possible illustration.


# 4 Mode choice -----------------------------------------------------

## Will be discussed in Lab session 2 on November 3rd, 2021.

