getwd()

# libraries 
library(dplyr)
library(tidycensus) # to scrape census api 
library(tidyverse)
library(usdata)

# loading ANES dataframe
anes <- read.csv("ANES_for_State_Dat.csv")

# api 
census_api_key("febde3046a57d2744500573b896b7f0a12e8ca93") # to rep put your key here 

# census reporter helpful for finding variables

####### race data from acs #######
race_dat_16 <- get_acs(geography = "state", 
                       variables = c(white = "B02001_002", total = "B01003_001"), 
                       year = 2016, survey = "acs1", output = "wide") 

race_dat_12 <- get_acs(geography = "state", 
                       variables = c(white = "B02001_002", total = "B01003_001"), 
                       year = 2012, survey = "acs1", output = "wide")

###### unemployment data from acs ######
un_dat_16 <- get_acs(geography = "state", 
                     variables = c(employed = "B23025_004", unemployed = "B23025_005"), 
                     year = 2016, survey = "acs1", output = "wide")

un_dat_12 <- get_acs(geography = "state", 
                     variables = c(employed = "B23025_004", unemployed = "B23025_005"), 
                     year = 2012, survey = "acs1", output = "wide")

###### calc percent unemployed #########
# for each state, unemployed/ (emp + un)
un_dat_16$ue_rate <- un_dat_16$unemployedE/(un_dat_16$employedE + un_dat_16$unemployedE)
un_dat_12$ue_rate <- un_dat_12$unemployedE/(un_dat_12$employedE + un_dat_12$unemployedE)
# very slight discrepancy from dol stats, but that number is the rate from dec, rather than the yr overall 

###### calc percent non white #########
race_dat_12$p_non_white <- (race_dat_12$totalE - race_dat_12$whiteE)/race_dat_12$totalE
race_dat_16$p_non_white <- (race_dat_16$totalE - race_dat_16$whiteE)/race_dat_16$totalE

##### adding state code variable ##### 
race_dat_12$abbr <- state2abbr(race_dat_12$NAME)
race_dat_16$abbr <- state2abbr(race_dat_16$NAME)
un_dat_12$abbr <- state2abbr(un_dat_12$NAME)
un_dat_16$abbr <- state2abbr(un_dat_16$NAME)

##### adding year variable ############
race_dat_12$Year <- 2012
race_dat_16$Year <- 2016
un_dat_12$Year <- 2012
un_dat_16$Year <- 2016

##### binding ########
long_race <- rbind(race_dat_12, race_dat_16)
long_un <- rbind(un_dat_12, un_dat_16)

df <- long_race %>% inner_join(long_un, by = c("abbr", "Year"))

#### selecting only vars we need ####
df <- df %>% select(abbr, Year, p_non_white, ue_rate)
df$state <- df$abbr

#### adding threat vars to anes dat for state years ####
combined <- df %>% inner_join(anes,by = c("state", "Year"))

### crime data from fbi 
crime <- read.csv("CleanedFBIDat.csv")[1:104,] 

### name to abbr so matches between datasets 
crime$state <- state2abbr(crime$State)

## merging 
all_threats <- crime %>% inner_join(combined, by = c("state", "Year"))

## writing into csv for analysis 
write.csv(all_threats, "with_threats.csv")

rm(list = ls()) # start fresh 
