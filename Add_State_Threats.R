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

# pull variable names from 16 acs just to double check website searches
vars_2010 <- load_variables(2010, "sf1", cache = TRUE)


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

##### divorce rates 
div_dat_16 <- get_acs(geography = "state", 
                     variables = c(total_f = "B12503_007", div_last_yr = "B12503_010"), 
                     year = 2016, survey = "acs1", output = "wide")

div_dat_12 <- get_acs(geography = "state", 
                     variables = c(total_f = "B12503_007", div_last_yr = "B12503_010"), 
                     year = 2012, survey = "acs1", output = "wide")

##### GINI index
gini_dat_16 <- get_acs(geography = "state", 
                       variables = c(gini = "B19083_001"), 
                       year = 2016, survey = "acs1", output = "wide")

gini_dat_12 <- get_acs(geography = "state", 
                       variables = c(gini = "B19083_001"), 
                       year = 2012, survey = "acs1", output = "wide")

###### Males between 15 and 25 
under_25_16 <- get_acs(geography = "state", 
                       variables = c(total = "B01001_002", fifteen_17 = "B01001_006", 
                                     eighteen_19 = "B01001_007", 
                                     twenty = "B01001_008", 
                                     twenty_1 = "B01001_009", 
                                     twenty_2_24 = "B01001_010"), 
                       year = 2016, survey = "acs1", output = "wide")

under_25_12 <- get_acs(geography = "state", 
                       variables = c(total = "B01001_002", fifteen_17 = "B01001_006", 
                                     eighteen_19 = "B01001_007", 
                                     twenty = "B01001_008", 
                                     twenty_1 = "B01001_009", 
                                     twenty_2_24 = "B01001_010"), 
                       year = 2012, survey = "acs1", output = "wide")

#### urban rural
#### census only calcs this for census years, 
#### so for 2016 we use closest census: 2020
#### same for 2012: we use 2010 
urban_12 <- get_decennial(
  geography = "state", 
  variables = c(total = "P002001", urban = "P002002"),
  year = 2010, output = "wide"
)

# 2020 census is not in tidycensus yet, so we have to read in and restructure that .csv 
# data from the census bureau directly 
urban_16 <- read.csv("2020_dec_urb_rural.csv")[-4,]
urban_16 <- urban_16 %>% pivot_longer(cols = colnames(urban_16)[2:53], names_to = "State", values_to = "Pop")
urban_16 <- urban_16 %>% pivot_wider(names_from = Label..Grouping., values_from = Pop)
urban_16$State <- gsub("\\.", " ", urban_16$State) # so that abbr works 

####### now creating poverty rate data 
poverty_16 <- get_acs(
  geography = "state",  
  variables = c(total = "B17001_001", poverty = "B17001_002"),  
  year = 2016,  
  survey = "acs1", output = "wide"
)

poverty_12 <- get_acs(
  geography = "state",  
  variables = c(total = "B17001_001", poverty = "B17001_002"),  
  year = 2012,  
  survey = "acs1", 
  output = "wide"
)

#### read in indicator for former confederate
confed <- read.csv("Confed.csv")

#### gdp state/year downloaded from BEA
gdp_state_year <- read.csv("gdp_state_year.csv")
gdp_state_year <- gdp_state_year %>% rename('2012' = X2012)
gdp_state_year <- gdp_state_year %>% rename('2016' = X2016)

###### urbanization 
urban_12$p_urban <- urban_12$urban/ urban_12$total
names(urban_16) <- c("State", "Total", "Urban", "Rural")
urban_16$Total <- as.numeric(gsub("\\,", "", urban_16$Total)) 
urban_16$Urban <- as.numeric(gsub("\\,", "", urban_16$Urban)) 
urban_16$p_urban <- urban_16$Urban/ urban_16$Total

##### poverty rate 
poverty_12$poverty_rate <- poverty_12$povertyE/ poverty_12$totalE
poverty_16$poverty_rate <- poverty_16$povertyE/ poverty_16$totalE

#### p male population under 25 
under_25_12$male_under25 <- (under_25_12$fifteen_17E + under_25_12$eighteen_19E + 
                               under_25_12$twentyE + under_25_12$twenty_1E + 
                               under_25_12$twenty_2_24E)/ under_25_12$totalE

under_25_16$male_under25 <- (under_25_16$fifteen_17E + under_25_16$eighteen_19E + 
                               under_25_16$twentyE + under_25_16$twenty_1E + 
                               under_25_16$twenty_2_24E)/ under_25_16$totalE

###### calc div rate f div yr/ total f *1000
div_dat_16$div_rate <- (div_dat_16$div_last_yrE/div_dat_16$total_fE) *1000
div_dat_12$div_rate <- (div_dat_12$div_last_yrE/div_dat_12$total_fE) *1000

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
div_dat_12$abbr <- state2abbr(div_dat_12$NAME) 
div_dat_16$abbr <- state2abbr(div_dat_16$NAME) 
under_25_12$abbr <- state2abbr(under_25_12$NAME)
under_25_16$abbr <- state2abbr(under_25_16$NAME)
poverty_12$abbr <- state2abbr(poverty_12$NAME)
poverty_16$abbr <- state2abbr(poverty_16$NAME)
urban_12$abbr <- state2abbr(urban_12$NAME)
urban_16$abbr <- state2abbr(urban_16$State)
confed$abbr <- state2abbr(confed$State)
gdp_state_year$abbr <- state2abbr(gdp_state_year$GeoName)
gini_dat_12$abbr <- state2abbr(gini_dat_12$NAME)
gini_dat_16$abbr <- state2abbr(gini_dat_16$NAME)

# pivot gdp dat to long 
gdp_state_year <- gdp_state_year %>% pivot_longer(cols = c('2012', '2016'), names_to = "Year", values_to = "GDP")


##### adding year variable ############
race_dat_12$Year <- 2012
race_dat_16$Year <- 2016
un_dat_12$Year <- 2012
un_dat_16$Year <- 2016
div_dat_12$Year <- 2012
div_dat_16$Year <- 2016
gini_dat_12$Year <- 2012
gini_dat_16$Year <- 2016 
poverty_12$Year <- 2012
poverty_16$Year <- 2016
urban_12$Year <- 2012 # we're using closest cen. est. so 2010  is 2012
urban_16$Year <- 2016 # we're using closest cen. est. so 2020  is 2016
under_25_12$Year <- 2012
under_25_16$Year <- 2016

##### binding ########
long_race <- rbind(race_dat_12, race_dat_16)
long_un <- rbind(un_dat_12, un_dat_16)
long_div <- rbind(div_dat_12, div_dat_16)
long_gini <- rbind(gini_dat_12, gini_dat_16)
long_poverty <- rbind(poverty_12, poverty_16)
variable.names(urban_16)
variable.names(urban_12)
urban_16 <- urban_16 %>% select(-Rural)
urban_12 <- urban_12 %>% select(-GEOID)
names(urban_16) <- c("NAME", "total", "urban" ,"p_urban", "abbr", "Year")
long_urban <- rbind(urban_12, urban_16)
long_youth <- rbind(under_25_12, under_25_16)

df <- long_race %>% inner_join(long_un, by = c("abbr", "Year"))
df <- df %>% inner_join(long_div, by = c("abbr", "Year")) 
df <- df %>% inner_join(long_gini, by = c("abbr", "Year")) 
df <- df %>% inner_join(long_poverty, by = c("abbr", "Year")) 
df <- df %>% inner_join(long_urban, by = c("abbr", "Year")) 
df <- df %>% inner_join(long_youth, by = c("abbr", "Year"))
df$confed <- ifelse(df$abbr %in% confed$abbr == TRUE, 1, 0)

#### selecting only vars we need ####
df <- df %>% select(abbr, Year, p_non_white, ue_rate, div_rate, poverty_rate, 
                    giniE, p_urban, confed, male_under25)
variable.names(df)

#### adding threat vars to anes dat for state years ####
df$state <- df$abbr
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
