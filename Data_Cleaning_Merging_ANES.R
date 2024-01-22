getwd()

### packages
library(haven)
library(dplyr)
library(tidyverse)
library(psych)
library(labelled)
library(weights)
library(performance)

### reading in data
dat_2012 <- read_dta("ANES_2012.dta")
dat_2016 <- read_dta("ANES_2016.dta")

### selecting vars of interest, rc = Extra_2, Agree_1, Cons_2, Neur_2, Open_2, < 7 = no resp
dat_2012 <- dat_2012 %>% select(Extra_1 = tipi_extra, Agree_1 = tipi_crit, Cons_1 = tipi_dep, 
                                Neur_1 = tipi_anx, Open_1 = tipi_open, Extra_2  = tipi_resv, 
                                Agree_2 = tipi_warm, Cons_2 = tipi_disorg, Neur_2 = tipi_calm, 
                                Open_2 = tipi_conv, age = dem_agegrp_iwdate_x, race = dem_raceeth_x, 
                                gender = gender_respondent_x, income = inc_incgroup_pre, edu = dem_edugroup_x, 
                                state = sample_state, ideology = libcpre_self, 
                                spending = spsrvpr_ssself, reg_business = govrole_regbus, 
                                govrole_big = govrole_big, strong_gov = govrole_market, 
                                less_gov_better = govrole_lessmore, trad_1_r = trad_adjust, 
                                trad_2 = trad_lifestyle, trad_3_r = trad_tolerant, trad_4 = trad_famval) 


dat_2016 <- dat_2016 %>% select(Extra_1 = V162333, Agree_1 = V162334, Cons_1 = V162335, 
                                Neur_1 = V162336, Open_1 = V162337, Extra_2  = V162338, 
                                Agree_2 = V162339, Cons_2 = V162340, Neur_2 = V162341, 
                                Open_2 = V162342, age = V161267x, race = V161310x, 
                                gender = V161342, income = V161361x, edu = V161270, 
                                state = V161010e, ideology = V161126, spending = V161178, 
                                less_gov_better  = V162185, strong_gov = V162184, 
                                reg_business = V162186, govrole_big = V162183 ,trad_1_r = V162207, 
                                trad_2 = V162208, trad_3_r = V162209, 
                                trad_4 = V162210) # pre and post vars
which(! variable.names(dat_2012) %in% variable.names(dat_2016))

############### Year Variable ###############
dat_2012$Year <- rep(2012, nrow(dat_2012))
dat_2016$Year <- rep(2016, nrow(dat_2016))

############### Personality #################
unique(dat_2012$Extra_1)
dat_2012 <- subset(dat_2012, (dat_2012$Extra_1 > 0 & dat_2012$Extra_2 >0 & 
                                dat_2012$Agree_1 > 0 & dat_2012$Agree_2 > 0 & dat_2012$Neur_1 > 0 &
                                dat_2012$Neur_2 > 0 & dat_2012$Open_1 > 0 & dat_2012$Open_2 > 0 &
                                dat_2012$Cons_1 > 0 & dat_2012$Cons_2 > 0)) 

dat_2016 <- subset(dat_2016, (dat_2016$Extra_1 > 0 & dat_2016$Extra_2 >0 & 
                                dat_2016$Agree_1 > 0 & dat_2016$Agree_2 > 0 & dat_2016$Neur_1 > 0 &
                                dat_2016$Neur_2 > 0 & dat_2016$Open_1 > 0 & dat_2016$Open_2 > 0 &
                                dat_2016$Cons_1 > 0 & dat_2016$Cons_2 > 0)) 

########### Demos ###############
dat_2012$age <- ifelse(dat_2012$age < 0, NA, dat_2012$age)
dat_2016$age <- ifelse(dat_2016$age < 0, NA, dat_2016$age)

dat_2012$race <- ifelse(dat_2012$race < 0, NA, dat_2012$race)
dat_2016$race <- ifelse(dat_2016$race < 0, NA, dat_2016$race)

dat_2012$male <- ifelse(dat_2012$gender ==1, 1, NA)
dat_2016$male <- ifelse(dat_2016$gender == 1, 1, NA)

dat_2012$income <- ifelse(dat_2012$income < 0, NA, dat_2012$income)
dat_2016$income <- ifelse(dat_2016$income < 0, NA, dat_2016$income)

dat_2016$edu # levels: 1) less than hs, 2) high school, 3) some post hs 
# 4) bachelors 5) grad degree 
dat_2012$edu <- ifelse(dat_2012$edu < 1 , NA, dat_2012$edu)
dat_2016$edu <- ifelse(dat_2016$edu < 0, NA, 
                       ifelse(dat_2016$edu > 0 & dat_2016$edu <= 8, 1, 
                              ifelse(dat_2016$edu == 9, 2, 
                                     ifelse(dat_2016$edu >= 10 & dat_2016$edu <= 12, 3, 
                                            ifelse(dat_2016$edu == 13, 4, 
                                                   ifelse(dat_2016$edu > 13 & dat_2016$edu <= 16, 5, NA))))))

sort(unique(dat_2012$state)) 
sort(unique(dat_2016$state))

# function to reverse 7 pt scale 
reverse <- function(x){
  ifelse(x == 1, 7, 
         ifelse(x == 2, 6, 
                ifelse(x == 3, 5, 
                       ifelse(x == 4, 4, 
                              ifelse(x == 5, 3, 
                                     ifelse(x == 6, 2, 
                                            ifelse(x == 7, 1, -999)))))))
}


######################## Symbolic Ideology ######################
dat_2012$ideology <- ifelse(dat_2012$ideology < 0, NA, dat_2012$ideology)
dat_2016$ideology <- ifelse(dat_2016$ideology <0 | dat_2016$ideology >7, NA, dat_2016$ideology)
# Higher more cons 

###################### Operational Ideology ######################
dat_2012$spending <- ifelse(dat_2012$spending < 1, NA, dat_2012$spending)
dat_2016$spending <- ifelse(dat_2016$spending < 1 | dat_2016$spending > 7, NA, dat_2016$spending)
dat_2012$spending <- reverse(dat_2012$spending)
dat_2016$spending <- reverse(dat_2016$spending)

# Higher more cons 
dat_2012$reg_business <- ifelse(dat_2012$reg_business < 1, NA, dat_2012$reg_business)
dat_2016$reg_business <- ifelse(dat_2016$reg_business < 1, NA, dat_2016$reg_business)
dat_2012$reg_business

# Higher more lib 
dat_2012$govrole_big <- ifelse(dat_2012$govrole_big < 1, NA, dat_2012$govrole_big)
dat_2016$govrole_big <- ifelse(dat_2016$govrole_big < 1, NA, dat_2016$govrole_big)
dat_2012$govrole_big <- ifelse(dat_2012$govrole_big == 1, 2, 1)
dat_2016$govrole_big <- ifelse(dat_2016$govrole_big == 1, 2, 1)

# Higher more cons 
dat_2012$strong_gov <- ifelse(dat_2012$strong_gov < 1, NA, dat_2012$strong_gov)
dat_2016$strong_gov <- ifelse(dat_2016$strong_gov < 1, NA, dat_2016$strong_gov)

# Higher more lib 
dat_2012$less_gov_better <- ifelse(dat_2012$less_gov_better < 1, NA, dat_2012$less_gov_better)
dat_2016$less_gov_better <- ifelse(dat_2016$less_gov_better < 1, NA, dat_2016$less_gov_better)
dat_2012$less_gov_better <- ifelse(dat_2012$less_gov_better == 1, 2, 1)
dat_2016$less_gov_better <- ifelse(dat_2016$less_gov_better == 1, 2, 1)

# releveling ordinal items 
dat_2012$reg_business <- nalevs(dat_2012$reg_business)
dat_2016$reg_business <- nalevs(dat_2016$reg_business)
dat_2012$spending <- nalevs(dat_2012$spending)
dat_2016$spending <- nalevs(dat_2016$spending)
dat_2012$less_gov_better <- nalevs(dat_2012$less_gov_better)
dat_2016$less_gov_better <- nalevs(dat_2016$less_gov_better)
dat_2012$strong_gov <- nalevs(dat_2012$strong_gov)
dat_2016$strong_gov <- nalevs(dat_2016$strong_gov)
dat_2012$govrole_big <- nalevs(dat_2012$govrole_big)
dat_2016$govrole_big <- nalevs(dat_2016$govrole_big)


######## Merging data ##########
dat_2012 <- remove_labels(dat_2012)
dat_2016 <- remove_labels(dat_2016)
dat_combined <- rbind(dat_2012, dat_2016)


########## Scoring TIPI ########
rs <- cbind(dat_combined$Extra_2, dat_combined$Agree_1, dat_combined$Cons_2, 
            dat_combined$Neur_2, dat_combined$Open_2)
reversed <- apply(rs, 2, reverse)

names <- c("Extra_rs", "Agree_rs", "Cons_rs", "Neur_rs", "Open_rs")
colnames(reversed) <- names

dat_combined <- cbind(reversed, dat_combined)

# Mean of personality sub-scales 
dat_combined$Open <- rowMeans(dat_combined[,c("Open_1", "Open_rs")], na.rm = TRUE)
dat_combined$Extra <- rowMeans(dat_combined[,c("Extra_1", "Extra_rs")], na.rm = TRUE)
dat_combined$Cons <- rowMeans(dat_combined[,c("Cons_1", "Cons_rs")], na.rm = TRUE)
dat_combined$Agree <- rowMeans(dat_combined[,c("Agree_rs", "Agree_2")], na.rm = TRUE)
dat_combined$Neur <- rowMeans(dat_combined[,c("Neur_rs", "Neur_1")], na.rm = TRUE)

######### Scoring Traditionalism ######
reverse_t <- function(x) { 
  ifelse(x == 1, 5, 
         ifelse(x == 2, 4, 
                ifelse(x == 3, 3,
                       ifelse(x == 4, 2, 
                              ifelse(x == 5, 1, -999)))))
}
dat_combined$trad_1_r
dat_combined$trad_1_r <- ifelse(dat_combined$trad_1_r < 1, NA, dat_combined$trad_1_r)
dat_combined$trad_2 <- ifelse(dat_combined$trad_2 < 1, NA, dat_combined$trad_2)
dat_combined$trad_3_r <- ifelse(dat_combined$trad_3_r < 1, NA, dat_combined$trad_3_r)
dat_combined$trad_4 <- ifelse(dat_combined$trad_4 < 1, NA, dat_combined$trad_4)


dat_combined$trad_2 <- reverse_t(dat_combined$trad_2)
dat_combined$trad_4 <- reverse_t(dat_combined$trad_4)

dat_combined$traditionalism <- rowMeans(dat_combined[,c("trad_1_r", "trad_2", "trad_3_r", "trad_4")], na.rm = TRUE)

# scoring combined operational ideology 
dat_combined$Operational <- rowMeans(dat_combined[,c("spending", "govrole_big", "reg_business", "strong_gov", "less_gov_better")], na.rm = TRUE)

# reliabilities 

# traditionalism 
psych::alpha(dat_combined[,c("trad_1_r", "trad_2", "trad_3_r", "trad_4")]) # .71

# economic 
psych::alpha(dat_combined[,c("spending", "govrole_big", "reg_business", "strong_gov", 
                             "less_gov_better")]) #.79
# personality 
item_intercor(dat_combined[,c("Open_1", "Open_rs")]) # .24
item_intercor(dat_combined[,c("Extra_1", "Extra_rs")]) # .32 
item_intercor(dat_combined[,c("Cons_1", "Cons_rs")]) # .37
item_intercor(dat_combined[,c("Agree_rs", "Agree_2")]) # .19
item_intercor(dat_combined[,c("Neur_rs", "Neur_1")]) # .37



############### Writing into .RData for Merging with State dat #########
write.csv(dat_combined, "ANES_for_State_Dat.csv")
rm(list = ls())
gc()
