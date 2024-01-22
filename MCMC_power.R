library(simr)
library(lme4)
library(lmerTest)
library(dplyr)

### load data 
dat <- readRDS("dat.rds")

### selecting variables 
dat <- dat %>% select(c("ideology", "Open", "Extra", "Agree", "Neur", "Cons", "male", "contrast1", 
                        "contrast2", "contrast3", "income", "edu", "age", "Year", "Crime.Rate", 
                        "ue_rate", "p_non_white", "state"))

#### can't handle NAs
dat <- na.omit(dat)

### mod no int
mod <- lmer(ideology ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                 contrast2 + contrast3 + income + edu + age  + as.factor(Year) +
                  Crime.Rate + ue_rate + p_non_white + 
                 (1|state), data = dat)
summary(mod)

### mod int 
mod_int <- lmer(ideology ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                 contrast2 + contrast3 + income + edu + age + Open*Crime.Rate + Cons*Crime.Rate + 
                 Open*ue_rate + Cons*ue_rate + Open*p_non_white + Cons*p_non_white + as.factor(Year) +
                 (1|state), data = dat)
summary(mod_int)

### power analysis for moderately sized direct effect ### 
fixef(mod)["ue_rate"] <- .20
pow_test <- powerSim(mod, test = fixed("ue_rate", "t"), nsim = 100)
pow_test # about 100 percent power for moderately sized threat effect 

### power analysis for moderately sized interaction effect #### 
fixef(mod_int)["Open:ue_rate"] <- .20
pow_test <- powerSim(mod_int, test = fixed("Open:ue_rate", "t"), nsim = 100)
pow_test # about 84 percent power for moderately sized interaction
