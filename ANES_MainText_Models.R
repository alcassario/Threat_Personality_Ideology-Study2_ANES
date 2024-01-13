getwd()

#### load libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(scales)
library(broom.mixed)

##### load data
dat <- read.csv("with_threats.csv")

### don't like Rate variable name
### Changing to Crime.Rate
dat$Crime.Rate <- dat$Rate

#### rescale 0-1
dat$ideology <- rescale(dat$ideology)
dat$Open <- rescale(dat$Open)
dat$Cons <- rescale(dat$Cons)
dat$Extra <- rescale(dat$Extra)
dat$Agree <- rescale(dat$Agree)
dat$Neur <- rescale(dat$Neur)
dat$Crime.Rate <- rescale(dat$Crime.Rate)
dat$p_non_white <- rescale(dat$p_non_white)
dat$ue_rate <- rescale(dat$ue_rate)
dat$income <- rescale(dat$income)
dat$edu <- rescale(dat$edu)
dat$age <- rescale(dat$age)

# mean centering 
dat$Open <- dat$Open - mean(dat$Open, na.rm = TRUE)
dat$Cons <- dat$Cons - mean(dat$Cons, na.rm = TRUE)
dat$Extra <- dat$Extra - mean(dat$Extra, na.rm = TRUE)
dat$Agree <- dat$Agree - mean(dat$Agree, na.rm = TRUE)
dat$Neur <- dat$Neur - mean(dat$Neur, na.rm = TRUE)
dat$Crime.Rate <- dat$Crime.Rate - mean(dat$Crime.Rate, na.rm = TRUE)
dat$p_non_white<- dat$p_non_white - mean(dat$p_non_white, na.rm = TRUE)
dat$ue_rate <- dat$ue_rate - mean(dat$ue_rate, na.rm = TRUE)
dat$income <- dat$income - mean(dat$income, na.rm = TRUE)
dat$edu <- dat$edu - mean(dat$edu, na.rm = TRUE)
dat$age <- dat$age - mean(dat$age, na.rm = TRUE)

# contrast coding race 
# white v min 
dat$contrast1[dat$race==1] <- .75 
dat$contrast1[dat$race==2] <- -.25 
dat$contrast1[dat$race==5] <- -.25 
dat$contrast1[dat$race==3 | dat$race==4 | dat$race==6] <- -.25
dat$contrast1<-dat$contrast1-(mean(na.omit(dat$contrast1))) 

# black v other min groups 
dat$contrast2[dat$race==1] <- 0 
dat$contrast2[dat$race==2] <- .666
dat$contrast2[dat$race==5] <- -.333 
dat$contrast2[dat$race==3 | dat$race==4 | dat$race==6] <- -.333
dat$contrast2<-dat$contrast2-(mean(na.omit(dat$contrast2)))

# hispanic v other minority groups except black
dat$contrast3[dat$race==1] <- 0 
dat$contrast3[dat$race==2] <- 0
dat$contrast3[dat$race==5] <- .5
dat$contrast3[dat$race==3 | dat$race==4 | dat$race==6] <- -.5
dat$contrast3<-dat$contrast3-(mean(na.omit(dat$contrast3)))

dat$Year <- as.factor(dat$Year)
dat$traditionalism <- rescale(dat$traditionalism)
dat$male <- ifelse(is.na(dat$male), 0, dat$male)

### models 

# symbolic no interactions
options(scipen = 999)
m_ideo_no_int <- lmer(ideology ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                        contrast2 + contrast3 + income + edu + age +  as.factor(Year) +Crime.Rate + 
                        ue_rate + p_non_white + 
                        (1|state), data = dat)
options(scipen = 999)
summary(m_ideo_no_int)

# traditionalism/social conservatism no interactions 
m_trad_no_int <- lmer(traditionalism ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                        contrast2 + contrast3 + income + edu + age + Crime.Rate + ue_rate + p_non_white + 
                        as.factor(Year) + 
                        (1| state), data = dat)
summary(m_trad_no_int)
# unemployment marginally more conservative socially 

# operational no interactions
m_operational_no_int <- lmer(Operational ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                               contrast2 + contrast3 + income + edu + age +  Crime.Rate  + 
                               ue_rate + p_non_white + 
                               as.factor(Year) + (1| state), data = dat)
summary(m_operational_no_int)


# symbolic interactions 
m_ideo <- lmer(ideology ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                 contrast2 + contrast3 + income + edu + age + Open*Crime.Rate + Cons*Crime.Rate + 
                 Open*ue_rate + Cons*ue_rate + Open*p_non_white + Cons*p_non_white + as.factor(Year) +
                 (1|state), data = dat)
summary(m_ideo)

# traditionalism interactions
m_trad <- lmer(traditionalism ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                 contrast2 + contrast3 + income + edu + age + Open*Crime.Rate + Cons*Crime.Rate + 
                 Open*ue_rate + Cons*ue_rate + Open*p_non_white + Cons*p_non_white + as.factor(Year) + 
                 (1| state), data = dat)
summary(m_trad)

# operational interactions 
m_operational <- lmer(Operational ~ Open + Extra + Agree + Neur + Cons + male + contrast1 + 
                        contrast2 + contrast3 + income + edu + age +  Open*Crime.Rate + Cons*Crime.Rate + 
                        Open*ue_rate + Cons*ue_rate + Open*p_non_white + Cons*p_non_white  + 
                        as.factor(Year) + (1| state), data = dat)
summary(m_operational)


# saving for future plotting 
save(m_operational, m_trad, m_ideo, m_operational_no_int, m_trad_no_int, 
     m_ideo_no_int, file = "ANES_for_plotting.Rdata")

## start fresh ## 
rm(list = ls())