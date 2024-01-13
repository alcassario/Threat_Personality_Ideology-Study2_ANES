getwd()

# libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(lmerTest)
library(scales)

# read in data
dat <- dat <- read.csv("with_threats.csv")

# renaming crime rate variable so it makes more conceptual sense
dat$Crime.Rate <- dat$Rate

# rescaling 0-1 and centering 
#### rescale 0-1
dat$ideology <- rescale(dat$ideology)
dat$traditionalism <- rescale(dat$traditionalism)
dat$Operational <- rescale(dat$Operational)
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
dat$male <- ifelse(is.na(dat$male), 0, dat$male)

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

# factoring year
dat$Year <- as.factor(dat$Year)


# specifications no interactions 
variable.names(dat)
results <- list()
specifications <- list(c("Open", "Extra", "Neur", "Agree", "Cons", "ue_rate", "p_non_white", "Crime.Rate"),
                       c("Open", "Cons", "p_non_white", "Crime.Rate", "ue_rate"), 
                       c("Open", "p_non_white", "Crime.Rate", "ue_rate"), 
                       c("Open", "p_non_white"), c("Open", "Crime.Rate"), c("Open", "ue_rate"), 
                       c("Cons", "p_non_white", "Crime.Rate", "ue_rate"), 
                       c("Cons", "p_non_white"), c("Cons", "Crime.Rate"), c("Cons", "ue_rate"), 
                       c("ue_rate", "p_non_white", "Crime.Rate"), c("ue_rate"), c("p_non_white"), c("Crime.Rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year", 
                         "Open", "Extra", "Neur", "Agree", "Cons", "ue_rate", "p_non_white", "Crime.Rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year", 
                         "Open", "Cons", "p_non_white", "Crime.Rate", "ue_rate"),
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year", 
                         "Open", "p_non_white", "Crime.Rate", "ue_rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Open", "p_non_white"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Open", "Crime.Rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Open", "ue_rate"),
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Cons", "p_non_white", "Crime.Rate", "ue_rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Cons", "p_non_white"),
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Cons", "Crime.Rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Cons", "ue_rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Crime.Rate", "ue_rate", "p_non_white"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "Crime.Rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "ue_rate"), 
                       c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                         "p_non_white"))

DVs <- c("traditionalism", "Operational", "ideology")

# fitting models
for(i in 1:length(DVs)){
  for(j in 1:length(specifications)){
    
    name <- paste0(paste(specifications[[j]], collapse = "_"), "-", DVs[[i]])
    
    form <- as.formula(paste(DVs[i], "~", paste(specifications[[j]], collapse = " + "), "+ (1|state)"))
    
    mod <-  lmer(form, 
                 data = dat[,c(specifications[[j]], DVs[i], "state")], 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    
    results[[name]] <- mod
  }
}



# specifications with interactions
results_1 <- list()
interactions <- list(c("Open", "Extra", "Neur", "Agree", "Cons", "ue_rate", "p_non_white", "Crime.Rate", 
                       "Open:ue_rate", "Open:p_non_white", "Open:Crime.Rate", "Cons:ue_rate", "Cons:p_non_white",
                       "Cons:Crime.Rate"),
                     c("Open", "ue_rate", "p_non_white", "Crime.Rate", 
                       "Open:ue_rate", "Open:p_non_white", "Open:Crime.Rate"),
                     c("Open", "ue_rate", "Open:ue_rate"),
                     c("Open", "p_non_white", "Open:p_non_white"),
                     c("Open", "Crime.Rate", "Open:Crime.Rate"),
                     c("Cons", "ue_rate", "p_non_white", "Crime.Rate", 
                       "Cons:ue_rate", "Cons:p_non_white", "Cons:Crime.Rate"),
                     c("Cons", "ue_rate", "Cons:ue_rate"),
                     c("Cons", "p_non_white", "Cons:p_non_white"),
                     c("Cons", "Crime.Rate", "Cons:Crime.Rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year", 
                       "Open", "Extra", "Neur", "Agree", "Cons", "ue_rate", "p_non_white", "Crime.Rate",
                       "Open:Crime.Rate", "Open:p_non_white", "Open:ue_rate", "Cons:Crime.Rate", 
                       "Cons:p_non_white", "Cons:ue_rate"), 
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Open", "ue_rate", "p_non_white", "Crime.Rate", 
                       "Open:ue_rate", "Open:p_non_white", "Open:Crime.Rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Open", "ue_rate", "Open:ue_rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Open", "p_non_white", "Open:p_non_white"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Open", "Crime.Rate", "Open:Crime.Rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Cons", "ue_rate", "p_non_white", "Crime.Rate", 
                       "Cons:ue_rate", "Cons:p_non_white", "Cons:Crime.Rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Cons", "ue_rate", "Cons:ue_rate"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Cons", "p_non_white", "Cons:p_non_white"),
                     c("male", "age", "contrast1", "contrast2", "contrast3", "income", "edu", "Year",
                       "Cons", "Crime.Rate", "Cons:Crime.Rate"))



# fitting models
for(i in 1:length(DVs)){
  for(j in 1:length(interactions)){
    
    name <- paste0(paste(interactions[[j]], collapse = "_"), "-", DVs[[i]])
    
    form <- as.formula(paste(DVs[i], "~", paste(interactions[[j]], collapse = " + "), "+ (1|state)"))
    
    mod <-  lmer(form, 
                 data = dat, 
                 control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
    
    results_1[[name]] <- mod
  }
}

# saving results for tidying 
saveRDS(results_1, "results_interactions.rds")
saveRDS(results, "results.rds")
rm(list = ls())
