getwd()

# libraries
library(dplyr)
library(tidyverse)
library(broom.mixed)

# data
interactions <- readRDS("results_interactions.rds")
mains <- readRDS("results.rds")

# tidying
interactions_t <- list()
for(i in 1:length(interactions)){
  interactions_t[[i]] <- tidy(interactions[[i]], effects = "fixed", conf.int = TRUE)
}

mains_t <- list()
for(i in 1:length(mains)){
  mains_t[[i]] <- tidy(mains[[i]], effects = "fixed", conf.int = TRUE)
}

names(interactions)[[1]]

# saving specification 
for(i in 1:length(interactions)){
  interactions_t[[i]]$spec <- names(interactions)[[i]]
}

for(i in 1:length(mains)){
  mains_t[[i]]$spec <- names(mains)[[i]]
}

# main effect or interaction for grouping 
for(i in 1:length(mains_t)){
  mains_t[[i]]$grouping <- "Main Effect"
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$grouping <- "Interaction"
}

# subsetting out terms of interest
variable.names(mains_t[[1]]) 
mains_t[[1]]$term

for(i in 1:length(mains_t)){
  mains_t[[i]] <- subset(mains_t[[i]], mains_t[[i]]$term == "ue_rate" | mains_t[[i]]$term == "p_non_white" | mains_t[[i]]$term == "Crime.Rate")
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]] <- subset(interactions_t[[i]], interactions_t[[i]]$term == "Cons:ue_rate" | interactions_t[[i]]$term == "Cons:p_non_white" | 
                                  interactions_t[[i]]$term == "Cons:Crime.Rate" | interactions_t[[i]]$term == "Open:ue_rate" |
                                  interactions_t[[i]]$term == "Open:p_non_white" | interactions_t[[i]]$term == "Open:Crime.Rate")
}

# coding last few facets to map, then saving in wide form for top plot 

# direction 
for(i in 1:length(mains_t)){
  mains_t[[i]]$Direction <- ifelse(mains_t[[i]]$estimate < 0 & mains_t[[i]]$p.value < .05, "Liberal", 
                                   ifelse(mains_t[[i]]$estimate > 0 & mains_t[[i]]$p.value < .05, "Conservative", "NS"))
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Direction <- ifelse(interactions_t[[i]]$estimate < 0 & interactions_t[[i]]$p.value < .05, "Liberal", 
                                          ifelse(interactions_t[[i]]$estimate > 0 & interactions_t[[i]]$p.value < .05, "Conservative", "NS"))
}

# dummy indicator for whether term included in model 
# controls (v no controls)
# dv (traditionalism, operational, ideology)
# personality traits 
# threats 
# personality moderators 

for(i in 1:length(mains_t)){
  mains_t[[i]]$Controls <- ifelse(str_detect(mains_t[[i]]$spec, "male") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Controls <- ifelse(str_detect(interactions_t[[i]]$spec, "male") == TRUE, 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Agree <- ifelse(str_detect(mains_t[[i]]$spec, "Agree") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Agree <- ifelse(str_detect(interactions_t[[i]]$spec, "Agree") == TRUE, 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Extra <- ifelse(str_detect(mains_t[[i]]$spec, "Extra") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Extra <- ifelse(str_detect(interactions_t[[i]]$spec, "Extra") == TRUE, 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Neur <- ifelse(str_detect(mains_t[[i]]$spec, "Neur") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Neur <- ifelse(str_detect(interactions_t[[i]]$spec, "Neur") == TRUE, 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Neur <- ifelse(str_detect(mains_t[[i]]$spec, "Neur") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Open <- ifelse(str_detect(interactions_t[[i]]$spec, "Open") == TRUE, 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Open <- ifelse(str_detect(mains_t[[i]]$spec, "Open") == TRUE, 1, 0)
}

for(i in 1:length(interactions_t)){
  interactions_t[[i]]$Cons <- ifelse(str_detect(interactions_t[[i]]$spec, "Cons") == TRUE , 1, 0)
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$Cons <- ifelse(str_detect(mains_t[[i]]$spec, "Cons") == TRUE, 1, 0)
}

# DVs
for(i in 1:length(interactions_t)){
  interactions_t[[i]]$dv <- ifelse(str_detect(interactions_t[[i]]$spec, "traditionalism") == TRUE , "Social Ideology", 
                                   ifelse(str_detect(interactions_t[[i]]$spec, "ideology") == TRUE, "Symbolic Ideology", 
                                          ifelse(str_detect(interactions_t[[i]]$spec, "Operational") == TRUE, "Economic Ideology", 0)))
}

for(i in 1:length(mains_t)){
  mains_t[[i]]$dv <- ifelse(str_detect(mains_t[[i]]$spec, "traditionalism") == TRUE , "Social Ideology", 
                            ifelse(str_detect(mains_t[[i]]$spec, "ideology") == TRUE, "Symbolic Ideology", 
                                   ifelse(str_detect(mains_t[[i]]$spec, "Operational") == TRUE, "Economic Ideology", 0)))
}


# unlist 
mains_df <- data.frame()
for(i in 1:length(mains_t)){ 
  mains_df <- rbind(mains_df, mains_t[[i]])
  
}

interactions_df <- data.frame()

for(i in 1:length(interactions_t)){ 
  interactions_df <- rbind(interactions_df, interactions_t[[i]])
  
}


# code n_effect as n_row for each dataframe 
interactions_df <- interactions_df %>% arrange(estimate)
interactions_df$n_effect <- 1:nrow(interactions_df)
mains_df <- mains_df %>% arrange(estimate)
mains_df$n_effect <- 1:nrow(mains_df)

# saving wide for top plot 
saveRDS(interactions_df, "interactions_wide.rds")
saveRDS(mains_df, "mains_wide.rds")

# threats
interactions_df$ue_rate <- ifelse(str_detect(interactions_df$term, ":ue_rate") == TRUE, 1, 0)
interactions_df$p_non_white <- ifelse(str_detect(interactions_df$term, ":p_non_white") == TRUE, 1, 0)
interactions_df$Crime.Rate <- ifelse(str_detect(interactions_df$term, ":Crime.Rate") == TRUE, 1, 0)

mains_df$ue_rate <- ifelse(str_detect(mains_df$spec, "ue_rate") == TRUE, 1, 0)
mains_df$p_non_white <- ifelse(str_detect(mains_df$spec, "p_non_white") == TRUE, 1, 0)
mains_df$Crime.Rate <- ifelse(str_detect(mains_df$spec, "Crime.Rate") == TRUE, 1, 0)

# personality moderators (for interaction models)
interactions_df$Open_Mod <- ifelse(str_detect(interactions_df$term, "Open:") == TRUE, 1, 0)
interactions_df$Cons_Mod <- ifelse(str_detect(interactions_df$term, "Cons:") == TRUE, 1, 0)



# pivoting longer 
interactions_long <- interactions_df %>% pivot_longer(c(Controls:Cons, ue_rate:Cons_Mod), names_to = "predictor", values_to = "present")
interactions_long <- subset(interactions_long, interactions_long$present == 1)
interactions_longer <- interactions_long %>% pivot_longer(c(dv,predictor), names_to = "type", values_to = "variable")

# coding class
unique(interactions_long$predictor)
interactions_longer$variable <- ifelse(interactions_longer$variable == "Controls", "Dem. Controls", interactions_longer$variable)

interactions_longer$class <- ifelse(interactions_longer$variable == "Agree" | interactions_longer$variable == "Open" | 
                                      interactions_longer$variable == "Cons" | interactions_longer$variable == "Extra" | 
                                      interactions_longer$variable == "Neur", "Controls", 
                                    ifelse(interactions_longer$variable == "Dem. Controls", "Controls", 
                                           ifelse(interactions_longer$variable == "p_non_white" | 
                                                    interactions_longer$variable == "ue_rate" | interactions_longer$variable == "Crime.Rate", "Threat",
                                                  ifelse(interactions_longer$variable == "Cons_Mod" | interactions_longer$variable == "Open_Mod", "Moderator", 
                                                         ifelse(interactions_longer$variable == "Social Ideology" | interactions_longer$variable == "Symbolic Ideology" | 
                                                                  interactions_longer$variable == "Economic Ideology", "Outcome", "failed"))))) 

# writing into dataframe for plotting 
saveRDS(interactions_longer, "long_interactions.rds")

# pivoting mains longer 
mains_long <- mains_df %>% pivot_longer(c(Controls:Cons, ue_rate:Crime.Rate), names_to = "variable", values_to = "present")
mains_long <- subset(mains_long, mains_long$present == 1)

mains_longer <- mains_long %>% pivot_longer(c(dv, variable), names_to = "type", values_to = "variable")
mains_longer$variable <- ifelse(mains_longer$variable == "Controls", "Dem. Controls", mains_longer$variable)
unique(mains_longer$variable) 

# coding class aesthetic
mains_longer$class <- ifelse(mains_longer$variable == "Agree" | mains_longer$variable == "Open" | 
                               mains_longer$variable == "Cons" | mains_longer$variable == "Extra" | 
                               mains_longer$variable == "Neur", "Controls", 
                             ifelse(mains_longer$variable == "Dem. Controls", "Controls", 
                                    ifelse(mains_longer$variable == "p_non_white" | 
                                             mains_longer$variable == "ue_rate" | mains_longer$variable == "Crime.Rate", "Threat",
                                           ifelse(mains_longer$variable == "Social Ideology" | mains_longer$variable == "Symbolic Ideology" | 
                                                    mains_longer$variable == "Economic Ideology", "Outcome", "failed"))))

# saving for plotting 
saveRDS(mains_longer, "mains_longer.rds")

rm(list = ls())
