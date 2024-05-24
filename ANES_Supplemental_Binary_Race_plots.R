getwd()

# libraries
library(ggplot2)
library(dplyr)
library(broom.mixed)

# loading rdata
load("ANES_for_plotting_binary_race.Rdata")

options(scipen = 999)
summary(m_ideo_binary_race)


# turning into tidy objects 
tidy_ideo <- tidy(m_ideo_binary_race, effects = "fixed", conf.int = TRUE)
tidy_trad <- tidy(m_trad_binary_race, effects = "fixed", conf.int = TRUE)
tidy_operational <- tidy(m_operational_binary_race, effects = "fixed", conf.int = TRUE)

tidy_ideo_noint <- tidy(m_ideo_no_int_binary_race, effects = "fixed", conf.int = TRUE)
tidy_trad_noint <- tidy(m_trad_no_int_binary_race, effects = "fixed", conf.int = TRUE)
tidy_op_noint <- tidy(m_operational_no_int_binary_race, effects = "fixed", conf.int = TRUE)

# Renaming for nice visuals 
unique(tidy_ideo$term)
tidy_ideo_noint <- tidy_ideo_noint %>% mutate(term = case_when(term == "Open" ~ "Openness", 
                                                               term == "Extra" ~ "Extraversion", 
                                                               term == "Agree" ~ "Agreeableness",
                                                               term == "Cons" ~ "Conscientious", 
                                                               term == "male" ~ "Male", 
                                                               term == "Crime.Rate" ~ "Crime Rate", 
                                                               term == "ue_rate" ~ "Unemployment", 
                                                               term == "p_non_white"~ "Proportion Non-White"))

tidy_op_noint <- tidy_op_noint %>% mutate(term = case_when(term == "Open" ~ "Openness", 
                                                           term == "Extra" ~ "Extraversion", 
                                                           term == "Agree" ~ "Agreeableness",
                                                           term == "Cons" ~ "Conscientious", 
                                                           term == "male" ~ "Male", 
                                                           term == "Crime.Rate" ~ "Crime Rate", 
                                                           term == "ue_rate" ~ "Unemployment", 
                                                           term == "p_non_white"~ "Proportion Non-White"))


tidy_trad_noint <- tidy_trad_noint %>% mutate(term = case_when(term == "Open" ~ "Openness", 
                                                               term == "Extra" ~ "Extraversion", 
                                                               term == "Agree" ~ "Agreeableness",
                                                               term == "Cons" ~ "Conscientious", 
                                                               term == "male" ~ "Male", 
                                                               term == "Crime.Rate" ~ "Crime Rate", 
                                                               term == "ue_rate" ~ "Unemployment", 
                                                               term == "p_non_white"~ "Proportion Non-White"))

# now for interactions with personality
unique(tidy_ideo$term)
tidy_ideo <- tidy_ideo %>% mutate(term = case_when(term == "Open:Crime.Rate" ~ "Open:CrimeRate", 
                                                   term == "Cons:Crime.Rate" ~ "Cons:CrimeRate", 
                                                   term == "Open:ue_rate" ~ "Open:Unemployment", 
                                                   term == "Cons:ue_rate" ~ "Cons:Unemployment", 
                                                   term == "Open:p_non_white" ~ "Open:NonWhite",
                                                   term == "Cons:p_non_white" ~ "Cons:NonWhite"))

tidy_trad <- tidy_trad %>% mutate(term = case_when(term == "Open:Crime.Rate" ~ "Open:CrimeRate", 
                                                   term == "Cons:Crime.Rate" ~ "Cons:CrimeRate", 
                                                   term == "Open:ue_rate" ~ "Open:Unemployment", 
                                                   term == "Cons:ue_rate" ~ "Cons:Unemployment", 
                                                   term == "Open:p_non_white" ~ "Open:NonWhite",
                                                   term == "Cons:p_non_white" ~ "Cons:NonWhite"))

tidy_operational <- tidy_operational %>% mutate(term = case_when(term == "Open:Crime.Rate" ~ "Open:CrimeRate", 
                                                                 term == "Cons:Crime.Rate" ~ "Cons:CrimeRate", 
                                                                 term == "Open:ue_rate" ~ "Open:Unemployment", 
                                                                 term == "Cons:ue_rate" ~ "Cons:Unemployment", 
                                                                 term == "Open:p_non_white" ~ "Open:NonWhite",
                                                                 term == "Cons:p_non_white" ~ "Cons:NonWhite"))

# filtering terms of interest for plotting 
unique(tidy_ideo_noint$term)
tidy_ideo_noint <- tidy_ideo_noint %>% filter(term == "Crime Rate" | term == "Unemployment" | 
                                                term == "Proportion Non-White")

tidy_trad_noint <- tidy_trad_noint %>%  filter(term == "Crime Rate" | term == "Unemployment" | 
                                                 term == "Proportion Non-White")

tidy_op_noint <- tidy_op_noint %>%  filter(term == "Crime Rate" | term == "Unemployment" | 
                                             term == "Proportion Non-White")

unique(tidy_ideo$term)
tidy_ideo <- tidy_ideo %>% filter(term == "Open:CrimeRate"| term == "Cons:CrimeRate"| 
                                    term == "Cons:Unemployment"| term == "Open:Unemployment"| 
                                    term == "Open:NonWhite"| term == "Cons:NonWhite")

tidy_trad <- tidy_trad %>% filter(term == "Open:CrimeRate"| term == "Cons:CrimeRate"| 
                                    term == "Cons:Unemployment"| term == "Open:Unemployment"| 
                                    term == "Open:NonWhite"| term == "Cons:NonWhite")

tidy_operational <- tidy_operational %>% filter(term == "Open:CrimeRate"| term == "Cons:CrimeRate"| 
                                                  term == "Cons:Unemployment"| term == "Open:Unemployment"| 
                                                  term == "Open:NonWhite"| term == "Cons:NonWhite")

View(tidy_operational)

# plotting adding model variable and binding df's 
tidy_ideo$Model <- "Symbolic"
tidy_trad$Model <- "Social"
tidy_operational$Model <- "Economic"

tidy_ideo_noint$Model <- "Symbolic"
tidy_trad_noint$Model <- "Social"
tidy_op_noint$Model <- "Economic"

interactions <- rbind(tidy_ideo, tidy_trad, tidy_operational)
threats <- rbind(tidy_ideo_noint, tidy_trad_noint, tidy_op_noint)

# coding aesthetics we want to map 
lib_con <- function(x) {
  ifelse(x < 0, "Liberal", 
         ifelse(x > 0, "Conservative", NA))
}

sig <- function(x) {
  ifelse(x < .05, "Significant", "Non-significant")
}



threats$Direction <- lib_con(threats$estimate)

threats$Significance <- sig(threats$p.value)

## decided to code theory instead of direction for interactions
interactions$Theory <- ifelse(stringr::str_detect(interactions$term, "Cons:") == TRUE & interactions$estimate > 0 & interactions$p.value < .05, "Neg. Bias", 
                              ifelse(stringr::str_detect(interactions$term, "Open:") == TRUE & interactions$estimate < 0 & interactions$p.value < .05, "Neg. Bias", 
                                     ifelse(stringr::str_detect(interactions$term, "Open:") == TRUE & interactions$estimate > 0 & interactions$p.value < .05, "Threat Constraint", "Neither"))) 


# relevel facts to match other figures
library(tidyverse)
interactions <- interactions %>% mutate(Theory = factor(Theory)) %>%
  mutate(Theory=fct_relevel(Theory, c("Neg. Bias", "Threat Constraint", "Neither")))

## forest plots 
plot1 <- ggplot(threats, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange(aes(color = Direction, shape = Significance), position = position_dodge(width = .2)) +
  geom_vline(xintercept = 0, lty = 3) + theme_classic() + theme(axis.text.x = element_text(angle = 45)) + 
  facet_wrap(~Model) +    scale_colour_manual(values = c("#ca0020", "#0571b0"))

plot1

plot2 <- ggplot(interactions, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange(aes(color = Theory), position = position_dodge(width = .2)) +
  geom_vline(xintercept = 0, lty = 3) + theme_classic() + theme(axis.text.x = element_text(angle = 45)) + 
  facet_wrap(~Model)  +  scale_colour_manual(values = c("#E69F00", "#009E73", "#000000"))

plot2

# start fresh 
rm(list = ls())
