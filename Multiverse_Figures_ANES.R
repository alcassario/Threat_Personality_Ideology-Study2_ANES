getwd()

# libraries
library(ggplot2)
library(tidyverse)
library(dplyr)

# data 
interactions_wide <- readRDS("interactions_wide.rds")
interactions_long <- readRDS("long_interactions.rds")
mains_wide <- readRDS("mains_wide.rds")
mains_long <- readRDS("mains_longer.rds")

# number of mods of theoretical interest 
length(unique(interactions_wide$spec)) # 54 
length(unique(mains_wide$spec)) # 84 

# adding theory variable, decided to map this aesthetic for interactions 
interactions_wide$Theory <- ifelse(stringr::str_detect(interactions_wide$term, "Cons:") == TRUE & interactions_wide$estimate > 0 & interactions_wide$p.value < .05, "Neg. Bias", 
                                   ifelse(stringr::str_detect(interactions_wide$term, "Open:") == TRUE & interactions_wide$estimate < 0 & interactions_wide$p.value < .05, "Neg. Bias", 
                                          ifelse(stringr::str_detect(interactions_wide$term, "Open:") == TRUE & interactions_wide$estimate > 0 & interactions_wide$p.value < .05, "Threat Constraint", "Neither"))) 


# relevel facts to match other figures
interactions_wide <- interactions_wide %>% mutate(Theory = factor(Theory)) %>%
  mutate(Theory=fct_relevel(Theory, c("Neg. Bias", "Threat Constraint", "Neither")))


# releveling so control variable facets in a better order 
interactions_long <- interactions_long %>% mutate(variable = fct_relevel(variable, "Open", "Cons", "Extra", "Agree", "Neur", "Dem. Controls")) 
mains_long <- mains_long %>% mutate(variable = fct_relevel(variable, "Open", "Cons", "Extra", "Agree", "Neur", "Dem. Controls")) 

unique(mains_long$class)
interactions_long <- interactions_long %>% mutate(class = fct_relevel(class, "Moderator", "Threat", "Controls", "Outcome")) 
mains_long <- mains_long %>% mutate(class = fct_relevel(class, "Threat", "Controls", "Outcome")) 

# now for adding theory variable to long 
# adding theory variable, decided to map this aesthetic for interactions 
interactions_long$Theory <- ifelse(stringr::str_detect(interactions_long$term, "Cons:") == TRUE & interactions_long$estimate > 0 & interactions_long$p.value < .05, "Neg. Bias", 
                                   ifelse(stringr::str_detect(interactions_long$term, "Open:") == TRUE & interactions_long$estimate < 0 & interactions_long$p.value < .05, "Neg. Bias", 
                                          ifelse(stringr::str_detect(interactions_long$term, "Open:") == TRUE & interactions_long$estimate > 0 & interactions_long$p.value < .05, "Threat Constraint", "Neither"))) 


# relevel facts to match other figures
interactions_long <- interactions_long %>% mutate(Theory = factor(Theory)) %>%
  mutate(Theory=fct_relevel(Theory, c("Neg. Bias", "Threat Constraint", "Neither")))


# main effect plots 

plotmains <- ggplot(mains_wide, aes(x = n_effect)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey90") +
  geom_point(aes(y = estimate, color = as.factor(Direction)), size = .5) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#FF0000", "#56B4E9", "#000000")) +
  facet_grid(grouping ~ ., scales = "free", space = "free") +
  labs(title = "Threat Main Effects",
       y = "Regression Coefficient") +
  theme_cowplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.y = element_text(size = 16), 
    strip.text = element_text(size = 16)
  )
plotmains

#interactions_wide <- interactions_wide %>% arrange(estimate)
#interactions_wide$n_effect <- 1:nrow(interactions_wide)


plotinteractions <- ggplot(interactions_wide, aes(x = n_effect)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey90") +
  geom_point(aes(y = estimate, color = as.factor(Theory)), size = .5) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#000000")) +
  facet_grid(grouping ~ ., scales = "free", space = "free") +
  labs(title = "Personality Interactions",
       y = "Regression Coefficient") +
  theme_cowplot() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(), 
    axis.text.y = element_text(size = 16), 
    strip.text = element_text(size = 16)
  )
plotinteractions

#### bottom plots ###
plot.multiverse.mains <-
  ggplot(data = mains_long, aes(x = n_effect, y = fct_rev(variable), color = Direction)) +
  geom_point(shape = "|", size = 4, alpha = 0.6) +
  scale_color_manual(values = c("#FF0000","#56B4E9", "#000000")) +
  facet_grid(class ~ ., scales = "free", space = "free") +
  labs(y = "Variables", 
       x = "Main Effect",
       caption = "Note: Error bars are 95% confidence intervals.") +
  theme_cowplot() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) 
plot.multiverse.mains

plot.multiverse.ints <-
  ggplot(data = interactions_long, aes(x = n_effect, y = fct_rev(variable), color = Theory)) +
  geom_point(shape = "|", size = 4, alpha = 0.6) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#000000")) +
  facet_grid(class ~ ., scales = "free", space = "free") +
  labs(y = "Variables", 
       x = "Interaction Effect",
       caption = "Note: Error bars are 95% confidence intervals.") +
  theme_cowplot() +
  theme(
    legend.position = "none", axis.text.x = element_text(size = 16), 
    axis.text.y = element_text(size = 16),
    strip.text = element_text(size = 16)
  ) 
plot.multiverse.ints

## arranging into single figure 
plots <- list(plotmains, plot.multiverse.mains)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

g <- do.call("grid.arrange", c(grobs, ncol = 1))
ggsave("mainsmultiplotanes.pdf", g, height = 17, width = 12, units = "in")


# interaction plot 
plots <- list(plotinteractions, plot.multiverse.ints)
grobs <- list()
widths <- list()

for (i in 1:length(plots)){
  grobs[[i]] <- ggplotGrob(plots[[i]])
  widths[[i]] <- grobs[[i]]$widths[2:5]
}

maxwidth <- do.call(grid::unit.pmax, widths)
for (i in 1:length(grobs)){
  grobs[[i]]$widths[2:5] <- as.list(maxwidth)
}

g <- do.call("grid.arrange", c(grobs, ncol = 1))
ggsave("intsmultiplotanes.pdf", g, height = 17, width = 15, units = "in")

