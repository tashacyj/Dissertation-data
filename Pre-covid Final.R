## set working directory and load dataset
setwd("~/Dissertation/Data")
com_filtered <- read.csv("pre-coviddata.csv")

## install and load packages 
library(haven)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(lmtest)
library(multiwayvcov)
library(fixest)
library(broom)
library(latticeExtra)
library(patchwork) 

## prepare variables
# recode variables
# dependent variables (DV)
# deportation 
com_filtered$deportation <- as.numeric(com_filtered$deportation)
com_filtered$deportation <- case_when(
  com_filtered$deportation == 2 ~ 0, 
  com_filtered$deportation == 1 ~ 1, 
  TRUE ~ NA_real_) 
summary(as.factor(com_filtered$deportation))

# wall 
com_filtered$wall <- as.numeric(com_filtered$wall)
com_filtered$wall <- case_when(
  com_filtered$wall == 2 ~ 0, 
  com_filtered$wall == 1 ~ 1, 
  TRUE ~ NA_real_) 
summary(as.factor(com_filtered$wall))

# dreamers
com_filtered$dreamers <- as.numeric(com_filtered$dreamers)
com_filtered$dreamers <- case_when(
  com_filtered$dreamers == 1 ~ 0, 
  com_filtered$dreamers == 2 ~ 1, 
  TRUE ~ NA_real_)
summary(as.factor(com_filtered$dreamers))


# mediator variable
# group favourability of undocumented immigrants 
com_filtered$group_favorability_undocumented <- as.numeric(com_filtered$group_favorability_undocumented)
com_filtered$group_favorability_undocumented <- case_when(
  com_filtered$group_favorability_undocumented == 1 ~ 4, 
  com_filtered$group_favorability_undocumented == 2 ~ 3,
  com_filtered$group_favorability_undocumented == 3 ~ 2, 
  com_filtered$group_favorability_undocumented == 4 ~ 1,
  com_filtered$group_favorability_undocumented == 999 ~ NA_real_,
  TRUE ~ NA_real_) # score increases with favourability of immigrants 
summary(as.factor(com_filtered$group_favorability_undocumented))


## group data by state and wave
# group by 'state' and 'wave', calculate the mean of corona and number of observations within each group
df <- com_filtered %>%
  group_by(state, wave) %>%
  summarise(deport = mean(deportation, na.rm = TRUE), 
            wall = mean(wall, na.rm = TRUE), 
            dreamers = mean(dreamers, na.rm = TRUE),
            grpfav = mean(group_favorability_undocumented, na.rm = TRUE), 
            num_observations = n())


## filter unrepresentative waves and states
# filter unrepresentative waves
df$representative <- ifelse(df$num_observations > 50, 1, 0)
summary(as.factor(df$representative)) #distribution 
df_rep <- df[df$representative > 0, ] # remove unrepresentative observations
length(unique(df_rep$state)) # number of states left - 39. 

# filter unrepresentative states
visual1 <- df_rep %>%
  group_by(state) %>%
  summarise(waves_number = n())
df_final <- subset(df_rep, !(state %in% c("HI", "NE", "NM"))) #remove states that only have one time period 

#### Visuals ####

## Visual 1: large increase VS small increase in death anxiety 
precovid_large <- read.csv("precovid_large.csv")

df_final$large <- ifelse(df_final$state %in% precovid_large$state, 1, 0)
visual1 <- df_final %>%
  group_by(wave, large) %>%
  summarise(wall = mean(wall),
            dreamers = mean(dreamers), 
            deport = mean(deport))
visual1$wave_new <- visual1$wave - 29
View(visual1)

plot_dreamers <- ggplot(visual1, aes(x = wave_new, y = dreamers, linetype = factor(large))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.223, ymax = 0.217), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.22, label = "Pandemic", hjust = 0.5, fontface = "bold") +
  ylim(0.17, 0.23) +
  xlim(-30, 3) +
  labs(title = "Support for no citizenship to immigrant children pre-pandemic", x = "Weeks leading to pandemic", y = "Support for no citizenship", color = "Large") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Change in death anxiety within state", labels = c("Small or no increase", "Large increase")) +
  theme_minimal()


plot_deport <- ggplot(visual1, aes(x = wave_new, y = deport, linetype = factor(large))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.4825, ymax = 0.4925), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.4875, label = "Pandemic", hjust = 0.5, fontface = "bold") +
  ylim(0.4, 0.5) +
  xlim(-30, 3) +
  labs(title = "Support for deportation of immigrants pre-pandemic", x = "Weeks leading to pandemic", y = "Support for deportation", color = "Large") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Change in death anxiety within state", labels = c("Small or no increase", "Large increase")) +
  theme_minimal()


plot_wall <- ggplot(visual1, aes(x = wave_new, y = wall, linetype = factor(large))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.4825, ymax = 0.4925), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.4875, label = "Pandemic",hjust = 0.5, fontface = "bold") +
  ylim(0.4, 0.5) +
  xlim(-30, 3) +
  labs(title = "Support for US-Mexico wall pre-pandemic", x = "Weeks leading to pandemic", y = "Support for US-Mexico wall", color = "Large") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "Change in death anxiety within state", labels = c("Small or no increase", "Large increase")) +
  theme_minimal()

combined_plot_with_title <- (plot_dreamers) / (plot_wall) / (plot_deport) +  
  plot_annotation(title = "Figure 2. Pre-pandemic trends in anti-immigration sentiment in states where there was a large increase in death anxiety") &  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("combined_plot_with_title.png", combined_plot_with_title, width = 10, height = 15, units = "in")



## Visual 2: favourable vs unfavourable of immigrants 
pre_covid_favourable <- read.csv("precovid_statesfavourable.csv")

unique_states_df <- pre_covid_favourable %>% 
  distinct(state, .keep_all = TRUE)

df_final$fav <- ifelse(df_final$state %in% unique_states_df$state, 1, 0)

visual2 <- df_final %>%
  group_by(wave, fav) %>%
  summarise(wall = mean(wall),
            dreamers = mean(dreamers), 
            deport = mean(deport))
visual2$wave_new <- visual2$wave - 29

plot_dreamers_fav <- ggplot(visual2, aes(x = wave_new, y = dreamers, linetype = factor(fav))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.223, ymax = 0.217), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.22, label = "Pandemic", hjust = 0.5, fontface = "bold") +
  ylim(0.17, 0.23) +
  xlim(-30, 3) +
  labs(title = "Support for no citizenship to immigrant children pre-pandemic", x = "Weeks leading to pandemic", y = "Support for no citizenship", color = "fav") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "States favourability of immigrants", labels = c("Not favourable", "Favourable")) +
  theme_minimal()


plot_deport_fav <- ggplot(visual2, aes(x = wave_new, y = deport, linetype = factor(fav))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.495, ymax = 0.505), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.5, label = "Pandemic", hjust = 0.5, fontface = "bold") +
  ylim(0.4, 0.52) +
  xlim(-30, 3) +
  labs(title = "Support for deportation of immigrants pre-pandemic", x = "Weeks leading to pandemic", y = "Support for deportation", color = "fav") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "States favourability of immigrants", labels = c("Not favourable", "Favourable")) +
  theme_minimal()


plot_wall_fav <- ggplot(visual2, aes(x = wave_new, y = wall, linetype = factor(fav))) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +  # Using loess method for smoothing
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rect(aes(xmin = - 3, xmax = 3, ymin = 0.495, ymax = 0.505), fill = "lightgray", alpha = 0.5, colour = NA) +
  annotate("text", x = 0, y = 0.5, label = "Pandemic",hjust = 0.5, fontface = "bold") +
  ylim(0.4, 0.52) +
  xlim(-30, 3) +
  labs(title = "Support for US-Mexico wall pre-pandemic", x = "Weeks leading to pandemic", y = "Support for US-Mexico wall", color = "fav") +
  scale_linetype_manual(values = c("solid", "dashed"), name = "States favourability of immigrants", labels = c("Not favourable", "Favourable")) +
  theme_minimal()

combined_plot_fav_with_title <- (plot_dreamers_fav) / (plot_wall_fav) / (plot_deport_fav) +  
  plot_annotation(title = "Figure 4. Pre-pandemic trends in anti-immigration sentiment in states favourable of immigrants") &  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("combined_plot_fav_with_title.png", combined_plot_fav_with_title, width = 10, height = 15, units = "in")

