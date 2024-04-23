## set working directory and load dataset
setwd("~/Dissertation/Data")
com_filtered <- read.csv("data.csv")

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


# summary statistics variables 
# gender
com_filtered$gender <- as.numeric(com_filtered$gender)
com_filtered$gender <- case_when(
  com_filtered$gender == 1 ~ 1, 
  com_filtered$gender == 2 ~ 0,
  TRUE ~ NA_real_) 
summary(as.factor(com_filtered$gender)) 

# education - no high sch
com_filtered$no_highsch <- as.numeric(com_filtered$education)
com_filtered$no_highsch <- case_when(
  com_filtered$no_highsch == 1 ~ 1, 
  com_filtered$no_highsch == 2 ~ 1,
  com_filtered$no_highsch == 3 ~ 1,
  com_filtered$no_highsch == 4 ~ 0, 
  com_filtered$no_highsch == 5 ~ 0,
  com_filtered$no_highsch == 6 ~ 0,
  com_filtered$no_highsch == 7 ~ 0, 
  com_filtered$no_highsch == 8 ~ 0,
  com_filtered$no_highsch == 9 ~ 0,
  com_filtered$no_highsch == 10 ~ 0,
  com_filtered$no_highsch == 11 ~ 0,
  TRUE ~ NA_real_) 
summary(as.factor(com_filtered$no_highsch)) 

# education - college degree
com_filtered$college_deg <- as.numeric(com_filtered$education)
com_filtered$college_deg <- case_when(
  com_filtered$college_deg == 1 ~ 0, 
  com_filtered$college_deg == 2 ~ 0,
  com_filtered$college_deg == 3 ~ 0,
  com_filtered$college_deg == 4 ~ 0, 
  com_filtered$college_deg == 5 ~ 0,
  com_filtered$college_deg == 6 ~ 0,
  com_filtered$college_deg == 7 ~ 0, 
  com_filtered$college_deg == 8 ~ 1,
  com_filtered$college_deg == 9 ~ 1,
  com_filtered$college_deg == 10 ~ 1,
  com_filtered$college_deg == 11 ~ 1,
  TRUE ~ NA_real_) 
summary(as.factor(com_filtered$college_deg))

# hispanic 
com_filtered$hisp <- ifelse(com_filtered$hispanic == 1, 0, 1)

# black 
com_filtered$black <- ifelse(com_filtered$race_ethnicity == 2, 1, 0)


# control variable
# state level unemployment
control <- read.csv("Control/Unemployment.csv")
control <- control %>%
  pivot_longer(cols = Wave_1: Wave_41,
               names_to = "wave",
               names_prefix = "Wave_", 
               values_to = "economy")

com_filtered <- merge(com_filtered, control, by = c("state", "wave")) # merge dataset


# independent variable (IV)
# fear of covid
com_filtered$extra_corona_concern <- as.numeric(com_filtered$extra_corona_concern)
com_filtered$extra_corona_concern <- case_when(
  com_filtered$extra_corona_concern == 4 ~ 1, 
  com_filtered$extra_corona_concern == 3 ~ 2, 
  com_filtered$extra_corona_concern == 2 ~ 3, 
  com_filtered$extra_corona_concern == 1 ~ 4, 
  TRUE ~ NA_real_) # increasingly concerned with Covid-19 as score increases 
summary(as.factor(com_filtered$extra_corona_concern))

# mortality salience: weekly death rates
mortality <- read.csv("Measure 2 - Mortality rate/MS_final.csv") # weekly death count
mortality <- mortality %>%
  pivot_longer(cols = Wave_1: Wave_41,
               names_to = "wave",
               names_prefix = "Wave_", 
               values_to = "X")

mortality$x_log <- log(mortality$X) # log scaling 

names(mortality)[1] <- "state"
mortality$state[mortality$state == "ME "] <- "ME"
mortality$state[mortality$state == "MD "] <- "MD"
mortality$state[mortality$state == "MO "] <- "MO"
mortality$state[mortality$state == "OK "] <- "OK"
mortality$state[mortality$state == "OR "] <- "OR"
mortality$state[mortality$state == "PA "] <- "PA"
mortality$state[mortality$state == "RI "] <- "RI"
mortality$state[mortality$state == "WI "] <- "WI" 
mortality$state[mortality$state == "WY "] <- "WY" #recode entries for merger 

merged_df <- merge(com_filtered, mortality, by = c("state", "wave")) # merge dataset


## group data by state and wave
# group by 'state' and 'wave', calculate the mean of corona and number of observations within each group
df <- merged_df %>%
  group_by(state, wave) %>%
  summarise(covid = mean(extra_corona_concern, na.rm = TRUE),
            deport = mean(deportation, na.rm = TRUE), 
            wall = mean(wall, na.rm = TRUE), 
            dreamers = mean(dreamers, na.rm = TRUE),
            grpfav = mean(group_favorability_undocumented, na.rm = TRUE), 
            mortality = mean(x_log, na.rm = TRUE),
            age = mean(age, na.rm = TRUE), 
            gender = mean(age, na.rm = TRUE), 
            no_highsch = mean(no_highsch, na.rm = TRUE), 
            college_deg = mean(college_deg, na.rm = TRUE), 
            hispanic = mean(hisp, na.rm = TRUE),
            black = mean(black, na.rm = TRUE), 
            household_income = mean(household_income, na.rm = TRUE),
            economy = mean(economy, na.rm = TRUE), 
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


## Figure 1: Boxplot of dependent variables
boxplot(df_final$deport, df_final$wall, df_final$dreamers, 
        names = c("Deportation", "US-Mexico wall", "Citizenship"),
        main = "Figure 1. Boxplot of Anti-immigration Variables",
        ylab = "Proportion of respondents with anti-immigration sentiment")




###### fixed effects regression ###### 
set.seed(1) # to ensure replicability of result 

#### Testing H1 ####

## H1 with death count 
deport_MS <- feols(deport ~ mortality + economy | state + wave, data = df_final, cluster =~state)
summary(deport_MS)
wall_MS <- feols(wall ~ mortality + economy | state + wave, data = df_final, cluster =~state)
summary(wall_MS)
dreamers_MS <- feols(dreamers ~ mortality + economy | state + wave, data = df_final, cluster =~state)
summary(dreamers_MS)

## H1 with fear of covid
deport_cov <- feols(deport ~ covid + economy | state + wave, data = df_final, cluster =~state)
summary(deport_cov)
wall_cov <- feols(wall ~ covid + economy | state + wave, data = df_final, cluster =~state)
summary(wall_cov)
dreamers_cov <- feols(dreamers ~ covid + economy | state + wave, data = df_final, cluster =~state)
summary(dreamers_cov)

## With state-specific trends 
# H1 with death count
deport_MS_trend <- feols(deport ~ mortality + economy + wave:state | state + wave, data = df_final, cluster =~state)
summary(deport_MS_trend)
wall_MS_trend <- feols(wall ~ mortality + economy +  wave:state | state + wave, data = df_final, cluster =~state)
summary(wall_MS_trend)
dreamers_MS_trend <- feols(dreamers ~ mortality + economy + wave:state | state + wave, data = df_final, cluster =~state)
summary(dreamers_MS_trend)

# H1 with fear of covid 
deport_cov_trend <- feols(deport ~ covid + economy + wave:state| state + wave, data = df_final, cluster =~state)
summary(deport_cov_trend)
wall_cov_trend <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_final, cluster =~state)
summary(wall_cov_trend)
dreamers_cov_trend <- feols(dreamers ~ covid + economy + wave:state| state + wave, data = df_final, cluster =~state)
summary(dreamers_cov_trend)

### Investigating measures ###
# mortality salience by wave and by state to investigate state most responsible for spike in MS 
ggplot(df_final, aes(x = wave, y = mortality, color = state, group = state)) +
  geom_line() +
  labs(x = "Wave", y = "MS") +
  theme_minimal() #identify states with largest jump in MS: NY large jump in MS 

# graph of MS and fear of covid in NY 
df_final_NY <- df_final[df_final$state == "NY", ] # subset for NY

scale = 2.1
ggplot(df_final_NY, aes(x = wave, y = covid)) +
  geom_line(aes(linetype = "Fear of Covid-19")) +
  geom_line(aes(y = mortality/scale, linetype = "Weekly death counts, logged")) +
  scale_x_continuous(breaks = seq(12, 52, 10)) + 
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name = "Weekly death counts, logged")) +
  labs(title = "Figure 2. Fear of Covid against death count in New York", 
       x = "Wave",
       y = "Fear of Covid-19",
       linetype = "Legend") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold", hjust = 0.5))

# fixed effect of covid against mortality
MS_covid <- feols(covid ~ mortality | state + wave, data = df_final, cluster =~state)
summary(MS_covid)
MS_covid_control <- feols(covid ~ mortality + economy | state + wave, data = df_final, cluster =~state)
summary(MS_covid_control)



#### H2: group favourability #### 

# Prepare data
avg_by_state <- df_final %>%
  group_by(state) %>%
  summarise(grpfav_avg = mean(grpfav, na.rm = TRUE)) # create new dataframe with average group fav values

df_final_grp <- merge(df_final, avg_by_state, by = "state", all.x = TRUE) # merge new dataframe into existing dataset

df_final_low <- df_final_grp[(df_final_grp$grpfav_avg < mean(df_final_grp$grpfav_avg) | df_final_grp$grpfav_avg == mean(df_final_grp$grpfav_avg)) , ] # states not favourable of immigrants
df_final_high <- df_final_grp[df_final_grp$grpfav_avg > mean(df_final_grp$grpfav_avg), ] # states highly favourable of immigrants 
write.csv(df_final_high, "precovid_statesfavourable.csv")

# H2 test 
deport_cov_low <- feols(deport ~ covid + economy | state + wave, data = df_final_low, cluster =~state)
summary(deport_cov_low) # deportation low grpfav 
deport_cov_high <- feols(deport ~ covid + economy | state + wave, data = df_final_high, cluster =~state)
summary(deport_cov_high) # deportation high grpfav

wall_cov_low <- feols(wall ~ covid + economy | state + wave, data = df_final_low, cluster =~state)
summary(wall_cov_low) # wall low 
wall_cov_high <- feols(wall ~ covid + economy | state + wave, data = df_final_high, cluster =~state)
summary(wall_cov_high) # wall high

dreamers_cov_low <- feols(dreamers ~ covid + economy | state + wave, data = df_final_low, cluster =~state)
summary(dreamers_cov_low) # dreamers low 
dreamers_cov_high <- feols(dreamers ~ covid + economy | state + wave, data = df_final_high, cluster =~state)
summary(dreamers_cov_high) # dreamers high



## regression assumptions 
# 1: linearity
fitted_values <- fitted(deport_cov)
residuals <- resid(deport_cov)

plot(fitted_values, residuals, 
     xlab = "Fitted values", ylab = "Residuals", 
     main = "Residuals Vs Fitted Values Plot")
lines(lowess(fitted_values, residuals), col = "blue")
abline(h = 0, col = "red")


# 2: normality of error terms
qqnorm(residuals, main = "Q-Q plot for testing normality of residuals")
qqline(residuals, col = "red")
hist(residuals)

#3: heteroskedasticity 
sqrt_abs_residuals <- sqrt(abs(residuals))

plot(fitted_values, sqrt_abs_residuals,
     xlab = "Fitted Values", ylab = "Square Root of Absolute Residuals",
     main = "Scale-Location Plot")

# Add a lowess smoother to the plot
lines(lowess(fitted_values, sqrt_abs_residuals), col = "red")


##### Pre-covid visuals ####
# save csv of states that that are have large increase in death anxiety 
View(df_final)

precovid <- df_final %>%
  group_by(state) %>%
  summarise(max_diff = max(covid) - min(covid))
median(precovid$max_diff)

precovid_large <- precovid[precovid$max_diff > median(precovid$max_diff), ] # states with large increase in death anxiety
write.csv(precovid_large, "precovid_large.csv")

