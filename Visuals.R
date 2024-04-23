######## Supplementary Material ########
### Figures in Supplementary Material ###
# Figure S1: Aggregate changes over time
visual1 <- df_final %>%
  group_by(wave) %>%
  summarise(deport =  mean(deport, na.rm = TRUE)*100, 
            wall = mean(wall, na.rm = TRUE)*100, 
            dreamers = mean(dreamers, na.rm = TRUE)*100)

data_long <- tidyr::gather(visual1, key = "variable", value = "value", -wave)

linetypes <- c("deport" = "solid", "wall" = "dashed", "dreamers" = "dotted")
labels <- c("deport" = "Support for deportation", "wall" = "Support for building a wall", "dreamers" = "Support for no citizenship path", "variable" = "Dependent Variables")

ggplot(data_long, aes(x = wave, y = value, linetype = variable)) +
  geom_line() +
  labs(x = "Wave", y = "Percentage", title = "Figure S1. Aggregate anti-immigration opinion over time") +
  ylim(0, 100) +
  labs(linetype = "Dependent Variables") + 
  scale_linetype_manual(values = linetypes, labels = labels) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



# Figure S2: Number of states and number of time periods for each state 
visual2 <- df_final %>%
  group_by(state) %>%
  summarise(waves_number = n())
View(visual2)

visual_mat_num <- c(visual2$waves_number)
visual_mat_state <- c(visual2$state)
barplot(visual_mat_num,names.arg=visual_mat_state, main = "Figure S2. Number of waves per state", xlab="Number of Waves",ylab="State",las=1, cex.names=0.6,space=1,horiz=TRUE,xlim=c(0,40))
axis(side = 1, at = seq(0,55,by=5))


# Figure S3: Change in death anxiety with time 
visual3 <- df_final %>%
  group_by(wave) %>%
  summarise(mean_corona = mean(covid, na.rm = TRUE))

ggplot(visual3, aes(x = wave, y = mean_corona)) +
  geom_line() +
  labs(title = "Figure S3. Changes in aggregate death anxiety with time",
       x = "Wave", y = "Death anxiety") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


### Tables in Supplementary Material ###
## Table S1 and Table S2
# results at other thresholds 
df_robust <- df # create new dataframe 
out1 <- NULL 
out2 <- NULL 
out3 <- NULL
out4 <- NULL 
out5 <- NULL 
out6 <- NULL
out7 <- NULL 
out8 <- NULL 
out9 <- NULL

for (x in c(35, 75, 100)){
  # define representative waves
  df_robust$representative <- ifelse(df_robust$num_observations > x, 1, 0)
  df_robust_rep <- df_robust[df_robust$representative > 0, ] 
  
  # filter out states with less than 2 waves
  df_by_state <- df_robust_rep %>%
    group_by(state) %>%
    summarise(waves_number = n())
  states <- df_by_state %>% filter(waves_number > 1)
  df_new <- merge(states, df_robust_rep, by = "state")
  
  # H1 results with state-specific time trend 
  out1[[x]] <- feols(deport ~ covid + economy + wave:state| state + wave, data = df_new, cluster =~state)
  out2[[x]] <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_new, cluster =~state)
  out3[[x]] <- feols(dreamers ~ covid + economy + wave:state| state + wave, data = df_new, cluster =~state)
  
  
  avg_by_state <- df_new %>%
    group_by(state) %>%
    summarise(grpfav_avg = mean(grpfav, na.rm = TRUE)) #create newdataframe with average group fav values
  
  df_new_grp <- merge(df_new, avg_by_state, by = "state", all.x = TRUE) #merge newdataframe into existing dataset
  
  df_new_low <- df_new_grp[df_new_grp$grpfav_avg > mean(df_new_grp$grpfav_avg), ]
  df_new_high <- df_new_grp[(df_new_grp$grpfav_avg < mean(df_new_grp$grpfav_avg) | df_new_grp$grpfav_avg == mean(df_new_grp$grpfav_avg)) , ]
  
  # H2 regression results with state-specific time trend 
  out4[[x]] <- feols(deport ~ covid + economy + wave:state| state + wave, data = df_new_low, cluster =~state)
  out5[[x]] <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_new_low, cluster =~state)
  out6[[x]] <- feols(dreamers ~ covid + economy + wave:state| state + wave, data = df_new_low, cluster =~state)
  out7[[x]] <- feols(deport ~ covid + economy + wave:state| state + wave, data = df_new_high, cluster =~state)
  out8[[x]] <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_new_high, cluster =~state)
  out9[[x]] <- feols(dreamers ~ covid + economy + wave:state| state + wave, data = df_new_high, cluster =~state)
}

# H1 at other thresholds 
lapply(out1, summary)
lapply(out2, summary)
lapply(out3, summary)

# H2 at other thresholds
# low fav
lapply(out4, summary)
lapply(out5, summary)
lapply(out6, summary)
# high fav 
lapply(out7, summary)
lapply(out8, summary)
lapply(out9, summary)


## Table S3
# Dependent variables 
mean(df_final$wall)
mean(df$wall)
mean(df_final$deport)
mean(df$deport)
mean(df_final$dreamers)
mean(df$dreamers)

# Independent variables
mean(df_final$mortality)
mean(df$mortality)
mean(df_final$covid)
mean(df$covid)

# Statistics
mean(df_final$gender) # female 
mean(df$gender)
mean(df_final$age) # mean age 
mean(df$age)
mean(df_final$no_highsch) # no high sch 
mean(df$no_highsch)
mean(df_final$college_deg) # college deg
mean(df$college_deg)
mean(df_final$black) # blacks
mean(df$black)
mean(df_final$hispanic) # hispanics
mean(df$hispanic)
mean(df_final$household_income) # household income
mean(df$household_income)

# other metrics
max(df$wave)


# Table S4: H2 with time-specific trends
deport_cov_low_trend <- feols(deport ~ covid + economy + wave:state | state + wave, data = df_final_low, cluster =~state)
summary(deport_cov_low_trend) # deportation low grpfav
deport_cov_high_trend <- feols(deport ~ covid + economy + wave:state | state + wave, data = df_final_high, cluster =~state)
summary(deport_cov_high_trend) # deportation high grpfav

wall_cov_low_trend <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_final_low, cluster =~state)
summary(wall_cov_low_trend) # wall low grpfav
wall_cov_high_trend <- feols(wall ~ covid + economy + wave:state| state + wave, data = df_final_high, cluster =~state)
summary(wall_cov_high_trend) # wall high grpfav

dreamers_cov_low_trend <- feols(dreamers ~ covid + economy  + wave:state | state + wave, data = df_final_low, cluster =~state)
summary(dreamers_cov_low_trend) # dreamers low grpfav
dreamers_cov_high_trend <- feols(dreamers ~ covid + economy  + wave:state| state + wave, data = df_final_high, cluster =~state)
summary(dreamers_cov_high_trend) # dreamers high grpfav

# Table S5: H2 with subset of data 
value = quantile(df_final_low$grpfav)[4]
df_low_subsample <- df_final_low %>%
  filter(state %in% filter(., wave == 1 & grpfav < value)$state)

# H2 test with subsample 
deport_cov_low <- feols(deport ~ covid + economy | state + wave, data = df_low_subsample, cluster =~state)
summary(deport_cov_low) # deportation low  

wall_cov_low <- feols(wall ~ covid + economy | state + wave, data = df_low_subsample, cluster =~state)
summary(wall_cov_low) # wall low 

dreamers_cov_low <- feols(dreamers ~ covid + economy | state + wave, data = df_low_subsample, cluster =~state)
summary(dreamers_cov_low) # dreamers low



