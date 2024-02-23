library(viridis)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lme4)
library(Matrix)
library(afex)
library(performance)
library(see)
library(broom)
library(readxl)
library(MuMIn)
#################################################
### 1. load and extract the new cleaned rds files
#################################################
new_folder_path <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR/Data/TOMST_Cleaned1"
#### 1-load ###
# Get a list of all RDS files in the folder
rds_files <- list.files(path = new_folder_path, pattern = "\\.rds$", full.names = TRUE)

# Create an empty list to store loaded data frames
loaded_data_frames <- list()

# Loop through each RDS file and load it into R
for (rds_file in rds_files) {
  loaded_data <- readRDS(rds_file)
  loaded_data_frames[[basename(rds_file)]] <- loaded_data
}

#### 2-extract ###
# Example: Extract the first loaded data frame from the list
first_data_frame <- loaded_data_frames[[1]]
head(first_data_frame)
# Create an empty list to store loaded data frames
loaded_data_frames <- list()

# Loop through each RDS file and load it into R
for (rds_file in rds_files) {
  loaded_data <- readRDS(rds_file)
  loaded_data_frames[[basename(rds_file)]] <- loaded_data
}

# Use list2env to create separate data frames in the R environment
list2env(loaded_data_frames, envir = .GlobalEnv)


#### 3-Combine all loaded data frames into one data frame ###
combined_data <- do.call(rbind, loaded_data_frames)
rownames(combined_data) <- NULL
head(combined_data)

summary(combined_data)

#######################################################################
###  2. Removing loggers that were out of soil and not working properly 
#######################################################################

## Filtering the loggers  that were of out-of-soil measurements and not working properly
filtered_data <- combined_data %>%
  filter(!(logger_id %in% c("94200377", "94242031", "94242033", "94242034", "94242039", "94242043", "94242047", "94242059")))

unique(filtered_data$logger_id)


##############################################################################
#### 3. Load sensor metadata containing information on whether the loggers are 
####  inside or outside the cage, and add this information to the main dataset
##############################################################################
#Sensors_data <- read_excel("C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST metadata/Sensors_20230904.xlsx",  sheet = "capteurs")
Sensors_data <-read_excel("C:/Users/David/Desktop/SeedFor/Project/SEEDFOR/Field/20230904_MonitoringC1/microclim/Sensors_20230904.xlsx", sheet = "capteurs")


# Convert sensor_id in Sensors_data to character and add In_out column 
# that inform us if the logger is inside or outside the cage
Sensors_data <- mutate(Sensors_data, sensor_id = as.character(sensor_id))
# Join the two data sets on logger_id and sensor_id
joined_data <- filtered_data %>%
  left_join(Sensors_data, by = c("logger_id" = "sensor_id")) %>%
  # Select the columns you want from the joined data and filtered data
  select(date, logger_id, T1, T2, T3, soil_moisture, Canopy_openness, Site, In_out)
head(joined_data)

#######################################################################################
##### 4. Aggregating the data by season, daily averages, daily min and max temperatures
#######################################################################################
# Convert date column to a datetime object
joined_data$date <- as.POSIXct(joined_data$date, format="%Y-%m-%d %H:%M:%S")
# Create a new column for the date without the time component
joined_data$day <- as.Date(joined_data$date)
head(joined_data)



Data_by_Season  <- joined_data %>%
  mutate(season = case_when(
    month(date) %in% c(12, 1, 2) ~ "winter",
    month(date) %in% c(3, 4, 5) ~ "spring",
    month(date) %in% c(6, 7, 8) ~ "summer",
    month(date) %in% c(9, 10, 11) ~ "fall"
  ))


daily_Tmean_Tmin_Tmax <- Data_by_Season %>%
  group_by(season,day,Canopy_openness,Site,In_out) %>%
  summarize(
    avg_T1 = mean(T1),
    avg_T3 = mean(T3),
    max_T1 = max(T1),
    min_T1 = min(T1),
    max_T3 = max(T3),
    min_T3 = min(T3)
  )


head(daily_Tmean_Tmin_Tmax)




gathered_data <- tidyr::gather(daily_Tmean_Tmin_Tmax, key = "Temperature_Variable", value = "Temperature", avg_T1, avg_T3,max_T1,max_T3,min_T1,min_T3)
head(gathered_data)

# Specify the order of levels for Canopy_openness
canopy_order <- c("Open", "Semi-closed", "Closed")

#############
## 4.1 Winter
#############

### 4.1.1 Daily mean temperature measured at 8 cm below ground in Winter ###


p1<-gathered_data %>%
  filter(Temperature_Variable == 'avg_T1' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily mean temperature measured at 8 cm below ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_avgT1 <- lmer(avg_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))
mixed(model_avgT1, data = daily_Tmean_Tmin_Tmax)

model2_avgT1 <- lmer(avg_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
lsmeans_interaction2 <- lsmeans::lsmeans(model2_avgT1, ~ Site)

check_model(model_avgT1)
# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_avgT1, ~ Canopy_openness*Site)

p1<- p1+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = lsmean, label = .group),
            size = 7)
plot(p1)



 

  
### 4.1.2 Daily maximum temperature measured at 8 cm below ground in Winter ###
  
  
p2<-gathered_data %>%
  filter(Temperature_Variable == 'max_T1' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily maximum temperature measured at 8 cm below ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_maxT1 <- lmer(max_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))


check_model(model_maxT1)
# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_maxT1, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p2<- p2+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 15, label = .group),
            size = 6)
plot(p2)



 

  
### 4.1.3 Daily minimum temperature measured at 8 cm below ground in Winter ###
  
  
p3<-gathered_data %>%
  filter(Temperature_Variable == 'min_T1' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily minimum temperature measured at 8 cm below ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_minT1 <- lmer(min_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))


check_model(model_minT1)
# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_minT1, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p3<- p3+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 11, label = .group),
            size = 7)
plot(p3)


 

  
### 4.1.4 Daily mean temperature measured at 15 cm above ground in winter ###
  
  
p4<-gathered_data %>%
  filter(Temperature_Variable == 'avg_T3' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily mean temperature measured at 15 cm above ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_avgT3 <- lmer(avg_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))
check_model(model_avgT3)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_avgT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p4<- p4+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 11, label = .group),
            size = 7)
plot(p4)



 

  
### 4.1.5 Daily maximum temperature measured at 15 cm above ground in winter ###
  
  
p5<-gathered_data %>%
  filter(Temperature_Variable == 'max_T3' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily maximum temperature measured at 15 cm above ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_maxT3 <- lmer(max_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))
check_model(model_maxT3)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_maxT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p5<- p5+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 20, label = .group),
            size = 7)
plot(p5)


 

  
### 4.1.6 Daily minimum temperature measured at 15 cm above ground in winter ###
  
  
p6<-gathered_data %>%
  filter(Temperature_Variable == 'min_T3' & season=="winter") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily minimum temperature measured at 15 cm above ground in winter", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_minT3 <- lmer(min_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter")))
check_model(model_minT3)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_minT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p6<- p6+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 11, label = .group),
            size = 7)
plot(p6)

##############
### 4.2 Summer
##############
### 4.2.1 Daily mean temperature measured at 8 cm below ground in Summer


p1<-gathered_data %>%
  filter(Temperature_Variable == 'avg_T1' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily mean temperature measured at 8 cm below ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_avgT1 <- lmer(avg_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
check_model(model_avgT1)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_avgT1, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p1<- p1+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 30, label = .group),
            size = 7)
plot(p1)



 

  
### 4.2.2 Daily maximum temperature measured at 8 cm below ground in Summer ###
  
  
p2<-gathered_data %>%
  filter(Temperature_Variable == 'max_T1' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily maximum temperature measured at 8 cm below ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_maxT1 <- lmer(max_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
check_model(model_maxT1)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_maxT1, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p2<- p2+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 30, label = .group),
            size = 7)
plot(p2)



 

  
### 4.2.3 Daily minimum temperature measured at 8 cm below ground in Summer ###
  
  
p3<-gathered_data %>%
  filter(Temperature_Variable == 'min_T1' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily minimum temperature measured at 8 cm below ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_minT1 <- lmer(min_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
check_model(model_minT1)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_minT1, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p3<- p3+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 23.5, label = .group),
            size = 7)
plot(p3)


 

  
### 4.2.4 Daily mean temperature measured at 15 cm above ground in Summer ###
  
  
p4<-gathered_data %>%
  filter(Temperature_Variable == 'avg_T3' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily mean temperature measured at 15 cm above ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_avgT3 <- lmer(avg_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
check_model(model_avgT3)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_avgT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p4<- p4+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 34 , label = .group),
            size = 7)
plot(p4)



### 4.2.5 Daily maximum temperature measured at 15 cm above ground in Summer ###
  
  
p5<-gathered_data %>%
  filter(Temperature_Variable == 'max_T3' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily maximum temperature measured at 15 cm above ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_maxT3 <- lmer(max_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
mixed(model_maxT3, data = daily_Tmean_Tmin_Tmax)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_maxT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p5<- p5+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 45, label = .group),
            size = 7)
plot(p5)


 
### 4.2.6 Daily minimum temperature measured at 15 cm above ground in Summer ###
  
  
p6<-gathered_data %>%
  filter(Temperature_Variable == 'min_T3' & season=="summer") %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Daily minimum temperature measured at 15 cm above ground in summer", x = "Canopy openness", y = "Temperature °C", fill = "Canopy openness") +
  scale_fill_manual(values = c("blue", "green","red")) + # Adjust fill colors for time_of_day
  #theme_minimal() +
  facet_grid(~(Site)) +  # Facet grid by site and time_of_day, with sites on the y-axis
  theme(text = element_text(size = 28),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1))

# Fit a linear mixed-effects model
anova(model_minT3 <- lmer(min_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer")))
mixed(model_minT3, data = daily_Tmean_Tmin_Tmax)

# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_minT3, ~ Canopy_openness*Site)
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")



p6<- p6+# Add text labels for post hoc results
  geom_text(data = multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "sidak"),
            aes(x = Canopy_openness, y = 25, label = .group),
            size = 7)
plot(p6)



######################################################################
####    Aggregate the data to daytime (6am-6pm) and nightime (6pm-6am)
######################################################################
## Aggregate the data to daytime (6am-6pm) and nightime (6pm-6am)
time_of_day <- Data_by_Season %>%
  mutate(time_of_day = ifelse(hour(date) >= 6 & hour(date) < 18, "daytime", "nighttime"))
head(time_of_day)

# Calculate daytime vs. nighttime averages
day_night_Tmean_Tmin_Tmax <- time_of_day %>%
  group_by(season,day,Canopy_openness,Site,time_of_day,In_out) %>%
  summarize(
    avg_T1 = mean(T1),
    avg_T2 = mean(T2),
    avg_T3 = mean(T3),
    avg_soil_moisture = mean(soil_moisture),
    max_T1 = max(T1),
    min_T1 = min(T1),
    max_T2 = max(T2),
    min_T2 = min(T2),
    max_T3 = max(T3),
    min_T3 = min(T3),
    avg_soil_moisture = mean(soil_moisture),
    max_soil_moisture = max(soil_moisture),
    min_soil_moisture = min(soil_moisture)
  )
head(daynight_averages)

daynight_averages[,c(1,2,3,4,5,6,7,8,9)]

daily_Tmean_Tmin_Tmax <- Data_by_Season %>%
  group_by(season,day,Canopy_openness,Site) %>%
  summarize(
    avg_T1 = mean(T1),
    avg_T3 = mean(T3),
    max_T1 = max(T1),
    min_T1 = min(T1),
    max_T3 = max(T3),
    min_T3 = min(T3)
  )
















#############
## 4.1 Winter
#############

### 4.1.1 Daily mean temperature measured at 8 cm below ground in Winter ###
# Fit a linear mixed-effects model
model1_avgT1 <- lmer(avg_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_avgT1 <- lmer(avg_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_avgT1 <- lmer(avg_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_avgT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_avgT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_avgT1, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")
### 4.1.2 Daily maximum temperature measured at 8 cm below ground in Winter ###
# Fit a linear mixed-effects model
model1_maxT1 <- lmer(max_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_maxT1 <- lmer(max_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_maxT1 <- lmer(max_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))



# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_maxT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_maxT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_maxT1, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")


### 4.1.3 Daily minimum temperature measured at 8 cm below ground in Winter ###
# Fit a linear mixed-effects model
model1_minT1 <- lmer(min_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_minT1 <- lmer(min_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_minT1 <- lmer(min_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))




# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_minT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_minT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_minT1, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")

### 4.1.4 Daily mean temperature measured at 15 cm above ground in winter ###
# Fit a linear mixed-effects model
model1_avgT3 <- lmer(avg_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_avgT3 <- lmer(avg_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_avgT3 <- lmer(avg_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_avgT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_avgT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_avgT3, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")




### 4.1.5 Daily maximum temperature measured at 15 cm above ground in winter ###
# Fit a linear mixed-effects model
model1_maxT3 <- lmer(max_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_maxT3 <- lmer(max_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_maxT3 <- lmer(max_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_maxT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_maxT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_maxT3, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")

### 4.1.6 Daily minimum temperature measured at 15 cm above ground in winter ###
# Fit a linear mixed-effects model
model1_minT3 <- lmer(min_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model2_minT3 <- lmer(min_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))
model3_minT3 <- lmer(min_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="winter"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_minT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_minT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_minT3, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")


##############
### 4.2 Summer
##############
### 4.2.1 Daily mean temperature measured at 8 cm below ground in Summer

# Fit a linear mixed-effects model
model1_avgT1 <- lmer(avg_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_avgT1 <- lmer(avg_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_avgT1 <- lmer(avg_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_avgT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_avgT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_avgT1, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")

### 4.2.2 Daily maximum temperature measured at 8 cm below ground in Summer ###
# Fit a linear mixed-effects model
model1_maxT1 <- lmer(max_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_maxT1 <- lmer(max_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_maxT1 <- lmer(max_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_maxT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_maxT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_maxT1, ~ Canopy_openness*Site)

multcomp::cld(lsmeans_interaction1, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction2, Letters = letters, adjust = "sidak")
multcomp::cld(lsmeans_interaction3, Letters = letters, adjust = "sidak")


### 4.2.3 Daily minimum temperature measured at 8 cm below ground in Summer ###

# Fit a linear mixed-effects model
model1_minT1 <- lmer(min_T1 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_minT1 <- lmer(min_T1 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_minT1 <- lmer(min_T1 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_minT1, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_minT1, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_minT1, ~ Canopy_openness*Site)
### 4.2.4 Daily mean temperature measured at 15 cm above ground in Summer ###

# Fit a linear mixed-effects model
model1_avgT3 <- lmer(avg_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_avgT3 <- lmer(avg_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_avgT3 <- lmer(avg_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_avgT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_avgT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_avgT3, ~ Canopy_openness*Site)

### 4.2.5 Daily maximum temperature measured at 15 cm above ground in Summer ###
# Fit a linear mixed-effects model
model1_maxT3 <- lmer(max_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_maxT3 <- lmer(max_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_maxT3 <- lmer(max_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_maxT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_maxT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_maxT3, ~ Canopy_openness*Site)

### 4.2.6 Daily minimum temperature measured at 15 cm above ground in Summer ###
# Fit a linear mixed-effects model
model1_minT3 <- lmer(min_T3 ~ Canopy_openness + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model2_minT3 <- lmer(min_T3 ~ Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))
model3_minT3 <- lmer(min_T3 ~ Canopy_openness*Site + (1|day)+ (1|In_out), data = subset(daily_Tmean_Tmin_Tmax, season=="summer"))

# Posthoc test #
lsmeans_interaction1 <- lsmeans::lsmeans(model1_minT3, ~ Canopy_openness)
lsmeans_interaction2 <- lsmeans::lsmeans(model2_minT3, ~ Site)
lsmeans_interaction3 <- lsmeans::lsmeans(model3_minT3, ~ Canopy_openness*Site)















