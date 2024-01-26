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

###############################################
###  load and extract the new cleaned rds files
###############################################
new_folder_path <- "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST_Cleaned1"
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

##########################
###  Aggregating the data 
#########################

## Filtering the loggers  that were of out-of-soil measurements and not working properly
filtered_data <- combined_data %>%
  filter(!(logger_id %in% c("94200377", "94242031", "94242033", "94242034", "94242039", "94242043", "94242047", "94242059")))

unique(filtered_data$logger_id)

## Load sensors data that contain the information of whether the loggers are inside or outside the cage
Sensors_data <- read_excel("C:/Users/nemer/Project/SEEDFOR/Field/20230904_MonitoringC1/microclim/Sensors_20230904.xlsx", 
                           sheet = "capteurs")

head(filtered_data)
head(Sensors_data)

# Convert sensor_id in Sensors_data to character and add In_out column 
# that infrom us if the logger is inside or outside the cage
Sensors_data <- mutate(Sensors_data, sensor_id = as.character(sensor_id))
# Join the two data sets on logger_id and sensor_id
joined_data <- filtered_data %>%
  left_join(Sensors_data, by = c("logger_id" = "sensor_id")) %>%
  # Select the columns you want from the joined data and filtered data
  select(date, logger_id, T1, T2, T3, soil_moisture, Canopy_openness, Site, In_out)
head(joined_data)

unique(joined_data$logger_id)







#############################
####       Data by Season 
#############################
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

head(Data_by_Season)



#############################
####  Detecting outliers
#############################
summary(Data_by_Season)

head(Data_by_Season)
gg<-Data_by_Season%>%
  group_by(season,Canopy_openness,Site,In_out)%>%
summarize( 
  max_T1 = max(T1),
  min_T1 = min(T1),
  max_T2 = max(T2),
  min_T2 = min(T2),
  max_T3 = max(T3),
  min_T3 = min(T3))%>%
arrange(desc(max_T3),desc(max_T2),desc(max_T1))
View(gg)


library(outliers)

grubbs.test(Data_by_Season$T2)
grubbs.test(Data_by_Season$T3,type=11)
grubbs.test(x,type=20)
grubbs.test(x,type=11)

# Reshape data using gather function
gathered_data <- tidyr::gather(Data_by_Season, key = "Temperature_Variable", value = "Temperature", T1, T2, T3)

# Create a boxplot with all temperature variables on the same plot
plotly::ggplotly(ggplot(gathered_data, aes(x = Temperature_Variable, y = Temperature, fill = Temperature_Variable)) +
                   geom_boxplot() +
                   labs(title = "Boxplots for T1, T2, and T3", x = "Temperature Variables", y = "Temperature") +
                   theme_minimal()+
                   facet_wrap(~ season + Canopy_openness)
                 
                 )




# Calculate IQR for each temperature variable
iqr_T1 <- IQR(joined_data$T1)
iqr_T2 <- IQR(joined_data$T2)
iqr_T3 <- IQR(joined_data$T3)

# Set a threshold as 1.5 times the IQR
threshold_multiplier <- 1.5
lower_threshold_T1 <- quantile(joined_data$T1, 0.25) - threshold_multiplier * iqr_T1
upper_threshold_T1 <- quantile(joined_data$T1, 0.75) + threshold_multiplier * iqr_T1

lower_threshold_T2 <- quantile(joined_data$T2, 0.25) - threshold_multiplier * iqr_T2
upper_threshold_T2 <- quantile(joined_data$T2, 0.75) + threshold_multiplier * iqr_T2

lower_threshold_T3 <- quantile(joined_data$T3, 0.25) - threshold_multiplier * iqr_T3
upper_threshold_T3 <- quantile(joined_data$T3, 0.75) + threshold_multiplier * iqr_T3

# Filter out values beyond the thresholds
joined_data_filtered <- joined_data %>%
  filter(
    T1 >= lower_threshold_T1 & T1 <= upper_threshold_T1,
    T2 >= lower_threshold_T2 & T2 <= upper_threshold_T2,
    T3 >= lower_threshold_T3 & T3 <= upper_threshold_T3
  )


head(joined_data_filtered)
summary(joined_data_filtered)


### visualize the data after removing the outliers
# Reshape data using gather function
gathered_data2 <- tidyr::gather(joined_data_filtered, key = "Temperature_Variable", value = "Temperature", T1, T2, T3)

# Create a boxplot with all temperature variables on the same plot
plotly::ggplotly(ggplot(gathered_data2, aes(x = Temperature_Variable, y = Temperature, fill = Temperature_Variable)) +
                   geom_boxplot() +
                   labs(title = "Boxplots for T1, T2, and T3", x = "Temperature Variables", y = "Temperature") +
                   theme_minimal())




# For season and canopy openness #
seasons <- unique(Data_by_Season$season)
canopies <- unique(Data_by_Season$Canopy_openness)
# Create an empty data frame to store the filtered results
joined_data_filtered2 <- data.frame()

# Loop through each season
for (season in seasons) {
  for (canopy in canopies) {
  #subset_data <- Data_by_Season[Data_by_Season$season == season, ]
  
  # Calculate IQR for each temperature variable
  iqr_T1 <- IQR(Data_by_Season$T1)
  iqr_T2 <- IQR(Data_by_Season$T2)
  iqr_T3 <- IQR(Data_by_Season$T3)
  
  # Set a threshold as 1.5 times the IQR
  threshold_multiplier <- 1.5
  lower_threshold_T1 <- quantile(Data_by_Season$T1, 0.25) - threshold_multiplier * iqr_T1
  upper_threshold_T1 <- quantile(Data_by_Season$T1, 0.75) + threshold_multiplier * iqr_T1
  
  lower_threshold_T2 <- quantile(Data_by_Season$T2, 0.25) - threshold_multiplier * iqr_T2
  upper_threshold_T2 <- quantile(Data_by_Season$T2, 0.75) + threshold_multiplier * iqr_T2
  
  lower_threshold_T3 <- quantile(Data_by_Season$T3, 0.25) - threshold_multiplier * iqr_T3
  upper_threshold_T3 <- quantile(Data_by_Season$T3, 0.75) + threshold_multiplier * iqr_T3
  
  # Filter out values beyond the thresholds for each season
  subset_data_filtered <- subset_data %>%
    filter(
      T1 >= lower_threshold_T1 & T1 <= upper_threshold_T1,
      T2 >= lower_threshold_T2 & T2 <= upper_threshold_T2,
      T3 >= lower_threshold_T3 & T3 <= upper_threshold_T3
    )
  
  # Append the filtered results to the main data frame
  joined_data_filtered2 <- rbind(joined_data_filtered2, subset_data_filtered)
  }
}

# Print the summary of the filtered data
summary(joined_data_filtered2)



### visualize the data after removing the outliers
# Reshape data using gather function
gathered_data2 <- tidyr::gather(joined_data_filtered, key = "Temperature_Variable", value = "Temperature", T1, T2, T3)

# Create a boxplot with all temperature variables on the same plot
plotly::ggplotly(ggplot(gathered_data2, aes(x = Temperature_Variable, y = Temperature, fill = Temperature_Variable)) +
                   geom_boxplot() +
                   labs(title = "Boxplots for T1, T2, and T3", x = "Temperature Variables", y = "Temperature") +
                   theme_minimal())

#######################################
####    daily min and max temperatures
######################################

#calculate daily averages and daily min and max temperatures
daily_averages_Tmin_Tmax<- Data_by_Season %>%
  group_by(season,day,Canopy_openness, Site) %>%
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

head(daily_averages_Tmin_Tmax)


## Aggregate the data to daytime (6am-6pm) and nightime (6pm-6am)
time_of_day <- Data_by_Season %>%
  mutate(time_of_day = ifelse(hour(date) >= 6 & hour(date) < 18, "daytime", "nighttime"))
head(time_of_day)

# Calculate daytime vs. nighttime averages
daynight_averages <- time_of_day %>%
  group_by(Canopy_openness,Site,time_of_day) %>%
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




#############################################
###             Data analysis
#############################################
ggplot(joined_data_filtered, aes(x = In_out, y = T3, fill = In_out)) +
  geom_boxplot() +
  #facet_wrap(~Site) +
  labs(x = "In_out", y = "T3", color = "Canopy_openness") +
  theme_minimal()

# Fit ANOVA model

anova(anova_model <- lm(T1 ~ In_out , data = joined_data_filtered))
anova(anova_model <- lm(T2 ~ In_out , data = joined_data_filtered))
anova(anova_model <- lm(T3 ~ In_out*Canopy_openness*Site*season , data = joined_data_filtered))

anova_model <- lm(sqrt_T3 ~ In_out , data = joined_data)
anova(anova_model)
qqnorm(residuals(anova_model))
qqline(residuals(anova_model))



# Check assumptions
check_normality(anova_model) 
check_heteroscedasticity(anova_model)
check_model(anova_model)


min(joined_data$sqrt_T3)

residuals_df <- data.frame(
  Predicted = predict(anova_model),
  Residuals = residuals(anova_model)
)

# Assuming joined_data is your data frame
offset_value <- abs(min(joined_data$T3)) + 1  # Add 1 to ensure a positive offset

# Apply the square root transformation with an offset
joined_data$sqrt_T3 <- sqrt(joined_data$T3 + offset_value)

# Check for missing or non-positive values in sqrt_T3
any(is.na(joined_data$sqrt_T3))
any(joined_data$sqrt_T3 <= 0)

# Conduct the Wilcoxon rank-sum test
wilcox_test_result <- wilcox.test(sqrt_T3 ~ In_out, data = joined_data)
print(wilcox_test_result)













































#### daily averages ###

# Fit a linear mixed-effects model
anova(model_T1 <- lmer(avg_T1 ~ Canopy_openness * Site + (1|day), data = daily_averages))
anova(model_T2 <- lmer(avg_T2 ~ Canopy_openness * Site + (1|day), data = daily_averages))
anova(model_T3 <- lmer(avg_T3 ~ Canopy_openness * Site + (1|day), data = daily_averages))


mixed(model_T3, data = daily_averages)

qqnorm(residuals(model_T3))
qqline(residuals(model_T3))


# Check assumptions
check_normality(model_T3) 
check_heteroscedasticity(model_T3)
check_model(model_T3)




# Extract residuals from the model
residuals <- resid(model_T3)

# Add residuals to the original data
daily_averages$residuals <- residuals
head(daily_averages)


# Create a  boxplot to visually check the outliers 
p1<-ggplot(daily_averages, aes(x = Canopy_openness, y = residuals, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Boxplot of Residuals by Canopy_openness",
       x = "Canopy_openness",
       y = "Residuals") +
  scale_fill_manual(values = c("Open" = "lightblue", "Close" = "lightgreen", "Semi_Close" = "lightpink")) +
  theme_minimal()#+
  #facet_wrap(~Site, scales = "free") 

plotly::ggplotly(p1)



# Keep the detected outliers 
daily_averages_resid <- daily_averages %>%
  filter(
    (residuals >= 1.44 | residuals <= -1.4) & Canopy_openness == 'Closed' |
        (residuals >=  1.8 | residuals <= -1.615) & Canopy_openness == 'Open' |
        (residuals >= 1.28 | residuals <= -1.23) & Canopy_openness == 'Semi-closed')

# remove the detected outliers 
daily_averages_resid <- daily_averages %>%
  filter(
    !((residuals >= 1.44 | residuals <= -1.4) & Canopy_openness == 'Closed' |
      (residuals >=  1.8 | residuals <= -1.615) & Canopy_openness == 'Open' |
      (residuals >= 1.28 | residuals <= -1.23) & Canopy_openness == 'Semi-closed'))


### remember to automate this !!!!!!!!


 # Fit a linear mixed-effects model with the removed outlier data

anova(model_T3_resid <- lmer(avg_T3 ~ Canopy_openness * Site + (1|day), data = daily_averages_resid))
mixed(model_T3_resid , data = daily_averages_resid)
 
# Check assumptions
check_normality(model_T3_resid) 
check_heteroscedasticity(model_T3_resid)
check_model(model_T3_resid)

qqnorm(residuals(model_T3_resid))
qqline(residuals(model_T3))


# Posthoc test #
lsmeans_interaction <- lsmeans::lsmeans(model_T3_resid, ~ Canopy_openness*Site)### David script
multcomp::cld(lsmeans_interaction, Letters = letters, adjust = "tukey")#### David script




































# Create a grouped bar plot for avg_T3 by Canopy_openness and Site, facet by Canopy_openness
ggplot(daily_averages_resid, aes(x = Site, y = avg_T3, fill = Canopy_openness)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Grouped Bar Plot of avg_T3 by Site (facet by Canopy_openness)",
       x = "Site",
       y = "avg_T3") +
  facet_wrap(~ Canopy_openness, scales = "free_y", ncol = 1) +
  theme_minimal()






