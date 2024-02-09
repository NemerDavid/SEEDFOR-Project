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
new_folder_path <- "C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST_Cleaned1"
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
Sensors_data <- read_excel("C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST metadata/Sensors_20230904.xlsx", 
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

head(Data_by_Season%>%
  group_by(season,Canopy_openness,Site,In_out)%>%
summarize( 
  max_T1 = max(T1),
  min_T1 = min(T1),
  max_T2 = max(T2),
  min_T2 = min(T2),
  max_T3 = max(T3),
  min_T3 = min(T3))%>%
arrange(desc(max_T3),desc(max_T2),desc(max_T1)))




# Reshape data using gather function
gathered_data <- tidyr::gather(Data_by_Season, key = "Temperature_Variable", value = "Temperature", T1, T2, T3)



# Create a boxplot with all temperature variables on the same plot
## winter ##
# Specify the order of levels for Canopy_openness
canopy_order <- c("Open", "Semi-closed", "Closed")

gathered_data %>%
  filter(season == 'winter') %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%  # Specify order
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot(position = "dodge") +  # Specify position to stack box plots
  labs(title = "Absolute winter temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal() +
  facet_wrap(~Site)+
  theme(text = element_text(size = 22),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)))  # Adjust margin for vertical spacing





## Spring ##
gathered_data %>%
  filter(season=='spring')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute spring temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)


## summer ##
gathered_data %>%
  filter(season=='summer')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute summer temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)

## Fall ##
gathered_data %>%
  filter(season=='fall')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute fall temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)



####### Daily average #######
# Reshape data using gather function
gathered_data2 <- tidyr::gather(daynight_averages, key = "Temperature_Variable", value = "Temperature °C", avg_T1, avg_T2, avg_T3)


## winter ##
# Specify the order of levels for Canopy_openness
canopy_order <- c("Open", "Semi-closed", "Closed")

gathered_data2 %>%
  filter(season == 'winter') %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%  # Specify order
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute winter temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)



## Spring ##
gathered_data2 %>%
  filter(season=='spring')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute spring temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)


## summer ##
gathered_data2 %>%
  filter(season=='summer')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute summer temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)

## Fall ##
gathered_data2 %>%
  filter(season=='fall')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute fall temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)



####### Daily extreme #######
# Reshape data using gather function
gathered_data3 <- tidyr::gather(daynight_averages, key = "Temperature_Variable", value = "Temperature °C", max_T1, max_T2, max_T3)

## MAX temp ##

## winter ##
# Specify the order of levels for Canopy_openness
canopy_order <- c("Open", "Semi-closed", "Closed")

gathered_data3 %>%
  filter(season == 'winter') %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%  # Specify order
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute winter temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)



## Spring ##
gathered_data3 %>%
  filter(season=='spring')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute spring temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)


## summer ##
gathered_data3 %>%
  filter(season=='summer')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute summer temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)

## Fall ##
gathered_data3 %>%
  filter(season=='fall')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute fall temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)


# Reshape data using gather function
gathered_data4 <- tidyr::gather(daynight_averages, key = "Temperature_Variable", value = "Temperature °C",min_T1, min_T2, min_T3)

## Min temp ##

## winter ##
# Specify the order of levels for Canopy_openness
canopy_order <- c("Open", "Semi-closed", "Closed")

gathered_data4 %>%
  filter(season == 'winter') %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%  # Specify order
  ggplot( aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute winter temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Site)



## Spring ##
gathered_data4 %>%
  filter(season=='spring')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Site, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute spring temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Canopy_openness)


## summer ##
gathered_data4 %>%
  filter(season=='summer')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Site, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute summer temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Canopy_openness)

## Fall ##
gathered_data4 %>%
  filter(season=='fall')%>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot( aes(x = Site, y = Temperature, fill = Temperature_Variable)) +
  geom_boxplot() +
  labs(title = "Absolute fall temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  theme_minimal()+
  facet_wrap(~Canopy_openness)




















### T2 and T3 follow the same trend throughout the different seasons and canopy types, 
### so in the following steps only T1 and T3 will be investigated


library(outliers)

grubbs.test(Data_by_Season$T3)
grubbs.test(Data_by_Season$T1)



### Here is the method in case we want to remove all the outliers above the upper fence and bellow the lower fence
head(Data_by_Season)
# For season and canopy openness #
seasons <- unique(Data_by_Season$season)
canopies <- unique(Data_by_Season$Canopy_openness)
sites<-unique(Data_by_Season$Site)
# Create an empty data frame to store the filtered results
joined_data_filtered<- data.frame()

# Loop through each season & canopy types
for (season in seasons) {
  for (canopy in canopies) {
    for (site in sites) {
  subset_data <- Data_by_Season[Data_by_Season$season == season & Data_by_Season$Canopy_openness == canopy & Data_by_Season$Site == site, ] 
  
  # Calculate IQR for each temperature variable
  iqr_T1 <- IQR(subset_data$T1)
  iqr_T2 <- IQR(subset_data$T2)
  iqr_T3 <- IQR(subset_data$T3)
  
  # Set a threshold as 1.5 times the IQR
  threshold_multiplier <- 1.5
  lower_threshold_T1 <- quantile(subset_data$T1, 0.25) - threshold_multiplier * iqr_T1
  upper_threshold_T1 <- quantile(subset_data$T1, 0.75) + threshold_multiplier * iqr_T1
  
  lower_threshold_T2 <- quantile(subset_data$T2, 0.25) - threshold_multiplier * iqr_T2
  upper_threshold_T2 <- quantile(subset_data$T2, 0.75) + threshold_multiplier * iqr_T2
  
  lower_threshold_T3 <- quantile(subset_data$T3, 0.25) - threshold_multiplier * iqr_T3
  upper_threshold_T3 <- quantile(subset_data$T3, 0.75) + threshold_multiplier * iqr_T3
  
  # Filter out values beyond the thresholds for each season
  subset_data_filtered <- subset_data %>%
    filter(
      T1 >= lower_threshold_T1 & T1 <= upper_threshold_T1,
      T2 >= lower_threshold_T2 & T2 <= upper_threshold_T2,
      T3 >= lower_threshold_T3 & T3 <= upper_threshold_T3
    )
  
  # Append the filtered results to the main data frame
  joined_data_filtered <- rbind(joined_data_filtered, subset_data_filtered)
  }
 }
}
# Print the summary of the filtered data
summary(Data_by_Season)
summary(joined_data_filtered)



### visualize the data after removing the outliers
# Reshape data using gather function
gathered_data22 <- tidyr::gather(joined_data_filtered, key = "Temperature_Variable", value = "Temperature °C", T1, T2, T3)

# Create a boxplot with all temperature variables on the same plot
ggplot(gathered_data22, aes(x = Temperature_Variable, y = Temperature, fill = Temperature_Variable)) +
                   geom_boxplot() +
                   labs(title = "Boxplots for T1, T2, and T3", x = "Temperature Variables", y = "Temperature °C") +
                   theme_minimal()+
                   facet_wrap(~ season + Canopy_openness)

#######################################
####    daily min and max temperatures
######################################

#calculate daily averages and daily min and max temperatures
daily_averages_Tmin_Tmax<- Data_by_Season %>%
  group_by(season,day,Canopy_openness,Site,In_out) %>%
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


gathered_data2 %>%
  filter(season == 'fall') %>%
  mutate(Canopy_openness = factor(Canopy_openness, levels = canopy_order)) %>%
  ggplot(aes(x = Canopy_openness, y = Temperature, fill = Temperature_Variable, color = time_of_day)) +
  geom_boxplot() +
  labs(title = "Daily average fall temperature for T1, T2, and T3", x = "Site", y = "Temperature °C") +
  scale_color_manual(values = c("daytime" = "blue", "nighttime" = "red")) +  # Adjust colors as needed
  theme_minimal() +
  facet_wrap(~Site) +
  theme(text = element_text(size = 22),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 50)))

#############################################
###             Data analysis
#############################################

### Check if there is a cage effect on T1 and T3


#################### T1  ####################
hist(Data_by_Season$T1, main = "Histogram of T1", xlab = "T1")


# Apply the log and square root transformation with an offset for T1
offset_value_T1 <- abs(min(Data_by_Season$T1)) + 1  # Add 1 to ensure a positive offset

Data_by_Season$sqrt_T1 <- sqrt(Data_by_Season$T1 + offset_value_T1)
Data_by_Season$log_T1 <- log(Data_by_Season$T1 + offset_value_T1)

# Check for missing or non-positive values in sqrt_T1
any(is.na(Data_by_Season$log_T1))
any(Data_by_Season$log_T1 <= 0)

# Check normality assumption using Q-Q plot
qqnorm(Data_by_Season$sqrt_T1)
qqline(Data_by_Season$sqrt_T1)

# Fit ANOVA model for sqrt_T1
anova(anova_model_T1 <- lm(sqrt_T1 ~ In_out, data = Data_by_Season))

# Check assumptions for sqrt_T1
qqnorm(residuals(anova_model_T1))
qqline(residuals(anova_model_T1))

# Conduct the Wilcoxon rank-sum test for T1
wilcox_test_result_T1 <- wilcox.test(T1 ~ In_out, data = Data_by_Season)
print(wilcox_test_result_T1)

t.test(Data_by_Season$T1 ~ Data_by_Season$In_out)

# Create a boxplot for T1
library(ggplot2)
ggplot(Data_by_Season, aes(x = In_out, y = T1, fill = In_out)) +
  geom_boxplot() +
  labs(x = "In_out", y = "T1", color = "Canopy_openness") +
  theme_minimal()

# mean In  11.16515
# mean out 11.59096 

#################### T3  ####################


hist(Data_by_Season$T3, main = "Histogram of T3", xlab = "T3")


qqnorm(Data_by_Season$T3)
qqline(Data_by_Season$T3)



# Check for missing or non-positive values
offset_value <- abs(min(Data_by_Season$T3)) + 1  # Add 1 to ensure a positive offset

# Apply the log and square root transformation with an offset
Data_by_Season$sqrt_T3 <- sqrt(Data_by_Season$T3 + offset_value)
Data_by_Season$log_T3 <- log(Data_by_Season$T3 + offset_value)
# Check for missing or non-positive values in sqrt_T3
any(is.na(Data_by_Season$log_T3))
any(Data_by_Season$log_T3 <= 0)


qqnorm(Data_by_Season$sqrt_T3)
qqline(Data_by_Season$sqrt_T3)

# Fit ANOVA model
anova(anova_model <- lm(sqrt_T3 ~ In_out , data = Data_by_Season))

# Check assumptions
qqnorm(residuals(anova_model))
qqline(residuals(anova_model))


# Conduct the Wilcoxon rank-sum test
wilcox_test_result <- wilcox.test(T3 ~ In_out, data = Data_by_Season)
print(wilcox_test_result)


ggplot(Data_by_Season, aes(x = In_out, y = T3, fill = In_out)) +
  geom_boxplot() +
  #facet_wrap(~Site) +
  labs(x = "In_out", y = "T3", color = "Canopy_openness") +
  theme_minimal()

## mean In  10.86659
## mean Out 11.03371



head(daynight_averages)
daynight_averages[,-c(8:26)]

#### daily averages ####

# Convert categorical variables to factors
daynight_averages$season <- as.factor(daynight_averages$season)
daynight_averages$Canopy_openness <- as.factor(daynight_averages$Canopy_openness)
daynight_averages$Site <- as.factor(daynight_averages$Site)
daynight_averages$time_of_day <- as.factor(daynight_averages$time_of_day)
daynight_averages$In_out <- as.factor(daynight_averages$In_out)

daynight_averages$avg_T1_transformed <- caret::preProcess(daynight_averages$avg_T1, method = "YeoJohnson")$x

# Build the mixed-effects model
mixed_model <- lmer(log_avg_T1 ~ season * Canopy_openness * Site  + (1|day) + (1|In_out), data = daynight_averages)
linear_model <- lm(avg_T1 ~ season * Canopy_openness * Site  , data = daynight_averages)
anova(mixed_model)

# Perform likelihood ratio test
lr_test <- anova(mixed_model, linear_model)

# Display the results
print(lr_test)
summary(model)

library(MuMIn)
# Compute marginal and conditional R-squared for the mixed-effects model
r_squared_marginal_full <- r.squaredGLMM(mixed_model, which = "marginal")
r_squared_conditional_full <- r.squaredGLMM(mixed_model, which = "conditional")

qqnorm(residuals(mixed_model))
qqline(residuals(mixed_model))


# Extract residuals from the model
residuals <- resid(mixed_model)

# Add residuals to the original data
daynight_averages$residuals <- residuals
head(daynight_averages)
daynight_averages[,-c(8:20)]



# Calculate the upper and lower fences within each combination of season, Canopy_openness, and Site
daynight_averages <- daynight_averages %>%
  group_by(season, Canopy_openness, Site) %>%
  mutate(Q1 = quantile(residuals, 0.25),
         Q3 = quantile(residuals, 0.75),
         IQR = Q3 - Q1,
         lower_fence = Q1 - 1.5 * IQR,
         upper_fence = Q3 + 1.5 * IQR)

# Filter out outliers based on the residuals within each combination
cleaned_data <- daynight_averages %>%
  filter(residuals >= lower_fence & residuals <= upper_fence)




# Create a  boxplot to visually check the outliers 
p1<-ggplot(cleaned_data, aes(x = Canopy_openness, y = residuals, fill = Canopy_openness)) +
  geom_boxplot() +
  labs(title = "Boxplot of Residuals by Canopy_openness",
       x = "Canopy_openness",
       y = "Residuals") +
  scale_fill_manual(values = c("Open" = "lightblue", "Close" = "lightgreen", "Semi_Close" = "lightpink")) +
  theme_minimal()+
  facet_wrap(~season + Site)
#facet_wrap(~Site, scales = "free") 

plotly::ggplotly(p1)

mixed_model2 <- lmer(log_avg_T1 ~ season * Canopy_openness * Site  + (1|day) + (1|In_out), data = cleaned_data)

qqnorm(residuals(mixed_model2))
qqline(residuals(mixed_model2))
check_normality(mixed_model2) 
check_heteroscedasticity(mixed_model2)
check_model(mixed_model2)



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


























































































# Fit a linear mixed-effects model
anova(model_T1 <- lmer(avg_T1 ~ season*Canopy_openness*Site + (1|day)+ (1|In_out), data = daynight_averages))
anova(model_T3 <- lmer(avg_T3 ~ Canopy_openness * Site + (1|day), data = daynight_averages))

summary(model_T1)

mixed(model_T3, data = daily_averages)

qqnorm(residuals(mixed_model))
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






