setwd("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST")
owddraw ="C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST"#New


library(lubridate)
library(tidyr)

#################################
### Clean logger 94242066_site5-O
#################################
# Read the CSV file
Df <- read.csv("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/Trial.csv", sep=";")
Df1 <- read_delim("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/data_94242066_2023_09_07_0.csv",delim = ";", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE)
head(Df)
head(Df1)

# Copy the column names from Df to Df1
colnames(Df1) <- colnames(Df)
# Print the resulting data frame
head(Df1)
str(Df1)
# Convert the "Date" column to a datetime object
Df1$Date <- as.POSIXct(Df1$Date, format="%Y.%m.%d %H:%M", tz="UTC")

# Identify rows with missing values in the "Date" column
na_rows <- which(is.na(Df1$Date))

# Replace missing values with the previous date + 15 minutes
for (row in na_rows) {
  previous_date <- Df1$Date[row - 1]
  new_date <- previous_date + minutes(15)
  Df1$Date[row] <- new_date
}

# Convert the "Date" column back to the original format
Df1$Date <- format(Df1$Date, "%Y.%m.%d %H:%M")

# Print the modified data frame
print(Df1[1110:1120, ])

write.csv2(Df1, "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/new_file.csv", row.names = FALSE)





#################################
### Clean logger 94242066_site4-C
#################################
# Read the CSV file
Df <- read.csv("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/Trial.csv", sep=";")
Df1 <- read_delim("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/4/4-C/data_94242043_2023_09_06_0.csv",delim = ";", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE)
head(Df)
head(Df1)
Df1<-Df1[,-10]
# Copy the column names from Df to Df1
colnames(Df1) <- colnames(Df)
# Print the resulting data frame
head(Df1)
str(Df1)
# Convert the "Date" column to a datetime object
Df1$Date <- as.POSIXct(Df1$Date, format = "%Y/%m/%d %H:%M")

Df1 <- Df1 %>%
  mutate(Date = format(Date, "%Y.%m.%d %H:%M"))



any_na <- any(is.na(Df1))

# Check for NA values in each column

na_in_each_column <- colSums(is.na(Df1))
rows_with_na <- which(apply(is.na(Df1), 1, any))

rows_with_na <- which(apply(is.na(Df1), 1, any))

# Show the rows with NA values and other columns
rows_with_na_values <- Df1[rows_with_na, ]



# Print the modified data frame
print(Df1[1110:1120, ])

write.csv2(Df1, "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/4/4-C/new_file.csv", row.names = FALSE)












#################################
### Clean logger 94242066_site4-O 
#################################
# Read the CSV file
Df <- read.csv("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/Trial.csv", sep=";")
Df1 <- read_delim("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/4/4-O/data_94242044_2023_09_06_0.csv", delim = ";", escape_double = FALSE, col_types = cols(`25,875` = col_character()),trim_ws = TRUE)

head(Df)
head(Df1)

Df1<-Df1[,-10]

str(Df1)

# Copy the column names from Df to Df1
colnames(Df1) <- colnames(Df)

# Print the resulting data frame

head(Df1)
Df1$Date <- as.POSIXct(Df1$Date, format="%Y.%m.%d %H:%M", tz="UTC")



Df1 <- Df1 %>%
  mutate(Date2 = format(Date, "%Y.%m.%d"),
         Time = format(Date, "%H:%M"))


duplicates <- Df1 %>%
  select(-Date)%>%
  group_by(Date2, Time) %>%
  filter(n() > 1) %>%
  arrange(Date2, Time)

# View the duplicates
print(duplicates)


# Remove duplicates and keep only the first occurrence of each duplicated time within each date group.
unique_data <- Df1 %>%
  distinct(Date2, Time, .keep_all = TRUE)
head(unique_data)

unique_data2<-unique_data[,c(1:9)]
head(unique_data2)
  
# Convert the "Date" column back to the original format
unique_data2$Date <- format(unique_data2$Date, "%Y.%m.%d %H:%M")

write.csv2(unique_data2, "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/4/4-O/new_file.csv", row.names = FALSE)




#################################
### Clean logger 94242052_site2-S 
#################################
Df <- read.csv("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/5/5-O/Trial.csv", sep=";")
Df1 <- read_delim("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/2/2-S/data_94242052_2023_09_04_0.csv", delim = ";", escape_double = FALSE, col_types = cols(`25,875` = col_character()),trim_ws = TRUE)

head(Df)
head(Df1)

Df1<-Df1[,-10]

str(Df1)

# Copy the column names from Df to Df1
colnames(Df1) <- colnames(Df)

# Print the resulting data frame

head(Df1)
Df1$Date <- as.POSIXct(Df1$Date, format="%Y.%m.%d %H:%M", tz="UTC")

Df1 <- Df1 %>%
  mutate(Date2 = format(Date, "%Y.%m.%d"),
         Time = format(Date, "%H:%M"))


duplicates <- Df1 %>%
  select(-Date)%>%
  group_by(Date2, Time) %>%
  filter(n() > 1) %>%
  arrange(Date2, Time)

# View the duplicates
print(duplicates)


# Remove duplicates and keep only the first occurrence of each duplicated time within each date group.
unique_data <- Df1 %>%
  distinct(Date2, Time, .keep_all = TRUE)
head(unique_data)

unique_data2<-unique_data[,c(1:9)]
head(unique_data2)

# Convert the "Date" column back to the original format
unique_data2$Date <- format(unique_data2$Date, "%Y.%m.%d %H:%M")

write.csv2(unique_data2, "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST/2/2-S/new_file.csv", row.names = FALSE)







