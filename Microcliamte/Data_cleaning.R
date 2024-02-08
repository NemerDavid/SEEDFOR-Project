library(myClim)
library(stringr)
library(ggplot2)

#Hp laptop#
setwd("C:/Users/nemer/Project/SEEDFOR/Field/terrain SEEDFOR/TOMST")#old
owddraw ="C:/Users/nemer/Project/SEEDFOR/Field/terrain SEEDFOR/TOMST"#old

setwd("C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST")#New
owddraw ="C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST"#New

new_folder_path <- "C:/Users/nemer/Project/Data analysis Git/SEEDFOR-Project/Data/TOMST_Cleaned1"

dataNames <- list.files(pattern = '^data.*\\.csv$', path = owddraw, full.names = FALSE, recursive = TRUE)
# Print the list of matching CSV files
print(dataNames)



# Remove the logger 6/6-S/data_94242064_2023_09_07_0.csv as it has few measurements recorded and causing an error while loading
# Remove the specified substring from each element in the list
dataNames <- dataNames[dataNames != "6/6-S/data_94242060_2023_09_07_0.csv"]

#########################################################################################
### Ensure that all data on each logger is clean (duplicates, start date, end date, etc.)
#########################################################################################
tms_data <- mc_read_files(dataNames,
                          dataformat_name = "TOMST", silent = T)


# clean runs automatically while reading
tms <- mc_prep_clean(tms_data, silent = T) # clean series
#> Warning in mc_prep_clean(tms.m, silent = T): MyClim object is already cleaned.
#> Repeated cleaning overwrite cleaning informations.
mc_info_clean(tms) #returning the data frame with cleaning log
head(mc_info(tms),12) # data frame with summary per sensor
head(mc_info_meta(tms),12) #returning the data frame with locality metadata


#########################################################################
###   Organizing the loggers in 3 categories (Open, Semi-closerd, Closed)
########################################################################
# Filter the file paths based on categories using grepl
open_files <- dataNames[grepl("O", dataNames)]
semi_closed_files <- dataNames[grepl("S", dataNames)]
closed_files <- dataNames[grepl("C", dataNames)]

# Print the lists of matching files
print(open_files)
print(semi_closed_files)
print(closed_files)

tms_open <- mc_read_files(open_files, dataformat_name = "TOMST",recursive = FALSE, silent = TRUE,clean = T)
tms_semi_closed <- mc_read_files(semi_closed_files, dataformat_name = "TOMST", recursive = FALSE, silent = TRUE,clean = T)
tms_closed <- mc_read_files(closed_files, dataformat_name = "TOMST", recursive = FALSE, silent = TRUE,clean = T)


mc_info_clean(tms_open)
mc_info_clean(tms_semi_closed)
mc_info_clean(tms_closed)

################################
### Graphical check of the data
###############################
# Before cropping #

#Open canopy#
mc_plot_line(tms_open, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1")) ## The Logger 94200377 on Site 6-O is not working properly 
#Semi_closed canopy#
mc_plot_line(tms_semi_closed, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1"))
#Closed canopy#
mc_plot_line(tms_closed, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1"))


# After cropping #
cropped_data_open <- mc_prep_crop(tms_open, start=as.POSIXct("2022-11-1", tz="UTC"),end=as.POSIXct("2023-09-04", tz="UTC"))
cropped_data_semi_closed <- mc_prep_crop(tms_semi_closed, start=as.POSIXct("2022-11-1", tz="UTC"),end=as.POSIXct("2023-09-04", tz="UTC"))
cropped_data_closed <- mc_prep_crop(tms_closed, start=as.POSIXct("2022-11-1", tz="UTC"),end=as.POSIXct("2023-09-04", tz="UTC"))

#Open canopy#
mc_plot_line(cropped_data_open, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1"))+ 
ggtitle("Open canopy")
#Semi_closed canopy#
mc_plot_line(cropped_data_semi_closed, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1"))+ 
ggtitle("Semi_closed canopy")
#Closed canopy#
mc_plot_line(cropped_data_closed, sensors = c("TMS_T3","TMS_T2" ,"TMS_T1"))+
ggtitle("Closed canopy")



# Detection of out-of-soil measurements from TMS logger #
mc_prep_TMSoffsoil(cropped_data_open)
data_open <- mc_prep_TMSoffsoil(cropped_data_open)
data_semi_closed <- mc_prep_TMSoffsoil(cropped_data_semi_closed)
data_closed <- mc_prep_TMSoffsoil(cropped_data_closed)
suppressWarnings({
  data_semi_closed <- mc_prep_TMSoffsoil(cropped_data_semi_closed)
})

mc_plot_line(data_open, sensors = c("off_soil","TMS_T1", "TMS_T2","TMS_T3"))+ 
ggtitle("Open canopy")
mc_plot_line(data_semi_closed, sensors = c("off_soil","TMS_T1", "TMS_T2","TMS_T3"))+ 
ggtitle("Semi_closed canopy")
#Closed canopy#
mc_plot_line(data_closed, sensors = c("off_soil","TMS_T1", "TMS_T2","TMS_T3"))+
ggtitle("Closed canopy")


### From the graphics we can see that the following loggers were out of soil:
### Open: 94200377-Site6 ; 94242033-Site2 ; 94242039-Site3
# Closed: 94242031-Site2 ; 94242043-Site4 ; 94242059-Site5
### Semi: 94242034-Site2 ; 94242047-Site3 




##############################################################
### Store the cleaned and cropped rds datasets in a new folder
##############################################################

# Specify the path for the new folder where you want to store the data frames
new_folder_path <- "C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST_Cleaned1"

# Loop through each CSV file and process the TOMST logger data
for (file_path in dataNames) {
  
  # Read data using mc_read_files
  tms_data <- mc_read_files(file_path, dataformat_name = "TOMST", silent = TRUE)
  
  # Crop datetime for all files
  cropped_data <- mc_prep_crop(tms_data, start = as.POSIXct("2022-11-1", tz = "UTC"), end = as.POSIXct("2023-09-04", tz = "UTC"))
  
  # Extract metadata and localities components
  metadata_component <- cropped_data$localities[[1]]$metadata
  loggers_component <- cropped_data$localities[[1]]$loggers[[1]]
  
  # Extract logger metadata
  logger_metadata <- loggers_component$metadata
  locality_id <- metadata_component@locality_id
  
  # Extract datetime and sensors data
  datetime <- loggers_component$datetime
  sensors <- loggers_component$sensors
  
  # Determine the canopy openness based on the directory part of the path
  common_prefix <- dirname(file_path)
  
  # Extract the modality code from the common prefix
  modality_code <- substr(common_prefix, nchar(common_prefix), nchar(common_prefix))
  
  # Map the modality code to its description
  modality <- switch(modality_code, "C" = "Closed", "S" = "Semi-closed", "O" = "Open")
  
  # Extract the site code from the common prefix
  site_code <- substr(common_prefix, 1, 1)
  
  # Map the site code to its description
  site <- paste0("site ", site_code)
  
  # Create a data frame for the current TOMST logger
  result_data <- data.frame(date = as.POSIXct(datetime),
                            logger_id = locality_id,
                            T1 = sensors$TMS_T1$values,
                            T2 = sensors$TMS_T2$values,
                            T3 = sensors$TMS_T3$values,
                            soil_moisture = sensors$TMS_moist$values,
                            Canopy_openness = rep(modality, length(datetime)),
                            Site = rep(site, length(datetime))
  )
  
  # Define the new file path in the new folder
  new_file_path <- file.path(new_folder_path, paste0(tools::file_path_sans_ext(basename(file_path)), ".rds"))
  
  # Save the result_data to a new RDS file in the new folder
  saveRDS(result_data, new_file_path)
}

lapply(result_data, head)
##############################################################################
###    We can also create a new folder in which we store each group of rds 
### files in separate folders according to the original paths of the raw data
##############################################################################
# Loop through each CSV file and process the TOMST logger data
for (file_path in dataNames) {
  
  # Read data using mc_read_files
  tms_data <- mc_read_files(file_path, dataformat_name = "TOMST", silent = TRUE)
  
  # Crop datetime for all files
  cropped_data <- mc_prep_crop(tms_data, start = as.POSIXct("2022-11-1", tz = "UTC"), end = as.POSIXct("2023-09-04", tz = "UTC"))
  
  # Extract metadata and localities components
  metadata_component <- cropped_data$localities[[1]]$metadata
  loggers_component <- cropped_data$localities[[1]]$loggers[[1]]
  
  # Extract logger metadata
  logger_metadata <- loggers_component$metadata
  locality_id <- metadata_component@locality_id
  
  # Extract datetime and sensors data
  datetime <- loggers_component$datetime
  sensors <- loggers_component$sensors
  
  # Determine the canopy openness based on the directory part of the path
  common_prefix <- dirname(file_path)
  # Extract the modality code from the common prefix
  modality_code <- substr(common_prefix, nchar(common_prefix), nchar(common_prefix))
  # Map the modality code to its description
  modality <- switch(modality_code, "C" = "Closed", "S" = "Semi-closed", "O" = "Open")
  
  # Extract the site code from the common prefix
  site_code <- substr(common_prefix, 1, 1)
  # Map the site code to its description
  site <- paste0("site ", site_code)
  
  
  # Create a data frame for the current TOMST logger
  result_data <- data.frame(
    date = as.POSIXct(datetime),
    logger_id = locality_id,
    T1 = sensors$TMS_T1$values,
    T2 = sensors$TMS_T2$values,
    T3 = sensors$TMS_T3$values,
    soil_moisture = sensors$TMS_moist$values,
    Canopy_openness = rep(modality, length(datetime)),
    Site = rep(site, length(datetime))
  )
  
  # Create a new folder based on the common prefix in the new directory
  new_folder_path2 <- file.path("C:/Users/nemer/Project/Data analysis/SEEDFOR/Data/TOMST_Cleaned2", common_prefix)
  
  # Create the new folder if it doesn't exist
  if (!dir.exists(new_folder_path2)) {
    dir.create(new_folder_path2, recursive = TRUE)
  }
  
  # Define the new file path in the new folder
  new_file_path <- file.path(new_folder_path2, paste0(tools::file_path_sans_ext(basename(file_path)), ".rds"))
  
  # Save the result_data to a new RDS file in the new folder
  saveRDS(result_data, new_file_path)
}


cat("Length of datetime:", length(datetime), "\n")
cat("Length of locality_id:", length(locality_id), "\n")


###################################################################################
### Initialize an empty list to store the resulting cleaned and cropped data frames
##################################################################################
result_data_list <- list()

# Loop through each CSV file and process the TOMST logger data
for (file_path in dataNames) {
  # Read data using mc_read_files
  tms_data <- mc_read_files(file_path, dataformat_name = "TOMST", silent = TRUE)
  
  # Crop datetime for all files
  cropped_data <- mc_prep_crop(tms_data, start = as.POSIXct("2022-11-1", tz = "UTC"), end = as.POSIXct("2023-01-10", tz = "UTC"))
  
  # Extract metadata and localities components
  metadata_component <- cropped_data$localities[[1]]$metadata
  loggers_component <- cropped_data$localities[[1]]$loggers[[1]]
  
  
  # Extract logger metadata
  logger_metadata <- loggers_component$metadata
  locality_id <- metadata_component@locality_id
  
  # Extract datetime and sensors data
  datetime <- loggers_component$datetime
  sensors <- loggers_component$sensors
  
 # Determine the canopy openness based on the directory part of the path
  common_prefix <- dirname(file_path)
  
  # Extract the modality code from the common prefix
  modality_code <- substr(common_prefix, nchar(common_prefix), nchar(common_prefix))
  
  # Map the modality code to its description
  modality <- switch(modality_code, "C" = "Closed", "S" = "Semi-closed", "O" = "Open")
  
  # Extract the site code from the common prefix
  site_code <- substr(common_prefix, 1, 1)
  
  # Map the site code to its description
  site <- paste0("site ", site_code)
  
  # Create a data frame for the current TOMST logger
  result_data <- data.frame(date = as.POSIXct(datetime),
                            logger_id = locality_id,
                            T1 = sensors$TMS_T1$values,
                            T2 = sensors$TMS_T2$values,
                            T3 = sensors$TMS_T3$values,
                            soil_moisture = sensors$TMS_moist$values,
                            Canopy_openness = rep(modality, length(datetime)),
                            Site = rep(site, length(datetime))
  )
  
  # Append the result_data to the result_data_list
  result_data_list[[file_path]] <- result_data
}

# Display the first few rows of each resulting data frame
lapply(result_data_list, head)








