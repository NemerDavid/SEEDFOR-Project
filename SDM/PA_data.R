remotes::install_github("Jeremy-borderieux/FrenchNFIfindeR")

library(FrenchNFIfindeR)
library(tidyverse)
library(data.table)

get_NFI()

### Saving the NFI datasets to my directory  ###
data_list <- list(NFI_cover = NFI_cover, NFI_ecology = NFI_ecology, NFI_dendro = NFI_dendro, NFI_plot_info = NFI_plot_info, NFI_flora = NFI_flora, NFI_tree = NFI_tree)
# Your directory path
path_NFI <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/NFI_Data"

# Loop through each item in the data list
for (name in names(data_list)) {
  # Create the full file path for each RDS file
  file_path <- file.path(path_NFI, paste0(name, ".rds"))
  # Save each dataset as an RDS file
  saveRDS(data_list[[name]], file_path)
}

### loading the saved datasets ### 
# List of dataset names
dataset_names <- c("NFI_cover", "NFI_ecology", "NFI_dendro", "NFI_plot_info", "NFI_flora", "NFI_tree")

# Initialize an empty list to store the loaded data
data_list_loaded <- list()

# Loop through each dataset name, load the RDS file, and store it in the list
for (name in dataset_names) {
  # Create the full file path for each RDS file
  file_path <- file.path(path_NFI, paste0(name, ".rds"))
  # Load each dataset and store it in the list with the corresponding name
  data_list_loaded[[name]] <- readRDS(file_path)
}

list2env(data_list_loaded, envir = .GlobalEnv)

### to load only specific datasets ###
NFI_tree <- readRDS("C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/NFI_Data/NFI_tree.rds")
NFI_tree <- as.data.frame(NFI_tree)
class(NFI_tree)

NFI_tree_filtered <- NFI_tree %>%
  select(campagne, idp, species_name) %>%
  filter(!is.na(species_name))

unique1<-unique(NFI_tree_filtered$idp)
###########################################################################
#### create a new column presence_absence where each row indicates whether 
### the species in that row is present (1) or absent (0) in a specific idp
###########################################################################

# Get unique species names
unique_species <- unique(NFI_tree_filtered$species_name)

# Create an empty list to store results for each species
summary_list <- list()

# Loop through each species
for (species in unique_species) {
  # Create a summary table for the current species
  species_summary <- NFI_tree_filtered %>%
    group_by(idp,campagne) %>%
    summarize(presence_absence = ifelse(any(species_name == species), "Present", "Absent")) %>%
    mutate(species = species)  # Add species name to the summary table
  
  # Append the summary table to the list
  summary_list[[species]] <- species_summary
}

# Combine all summary tables into one data frame
summary_df <- bind_rows(summary_list)

# Print the summary data frame
head(summary_df)
nrow(summary_df)

path_SDM <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data"
file_path <- file.path(path_SDM, "summary_df.rds")
# Save each dataset as an RDS file
saveRDS(summary_df, file_path)
summary_df<- readRDS("C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data/summary_df.rds")
class(summary_df)

# Convert to data.table
summary_dt <- as.data.table(summary_df)

# Check the class of the new object
class(summary_dt)


Species_data <- subset(summary_dt, 
                       species %in% c("Fagus sylvatica", 
                                           "Quercus petraea subsp. petraea",
                                           "Quercus robur var. robur",
                                           "Quercus ilex",
                                           "Quercus pubescens"))
head(Species_data)
unique_species <- unique(Species_data$species)
nrow(Species_data)

# Check if the lengths are the same
unique1<-unique(NFI_tree_filtered$idp)
unique2<-unique(summary_dt$idp)
unique3<-unique(Species_data$idp)

length(unique2) == length(unique3)



path_SDM <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data"
file_path <- file.path(path_SDM, "Species_data.rds")
# Save each dataset as an RDS file
saveRDS(Species_data, file_path)
Species_data<- readRDS("C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data/Species_data.rds")


############################################
#### joining NFI_plot_info with species_data 
############################################
NFI_plot_info <- readRDS("C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/NFI_Data/NFI_plot_info.rds")


NFI_plot_info2<-NFI_plot_info%>%
  select(campagne,idp,xl,yl)
head(NFI_plot_info2)

head(NFI_plot_info2)


###  first way 
# Perform left join
result <- merge(Species_data, NFI_plot_info2, by = c("idp", "campagne"), all.x = TRUE)
head(result)
duplicates <- table(duplicated(result))


###  second way 
joined_data <- Species_data %>%
  left_join(NFI_plot_info2, by = c("idp","campagne"), relationship = "many-to-many") %>%
  # Select the columns you want from the joined data and filtered data
  select(idp, campagne, species, presence_absence, xl, yl)

head(joined_data)

duplicates <- table(duplicated(joined_data))
class(joined_data)


### in case we have duplicates we do the following
# Identify rows with all same values
all_same_values <- joined_data[duplicated(joined_data) | duplicated(joined_data, fromLast = TRUE), ]
# Remove duplicates from joined_data2
unique_data <- distinct(joined_data)

# Check the dimensions of the unique data
dim(unique_data)
# Print the head of the unique data
head(unique_data) 
duplicates <- table(duplicated(unique_data))




################################################################
###                          SDM                             ###
###############################################################
library(sf)
library(terra)
library(colorspace)
library(ggspatial)
library(dismo)
library(spatstat)
library(ROCR)
library(gbm)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(mapview)
library(leaflet)
##########################################
### Extract the French territory shapefile
##########################################
# Load high-detail world data
world_high <- ne_countries(scale = "large", type = 'countries', returnclass = "sf")

# Filter for France
france <- world_high[world_high$admin == "France", ]
class(france)


# Plotting France with an emphasis on Metropolitan France by adjusting plot limits
# Note: These coordinates are an approximation and might need fine-tuning
france
ggplot(data = france) +
  geom_sf(fill = "lightblue", color = "black") +
  coord_sf(xlim = c(-5, 10), ylim = c(41, 52), expand = FALSE) +
  ggtitle("Metropolitan France with High Detail")


# Define the bounding box for Metropolitan France
bbox_metropolitan <- c(xmin = -5, ymin = 41, xmax = 10, ymax = 52)
# Crop the polygons to include only those within the bounding box
france_metropolitan <- st_crop(france, bbox_metropolitan)
# Plot the subsetted geometry
ggplot() +
  geom_sf(data = france_metropolitan, fill = "lightblue", color = "black") +
  ggtitle("Metropolitan France with High Detail")


leaflet(data = france) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(fillColor = "lightblue", weight = 1, color = "black") %>%
  setView(lng = 2.2137, lat = 46.2276, zoom = 6)  # Centered on France, adjust zoom for detail



##########################
### 12.1 Tree Species Data
##########################
head(joined_data)
class(joined_data)


# These data frames are converted into sf objects and assigned the correct
# coordinate system  
# Convert data.table to data.frame
joined_df <- as.data.frame(joined_data)




joined_df
summary(joined_df)
# Assuming joined_df is your dataframe
joined_df$new_column <- ifelse(joined_df$presence_absence == "Present", 1, 0)


complete_rows <- na.omit(joined_df[, c("xl", "yl")])
joined_df_complete <- joined_df[complete.cases(joined_df[, c("xl", "yl")]), ]



# Define the projection
utm_zone <- "+proj=utm +zone=31 +datum=WGS84"  # UTM Zone 31 for France
proj4string <- CRS(utm_zone)


# Create SpatialPoints object
coordinates <- cbind(joined_df_complete$xl, joined_df_complete$yl)
spatial_points <- SpatialPoints(coordinates, proj4string = proj4string)

# Project coordinates to geographic (longitude/latitude) using spTransform
spatial_points_geo <- spTransform(spatial_points, CRS("+proj=longlat +datum=WGS84"))

# Extract coordinates in degrees
coordinates_degrees <- coordinates(spatial_points_geo)

# Update the original data frame with transformed coordinates
joined_df_complete$longitude <- coordinates_degrees[, 1]
joined_df_complete$latitude <- coordinates_degrees[, 2]



joined_df_complete


# Convert to sf object
head(joined_df_complete)
coordinates <- c("longitude", "latitude")
joined_sf <- st_as_sf(joined_df_complete, coords = coordinates)
# Assign CRS
joined_sf <- st_set_crs(joined_sf, 4326) 


bbox_joined <- sf::st_bbox(joined_sf)
bbox_france <- sf::st_bbox(france_metropolitan)

####################################
### Calculate the correction factors
####################################

# Calculate the range of coordinates in species_sf
x_range_species <- bbox_joined["xmax"] - bbox_joined["xmin"]
y_range_species <- bbox_joined["ymax"] - bbox_joined["ymin"]

# Calculate the range of metropolitan France coordinates
x_range_france <- bbox_france["xmax"] - bbox_france["xmin"]
y_range_france <- bbox_france["ymax"] - bbox_france["ymin"]

# Calculate correction factors for scaling
x_correction_factor <- x_range_france / x_range_species
y_correction_factor <- y_range_france / y_range_species


# Apply correction factors to all coordinates
Presence_Absence<-joined_df_complete
Presence_Absence$longitude <- (Presence_Absence$longitude - bbox_joined["xmin"]) * x_correction_factor + bbox_france["xmin"]
Presence_Absence$latitude <- (Presence_Absence$latitude - bbox_joined["ymin"]) * y_correction_factor + bbox_france["ymin"]



path_SDM <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data"
file_path <- file.path(path_SDM, "Presence_Absence.rds")
# Save each dataset as an RDS file
saveRDS(Presence_Absence, file_path)
Presence_Absence<- readRDS("C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/SDM_Data/Presence_Absence.rds")



# Convert to sf object
coordinates <- c("longitude", "latitude")
Presence_Absence_sf2 <- st_as_sf(Presence_Absence, coords = coordinates)
# Assign CRS
Presence_Absence_sf2 <- st_set_crs(Presence_Absence_sf2, 4326) 


bbox_joined <- sf::st_bbox(joined_sf)
bbox_france <- sf::st_bbox(france_metropolitan)
bbox_Presence_Absence<- sf::st_bbox(Presence_Absence_sf2)

###################################################
### Filter Presence_Absence for the desired species
###################################################
unique(Presence_Absence_sf2$species) 


### 1. quercus petraea subsp. petraea ###
#=======================================#

species_name <- "Quercus petraea subsp. petraea"
species_sf <- Presence_Absence_sf2[Presence_Absence_sf2$species == species_name, ]
france_metropolitan
# Assuming france is in a projected coordinate system, let's reproject species_sf to match
species_sf <- st_transform(species_sf, st_crs(france_metropolitan))

# Get the extent of the France outline
bbox_species <- sf::st_bbox(species_sf)
# Perform spatial join to keep only points inside the borders of polygons
points_inside <- st_intersection(species_sf, france_metropolitan)
# Retain only the columns from species_sf
species_sf <- points_inside[, names(species_sf)]




# Plot
ggplot() +
  geom_sf(data = france_metropolitan, fill = "gray",lwd = 0.7,color = "gray") +
  geom_sf(data = species_sf,
          aes(color =as.character(new_column)),
          size = 0.3) +
  scale_color_manual(name = "Quercus petraea subsp. petraea",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present"))+
  coord_sf(xlim = c(bbox_species["xmin"], bbox_species["xmax"]),
           ylim = c(bbox_species["ymin"], bbox_species["ymax"])) +
  annotation_scale(location = 'bl')+ # this add a scale to the map
  theme_minimal()+
  theme_void()+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+ # Increase point size in legend
  labs(title = "Map of presence-absence of Quercus petraea subsp. petraea",
       x = NULL, y = NULL)


### 2. Quercus robur var. robur ###
#=================================#

### 3. Quercus pubescens ###
#=========================#

### 4. Quercus ilex ###
#=====================#


### 5. Fagus sylvatica ###
#=======================#
species_name <- "Fagus sylvatica"
species_sf <- Presence_Absence_sf2[Presence_Absence_sf2$species == species_name, ]
france_metropolitan
# Assuming france is in a projected coordinate system, let's reproject species_sf to match
species_sf <- st_transform(species_sf, st_crs(france_metropolitan))

# Get the extent of the France outline
bbox_species <- sf::st_bbox(species_sf)
# Perform spatial join to keep only points inside the borders of polygons
points_inside <- st_intersection(species_sf, france_metropolitan)
# Retain only the columns from species_sf
species_sf <- points_inside[, names(species_sf)]


# Plot
ggplot() +
  geom_sf(data = france_metropolitan, fill = "gray",lwd = 0.7,color = "gray") +
  geom_sf(data = species_sf,
          aes(color =as.character(new_column)),
          size = 0.3) +
  scale_color_manual(name = "Fagus sylvatica",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present"))+
  coord_sf(xlim = c(bbox_species["xmin"], bbox_species["xmax"]),
           ylim = c(bbox_species["ymin"], bbox_species["ymax"])) +
  annotation_scale(location = 'bl')+ # this add a scale to the map
  theme_minimal()+
  theme_void()+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+ # Increase point size in legend
  labs(title = "Map of presence-absence of Fagus sylvatica",
       x = NULL, y = NULL)


species_sf2 <- species_sf %>%
  select(new_column, geometry)

species_sf2 <- species_sf[, c("new_column", "geometry")]
# compute kernel-weighted local means #
boundary_utm <- st_transform(france_metropolitan, 32631)
abla_utm <- st_transform(species_sf2, 32631)


abla_ppp <- as.ppp(abla_utm)
class(abla_ppp)
## [1] "ppp"
Window(abla_ppp) <- as.owin(boundary_utm)
abla_ppp


# The Smooth.ppp() function can now be used to compute kernel-weighted means
# The sigma argument specifies the bandwidth of the kernel, which can be conceptualized
# as the approximate radius of the local window used to compute the means. The eps argument
# specifies the grid cell size of the output grid. The output is a spatstat im object, which can be
# converted to a terra SpatRaster object. The coordinate reference system information is lost in
# the conversion, so the same CRS as the original abla_ppp object is assigned to the new raster dataset.
abla_im <- Smooth.ppp(abla_ppp,
                      sigma=100000,
                      eps=c(1000, 1000))
class(abla_im)

abla_grid <- rast(abla_im)
crs(abla_grid) <-"epsg:32631"
class(abla_grid)


# Convert SpatRaster to a raster object from the raster package
abla_raster <- raster(abla_grid)

# Extract values and coordinates from the raster
abla_df <- rasterToPoints(abla_raster)
abla_df <- data.frame(abla_df)


ggplot(data = abla_df) +
  geom_raster(aes(x = x, y = y, fill = lyr.1)) +
  scale_fill_gradient(name = "Subalpine Fir",
                      low = "lightyellow",
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_utm,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()


ggplot(data = abla_df) +
  geom_tile(aes(x = x, y = y, fill = lyr.1), width = 1000, height = 1000) +  # Adjust width and height as needed
  scale_fill_gradient(name = "Subalpine Fir",
                      low = "lightyellow",
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_utm,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()


###########################################
#### 12.2 WorldClim Historical Climate Data
###########################################
library("geodata")

bioclim_data <- worldclim_global(var = "bio",res = 2.5, path = "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/Worldlcim_Data")

# Set the directory where you downloaded the WorldClim data
download_dir <- "C:/Users/David/Desktop/SeedFor/Data analysis/SEEDFOR-Project/SDM/Worldlcim_Data/wc2.1_2.5m/"

# List all files in the download directory
file_list <- list.files(download_dir, pattern = "\\.tif$", full.names = TRUE)

# Load all GeoTIFF files into R
bioclim_data <- lapply(file_list, raster)


# Combine all raster layers into a single raster stack
bioclim_stack <- stack(bioclim_data)


wcbio <- rast(bioclim_stack)
class(wcbio)
nlyr(wcbio)



# Extract the existing names
existing_names <- names(wcbio)
# Generate new names
new_names <- paste0("bio", c(1, 10:19, 2:9))
# Assign new names to the RasterBrick object
names(wcbio) <- new_names

class(france_metropolitan)
class(wcbio)

boundary_wgs84 <- st_transform(france_metropolitan, st_crs(wcbio))
# Crop all layers in the raster stack 'wcbio' to the extent of France metropolitan
wcbio_crop <- crop(wcbio, vect(boundary_wgs84))
# Mask the cropped raster stack using the rasterized boundary
wcbio_msk <- mask(wcbio_crop, vect(boundary_wgs84))
class(wcbio_msk)





# Convert SpatRaster to a raster object from the raster package
mtwm_raster <- raster(wcbio_msk[["bio5"]])

# Extract values and coordinates from the raster
mtwm_df <- rasterToPoints(mtwm_raster)
mtwm_df <- data.frame(mtwm_df)


ggplot(data = mtwm_df) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = bio5)) +
  scale_fill_gradient(name = "Temperature (\u00B0C)",
                      low = "lightyellow",
                      high = "darkred",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()



bbox_joined <- sf::st_bbox(wcbio_msk)
bbox_france <- sf::st_bbox(france_metropolitan)
bbox_Presence_Absence<- sf::st_bbox(Presence_Absence_sf2)

class(mtwm_df)

ggplot()+
  geom_tile(data = mtwm_df,aes(x = x,
                  y = y,
                  fill = bio5))


ggplot() +
  geom_sf(data = france_metropolitan, fill = "gray",lwd = 0.7,color = "gray") +
  geom_sf(data = species_sf,
          aes(color =as.character(new_column)),
          size = 0.3) +
  scale_color_manual(name = "Fagus sylvatica",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present"))+
  coord_sf(xlim = c(bbox_species["xmin"], bbox_species["xmax"]),
           ylim = c(bbox_species["ymin"], bbox_species["ymax"])) +
  annotation_scale(location = 'bl')+ # this add a scale to the map
  theme_minimal()+
  theme_void()+
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 18, hjust = 0.5))+
  guides(color = guide_legend(override.aes = list(size = 5)))+ # Increase point size in legend
  labs(title = "Map of presence-absence of Fagus sylvatica",
       x = NULL, y = NULL)
