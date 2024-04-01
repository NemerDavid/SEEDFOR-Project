################################################################
###                          SDM                            ###
###############################################################
library(tidyverse)
library(sf)
library(terra)
library(colorspace)
library(ggspatial)
library(dismo)
library(spatstat)
library(ROCR)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(geodata)
library(raster)
library(sp)
library(precrec)
library(pROC)

##########################################
### Extract the French territory shapefile
##########################################
# Load high-detail world data
world_high <- ne_countries(scale = "large", type = 'countries', returnclass = "sf")

# Filter for France
france <- world_high[world_high$admin == "France", ]
class(france)


# Plotting France with an emphasis on Metropolitan France by adjusting plot limits
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




##################################
### Load the presence absence data
##################################
Presence_Absence<- readRDS("C:/Users/nemer/Project/SEEDFOR-Project/SDM/SDM_Data/Presence_Absence.rds")

head(Presence_Absence)


# Convert to sf object
coordinates <- c("longitude", "latitude")
Presence_Absence_sf2 <- st_as_sf(Presence_Absence, coords = coordinates)
# Assign CRS
Presence_Absence_sf2 <- st_set_crs(Presence_Absence_sf2, 4326) 

### check if the extent are the same  ###
bbox_france <- sf::st_bbox(france_metropolitan)
bbox_Presence_Absence<- sf::st_bbox(Presence_Absence_sf2)

###################################################
### Filter Presence_Absence for the desired species
###################################################
### 1. Fagus sylvatica ###
#=======================#
species_name <- "Fagus sylvatica"
species_sf <- Presence_Absence_sf2[Presence_Absence_sf2$species == species_name, ]
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
          aes(color =as.character(presence_absence)),
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

class(species_sf)

##########################################################
#################### bioclimatic Data ####################
##########################################################

#### load the pH  ######
download_ph <- "C:/Users/nemer/Project/SEEDFOR-Project/SDM/pH/"
file_ph<-list.files(download_ph, pattern = "\\.tif$", full.names = TRUE)
raster_ph<-raster(file_ph)

# Define the new CRS
new_crs <- "+proj=longlat +datum=WGS84 +no_defs"
# Project the raster to the new CRS
raster_ph_wgs84 <- projectRaster(raster_ph, crs = new_crs)
raster_ph_wgs84<-rast(raster_ph_wgs84)

boundary_wgs84 <- st_transform(france_metropolitan, st_crs(raster_ph_wgs84))
# Crop all layers in the raster stack 'ph' to the extent of France metropolitan
ph_crop <- crop(raster_ph_wgs84, vect(boundary_wgs84))
# Mask the cropped raster stack using the rasterized boundary
ph_msk <- mask(ph_crop, vect(boundary_wgs84))
names(ph_msk) <- "pH"



### 1a- Load current climate (Chelsa)   ###
###########################################

Chelsa_pastData <- readRDS("C:/Users/nemer/Project/SEEDFOR-Project/SDM/Chelsa/Chelsa_HistoricalData_1981_2010/rast_combi.rds")


## Filter for specefic bioclimatic variables  
Chelsabio_filtered<-Chelsa_pastData[[c(1, 5, 6, 12, 16, 17)]]

boundary_wgs84_chelsa <- st_transform(france_metropolitan, st_crs(Chelsabio_filtered))
# Crop all layers in the raster stack 'wcbio' to the extent of France metropolitan
Chelsabio_crop <- crop(Chelsabio_filtered, vect(boundary_wgs84_chelsa))
# Mask the cropped raster stack using the rasterized boundary
Chelsabio_msk <- mask(Chelsabio_crop, vect(boundary_wgs84_chelsa))


# Resample ph_msk to match the resolution and extent of wcbio_msk
Chelsa_ph_msk_resampled <- resample(ph_msk, Chelsabio_msk, method = "bilinear")
#plot(Chelsa_ph_msk_resampled)

ChelsaCombined_msk<-c(Chelsabio_msk,Chelsa_ph_msk_resampled)
names(ChelsaCombined_msk)



# Indices of the layers i want to convert to Celsius
indices_to_convert <- c(1, 2, 3)  # Bio1, Bio5, and Bio6

# Convert specific layers from Kelvin to Celsius
for (i in indices_to_convert) {
  ChelsaCombined_msk[[i]] <- ChelsaCombined_msk[[i]] - 273.15
}



###    2b- Load Future climate (chelsa 2021-2100 ssp585) ###
############################################################
Chelsa_futData <- readRDS("C:/Users/nemer/Project/SEEDFOR-Project/SDM/Chelsa/Chelsa_Futureclim_Arnaud/ssp585/Future_rast_combi.rds")



# Extract the existing names
existing_names <- names(Chelsa_futData)
# Generate new names
new_names <- paste0("bio", c(1:19))
# Assign new names to the RasterBrick object
names(Chelsa_futData) <- new_names

## Filter for specefic bioclimatic variables  
FutChelsa_bio_filtered<-Chelsa_futData[[c(1, 5, 6, 12, 16, 17)]]

boundary_wgs84_futchelsa <- st_transform(france_metropolitan, st_crs(FutChelsa_bio_filtered))
# Crop all layers in the raster stack 'wcbio' to the extent of France metropolitan
FutChelsabio_crop <- crop(FutChelsa_bio_filtered, vect(boundary_wgs84_futchelsa))
# Mask the cropped raster stack using the rasterized boundary
FutChelsabio_msk <- mask(FutChelsabio_crop, vect(boundary_wgs84_futchelsa))

# Resample ph_msk to match the resolution and extent of wcbio_msk
Futchelsa_ph_msk_resampled <- resample(ph_msk, FutChelsabio_msk, method = "bilinear")


Fut_ChelsaCombined_msk<-c(FutChelsabio_msk,Futchelsa_ph_msk_resampled)


# Indices of the layers i want to convert to Celsius
indices_to_convert <- c(1, 2, 3)  # Bio1, Bio5, and Bio6

# Convert specific layers from Kelvin to Celsius
for (i in indices_to_convert) {
  Fut_ChelsaCombined_msk[[i]] <- Fut_ChelsaCombined_msk[[i]] - 273.15
}



