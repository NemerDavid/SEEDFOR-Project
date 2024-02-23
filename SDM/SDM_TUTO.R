library(tidyverse)
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

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


wacascades <- st_read("C:/Users/David/Desktop/Important doucumetns/Datacamp/R/Species distribution models/SDM 4/Full Data/Chapter12/wacascades.shp", quiet = TRUE)


abla <- read_csv("C:/Users/David/Desktop/Important doucumetns/Datacamp/R/Species distribution models/SDM 4/Full Data/Chapter12/abla.csv") # subalpine fir (Abieslasiocarpa)
psme <- read_csv("C:/Users/David/Desktop/Important doucumetns/Datacamp/R/Species distribution models/SDM 4/Full Data/Chapter12/psme.csv") # Douglas-fir (Pseudotsuga menziesii)

head(abla)
head(psme)




##########################
### 12.1 Tree Species Data
##########################


# These data frames are converted into sf objects and assigned the correct
# coordinate system (geographic with NAD83 datum) #
abla_pts <- st_as_sf(abla,
                     coords = c("X", "Y"))
st_crs(abla_pts) <- 4326

class(abla_pts)


psme_pts <- st_as_sf(psme,
                     coords = c("X", "Y"))
st_crs(psme_pts) <- 4326

# The map of presence-absence points shows that subalpine fir is restricted to relatively high elevations near the Cascade crest

### subalpine fir

ggplot() +
  geom_sf(data = abla_pts,
          aes(color = as.character(abla)),
          size = 0.25) +
  geom_sf(data = wacascades,
          fill = NA) +
  scale_color_manual(name = "Subalpine Fir",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present")) +
  annotation_scale(location = 'br') +
  theme_void()



#### Douglas-fir
ggplot() +
  geom_sf(data = psme_pts,
          aes(color = as.character(psme)),
          size = 0.25) +
  geom_sf(data = wacascades,
          fill = NA) +
  scale_color_manual(name = "Douglas-fir",
                     values = c("gray", "darkgreen"),
                     labels = c("Absent", "Present")) +
  annotation_scale(location = 'br') +
  theme_void()


# compute kernel-weighted local means #

boundary_utm <- st_transform(wacascades, 32610)
abla_utm <- st_transform(abla_pts, 32610)
psme_utm <- st_transform(psme_pts, 32610)

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
                      sigma=10000,
                      eps=c(1000, 1000))
class(abla_im)

abla_grid <- rast(abla_im)
crs(abla_grid) <-"epsg:32610"
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


# The same procedure is repeated for Douglas-fir to generate a smoothed raster of kernel-weighted means.
psme_ppp <- as.ppp(psme_utm)
Window(psme_ppp) <- as.owin(boundary_utm)
psme_im <- Smooth.ppp(psme_ppp,
                      sigma=10000,
                      eps=c(1000, 1000))


psme_grid <- rast(psme_im)
crs(psme_grid) <-"epsg:32610"



# Convert SpatRaster to a raster object from the raster package
psme_raster <- raster(psme_grid)

# Extract values and coordinates from the raster
psme_df <- rasterToPoints(psme_raster)
psme_df <- data.frame(psme_df)



ggplot(data = psme_df) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = lyr.1)) +
  scale_fill_gradient(name = "Douglas-fir",
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

wcbio <- rast("C:/Users/David/Desktop/Important doucumetns/Datacamp/R/Species distribution models/SDM 4/Full Data/Chapter12/wc2.1_30s_bio_washington.tif")

nlyr(wcbio)
names(wcbio)
class(wcbio)

# The default layer names are replaced by a set of abbreviated codes.

wcbnames <- paste0("bio", c(1, 10:19, 2:9))
names(wcbio) <- wcbnames
wcbnames


### The Washington Cascades ecoregion boundaries are reprojected into a geographic coordinate system with WGS84
### datum to match the coordinate reference system of the plot data and the WorldClim data.
### These polygons are then used to crop and mask the Washington Cascades from the larger Worldclim dataset.

boundary_wgs84 <- st_transform(wacascades, st_crs(wcbio))
wcbio_crop <- crop(wcbio, vect(boundary_wgs84))
wcbio_msk <- mask(wcbio_crop, vect(boundary_wgs84))
class(wcbio_msk)

### The map of maximum temperature during the warmest month of the year highlights the effects of elevation,
### with the highest temperatures at the fringes of the mountain range and in the larger river valleys.
### The lowest temperatures occur along the Cascade crest and at the peaks of the large volcanoes


# Convert SpatRaster to a raster object from the raster package
mtwm_raster <- raster(wcbio_msk[["bio5"]])
class(mtwm_raster)
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




# The map of precipitation during the wettest month shows the interactions of moist maritime air with topography,
# with the highest values occurring west of the Cascade Crest and the lowest values in the rain shadow on the eastern slopes


# Convert SpatRaster to a raster object from the raster package
mtwm_raster <- raster(wcbio_msk[["bio13"]])

# Extract values and coordinates from the raster
mtwm_df <- rasterToPoints(mtwm_raster)
mtwm_df <- data.frame(mtwm_df)



ggplot(data = mtwm_df) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = bio13)) +
  scale_fill_gradient(name = "Precipitation (mm)",
                      low = "lightblue",
                      high = "darkblue",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()




####################################
#### 12.3 Modeling the Climate Niche
####################################

#### 12.3.1 Subalpine Fir #####
###############################
abla_bio <- terra::extract(wcbio_msk, vect(abla_pts)) %>%
  bind_cols(abla_pts) %>%
  as.data.frame()

abla_bio <- terra::extract(wcbio_msk, vect(abla_pts)) %>%
  bind_cols(st_coordinates(abla_pts) %>%
              as.data.frame())


names(abla_bio)
str(abla_bio)



set.seed(22003)
abla_train <- abla_bio %>%
  sample_frac(size = 0.7)
abla_val <- abla_bio %>%
  anti_join(abla_train, by = "ID")

str(abla_train)


abla_mod <- gbm.step(data = abla_train,
                     gbm.x = 2:20,
                     gbm.y = 21,
                     family = "bernoulli",
                     tree.complexity = 3,
                     learning.rate = 0.01,
                     bag.fraction = 0.5,
                     plot.main = FALSE,
                     verbose = FALSE,
                     silent = TRUE)


abla_imp <- summary(abla_mod, plotit = FALSE)
abla_imp

gbm.plot(abla_mod,
         n.plots = 4,
         write.title = FALSE,
         plot.layout = c(2, 2))


abla_cur <- predict(object = wcbio_msk,
                    model = abla_mod,
                    type = "response",
                    na.rm = TRUE)

class(abla_cur)

abla_cur_df <- rasterdf(abla_cur)

# Convert SpatRaster to a raster object from the raster package
abla_cur_raster <- raster(abla_cur)

# Extract values and coordinates from the raster
abla_cur_df <- rasterToPoints(abla_cur_raster)
abla_cur_df <- data.frame(abla_cur_df)




ggplot(data = abla_cur_df) +
  geom_raster(aes(x = x,
                  y = y,
                  fill = lyr1)) +
  scale_fill_gradient(name = "Subalpine Fir",
                      low = "lightyellow",
                      high = "darkgreen",
                      na.value = NA) +
  geom_sf(data = boundary_wgs84,
          fill = NA) +
  annotation_scale(location = 'br') +
  coord_sf(expand = F) +
  theme_void()



############################
### 12.4 Accuracy Assessment
############################
abla_pred <- predict(abla_mod,
                     newdata = abla_val,
                     type = "response")

abla_predobs <- prediction(abla_pred, abla_val$abla)

abla_roc = performance(abla_predobs,
                       measure = "tpr",
                       x.measure = "fpr")
class(abla_roc)
slotNames(abla_roc)


abla_fpr <- slot(abla_roc, "x.values")[[1]]
abla_tpr <- slot(abla_roc, "y.values")[[1]]
abla_aucplot <- data.frame(abla_fpr, abla_tpr)



ggplot(data = abla_aucplot) +
  geom_line(aes(x = abla_fpr,
                y = abla_tpr),
            col = "red") +
  labs(x = "False Positive Rate",
       y = "True Positive Rate") +
  geom_abline(slope = 1, intercept = 0) +
  scale_x_continuous(expand = c(0.005, 0)) +
  scale_y_continuous(expand = c(0.005, 0)) +
  coord_fixed() +
  theme_bw()