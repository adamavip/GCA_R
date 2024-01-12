library(rgdal)
library(sf)
library(rgeos)

library(tidyverse)
library(tidyterra)
library(ggspatial)
library(RColorBrewer)
library(paletteer)
library(viridis)

library(terra)
library(exactextractr) #fast data extraction
library(raster)

setwd("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023")

#### Load vector data ####

AEZ_coffee <- sf::st_read("input/AEZs", layer = "4main_AEZ_coffee_shp", quiet = TRUE) %>%
  st_transform(4326)
AEZ_coffee_vect <- vect(AEZ_coffee)
# Load the coffee shapefile
fsrp_woreda<- sf::st_read("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/output/coffee/FSRP_Woreda_vect_coffee_socioecon.shp"
) %>%
  st_transform(4326)
fsrp_woreda_vect <- vect(fsrp_woreda)
plot(fsrp_woreda_vect)

# Load Ethiopia shapefile
Eth <- sf::st_read("Input/admin", layer = "Ethiopia_2013_UTM", quiet = TRUE) %>%
  st_transform(4326)
Eth_vect <- vect(Eth)
plot(Eth_vect)

## load dummy for resampling
r_dummy <- rast("input/AGRA_tiff_nomask/eth/historical_seasonal/precpJJASclim_EastAfr_ETH.tiff")


#### Load the annual data
Hist_Ann_prep = rast("./input/AGRA_tiff_nomask/eth/historical_annual/precp_annual_clim_EastAfr_1981T2021.nc")
Hist_Ann_prep_coffee <- crop(Hist_Ann_prep, AEZ_coffee_vect, mask=TRUE) 
Fut_Ann_prep_coffee = rast("./input/AGRA_tiff_nomask/eth/future_annual/precp_model_annual_clim_EastAfr.nc")
Fut_Ann_prep_coffee <- crop(Fut_Ann_prep, AEZ_coffee_vect, mask=TRUE) 
Fut_Ann_prep_coffee <- resample(Fut_Ann_prep_coffee,Hist_Ann_prep_coffee)

# Compute annual changes
changes_ann_prep_coffee <- Fut_Ann_prep_coffee - Hist_Ann_prep_coffee
plot(changes_ann_prep_coffee)




#### load & prepare data the seasonal data ####

# Historical Belg
Hist_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "MAM", full.names = TRUE)
Hist_Belg <-rast(Hist_Belg_list) #stack into one dataset
crs(Hist_Belg)  <- "epsg:4326"
names(Hist_Belg) <- c('precip_clim_MAM', 'precip_cv_MAM', 'tmax_clim_MAM', 'tmin_clim_MAM')

# subset & mask imagery to shape polygons
Hist_Belg_coffee <- resample(Hist_Belg, r_dummy) #downsampling to climate pixels
Hist_Belg_coffee <- crop(Hist_Belg_coffee, AEZ_coffee_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Belg_coffee)


# Future Belg
Fut_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "MAM", full.names = TRUE)
Fut_Belg <-rast(Fut_Belg_list) #stack into one dataset
crs(Fut_Belg)  <- "epsg:4326"
names(Fut_Belg) <- c('precip_clim_MAM_ssp585', 'precip_cv_MAM_ssp585', 'tmax_clim_MAM_ssp585', 'tmax_cv_MAM_ssp585', 'tmin_clim_MAM_ssp585', 'tmin_cv_MAM_ssp585')

# subset & mask imagery to shape polygons
Fut_Belg_coffee <- resample(Fut_Belg, r_dummy) #downsampling to climate pixels
Fut_Belg_coffee <- crop(Fut_Belg_coffee, AEZ_coffee_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Belg_coffee)
#writeRaster(Fut_Belg_coffee, "output/coffee/MAM/Fut_Belg_coffee.tif", overwrite=TRUE)


# Historical Meher
Hist_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "JJAS", full.names = TRUE)
Hist_Meher <-rast(Hist_Meher_list) #stack into one dataset
crs(Hist_Meher)  <- "epsg:4326"
names(Hist_Meher) <- c('precip_clim_JJAS', 'precip_cv_JJAS', 'tmax_clim_JJAS', 'tmin_clim_JJAS')

# subset & mask imagery to shape polygons
Hist_Meher_coffee <- resample(Hist_Meher, r_dummy) #downsampling to climate pixels
Hist_Meher_coffee <- crop(Hist_Meher_coffee, AEZ_coffee, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Meher_coffee)
writeRaster(Hist_Meher_coffee, "output/coffee/JJAS/Hist_Meher_coffee.tif", overwrite=TRUE)


# Future Meher
Fut_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "JJAS", full.names = TRUE)
Fut_Meher <-rast(Fut_Meher_list) #stack into one dataset
crs(Fut_Meher)  <- "epsg:4326"
names(Fut_Meher) <- c('precip_clim_JJAS_ssp585', 'precip_cv_JJAS_ssp585', 'tmax_clim_JJAS_ssp585', 'tmax_cv_JJAS_ssp585', 'tmin_clim_JJAS_ssp585', 'tmin_cv_JJAS_ssp585')

# subset & mask imagery to shape polygons
Fut_Meher_coffee <- resample(Fut_Meher, r_dummy) #downsampling to climate pixels
Fut_Meher_coffee <- crop(Fut_Meher_coffee, AEZ_coffee, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Meher_coffee)


# Historical Bega
Hist_Bega_prep_coffee <- Hist_Ann_prep_coffee - (Hist_Belg_coffee['precip_clim_MAM'] + Hist_Meher_coffee['precip_clim_JJAS'])
plot(Hist_Bega_prep_coffee)


# Future Bega
Fut_Ann_prep_coffee<- resample(Fut_Ann_prep_coffee["pr_ssp=_3"],Fut_Belg_coffee['precip_clim_MAM_ssp585'])
Fut_Bega_prep_coffee <- Fut_Ann_prep_coffee["pr_ssp=_3"] - (Fut_Belg_coffee['precip_clim_MAM_ssp585'] + Fut_Meher_coffee['precip_clim_JJAS_ssp585'])
plot(Fut_Bega_prep_coffee)

# Bega changes
Bega_prep_changes = Fut_Bega_prep_coffee - Hist_Bega_prep_coffee
plot(Bega_prep_changes)
