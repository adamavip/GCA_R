
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
# library(gdalUtilities)

setwd("D:/R/GCA_2023")
gc()
terraOptions()
terraOptions(chunksize = 1e+09)
terraOptions(maxmemory = 1e+09)




####### Load Data ####### 

## load shapefiles and transform to WGS84

# country .shp
Eth <- sf::st_read("Input/admin", layer = "Ethiopia_2013_UTM", quiet = TRUE) %>%
  st_transform(4326)
Eth_vect <- vect(Eth)

# regions .shp
Eth_regions <- sf::st_read("Input/admin", layer = "Eth_Region_2013_UTM", quiet = TRUE) %>%
  st_transform(4326) %>%
  dplyr::select(REGIONNAME) %>% 
  dplyr::mutate(REGIONNAME = str_replace(REGIONNAME, "/", " "))
Eth_vect_reg <- vect(Eth_regions)


# Belg mask
Belg_mask <- sf::st_read("Input/AEZs", layer = "Belg_mask", quiet = TRUE) %>%
  st_transform(4326) #%>%
#dplyr::select(MAJORAEZNA) 
Belg_mask_vect <- vect(Belg_mask)


# Woreda for Belg cattle .shp (created in QGIS)
Woreda_cattle <- sf::st_read("Input/admin", layer = "Eth_Woreda_2021_cattle", quiet = TRUE) %>%
  st_transform(4326) #%>%
#dplyr::select(MAJORAEZNA) 
Woreda_cattle_vect <- vect(Woreda_cattle)


# AEZs .shp (agro-ecological zones)
AEZ <- sf::st_read("Input/AEZs", layer = "Cropaez_region_UTM", quiet = TRUE) %>%
  st_transform(4326) #%>%
  #dplyr::select(MAJORAEZNA) 
AEZ_vect <- vect(AEZ)

# FSRP Woredas
FSRP_Woreda <- sf::st_read("Input/FSRP_woredas", layer = "FSRP_project_woredas_final", quiet = TRUE) %>%
  st_transform(4326) #%>%
#dplyr::select(MAJORAEZNA) 
FSRP_Woreda_vect <- vect(FSRP_Woreda)


## load dummy for resampling
r_dummy <- rast("input/AGRA_tiff_nomask/eth/historical_seasonal/precpJJASclim_EastAfr_ETH.tiff")



#########################################
##### Data processing & exploration #####
#########################################


####### MapSPAM ####### 

## load MapSPAM
cattle_list <- list.files("input/MapSpam/", pattern = "cattle_density_map", full.names = TRUE)
MapSPAM_cattle <-rast(cattle_list) #stack into one dataset
names(MapSPAM_cattle) <- 'cattle density'

## subset & mask imagery to shape polygons
# Ethiopia
MapSPAM_cattle_Eth <- crop(MapSPAM_cattle, Eth_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
#MapSPAM_cattle_Eth <- resample(MapSPAM_cattle_Eth, r_dummy) #downsampling to climate pixels
summary(MapSPAM_cattle_Eth)
plot(MapSPAM_cattle_Eth)
writeRaster(MapSPAM_cattle_Eth, "output/cattle/MapSPAM_cattle_Eth.tif", overwrite=TRUE)

## zonal stats
# per country
Zonal_MapSPAM_cattle_Eth_min <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(Eth_vect), 'min', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_max <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(Eth_vect), 'max', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_mean <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(Eth_vect), 'mean', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_sd <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(Eth_vect), 'stdev', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_sum <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(Eth_vect), 'sum', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_cattle_Eth <- cbind(Zonal_MapSPAM_cattle_Eth_min, Zonal_MapSPAM_cattle_Eth_max, Zonal_MapSPAM_cattle_Eth_mean, Zonal_MapSPAM_cattle_Eth_sd, Zonal_MapSPAM_cattle_Eth_sum)
write.csv(Zonal_MapSPAM_cattle_Eth, "output/cattle/Zonal_MapSPAM_cattle_Eth.csv")

# per AEZ (entire country)
Zonal_MapSPAM_cattle_Eth_AEZ_min <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(AEZ_vect), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_max <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(AEZ_vect), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_mean <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(AEZ_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_sd <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(AEZ_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_sum <- exact_extract(MapSPAM_cattle_Eth, st_as_sf(AEZ_vect), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_cattle_Eth_AEZ <- cbind(Zonal_MapSPAM_cattle_Eth_AEZ_min, Zonal_MapSPAM_cattle_Eth_AEZ_max, Zonal_MapSPAM_cattle_Eth_AEZ_mean, Zonal_MapSPAM_cattle_Eth_AEZ_sd, Zonal_MapSPAM_cattle_Eth_AEZ_sum)
write.csv(Zonal_MapSPAM_cattle_Eth_AEZ, "output/cattle/Zonal_MapSPAM_cattle_Eth_AEZ.csv")



####### Crop Mask ####### 

## create binary raster crop mask (physical area)
threshold <- 5000
MapSPAM_cattle_Eth_mask <- MapSPAM_cattle_Eth$`cattle density` > threshold
NAflag(MapSPAM_cattle_Eth_mask) <- 0
plot(MapSPAM_cattle_Eth_mask)
writeRaster(MapSPAM_cattle_Eth_mask, "output/cattle/MapSPAM_cattle_Eth_mask.tif", overwrite=TRUE)

MapSPAM_cattle_Eth_mask_poly <- rasterToPolygons(stack(MapSPAM_cattle_Eth_mask))
MapSPAM_cattle_Eth_mask_poly <- aggregate(MapSPAM_cattle_Eth_mask_poly, dissolve=T)
plot(MapSPAM_cattle_Eth_mask_poly)
shapefile(MapSPAM_cattle_Eth_mask_poly, filename='output/cattle/MapSPAM_cattle_Eth_mask_poly.shp', overwrite=TRUE)

MapSPAM_cattle_Eth_mask_poly <- vect(MapSPAM_cattle_Eth_mask_poly)
crs(MapSPAM_cattle_Eth_mask_poly) <- "epsg:4326"
# MapSPAM_cattle_Eth_mask_poly_c <- union(MapSPAM_cattle_Eth_mask_poly, Woreda_cattle_vect) # union with additional cattle bean Woredas
# plot(MapSPAM_cattle_Eth_mask_poly_c)
MapSPAM_cattle_Eth_plots <- crop(MapSPAM_cattle_Eth, MapSPAM_cattle_Eth_mask_poly, mask=T)


## AEZ for cattle
AEZ_cattle <- crop(AEZ_vect, MapSPAM_cattle_Eth_mask_poly) # spatial subset + mask = clip to extent + mask out all pixels raster
AEZ_cattle_Belg <- crop(AEZ_vect, MapSPAM_cattle_Eth_mask_poly) # spatial subset + mask = clip to extent + mask out all pixels raster
AEZ_cattle_Belg_pot <- crop(AEZ_cattle, MapSPAM_cattle_Eth_mask_poly) # spatial subset + mask = clip to extent + mask out all pixels raster


## select FSRP Woredas within crop mask
FSRP_Woreda_vect_cattle <- intersect(FSRP_Woreda_vect, AEZ_cattle)
plot(FSRP_Woreda_vect_cattle)
FSRP_Woreda_vect_cattle <- FSRP_Woreda_vect_cattle %>%
  dplyr::select(c("ADM3_EN", "ADM2_EN", "ADM1_EN", "ADM0_EN", "AEZ98_ID", "MAEZ")) %>%
  dplyr::mutate(ID = 1:n()) #%>%
# dplyr::mutate(geom = geom(FSRP_Woreda_vect_cattle, wkt=TRUE))
writeVector(FSRP_Woreda_vect_cattle, filename='output/cattle/FSRP_Woreda_vect_cattle.shp', overwrite=TRUE)

FSRP_Woreda_vect_cattle_Belg_pot <- intersect(FSRP_Woreda_vect, AEZ_cattle_Belg_pot)
plot(FSRP_Woreda_vect_cattle_Belg_pot)
FSRP_Woreda_vect_cattle_Belg <- intersect(FSRP_Woreda_vect, AEZ_cattle_Belg)
plot(FSRP_Woreda_vect_cattle_Belg)


## ggplot template
ggplot_template_JJAS <-
  list(geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey"),
       geom_spatvector(data = FSRP_Woreda_vect_cattle, fill = NA, color = "brown"),
       geom_spatvector(data = Eth_vect, fill = NA, color = "black"),
       theme_bw(),
       theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"), legend.position = c(.86, .5)),
       annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")), 
       annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
  )

ggplot_template_MAM <-
  list(geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey"),
       geom_spatvector(data = FSRP_Woreda_vect_cattle_Belg, fill = NA, color = "brown"),
       geom_spatvector(data = Eth_vect, fill = NA, color = "black"),
       theme_bw(),
       theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"), legend.position = c(.86, .5)),
       annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")), 
       annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
  )


## create figures "cattle density"
p <- ggplot() +
  geom_spatraster(data = MapSPAM_cattle_Eth_plots$`cattle density`) +
    scale_fill_whitebox_c(
      palette = "muted",
      breaks = seq(10000, 60000, 10000),
      limits=c(5000,65000)) +
    labs(fill = "[cattle/km2]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=("output/cattle/cattle_MapSPAM_cattle_density.png"), width = 4, height = 3, dpi = 300)




####### AEZ per VC ####### 

cols <- c("WB"="#000000",
          "A1"="#fef0d9", "SA1"="#fed8a4", "SM1"="#fdb77a", "M1"="#fc8d59", "SH1"="#ec603f", "H1"="#d33122", "PH1"="#b30000",
          "A2"="#ffffcc", "SA2"="#d7efaa", "SM2"="#a9dc8e", "M2"="#78c679", "SH2"="#48af60", "H2"="#208f4a", "PH2"="#006837",
          "SM3"="#e8f1fa", "M3"="#b0d2e8", "SH3"="#3e8ec4", "H3"="#08306b") # define colors for AEZ maps


p <- ggplot() +
  geom_spatvector(data = AEZ_cattle, aes(fill = MAEZ), color = NA) +
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") + 
  geom_spatvector(data = FSRP_Woreda_vect_cattle, fill = NA, color = "brown") +
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") +
  scale_fill_manual(values = cols, breaks=c('A1', 'SA1', 'SM1', 'M1', 'SH1', 'H1', 'PH1',
                                            'A2', 'SA2', 'SM2', 'M2', 'SH2', 'H2', 'PH2',
                                            'SM3', 'M3', 'SH3', 'H3', 'WB')) +
  labs(fill = "AEZ") +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.84, .4),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=("output/cattle/cattle_AEZ.png"), width = 4, height = 3, dpi = 300)


p <- ggplot() +
  geom_spatvector(data = AEZ_cattle_Belg_pot, aes(fill = MAEZ), color = NA) + 
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") + 
  geom_spatvector(data = FSRP_Woreda_vect_cattle_Belg_pot, fill = NA, color = "brown") +
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") +
  scale_fill_manual(values = cols, breaks=c('A1', 'SA1', 'SM1', 'M1', 'SH1', 'H1', 'PH1',
                                            'A2', 'SA2', 'SM2', 'M2', 'SH2', 'H2', 'PH2',
                                            'SM3', 'M3', 'SH3', 'H3', 'WB')) +
  labs(fill = "AEZ") +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.84, .4),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=("output/cattle/cattle_Belg_AEZ.png"), width = 4, height = 3, dpi = 300)



# per AEZ both seasons
Zonal_MapSPAM_cattle_Eth_AEZ_min <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_max <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_mean <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_sd <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_sum <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_cattle_Eth_AEZ <- cbind(Zonal_MapSPAM_cattle_Eth_AEZ_min, Zonal_MapSPAM_cattle_Eth_AEZ_max, Zonal_MapSPAM_cattle_Eth_AEZ_mean, Zonal_MapSPAM_cattle_Eth_AEZ_sd, Zonal_MapSPAM_cattle_Eth_AEZ_sum)
write.csv(Zonal_MapSPAM_cattle_Eth_AEZ, "output/cattle/Zonal_MapSPAM_cattle_Eth_AEZ_masked.csv")

writeVector(AEZ_cattle, filename='output/cattle/AEZ_cattle_shp.shp', overwrite=TRUE)


# AEZ_cattle <- sf::st_read("input/AEZs", layer = "AEZ_cattle_shp_majorAEZ", quiet = TRUE) %>%
#   st_transform(4326)
AEZ_cattle_vect <- AEZ_cattle


# per AEZ Belg cattle
Zonal_MapSPAM_cattle_Eth_AEZ_Belg_min <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_Belg_max <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_Belg_mean <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_Belg_sd <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_cattle_Eth_AEZ_Belg_sum <- exact_extract(MapSPAM_cattle_Eth_plots, st_as_sf(AEZ_cattle_Belg), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_cattle_Eth_AEZ_Belg <- cbind(Zonal_MapSPAM_cattle_Eth_AEZ_Belg_min, Zonal_MapSPAM_cattle_Eth_AEZ_Belg_max, Zonal_MapSPAM_cattle_Eth_AEZ_Belg_mean, Zonal_MapSPAM_cattle_Eth_AEZ_Belg_sd, Zonal_MapSPAM_cattle_Eth_AEZ_Belg_sum)
write.csv(Zonal_MapSPAM_cattle_Eth_AEZ_Belg, "output/cattle/Zonal_MapSPAM_cattle_Eth_AEZ_Belg_masked.csv")

writeVector(AEZ_cattle_Belg, filename='output/cattle/AEZ_cattle_Belg_shp.shp', overwrite=TRUE)


# AEZ_cattle_Belg <- sf::st_read("input/AEZs", layer = "AEZ_cattle_Belg_shp_majorAEZ", quiet = TRUE) %>%
#   st_transform(4326)
# AEZ_cattle_vect <- vect(AEZ_cattle_Belg)


# socio-economics per cattle AEZ 

#cattle density
se_cattle_density <- rast("input/socioeconomics/cattle_density_map.tif")
se_cattle_density <- crop(se_cattle_density, MapSPAM_cattle_Eth_mask_poly, mask=T)
se_cattle_density_AEZ_mean <- exact_extract(se_cattle_density, st_as_sf(FSRP_Woreda_vect_cattle), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_cattle_density_AEZ_mean <- se_cattle_density_AEZ_mean %>% tidyterra::rename(cattle_density_AEZ_mean = mean)

#health
se_health <- rast("input/socioeconomics/cell5m_Demographics_HealthandNutrition_RSTUN_M.tif")
se_health <- crop(se_health, MapSPAM_cattle_Eth_mask_poly, mask=T)
se_health_AEZ_mean <- exact_extract(se_health, st_as_sf(FSRP_Woreda_vect_cattle), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_health_AEZ_mean <- se_health_AEZ_mean %>% tidyterra::rename(health_AEZ_mean = mean)

#population density
se_pop <- rast("input/socioeconomics/ppp_2020_1km_Aggregated.tif")
se_pop <- crop(se_pop, MapSPAM_cattle_Eth_mask_poly, mask=T)
se_pop_AEZ_mean <- exact_extract(se_pop, st_as_sf(FSRP_Woreda_vect_cattle), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_pop_AEZ_mean <- se_pop_AEZ_mean %>% tidyterra::rename(pop_AEZ_mean = mean)

#travel distance to market
se_travel <- rast("input/socioeconomics/traveltimetomarket_ssa_020k.tif")
se_travel <- crop(se_travel, MapSPAM_cattle_Eth_mask_poly, mask=T)
se_travel_AEZ_mean <- exact_extract(se_travel, st_as_sf(FSRP_Woreda_vect_cattle), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_travel_AEZ_mean <- se_travel_AEZ_mean %>% tidyterra::rename(travel_AEZ_mean = mean)

#rural poverty headcount ratio
se_rpv08 <- rast("input/socioeconomics/RPV08_PT19.tif")
se_rpv08 <- crop(se_rpv08, MapSPAM_cattle_Eth_mask_poly, mask=T)
se_rpv08_AEZ_mean <- exact_extract(se_rpv08, st_as_sf(FSRP_Woreda_vect_cattle), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_rpv08_AEZ_mean <- se_rpv08_AEZ_mean %>% tidyterra::rename(rpv08_AEZ_mean = mean)

FSRP_Woreda_vect_cattle_socioecon_means <- c(se_cattle_density_AEZ_mean, se_health_AEZ_mean, se_pop_AEZ_mean, se_travel_AEZ_mean, se_rpv08_AEZ_mean)
FSRP_Woreda_vect_cattle_socioecon_means <- data.frame(FSRP_Woreda_vect_cattle_socioecon_means)
FSRP_Woreda_vect_cattle_socioecon <- merge(FSRP_Woreda_vect_cattle, FSRP_Woreda_vect_cattle_socioecon_means, by.x="ID", by.y="ID")

writeVector(FSRP_Woreda_vect_cattle_socioecon, filename='output/cattle/FSRP_Woreda_vect_cattle_socioecon.shp', overwrite=TRUE)




####### Climate Data Processing/Analysis ####### 


#### load & prepare data ####

# Historical Belg
Hist_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "MAM", full.names = TRUE)
Hist_Belg <-rast(Hist_Belg_list) #stack into one dataset
crs(Hist_Belg)  <- "epsg:4326"
names(Hist_Belg) <- c('precip_clim_MAM', 'precip_cv_MAM', 'tmax_clim_MAM', 'tmin_clim_MAM')

# subset & mask imagery to shape polygons
Hist_Belg_cattle <- resample(Hist_Belg, r_dummy) #downsampling to climate pixels
Hist_Belg_cattle <- crop(Hist_Belg_cattle, AEZ_cattle_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Belg_cattle)
writeRaster(Hist_Belg_cattle, "output/cattle/MAM/Hist_Belg_cattle.tif", overwrite=TRUE)


# Future Belg
Fut_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "MAM", full.names = TRUE)
Fut_Belg <-rast(Fut_Belg_list) #stack into one dataset
crs(Fut_Belg)  <- "epsg:4326"
names(Fut_Belg) <- c('precip_clim_MAM_ssp585', 'precip_cv_MAM_ssp585', 'tmax_clim_MAM_ssp585', 'tmax_cv_MAM_ssp585', 'tmin_clim_MAM_ssp585', 'tmin_cv_MAM_ssp585')

# subset & mask imagery to shape polygons
Fut_Belg_cattle <- resample(Fut_Belg, r_dummy) #downsampling to climate pixels
Fut_Belg_cattle <- crop(Fut_Belg_cattle, AEZ_cattle_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Belg_cattle)
writeRaster(Fut_Belg_cattle, "output/cattle/MAM/Fut_Belg_cattle.tif", overwrite=TRUE)


# Historical Meher
Hist_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "JJAS", full.names = TRUE)
Hist_Meher <-rast(Hist_Meher_list) #stack into one dataset
crs(Hist_Meher)  <- "epsg:4326"
names(Hist_Meher) <- c('precip_clim_JJAS', 'precip_cv_JJAS', 'tmax_clim_JJAS', 'tmin_clim_JJAS')

# subset & mask imagery to shape polygons
Hist_Meher_cattle <- resample(Hist_Meher, r_dummy) #downsampling to climate pixels
Hist_Meher_cattle <- crop(Hist_Meher_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Meher_cattle)
writeRaster(Hist_Meher_cattle, "output/cattle/JJAS/Hist_Meher_cattle.tif", overwrite=TRUE)


# Future Meher
Fut_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "JJAS", full.names = TRUE)
Fut_Meher <-rast(Fut_Meher_list) #stack into one dataset
crs(Fut_Meher)  <- "epsg:4326"
names(Fut_Meher) <- c('precip_clim_JJAS_ssp585', 'precip_cv_JJAS_ssp585', 'tmax_clim_JJAS_ssp585', 'tmax_cv_JJAS_ssp585', 'tmin_clim_JJAS_ssp585', 'tmin_cv_JJAS_ssp585')

# subset & mask imagery to shape polygons
Fut_Meher_cattle <- resample(Fut_Meher, r_dummy) #downsampling to climate pixels
Fut_Meher_cattle <- crop(Fut_Meher_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Meher_cattle)
writeRaster(Fut_Meher_cattle, "output/cattle/JJAS/Fut_Meher_cattle.tif", overwrite=TRUE)


# Historical Extremes
Hist_Extreme_list1 <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_1", pattern = ".tif", full.names = TRUE)
Hist_Extreme1 <-rast(Hist_Extreme_list1) #stack into one dataset
crs(Hist_Extreme1)  <- "epsg:4326"
names(Hist_Extreme1) <- c('r01', 'r10', 'r20', 'sdii', 'tnlt4', 'tr')

Hist_Extreme_list2 <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_2", pattern = ".tif", full.names = TRUE)
Hist_Extreme2 <-rast(Hist_Extreme_list2) #stack into one dataset
crs(Hist_Extreme2)  <- "epsg:4326"
names(Hist_Extreme2) <- c('dtr', 'txge35')

# subset & mask imagery to shape polygons
Hist_Extreme1_cattle <- resample(Hist_Extreme1, r_dummy) #downsampling to climate pixels
Hist_Extreme1_cattle <- crop(Hist_Extreme1_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Hist_Extreme2_cattle <- resample(Hist_Extreme2, r_dummy) #downsampling to climate pixels
Hist_Extreme2_cattle <- crop(Hist_Extreme2_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Hist_Extreme_cattle <- c(Hist_Extreme1_cattle, Hist_Extreme2_cattle)
plot(Hist_Extreme_cattle)
writeRaster(Hist_Extreme_cattle, "output/cattle/extremes/Hist_Extreme_cattle.tif", overwrite=TRUE)


# Future Extremes
Fut_Extreme_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes", pattern = ".tif", full.names = TRUE)
Fut_Extreme <-rast(Fut_Extreme_list) #stack into one dataset
crs(Fut_Extreme)  <- "epsg:4326"
names(Fut_Extreme) <- c('dtr_ssp585', 'r01_ssp585', 'r10_ssp585', 'r20_ssp585', 'sdii_ssp585', 'tr_ssp585', 'txge35_ssp585')

# subset & mask imagery to shape polygons
Fut_Extreme_cattle <- resample(Fut_Extreme, r_dummy) #downsampling to climate pixels
Fut_Extreme_cattle <- crop(Fut_Extreme_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Extreme_cattle)
writeRaster(Fut_Extreme_cattle, "output/cattle/extremes/Fut_Extreme_cattle.tif", overwrite=TRUE)


# Changes
Changes_list <- list.files("input/AGRA_tiff_nomask/eth/change", pattern = ".nc", full.names = TRUE)
Changes1 <-rast(Changes_list) #stack into one dataset
crs(Changes1)  <- "epsg:4326"
names(Changes1) <- c('cdd_JJAS_change_ssp245', 'cdd_JJAS_change_ssp585', 'cdd_MAM_change_ssp245', 'cdd_MAM_change_ssp585',
                    'cwd_JJAS_change_ssp245', 'cwd_JJAS_change_ssp585', 'cwd_MAM_change_ssp245', 'cwd_MAM_change_ssp585',
                    'dtr_ann_change_ssp245', 'dtr_ann_change_ssp585',
                    'precip_clim_JJAS_change_ssp245', 'precip_clim_JJAS_change_ssp585',
                    'precip_clim_MAM_change_ssp245', 'precip_clim_MAM_change_ssp585',
                    'r01_ann_change_ssp245', 'r01_ann_change_ssp585', 'r10_ann_change_ssp245', 'r10_ann_change_ssp585',
                    'r20_ann_change_ssp245', 'r20_ann_change_ssp585', 'sdii_ann_change_ssp245', 'sdii_ann_change_ssp585',
                    'tmax_JJAS_change_ssp245', 'tmax_JJAS_change_ssp585', 'tmax_MAM_change_ssp245', 'tmax_MAM_change_ssp585',
                    'tmin_JJAS_change_ssp245', 'tmin_JJAS_change_ssp585', 'tmin_MAM_change_ssp245', 'tmin_MAM_change_ssp585',
                    'tr_ann_change_ssp245', 'tr_ann_change_ssp585')

Changes2 <- rast("input/AGRA_tiff_nomask/eth/change2/txege35_model_annual_change_EastAfr.nc")
crs(Changes2)  <- "epsg:4326"
names(Changes2) <- c('txge35_ann_change_ssp245', 'txge35_ann_change_ssp585')

# subset & mask imagery to shape polygons
Changes1_cattle <- resample(Changes1, r_dummy) #downsampling to climate pixels
Changes1_cattle <- crop(Changes1_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Changes2_cattle <- resample(Changes2, r_dummy) #downsampling to climate pixels
Changes2_cattle <- crop(Changes2_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Changes_cattle <- c(Changes1_cattle, Changes2_cattle)
plot(Changes_cattle)
writeRaster(Changes_cattle, "output/cattle/changes_cattle.tif", overwrite=TRUE)

Changes_cattle_MAM_ssp585 <- Changes_cattle["MAM_change_ssp585"]
Changes_cattle_MAM_ssp585 <- crop(Changes_cattle_MAM_ssp585, AEZ_cattle_Belg, mask=TRUE)
plot(Changes_cattle_MAM_ssp585)

Changes_cattle_JJAS_ssp585 <- Changes_cattle["JJAS_change_ssp585"]
plot(Changes_cattle_JJAS_ssp585)

Changes_cattle_extr_ssp585 <- Changes_cattle["ann_change_ssp585"]
plot(Changes_cattle_extr_ssp585)


# CDD & CWD - Belg
Hist_CDD_CDW_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_nc", pattern = "MAM", full.names = TRUE)
Hist_CDD_CDW_Belg <-rast(Hist_CDD_CDW_Belg_list) #stack into one dataset
crs(Hist_CDD_CDW_Belg)  <- "epsg:4326"
names(Hist_CDD_CDW_Belg) <- c('cdd_MAM', 'cwd_MAM')

# subset & mask imagery to shape polygons
Hist_CDD_CDW_Belg_cattle <- resample(Hist_CDD_CDW_Belg, r_dummy) #downsampling to climate pixels
Hist_CDD_CDW_Belg_cattle <- crop(Hist_CDD_CDW_Belg_cattle, AEZ_cattle_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_CDD_CDW_Belg_cattle)
writeRaster(Hist_CDD_CDW_Belg_cattle, "output/cattle/MAM/Hist_CDD_CDW_Belg_cattle.tif", overwrite=TRUE)


Fut_CDD_CDW_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes_nc", pattern = "mam", full.names = TRUE)
Fut_CDD_CDW_Belg <-rast(Fut_CDD_CDW_Belg_list) #stack into one dataset
crs(Fut_CDD_CDW_Belg)  <- "epsg:4326"
names(Fut_CDD_CDW_Belg) <- c('cdd_MAM_ssp245', 'cdd_MAM_ssp585', 'cdd_MAM_ssp???','cwd_MAM_ssp245', 'cwd_MAM_ssp585', 'cdd_MAM_ssp???')

# subset & mask imagery to shape polygons
Fut_CDD_CDW_Belg_cattle <- resample(Fut_CDD_CDW_Belg, r_dummy) #downsampling to climate pixels
Fut_CDD_CDW_Belg_cattle <- crop(Fut_CDD_CDW_Belg_cattle, AEZ_cattle_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_CDD_CDW_Belg_cattle)
writeRaster(Fut_CDD_CDW_Belg_cattle, "output/cattle/MAM/Fut_CDD_CDW_Belg_cattle.tif", overwrite=TRUE)


# CDD & CWD - Meher
Hist_CDD_CDW_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_nc", pattern = "JJAS", full.names = TRUE)
Hist_CDD_CDW_Meher <-rast(Hist_CDD_CDW_Meher_list) #stack into one dataset
crs(Hist_CDD_CDW_Meher)  <- "epsg:4326"
names(Hist_CDD_CDW_Meher) <- c('cdd_JJAS', 'cwd_JJAS')

# subset & mask imagery to shape polygons
Hist_CDD_CDW_Meher_cattle <- resample(Hist_CDD_CDW_Meher, r_dummy) #downsampling to climate pixels
Hist_CDD_CDW_Meher_cattle <- crop(Hist_CDD_CDW_Meher_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_CDD_CDW_Meher_cattle)
writeRaster(Hist_CDD_CDW_Meher_cattle, "output/cattle/JJAS/Hist_CDD_CDW_Meher_cattle.tif", overwrite=TRUE)


Fut_CDD_CDW_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes_nc", pattern = "jjas", full.names = TRUE)
Fut_CDD_CDW_Meher <-rast(Fut_CDD_CDW_Meher_list) #stack into one dataset
crs(Fut_CDD_CDW_Meher)  <- "epsg:4326"
names(Fut_CDD_CDW_Meher) <- c('cdd_JJAS_ssp245', 'cdd_JJAS_ssp585', 'cdd_JJAS_ssp???','cwd_JJAS_ssp245', 'cwd_JJAS_ssp585', 'cdd_JJAS_ssp???')

# subset & mask imagery to shape polygons
Fut_CDD_CDW_Meher_cattle <- resample(Fut_CDD_CDW_Meher, r_dummy) #downsampling to climate pixels
Fut_CDD_CDW_Meher_cattle <- crop(Fut_CDD_CDW_Meher_cattle, AEZ_cattle, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_CDD_CDW_Meher_cattle)
writeRaster(Fut_CDD_CDW_Meher_cattle, "output/cattle/JJAS/Fut_CDD_CDW_Meher_cattle.tif", overwrite=TRUE)



#### zonal stats ####

# per AEZ Belg
zonal_climate_MAM_stats <- c(Hist_Belg_cattle, Hist_CDD_CDW_Belg_cattle) #, Fut_Belg_cattle, Fut_CDD_CDW_Belg_cattle)

zonal_climate_MAM_stats_min <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_cattle_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_max <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_cattle_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_mean <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_cattle_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_sd <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_cattle_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_climate_MAM_stats_AEZ <- cbind(zonal_climate_MAM_stats_min, zonal_climate_MAM_stats_max, zonal_climate_MAM_stats_mean, zonal_climate_MAM_stats_sd)

zonal_climate_MAM_stats_AEZ_final <- zonal_climate_MAM_stats_AEZ[!duplicated(as.list(zonal_climate_MAM_stats_AEZ))]
write.csv(zonal_climate_MAM_stats_AEZ_final, "output/cattle/MAM/zonal_climate_MAM_stats_AEZ_cattle.csv")


# per AEZ Meher
zonal_climate_JJAS_stats <- c(Hist_Meher_cattle, Hist_CDD_CDW_Meher_cattle) #, Fut_Meher_cattle, Fut_CDD_CDW_Meher_cattle)

zonal_climate_JJAS_stats_min <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_cattle), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_max <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_cattle), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_mean <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_cattle), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_sd <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_cattle), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_climate_JJAS_stats_AEZ <- cbind(zonal_climate_JJAS_stats_min, zonal_climate_JJAS_stats_max, zonal_climate_JJAS_stats_mean, zonal_climate_JJAS_stats_sd)

zonal_climate_JJAS_stats_AEZ_final <- zonal_climate_JJAS_stats_AEZ[!duplicated(as.list(zonal_climate_JJAS_stats_AEZ))]
write.csv(zonal_climate_JJAS_stats_AEZ_final, "output/cattle/JJAS/zonal_climate_JJAS_stats_AEZ_cattle.csv")


# per AEZ Annual Extremes
zonal_climate_extr_stats <- c(Hist_Extreme_cattle, Fut_Extreme_cattle)

zonal_climate_extr_stats_min <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_cattle), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_max <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_cattle), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_mean <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_cattle), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_sd <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_cattle), 'stdev', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_AEZ <- cbind(zonal_climate_extr_stats_min, zonal_climate_extr_stats_max, zonal_climate_extr_stats_mean, zonal_climate_extr_stats_sd)

zonal_climate_extr_stats_AEZ_final <- zonal_climate_extr_stats_AEZ[!duplicated(as.list(zonal_climate_extr_stats_AEZ))]
write.csv(zonal_climate_extr_stats_AEZ_final, "output/cattle/extremes/zonal_climate_extr_stats_AEZ_cattle.csv")



#### Belg climate plots ####

AEZ_cattle_Belg_raster <- rasterize(AEZ_cattle_Belg, zonal_climate_MAM_stats, "MAEZ") # rasterize AEZ layer

# precip clim
precip_clim_Belg_cattle <- c(Hist_Belg_cattle$precip_clim_MAM, Fut_Belg_cattle$precip_clim_MAM_ssp585)
plot_list <- names(precip_clim_Belg_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/MAM/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(precip_clim_Belg_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(28, 752))
  dev.off()
}
figure_list <- list(Hist_Belg_cattle$precip_clim_MAM, Fut_Belg_cattle$precip_clim_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 750, 250),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,752)) +
    labs(fill = "[mm]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/cattle/MAM/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# precip cv
precip_cv_Belg_cattle <- c(Hist_Belg_cattle$precip_cv_MAM, Fut_Belg_cattle$precip_cv_MAM_ssp585)
plot_list <- names(precip_cv_Belg_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/MAM/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(precip_cv_Belg_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 80))
  dev.off()
}
figure_list <- list(Hist_Belg_cattle$precip_cv_MAM, Fut_Belg_cattle$precip_cv_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "muted",
      breaks = seq(0, 80, 20),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,81)) +
    labs(fill = "[%]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/cattle/MAM/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmax
tmax_clim_Belg_cattle <- c(Hist_Belg_cattle$tmax_clim_MAM, Fut_Belg_cattle$tmax_clim_MAM_ssp585)
plot_list <- names(tmax_clim_Belg_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/MAM/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(tmax_clim_Belg_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(15, 40))
  dev.off()
}
figure_list <- list(Hist_Belg_cattle$tmax_clim_MAM, Fut_Belg_cattle$tmax_clim_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(10, 40, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(10,40)) +
    labs(fill = "[°C]") +
    ggplot_template_MAM
    print(p)
  ggsave(p, file=paste0("output/cattle/MAM/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmin
tmin_clim_Belg_cattle <- c(Hist_Belg_cattle$tmin_clim_MAM, Fut_Belg_cattle$tmin_clim_MAM_ssp585)
plot_list <- names(tmin_clim_Belg_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/MAM/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(tmin_clim_Belg_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 30))
  dev.off()
}
figure_list <- list(Hist_Belg_cattle$tmin_clim_MAM, Fut_Belg_cattle$tmin_clim_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(0, 30, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,30)) +
    labs(fill = "[°C]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/cattle/MAM/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Belg climate plots CHANGE ####

# precip_clim
precip_clim_Belg_change <- Changes_cattle_MAM_ssp585$precip_clim_MAM_change_ssp585
writeRaster(precip_clim_Belg_change, "output/cattle/MAM/cattle_precip_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/MAM/cattle_precip_clim_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(precip_clim_Belg_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-70, 30))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_clim_Belg_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    #breaks = seq(-250, 250, 100),
    breaks = c(-75, -50, -25, 0, 25, 50, 75),
    limits=c(-75,75), direction = -1) +
  labs(fill = "[mm]") +
  ggplot_template_MAM
print(p)
ggsave(p, file=paste0("output/cattle/MAM/cattle_precip_clim_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)


# precip_cv
precip_cv_Belg_change <- Fut_Belg_cattle$precip_cv_MAM_ssp585 - Hist_Belg_cattle$precip_cv_MAM
names(precip_cv_Belg_change) <- "precip_cv_MAM_change_ssp585"
writeRaster(precip_cv_Belg_change, "output/cattle/MAM/cattle_precip_cv_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/MAM/cattle_precip_cv_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(precip_cv_Belg_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-10, 52))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_cv_Belg_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(-50, 50, 25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-52,52)) +
  labs(fill = "[%]") +
  ggplot_template_MAM
print(p)
ggsave(p, file=paste0("output/cattle/MAM/cattle_precip_cv_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmax
tmax_clim_Belg_change <- Changes_cattle_MAM_ssp585$tmax_MAM_change_ssp585
writeRaster(tmax_clim_Belg_change, "output/cattle/MAM/cattle_tmax_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/MAM/cattle_tmax_clim_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(tmax_clim_Belg_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(1, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tmax_clim_Belg_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(1, 2, 0.25),
    #breaks = c(-50, -25, 0, 25, 50),
    limits=c(1,2)) +
  labs(fill = "[°C]") +
  ggplot_template_MAM
print(p)
ggsave(p, file=paste0("output/cattle/MAM/cattle_tmax_clim_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmin
tmin_clim_Belg_change <- Changes_cattle_MAM_ssp585$tmin_MAM_change_ssp585
writeRaster(tmin_clim_Belg_change, "output/cattle/MAM/cattle_tmin_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/MAM/cattle_tmin_clim_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(tmin_clim_Belg_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(1, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tmin_clim_Belg_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(1, 2, 0.25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(1,2)) +
  labs(fill = "[°C]") +
  ggplot_template_MAM
print(p)
ggsave(p, file="output/cattle/MAM/cattle_tmin_clim_MAM_change_fig.png", width = 4, height = 3, dpi = 300)



#### Meher climate plots ####

AEZ_cattle_raster <- rasterize(AEZ_cattle_vect, zonal_climate_JJAS_stats, "MAEZ") # rasterize AEZ layer

# precip clim
precip_clim_Meher_cattle <- c(Hist_Meher_cattle$precip_clim_JJAS, Fut_Meher_cattle$precip_clim_JJAS_ssp585)
plot_list <- names(precip_clim_Meher_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/JJAS/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(precip_clim_Meher_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 2005))
  dev.off()
}
figure_list <- list(Hist_Meher_cattle$precip_clim_JJAS, Fut_Meher_cattle$precip_clim_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 2000, 500),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,2005)) +
    labs(fill = "[mm]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/JJAS/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# precip cv
precip_cv_Meher_cattle <- c(Hist_Meher_cattle$precip_cv_JJAS, Fut_Meher_cattle$precip_cv_JJAS_ssp585)
plot_list <- names(precip_cv_Meher_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/JJAS/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(precip_cv_Meher_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 100))
  dev.off()
}
figure_list <- list(Hist_Meher_cattle$precip_cv_JJAS, Fut_Meher_cattle$precip_cv_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "muted",
      breaks = seq(0, 100, 25),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,100)) +
    labs(fill = "[%]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/JJAS/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmax
tmax_clim_Meher_cattle <- c(Hist_Meher_cattle$tmax_clim_JJAS, Fut_Meher_cattle$tmax_clim_JJAS_ssp585)
plot_list <- names(tmax_clim_Meher_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/JJAS/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(tmax_clim_Meher_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(10, 42))
  dev.off()
}
figure_list <- list(Hist_Meher_cattle$tmax_clim_JJAS, Fut_Meher_cattle$tmax_clim_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(10, 40, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(10,42)) +
    labs(fill = "[°C]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/JJAS/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmin
tmin_clim_Meher_cattle <- c(Hist_Meher_cattle$tmin_clim_JJAS, Fut_Meher_cattle$tmin_clim_JJAS_ssp585)
plot_list <- names(tmin_clim_Meher_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/JJAS/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(tmin_clim_Meher_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 31))
  dev.off()
}
figure_list <- list(Hist_Meher_cattle$tmin_clim_JJAS, Fut_Meher_cattle$tmin_clim_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(0, 30, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,31)) +
    labs(fill = "[°C]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/JJAS/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Meher climate plots CHANGE ####

# precip_clim
precip_clim_Meher_change <- Changes_cattle_JJAS_ssp585$precip_clim_JJAS_change_ssp585
writeRaster(precip_clim_Meher_change, "output/cattle/JJAS/cattle_precip_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/JJAS/cattle_precip_clim_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(precip_clim_Meher_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-20, 270))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_clim_Meher_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-200, 200, 100),
    #breaks = c(-250, -25, 0, 25, 50),
    limits=c(-275,275), direction = -1) +
  labs(fill = "[mm]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=paste0("output/cattle/JJAS/cattle_precip_clim_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# precip_cv
precip_cv_Meher_change <- Fut_Meher_cattle$precip_cv_JJAS_ssp585 - Hist_Meher_cattle$precip_cv_JJAS
names(precip_cv_Meher_change) <- "precip_cv_JJAS_change_ssp585"
writeRaster(precip_cv_Meher_change, "output/cattle/JJAS/cattle_precip_cv_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/JJAS/cattle_precip_cv_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(precip_cv_Meher_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-17, 65))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_cv_Meher_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(-50, 50, 25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-65,65)) +
  labs(fill = "[%]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=paste0("output/cattle/JJAS/cattle_precip_cv_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmax
tmax_clim_Meher_change <- Changes_cattle_JJAS_ssp585$tmax_JJAS_change_ssp585
writeRaster(tmax_clim_Meher_change, "output/cattle/JJAS/cattle_tmax_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/JJAS/cattle_tmax_clim_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(tmax_clim_Meher_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(0, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tmax_clim_Meher_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0.5, 1.5, 0.25),
    #breaks = c(-50, -25, 0, 25, 50),
    limits=c(0.5,1.5)) +
  labs(fill = "[°C]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=paste0("output/cattle/JJAS/cattle_tmax_clim_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmin
tmin_clim_Meher_change <- Changes_cattle_JJAS_ssp585$tmin_JJAS_change_ssp585
writeRaster(tmin_clim_Meher_change, "output/cattle/JJAS/cattle_tmin_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/JJAS/cattle_tmin_clim_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(tmin_clim_Meher_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(0, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tmin_clim_Meher_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0.5, 1.5, 0.25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(0.5, 1.6)) +
  labs(fill = "[°C]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/JJAS/cattle_tmin_clim_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)



#### Annual Extremes plots ####

AEZ_cattle_raster <- rasterize(AEZ_cattle_vect, zonal_climate_extr_stats, "MAEZ") # rasterize AEZ layer

# r01
r01_cattle <- c(Hist_Extreme_cattle$r01, Fut_Extreme_cattle$r01_ssp585)
plot_list <- names(r01_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(r01_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(30, 170))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$r01, Fut_Extreme_cattle$r01_ssp585)
for(i in figure_list) {
  p <- ggplot() + 
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      #breaks = seq(0, 200, 50),
      breaks = c(1, 25, 50, 75, 100, 125, 150, 175),
      limits=c(1,175)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# r10
r10_cattle <- c(Hist_Extreme_cattle$r10, Fut_Extreme_cattle$r10_ssp585)
plot_list <- names(r10_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(r10_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 100))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$r10, Fut_Extreme_cattle$r10_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      #breaks = seq(0, 100, 25),
      breaks = c(1, 25, 50, 75, 100),
      limits=c(1,100)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# r20
r20_cattle <- c(Hist_Extreme_cattle$r20, Fut_Extreme_cattle$r20_ssp585)
plot_list <- names(r20_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(r20_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 50))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$r20, Fut_Extreme_cattle$r20_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      #breaks = seq(0, 50, 10),
      breaks = c(1, 10, 20, 30, 40),
      limits=c(1,45)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# sdii
sdii_cattle <- c(Hist_Extreme_cattle$sdii, Fut_Extreme_cattle$sdii_ssp585)
plot_list <- names(sdii_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(sdii_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 25))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$sdii, Fut_Extreme_cattle$sdii_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 20, 5),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,23)) +
    labs(fill = "[mm/day]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tr
tr_cattle <- c(Hist_Extreme_cattle$tr, Fut_Extreme_cattle$tr_ssp585)
plot_list <- names(tr_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(tr_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 365))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$tr, Fut_Extreme_cattle$tr_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      #breaks = seq(0, 150, 50),
      breaks = c(1, 100, 200, 300),
      limits=c(1,365)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# txge35
txge35_cattle <- c(Hist_Extreme_cattle$txge35, Fut_Extreme_cattle$txge35_ssp585)
plot_list <- names(txge35_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(txge35_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 300))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$txge35, Fut_Extreme_cattle$txge35_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      #breaks = seq(0, 50, 10),
      breaks = c(1, 50, 100, 150, 200, 250, 300),
      limits=c(1,300)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# dtr
dtr_cattle <- c(Hist_Extreme_cattle$dtr, Fut_Extreme_cattle$dtr_ssp585)
plot_list <- names(dtr_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(dtr_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(4, 16))
  dev.off()
}
figure_list <- list(Hist_Extreme_cattle$dtr, Fut_Extreme_cattle$dtr_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(5, 15, 2.5),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(4,16)) +
    labs(fill = "[°C]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# cdd - Belg
cdd_MAM_cattle <- c(Hist_CDD_CDW_Belg_cattle$cdd_MAM, Fut_CDD_CDW_Belg_cattle$cdd_MAM_ssp585)
plot_list <- names(cdd_MAM_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(cdd_MAM_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(15, 75))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Belg_cattle$cdd_MAM, Fut_CDD_CDW_Belg_cattle$cdd_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(15, 75, 15),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(15,77), direction = -1) +
    labs(fill = "[days]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

## cdd - Meher
cdd_JJAS_cattle <- c(Hist_CDD_CDW_Meher_cattle$cdd_JJAS, Fut_CDD_CDW_Meher_cattle$cdd_JJAS_ssp585)
plot_list <- names(cdd_JJAS_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(cdd_JJAS_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(0, 110))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Meher_cattle$cdd_JJAS, Fut_CDD_CDW_Meher_cattle$cdd_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 100, 25),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,110), direction = -1) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# cwd - Belg
cwd_MAM_cattle <- c(Hist_CDD_CDW_Belg_cattle$cwd_MAM, Fut_CDD_CDW_Belg_cattle$cwd_MAM_ssp585)
plot_list <- names(cwd_MAM_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(cwd_MAM_cattle[i], AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(3, 20))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Belg_cattle$cwd_MAM, Fut_CDD_CDW_Belg_cattle$cwd_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 20, 5),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,20), direction = 1) +
    labs(fill = "[days]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

## cwd - Meher
cwd_JJAS_cattle <- c(Hist_CDD_CDW_Meher_cattle$cwd_JJAS, Fut_CDD_CDW_Meher_cattle$cwd_JJAS_ssp585)
plot_list <- names(cwd_JJAS_cattle)
for(i in plot_list) {
  png(file = paste0("output/cattle/extremes/cattle_", i, "_box.png"), pointsize=10, width=3000, height=1920, res=300)
  boxplot(cwd_JJAS_cattle[i], AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = gsub('_',' ', i), ylim = c(2, 110))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Meher_cattle$cwd_JJAS, Fut_CDD_CDW_Meher_cattle$cwd_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 100, 25),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,110), direction = 1) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/cattle/extremes/cattle_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Annual Extremes CHANGE plots ####

# r01
r01_change <- Changes_cattle_extr_ssp585$r01_ann_change_ssp585
writeRaster(r01_change, "output/cattle/extremes/cattle_r01_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_r01_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(r01_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-2, 45))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r01_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-50, 50, 10),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-45,45)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_r01_change_fig.png", width = 4, height = 3, dpi = 300)


# r10
r10_change <- Changes_cattle_extr_ssp585$r10_ann_change_ssp585
writeRaster(r10_change, "output/cattle/extremes/cattle_r10_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_r10_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(r10_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-1, 11))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r10_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-11,11)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_r10_change_fig.png", width = 4, height = 3, dpi = 300)


# r20
r20_change <- Changes_cattle_extr_ssp585$r20_ann_change_ssp585
writeRaster(r20_change, "output/cattle/extremes/cattle_r20_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_r20_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(r20_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-1, 11))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r20_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-11,11)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_r20_change_fig.png", width = 4, height = 3, dpi = 300)


# sdii
sdii_change <- Changes_cattle_extr_ssp585$sdii_ann_change_ssp585
writeRaster(sdii_change, "output/cattle/extremes/cattle_sdii_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_sdii_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(sdii_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-3, 1))
dev.off()

p <- ggplot() +
  geom_spatraster(data = sdii_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-3, 3, 1),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-3,3)) +
  labs(fill = "[mm/day]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_sdii_change_fig.png", width = 4, height = 3, dpi = 300)


# tr
tr_change <- Changes_cattle_extr_ssp585$tr_ann_change_ssp585
writeRaster(tr_change, "output/cattle/extremes/cattle_tr_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_tr_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(tr_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(0, 215))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tr_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    #breaks = seq(0, 100, 50),
    breaks = c(1, 50, 100, 150, 200),
    limits=c(1,215)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_tr_change_fig.png", width = 4, height = 3, dpi = 300)


# # txge35
# txge35_change <- Fut_Extreme_cattle$txge35_ssp585 - Hist_Extreme_cattle$txge35
# #txge35_change <- Changes2_cattle$txge35_ann_change_ssp585
# writeRaster(txge35_change, "output/cattle/extremes/cattle_txge35_change.tif", overwrite=TRUE)
# 
# png(file = "output/cattle/extremes/cattle_txge35_change_box.png", pointsize=10, width=3000, height=1920, res=300)
# boxplot(txge35_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-83, 162))
# dev.off()
# 
# p <- ggplot() +
#   geom_spatraster(data = txge35_change) +
#   scale_fill_whitebox_c(
#     palette = "muted",
#     breaks = seq(-100, 100, 50),
#     #breaks = c(0, 20, 40, 60),
#     limits=c(-100,100)) +
#   labs(fill = "[days]") +
#   ggplot_template_JJAS
# print(p)
# ggsave(p, file="output/cattle/extremes/cattle_txge35_change_fig.png", width = 4, height = 3, dpi = 300)


# dtr
dtr_change <- Changes_cattle_extr_ssp585$dtr_ann_change_ssp585
writeRaster(dtr_change, "output/cattle/extremes/cattle_dtr_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_dtr_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(dtr_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-1, 1))
dev.off()

p <- ggplot() +
  geom_spatraster(data = dtr_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-1, 1, 0.5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-1,1)) +
  labs(fill = "[°C]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_dtr_change_fig.png", width = 4, height = 3, dpi = 300)


## cdd - Belg
cdd_MAM_change <- Changes_cattle_MAM_ssp585$cdd_MAM_change_ssp585
writeRaster(cdd_MAM_change, "output/cattle/extremes/cattle_cdd_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_cdd_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(cdd_MAM_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-12, 4))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cdd_MAM_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-12, 12)) +
  labs(fill = "[days]") +
  ggplot_template_MAM
print(p)
ggsave(p, file="output/cattle/extremes/cattle_cdd_MAM_change_fig.png", width = 4, height = 3, dpi = 300)


## cdd - Meher
cdd_JJAS_change <- Changes_cattle_JJAS_ssp585$cdd_JJAS_change_ssp585
writeRaster(cdd_JJAS_change, "output/cattle/extremes/cattle_cdd_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_cdd_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(cdd_JJAS_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-25, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cdd_JJAS_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-25, 25, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-25, 25)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_cdd_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)


## cwd - Belg
cwd_MAM_change <- Changes_cattle_MAM_ssp585$cwd_MAM_change_ssp585
writeRaster(cwd_MAM_change, "output/cattle/extremes/cattle_cwd_MAM_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_cwd_MAM_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(cwd_MAM_change, AEZ_cattle_Belg_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-2, 6))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cwd_MAM_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-6, 6, 2),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-6, 6), direction = -1) +
  labs(fill = "[days]") +
  ggplot_template_MAM
print(p)
ggsave(p, file="output/cattle/extremes/cattle_cwd_MAM_change_fig.png", width = 4, height = 3, dpi = 300)


## cwd - Meher
cwd_JJAS_change <- Changes_cattle_JJAS_ssp585$cwd_JJAS_change_ssp585
writeRaster(cwd_JJAS_change, "output/cattle/extremes/cattle_cwd_JJAS_change.tif", overwrite=TRUE)

png(file = "output/cattle/extremes/cattle_cwd_JJAS_change_box.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(cwd_JJAS_change, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = "change", ylim = c(-2.5, 26))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cwd_JJAS_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-25, 25, 10),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-26, 26)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/cattle/extremes/cattle_cwd_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)




#### zonal stats for change ####

## climate change per AEZ (Belg)
zonal_clim_MAM_change_stats <- c(Changes_cattle_MAM_ssp585, precip_cv_Belg_change)

zonal_clim_MAM_change_stats_min <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_cattle_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_max <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_cattle_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_mean <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_cattle_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_sd <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_cattle_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_clim_MAM_change_stats_AEZ <- cbind(zonal_clim_MAM_change_stats_min, zonal_clim_MAM_change_stats_max, zonal_clim_MAM_change_stats_mean, zonal_clim_MAM_change_stats_sd)

zonal_clim_MAM_change_stats_AEZ_final <- zonal_clim_MAM_change_stats_AEZ[!duplicated(as.list(zonal_clim_MAM_change_stats_AEZ))]
write.csv(zonal_clim_MAM_change_stats_AEZ_final, "output/cattle/MAM/zonal_clim_MAM_change_stats_AEZ_cattle.csv")


## climate change per AEZ (Meher)
zonal_clim_JJAS_change_stats <- c(Changes_cattle_JJAS_ssp585, precip_cv_Meher_change)

zonal_clim_JJAS_change_stats_min <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_cattle_vect), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_max <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_cattle_vect), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_mean <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_cattle_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_sd <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_cattle_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_clim_JJAS_change_stats_AEZ <- cbind(zonal_clim_JJAS_change_stats_min, zonal_clim_JJAS_change_stats_max, zonal_clim_JJAS_change_stats_mean, zonal_clim_JJAS_change_stats_sd)

zonal_clim_JJAS_change_stats_AEZ_final <- zonal_clim_JJAS_change_stats_AEZ[!duplicated(as.list(zonal_clim_JJAS_change_stats_AEZ))]
write.csv(zonal_clim_JJAS_change_stats_AEZ_final, "output/cattle/JJAS/zonal_clim_JJAS_change_stats_AEZ_cattle.csv")


## extremes change per AEZ
zonal_extreme_change_stats <- Changes_cattle_extr_ssp585

zonal_extreme_change_stats_min <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_cattle_vect), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_max <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_cattle_vect), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_mean <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_cattle_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_sd <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_cattle_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_extreme_change_stats_AEZ <- cbind(zonal_extreme_change_stats_min, zonal_extreme_change_stats_max, zonal_extreme_change_stats_mean, zonal_extreme_change_stats_sd)

zonal_extreme_change_stats_AEZ_final <- zonal_extreme_change_stats_AEZ[!duplicated(as.list(zonal_extreme_change_stats_AEZ))]
write.csv(zonal_extreme_change_stats_AEZ_final, "output/cattle/extremes/zonal_extreme_change_stats_AEZ_cattle.csv")




#### drought frequency ####

drought <- rast("input/Adama_data/drought_frequency2.tif")
crs(drought)  <- "epsg:4326"
names(drought) <- 'drought_frequency'

# subset & mask imagery to shape polygons
drought_cattle <- resample(drought, r_dummy) #downsampling to climate pixels
drought_cattle <- crop(drought_cattle, AEZ_cattle_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(drought_cattle)
writeRaster(drought_cattle, "output/cattle/Drought_cattle.tif", overwrite=TRUE)


png(file = "output/cattle/cattle_drought_frequency.png", pointsize=10, width=3000, height=1920, res=300)
boxplot(drought_cattle, AEZ_cattle_raster, notch=FALSE, col=viridis(18), ylab = names(drought_cattle), ylim = c(0, 4))
dev.off()

p <- ggplot() +
  geom_spatraster(data = drought_cattle) +
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") +
  geom_spatvector(data = FSRP_Woreda_vect_cattle, fill = NA, color = "brown") +
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") + 
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    #breaks = seq(0, 200, 50),
    breaks = c(1, 2, 3, 4),
    limits=c(1,4)) +
  labs(fill = "[f]") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.87, .5)) +
  annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=paste0("output/cattle/cattle_drought_frequency_fig.png"), width = 4, height = 3, dpi = 300)



