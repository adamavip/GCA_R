
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

setwd("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023")
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
Belg_mask <- sf::st_read("Input/AEZs", layer = "belg_merged", quiet = TRUE) %>%
  st_transform(4326) #%>%
#dplyr::select(MAJORAEZNA) 
Belg_mask_vect <- vect(Belg_mask)


# Woreda for Belg maize .shp (created in QGIS)
Belg_Woreda_maize <- sf::st_read("Input/admin", layer = "Eth_Woreda_2021_UTM_Belgmaize", quiet = TRUE) %>%
  st_transform(4326) #%>%
#dplyr::select(MAJORAEZNA) 
Belg_Woreda_maize_vect <- vect(Belg_Woreda_maize)


# AEZs .shp (agro-ecological zones)
AEZ <- sf::st_read("Input/AEZs/V6_FinalAnalysisUnitsEtRevisedWzCRGET_C", layer = "RevisedUnitsByCRG_Ethiopia_V3_shList_C", quiet = TRUE) %>%
  st_transform(4326) %>%
  dplyr::mutate(MAEZ = ifelse(AEZ_name == "Sub-moist", "WSML",
                                       ifelse (AEZ_name == "Moist to sub-humid lowlands", "MSHL", 
                                               ifelse (AEZ_name == "Tepid humid mid-highlands", "THMH",
                                                       ifelse (AEZ_name == "Semi-Arid lowlands", "SAL",
                                                               ifelse (AEZ_name == "Moist highlands", "MH", "SAMH"))))))
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
maize_list <- list.files("input/MapSpam/", pattern = "MAIZ_A.tif", full.names = TRUE)
MapSPAM_maize <-rast(maize_list) #stack into one dataset
names(MapSPAM_maize) <- c('Physical Area', 'Harvest Area', 'Production', 'Value', 'Yield')

## subset & mask imagery to shape polygons
# Ethiopia
MapSPAM_maize_Eth <- crop(MapSPAM_maize, Eth_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
#MapSPAM_maize_Eth <- resample(MapSPAM_maize_Eth, r_dummy) #downsampling to climate pixels
summary(MapSPAM_maize_Eth)
plot(MapSPAM_maize_Eth)
writeRaster(MapSPAM_maize_Eth, "output/maize/MapSPAM_maize_Eth.tif", overwrite=TRUE)

## zonal stats
# per country
Zonal_MapSPAM_maize_Eth_min <- exact_extract(MapSPAM_maize_Eth, st_as_sf(Eth_vect), 'min', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_maize_Eth_max <- exact_extract(MapSPAM_maize_Eth, st_as_sf(Eth_vect), 'max', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_maize_Eth_mean <- exact_extract(MapSPAM_maize_Eth, st_as_sf(Eth_vect), 'mean', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_maize_Eth_sd <- exact_extract(MapSPAM_maize_Eth, st_as_sf(Eth_vect), 'stdev', append_cols = "Regime", progress = FALSE)
Zonal_MapSPAM_maize_Eth_sum <- exact_extract(MapSPAM_maize_Eth, st_as_sf(Eth_vect), 'sum', append_cols = "Regime", progress = FALSE)

Zonal_MapSPAM_maize_Eth <- cbind(Zonal_MapSPAM_maize_Eth_min, Zonal_MapSPAM_maize_Eth_max, Zonal_MapSPAM_maize_Eth_mean, Zonal_MapSPAM_maize_Eth_sd, Zonal_MapSPAM_maize_Eth_sum)
write.csv(Zonal_MapSPAM_maize_Eth, "output/maize/Zonal_MapSPAM_maize_Eth.csv")

# per AEZ (entire country)
Zonal_MapSPAM_maize_Eth_AEZ_min <- exact_extract(MapSPAM_maize_Eth, st_as_sf(AEZ_vect), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_max <- exact_extract(MapSPAM_maize_Eth, st_as_sf(AEZ_vect), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_mean <- exact_extract(MapSPAM_maize_Eth, st_as_sf(AEZ_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_sd <- exact_extract(MapSPAM_maize_Eth, st_as_sf(AEZ_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_sum <- exact_extract(MapSPAM_maize_Eth, st_as_sf(AEZ_vect), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_maize_Eth_AEZ <- cbind(Zonal_MapSPAM_maize_Eth_AEZ_min, Zonal_MapSPAM_maize_Eth_AEZ_max, Zonal_MapSPAM_maize_Eth_AEZ_mean, Zonal_MapSPAM_maize_Eth_AEZ_sd, Zonal_MapSPAM_maize_Eth_AEZ_sum)
write.csv(Zonal_MapSPAM_maize_Eth_AEZ, "output/maize/Zonal_MapSPAM_maize_Eth_AEZ.csv")





####### Crop Mask ####### 

## create binary raster crop mask (physical area)
threshold <- 100
MapSPAM_maize_Eth_mask <- MapSPAM_maize_Eth$`Physical Area` > threshold
NAflag(MapSPAM_maize_Eth_mask) <- 0
plot(MapSPAM_maize_Eth_mask)
writeRaster(MapSPAM_maize_Eth_mask, "output/maize/MapSPAM_maize_Eth_mask.tif", overwrite=TRUE)

MapSPAM_maize_Eth_mask_poly <- rasterToPolygons(stack(MapSPAM_maize_Eth_mask))
MapSPAM_maize_Eth_mask_poly <- aggregate(MapSPAM_maize_Eth_mask_poly, dissolve=T)
plot(MapSPAM_maize_Eth_mask_poly)
shapefile(MapSPAM_maize_Eth_mask_poly, filename='output/maize/MapSPAM_maize_Eth_mask_poly.shp', overwrite=TRUE)

MapSPAM_maize_Eth_mask_poly <- vect(MapSPAM_maize_Eth_mask_poly)
MapSPAM_maize_Eth_plots <- crop(MapSPAM_maize_Eth, MapSPAM_maize_Eth_mask_poly, mask=T)


## AEZ for maize
AEZ_maize <- crop(AEZ_vect, MapSPAM_maize_Eth_mask_poly) # spatial subset + mask = clip to extent + mask out all pixels raster
AEZ_maize_Belg <- crop(AEZ_vect, Belg_mask_vect) # spatial subset + mask = clip to extent + mask out all pixels raster
AEZ_maize_Belg_pot <- crop(AEZ_maize, Belg_mask_vect) # spatial subset + mask = clip to extent + mask out all pixels raster


## select FSRP Woredas within crop mask
FSRP_Woreda_vect_maize <- intersect(FSRP_Woreda_vect, AEZ_maize)
plot(FSRP_Woreda_vect_maize)
FSRP_Woreda_vect_maize <- FSRP_Woreda_vect_maize %>%
  dplyr::select(c("ADM3_EN", "ADM2_EN", "ADM1_EN", "ADM0_EN", "MAEZ")) %>%
  dplyr::mutate(ID = 1:n()) #%>%
# dplyr::mutate(geom = geom(FSRP_Woreda_vect_maize, wkt=TRUE))
writeVector(FSRP_Woreda_vect_maize, filename='output/maize/FSRP_Woreda_vect_maize.shp', overwrite=TRUE)

FSRP_Woreda_vect_maize_Belg_pot <- intersect(FSRP_Woreda_vect, AEZ_maize_Belg_pot)
plot(FSRP_Woreda_vect_maize_Belg_pot)
FSRP_Woreda_vect_maize_Belg <- intersect(FSRP_Woreda_vect, AEZ_maize_Belg_pot)
plot(FSRP_Woreda_vect_maize_Belg)


## ggplot template
ggplot_template_JJAS <-
  list(geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey",show.legend = TRUE),
       geom_spatvector(data = FSRP_Woreda_vect_maize, fill = NA, color = "brown",show.legend = TRUE),
       geom_spatvector(data = Eth_vect, fill = NA, color = "black",show.legend = TRUE),
       theme_bw(),
       theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"), legend.position = c(.86, .5)),
       annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")), 
       annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering),
       
  )

ggplot_template_MAM <-
  list(geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey"),
       geom_spatvector(data = FSRP_Woreda_vect_maize_Belg, fill = NA, color = "brown"),
       geom_spatvector(data = Eth_vect, fill = NA, color = "black"),
       theme_bw(),
       theme(plot.background = element_rect(fill = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"), legend.position = c(.86, .5)),
       annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")), 
       annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
  )


## create figures "Physcial Area"
mn <- mx <- minmax(MapSPAM_maize_Eth_plots$`Physical Area`)[1,1]
mx <- minmax(MapSPAM_maize_Eth_plots$`Physical Area`)[2,1]
p <- ggplot() +
  geom_spatraster(data = MapSPAM_maize_Eth_plots$`Physical Area`) +
    scale_fill_whitebox_c(
      palette = "muted",
      breaks = seq(as.integer(mn),as.integer(mx),length.out=5),
      limits=c(100,2650)) +
    labs(fill = "[ha]") +
  ggplot_template_JJAS 
  
print(p)
ggsave(p, file=("output/maize/maize_MapSPAM_physical_area.png"), width = 4, height = 3, dpi = 300)

## create figures "Production"
p <- ggplot() +
  geom_spatraster(data = MapSPAM_maize_Eth_plots$`Production`) + 
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = c(1000, 5000, 10000, 15000, 20000, 25000),
    limits=c(1000,26001)) +
  labs(fill = "[kg]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=("output/maize/maize_MapSPAM_production.png"), width = 4, height = 3, dpi = 300)

## create figures "Yield"
p <- ggplot() +
  geom_spatraster(data = MapSPAM_maize_Eth_plots$`Yield`) + 
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = c(1000, 2000, 4000, 6000, 8000, 10000),
    limits=c(1000,11000)) +
  labs(fill = "[kg/ha]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=("output/maize/maize_MapSPAM_yield.png"), width = 4, height = 3, dpi = 300)



####### AEZ per VC ####### 

cols <- c("SAL"="#fed8a4", "WSML"="#fdb77a", "MSHL"="#ec603f",
          "SAMH"="#d7efaa", "MH"="#78c679", "THMH"="#208f4a") # define colors for AEZ maps


p <- ggplot() +
  geom_spatvector(data = AEZ_maize, aes(fill = MAEZ), color = NA) +
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") + 
  geom_spatvector(data = FSRP_Woreda_vect_maize, fill = NA, color = "brown") +
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") +
  scale_fill_manual(values = cols, breaks=c('SAL', 'WSML', 'MSHL', 'SAMH', 'MH', 'THMH')) +
  labs(fill = "AEZ") +
  guides(fill=guide_legend(ncol=1)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.87, .4),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=("output/maize/maize_AEZ.png"), width = 4, height = 3, dpi = 300)


p <- ggplot() +
  geom_spatvector(data = AEZ_maize_Belg_pot, aes(fill = MAEZ), color = NA) + 
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") + 
  geom_spatvector(data = FSRP_Woreda_vect_maize_Belg_pot, fill = NA, color = "brown") +
  #geom_spatvector(data = Belg_Woreda_maize_vect, fill = NA, color = "yellow") +
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") +
  scale_fill_manual(values = cols, breaks=c('SAL', 'WSML', 'MSHL', 'SAMH', 'MH', 'THMH')) +
  labs(fill = "AEZ") +
  guides(fill=guide_legend(ncol=1)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.87, .4),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=9),
        legend.text = element_text(size=7)) +
  annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "tr", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=("output/maize/maize_Belg_AEZ.png"), width = 4, height = 3, dpi = 300)



# per AEZ both seasons
Zonal_MapSPAM_maize_Eth_AEZ_min <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_max <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_mean <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_sd <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_sum <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_maize_Eth_AEZ <- cbind(Zonal_MapSPAM_maize_Eth_AEZ_min, Zonal_MapSPAM_maize_Eth_AEZ_max, Zonal_MapSPAM_maize_Eth_AEZ_mean, Zonal_MapSPAM_maize_Eth_AEZ_sd, Zonal_MapSPAM_maize_Eth_AEZ_sum)
write.csv(Zonal_MapSPAM_maize_Eth_AEZ, "output/maize/Zonal_MapSPAM_maize_Eth_AEZ_masked.csv")

writeVector(AEZ_maize, filename='output/maize/AEZ_maize_shp.shp', overwrite=TRUE)


# AEZ_maize <- sf::st_read("input/AEZs", layer = "AEZ_maize_shp_majorAEZ", quiet = TRUE) %>%
#   st_transform(4326)
AEZ_maize_vect <- AEZ_maize


# per AEZ Belg maize
Zonal_MapSPAM_maize_Eth_AEZ_Belg_min <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_Belg_max <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_Belg_mean <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_Belg_sd <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)
Zonal_MapSPAM_maize_Eth_AEZ_Belg_sum <- exact_extract(MapSPAM_maize_Eth_plots, st_as_sf(AEZ_maize_Belg), 'sum', append_cols = "MAEZ", progress = FALSE)

Zonal_MapSPAM_maize_Eth_AEZ_Belg <- cbind(Zonal_MapSPAM_maize_Eth_AEZ_Belg_min, Zonal_MapSPAM_maize_Eth_AEZ_Belg_max, Zonal_MapSPAM_maize_Eth_AEZ_Belg_mean, Zonal_MapSPAM_maize_Eth_AEZ_Belg_sd, Zonal_MapSPAM_maize_Eth_AEZ_Belg_sum)
write.csv(Zonal_MapSPAM_maize_Eth_AEZ_Belg, "output/maize/Zonal_MapSPAM_maize_Eth_AEZ_Belg_masked.csv")

writeVector(AEZ_maize_Belg, filename='output/maize/AEZ_maize_Belg_shp.shp', overwrite=TRUE)


# AEZ_maize_Belg <- sf::st_read("input/AEZs", layer = "AEZ_maize_shp_majorAEZ_Belg", quiet = TRUE) %>%
#  st_transform(4326)
# AEZ_maize_Belg <- vect(AEZ_maize_Belg)


# socio-economics per maize AEZ 

#cattle density
se_cattle_density <- rast("input/socioeconomics/cattle_density_map.tif")
se_cattle_density <- crop(se_cattle_density, MapSPAM_maize_Eth_mask_poly, mask=T)
se_cattle_density_AEZ_mean <- exact_extract(se_cattle_density, st_as_sf(FSRP_Woreda_vect_maize), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_cattle_density_AEZ_mean <- se_cattle_density_AEZ_mean %>% tidyterra::rename(cattle_density_AEZ_mean = mean)

#health
se_health <- rast("input/socioeconomics/cell5m_Demographics_HealthandNutrition_RSTUN_M.tif")
se_health <- crop(se_health, MapSPAM_maize_Eth_mask_poly, mask=T)
se_health_AEZ_mean <- exact_extract(se_health, st_as_sf(FSRP_Woreda_vect_maize), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_health_AEZ_mean <- se_health_AEZ_mean %>% tidyterra::rename(health_AEZ_mean = mean)

#population density
se_pop <- rast("input/socioeconomics/ppp_2020_1km_Aggregated.tif")
se_pop <- crop(se_pop, MapSPAM_maize_Eth_mask_poly, mask=T)
se_pop_AEZ_mean <- exact_extract(se_pop, st_as_sf(FSRP_Woreda_vect_maize), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_pop_AEZ_mean <- se_pop_AEZ_mean %>% tidyterra::rename(pop_AEZ_mean = mean)

#travel distance to market
se_travel <- rast("input/socioeconomics/traveltimetomarket_ssa_020k.tif")
se_travel <- crop(se_travel, MapSPAM_maize_Eth_mask_poly, mask=T)
se_travel_AEZ_mean <- exact_extract(se_travel, st_as_sf(FSRP_Woreda_vect_maize), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_travel_AEZ_mean <- se_travel_AEZ_mean %>% tidyterra::rename(travel_AEZ_mean = mean)

#rural poverty headcount ratio
se_rpv08 <- rast("input/socioeconomics/RPV08_PT19.tif")
se_rpv08 <- crop(se_rpv08, MapSPAM_maize_Eth_mask_poly, mask=T)
se_rpv08_AEZ_mean <- exact_extract(se_rpv08, st_as_sf(FSRP_Woreda_vect_maize), 'mean', append_cols = c("ID","MAEZ"), progress = FALSE)
se_rpv08_AEZ_mean <- se_rpv08_AEZ_mean %>% tidyterra::rename(rpv08_AEZ_mean = mean)

FSRP_Woreda_vect_maize_socioecon_means <- c(se_cattle_density_AEZ_mean, se_health_AEZ_mean, se_pop_AEZ_mean, se_travel_AEZ_mean, se_rpv08_AEZ_mean)
FSRP_Woreda_vect_maize_socioecon_means <- data.frame(FSRP_Woreda_vect_maize_socioecon_means)
FSRP_Woreda_vect_maize_socioecon <- merge(FSRP_Woreda_vect_maize, FSRP_Woreda_vect_maize_socioecon_means, by.x="ID", by.y="ID")

writeVector(FSRP_Woreda_vect_maize_socioecon, filename='output/maize/FSRP_Woreda_vect_maize_socioecon.shp', overwrite=TRUE)




####### Climate Data Processing/Analysis ####### 


#### load & prepare data ####

# Historical Belg
Hist_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "MAM", full.names = TRUE)
Hist_Belg <-rast(Hist_Belg_list) #stack into one dataset
crs(Hist_Belg)  <- "epsg:4326"
names(Hist_Belg) <- c('precip_clim_MAM', 'precip_cv_MAM', 'tmax_clim_MAM', 'tmin_clim_MAM')

# subset & mask imagery to shape polygons
Hist_Belg_maize <- resample(Hist_Belg, r_dummy) #downsampling to climate pixels
Hist_Belg_maize <- crop(Hist_Belg_maize, AEZ_maize_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Belg_maize)
writeRaster(Hist_Belg_maize, "output/maize/MAM/Hist_Belg_maize.tif", overwrite=TRUE)


# Future Belg
Fut_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "MAM", full.names = TRUE)
Fut_Belg <-rast(Fut_Belg_list) #stack into one dataset
crs(Fut_Belg)  <- "epsg:4326"
names(Fut_Belg) <- c('precip_clim_MAM_ssp585', 'precip_cv_MAM_ssp585', 'tmax_clim_MAM_ssp585', 'tmax_cv_MAM_ssp585', 'tmin_clim_MAM_ssp585', 'tmin_cv_MAM_ssp585')

# subset & mask imagery to shape polygons
Fut_Belg_maize <- resample(Fut_Belg, r_dummy) #downsampling to climate pixels
Fut_Belg_maize <- crop(Fut_Belg_maize, AEZ_maize_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Belg_maize)
writeRaster(Fut_Belg_maize, "output/maize/MAM/Fut_Belg_maize.tif", overwrite=TRUE)


# Historical Meher
Hist_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/historical_seasonal", pattern = "JJAS", full.names = TRUE)
Hist_Meher <-rast(Hist_Meher_list) #stack into one dataset
crs(Hist_Meher)  <- "epsg:4326"
names(Hist_Meher) <- c('precip_clim_JJAS', 'precip_cv_JJAS', 'tmax_clim_JJAS', 'tmin_clim_JJAS')

# subset & mask imagery to shape polygons
Hist_Meher_maize <- resample(Hist_Meher, r_dummy) #downsampling to climate pixels
Hist_Meher_maize <- crop(Hist_Meher_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_Meher_maize)
writeRaster(Hist_Meher_maize, "output/maize/JJAS/Hist_Meher_maize.tif", overwrite=TRUE)


# Future Meher
Fut_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/future_seasonal", pattern = "JJAS", full.names = TRUE)
Fut_Meher <-rast(Fut_Meher_list) #stack into one dataset
crs(Fut_Meher)  <- "epsg:4326"
names(Fut_Meher) <- c('precip_clim_JJAS_ssp585', 'precip_cv_JJAS_ssp585', 'tmax_clim_JJAS_ssp585', 'tmax_cv_JJAS_ssp585', 'tmin_clim_JJAS_ssp585', 'tmin_cv_JJAS_ssp585')

# subset & mask imagery to shape polygons
Fut_Meher_maize <- resample(Fut_Meher, r_dummy) #downsampling to climate pixels
Fut_Meher_maize <- crop(Fut_Meher_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Meher_maize)
writeRaster(Fut_Meher_maize, "output/maize/JJAS/Fut_Meher_maize.tif", overwrite=TRUE)


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
Hist_Extreme1_maize <- resample(Hist_Extreme1, r_dummy) #downsampling to climate pixels
Hist_Extreme1_maize <- crop(Hist_Extreme1_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Hist_Extreme2_maize <- resample(Hist_Extreme2, r_dummy) #downsampling to climate pixels
Hist_Extreme2_maize <- crop(Hist_Extreme2_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Hist_Extreme_maize <- c(Hist_Extreme1_maize, Hist_Extreme2_maize)
plot(Hist_Extreme_maize)
writeRaster(Hist_Extreme_maize, "output/maize/extremes/Hist_Extreme_maize.tif", overwrite=TRUE)


# Future Extremes
Fut_Extreme_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes", pattern = ".tif", full.names = TRUE)
Fut_Extreme <-rast(Fut_Extreme_list) #stack into one dataset
crs(Fut_Extreme)  <- "epsg:4326"
names(Fut_Extreme) <- c('dtr_ssp585', 'r01_ssp585', 'r10_ssp585', 'r20_ssp585', 'sdii_ssp585', 'tr_ssp585', 'txge35_ssp585')

# subset & mask imagery to shape polygons
Fut_Extreme_maize <- resample(Fut_Extreme, r_dummy) #downsampling to climate pixels
Fut_Extreme_maize <- crop(Fut_Extreme_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_Extreme_maize)
writeRaster(Fut_Extreme_maize, "output/maize/extremes/Fut_Extreme_maize.tif", overwrite=TRUE)


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

Changes2 <- rast("input/AGRA_tiff_nomask/eth/change2/txge35_model_annual_change_EastAfr.nc")
crs(Changes2)  <- "epsg:4326"
names(Changes2) <- c('txge35_ann_change_ssp245', 'txge35_ann_change_ssp585')

# subset & mask imagery to shape polygons
Changes1_maize <- resample(Changes1, r_dummy) #downsampling to climate pixels
Changes1_maize <- crop(Changes1_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Changes2_maize <- resample(Changes2, r_dummy) #downsampling to climate pixels
Changes2_maize <- crop(Changes2_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster

Changes_maize <- c(Changes1_maize, Changes2_maize)
plot(Changes_maize)
writeRaster(Changes_maize, "output/maize/changes_maize.tif", overwrite=TRUE)

Changes_maize_MAM_ssp585 <- Changes_maize["MAM_change_ssp585"]
Changes_maize_MAM_ssp585 <- crop(Changes_maize_MAM_ssp585, AEZ_maize_Belg, mask=TRUE)
plot(Changes_maize_MAM_ssp585)

Changes_maize_JJAS_ssp585 <- Changes_maize["JJAS_change_ssp585"]
plot(Changes_maize_JJAS_ssp585)

Changes_maize_extr_ssp585 <- Changes_maize["ann_change_ssp585"]
plot(Changes_maize_extr_ssp585)


# CDD & CWD - Belg
Hist_CDD_CDW_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_nc", pattern = "MAM", full.names = TRUE)
Hist_CDD_CDW_Belg <-rast(Hist_CDD_CDW_Belg_list) #stack into one dataset
crs(Hist_CDD_CDW_Belg)  <- "epsg:4326"
names(Hist_CDD_CDW_Belg) <- c('cdd_MAM', 'cwd_MAM')

# subset & mask imagery to shape polygons
Hist_CDD_CDW_Belg_maize <- resample(Hist_CDD_CDW_Belg, r_dummy) #downsampling to climate pixels
Hist_CDD_CDW_Belg_maize <- crop(Hist_CDD_CDW_Belg_maize, AEZ_maize_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_CDD_CDW_Belg_maize)
writeRaster(Hist_CDD_CDW_Belg_maize, "output/maize/MAM/Hist_CDD_CDW_Belg_maize.tif", overwrite=TRUE)


Fut_CDD_CDW_Belg_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes_nc", pattern = "mam", full.names = TRUE)
Fut_CDD_CDW_Belg <-rast(Fut_CDD_CDW_Belg_list) #stack into one dataset
crs(Fut_CDD_CDW_Belg)  <- "epsg:4326"
names(Fut_CDD_CDW_Belg) <- c('cdd_MAM_ssp245', 'cdd_MAM_ssp585', 'cdd_MAM_ssp???','cwd_MAM_ssp245', 'cwd_MAM_ssp585', 'cdd_MAM_ssp???')

# subset & mask imagery to shape polygons
Fut_CDD_CDW_Belg_maize <- resample(Fut_CDD_CDW_Belg, r_dummy) #downsampling to climate pixels
Fut_CDD_CDW_Belg_maize <- crop(Fut_CDD_CDW_Belg_maize, AEZ_maize_Belg, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_CDD_CDW_Belg_maize)
writeRaster(Fut_CDD_CDW_Belg_maize, "output/maize/MAM/Fut_CDD_CDW_Belg_maize.tif", overwrite=TRUE)


# CDD & CWD - Meher
Hist_CDD_CDW_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_nc", pattern = "JJAS", full.names = TRUE)
Hist_CDD_CDW_Meher <-rast(Hist_CDD_CDW_Meher_list) #stack into one dataset
crs(Hist_CDD_CDW_Meher)  <- "epsg:4326"
names(Hist_CDD_CDW_Meher) <- c('cdd_JJAS', 'cwd_JJAS')

# subset & mask imagery to shape polygons
Hist_CDD_CDW_Meher_maize <- resample(Hist_CDD_CDW_Meher, r_dummy) #downsampling to climate pixels
Hist_CDD_CDW_Meher_maize <- crop(Hist_CDD_CDW_Meher_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Hist_CDD_CDW_Meher_maize)
writeRaster(Hist_CDD_CDW_Meher_maize, "output/maize/JJAS/Hist_CDD_CDW_Meher_maize.tif", overwrite=TRUE)


Fut_CDD_CDW_Meher_list <- list.files("input/AGRA_tiff_nomask/eth/future_extremes_nc", pattern = "jjas", full.names = TRUE)
Fut_CDD_CDW_Meher <-rast(Fut_CDD_CDW_Meher_list) #stack into one dataset
crs(Fut_CDD_CDW_Meher)  <- "epsg:4326"
names(Fut_CDD_CDW_Meher) <- c('cdd_JJAS_ssp245', 'cdd_JJAS_ssp585', 'cdd_JJAS_ssp???','cwd_JJAS_ssp245', 'cwd_JJAS_ssp585', 'cdd_JJAS_ssp???')

# subset & mask imagery to shape polygons
Fut_CDD_CDW_Meher_maize <- resample(Fut_CDD_CDW_Meher, r_dummy) #downsampling to climate pixels
Fut_CDD_CDW_Meher_maize <- crop(Fut_CDD_CDW_Meher_maize, AEZ_maize, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(Fut_CDD_CDW_Meher_maize)
writeRaster(Fut_CDD_CDW_Meher_maize, "output/maize/JJAS/Fut_CDD_CDW_Meher_maize.tif", overwrite=TRUE)





#### zonal stats ####

# per AEZ Belg
zonal_climate_MAM_stats <- c(Hist_Belg_maize, Hist_CDD_CDW_Belg_maize) #, Fut_Belg_maize, Fut_CDD_CDW_Belg_maize)
  
zonal_climate_MAM_stats_min <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_maize_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_max <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_maize_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_mean <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_maize_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_MAM_stats_sd <- exact_extract(zonal_climate_MAM_stats, st_as_sf(AEZ_maize_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_climate_MAM_stats_AEZ <- cbind(zonal_climate_MAM_stats_min, zonal_climate_MAM_stats_max, zonal_climate_MAM_stats_mean, zonal_climate_MAM_stats_sd)

zonal_climate_MAM_stats_AEZ_final <- zonal_climate_MAM_stats_AEZ[!duplicated(as.list(zonal_climate_MAM_stats_AEZ))]
write.csv(zonal_climate_MAM_stats_AEZ_final, "output/maize/MAM/zonal_climate_MAM_stats_AEZ_maize.csv")


# per AEZ Meher
zonal_climate_JJAS_stats <- c(Hist_Meher_maize, Hist_CDD_CDW_Meher_maize) #, Fut_Meher_maize, Fut_CDD_CDW_Meher_maize)

zonal_climate_JJAS_stats_min <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_maize), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_max <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_maize), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_mean <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_maize), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_JJAS_stats_sd <- exact_extract(zonal_climate_JJAS_stats, st_as_sf(AEZ_maize), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_climate_JJAS_stats_AEZ <- cbind(zonal_climate_JJAS_stats_min, zonal_climate_JJAS_stats_max, zonal_climate_JJAS_stats_mean, zonal_climate_JJAS_stats_sd)

zonal_climate_JJAS_stats_AEZ_final <- zonal_climate_JJAS_stats_AEZ[!duplicated(as.list(zonal_climate_JJAS_stats_AEZ))]
write.csv(zonal_climate_JJAS_stats_AEZ_final, "output/maize/JJAS/zonal_climate_JJAS_stats_AEZ_maize.csv")


# per AEZ Annual Extremes
zonal_climate_extr_stats <- c(Hist_Extreme_maize, Fut_Extreme_maize)

zonal_climate_extr_stats_min <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_maize), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_max <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_maize), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_mean <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_maize), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_climate_extr_stats_sd <- exact_extract(zonal_climate_extr_stats, st_as_sf(AEZ_maize), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_climate_extr_stats_AEZ <- cbind(zonal_climate_extr_stats_min, zonal_climate_extr_stats_max, zonal_climate_extr_stats_mean, zonal_climate_extr_stats_sd)

zonal_climate_extr_stats_AEZ_final <- zonal_climate_extr_stats_AEZ[!duplicated(as.list(zonal_climate_extr_stats_AEZ))]
write.csv(zonal_climate_extr_stats_AEZ_final, "output/maize/extremes/zonal_climate_extr_stats_AEZ_maize.csv")



#### Belg climate plots ####

AEZ_maize_Belg_raster <- rasterize(AEZ_maize_Belg, zonal_climate_MAM_stats, "MAEZ") # rasterize AEZ layer

# precip clim
precip_clim_Belg_maize <- c(Hist_Belg_maize$precip_clim_MAM, Fut_Belg_maize$precip_clim_MAM_ssp585)
plot_list <- names(precip_clim_Belg_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/MAM/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(precip_clim_Belg_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 752))
  dev.off()
}
figure_list <- list(Hist_Belg_maize$precip_clim_MAM, Fut_Belg_maize$precip_clim_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 750, 125),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,752)) +
    labs(fill = "[mm]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/maize/MAM/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# precip cv
precip_cv_Belg_maize <- c(Hist_Belg_maize$precip_cv_MAM, Fut_Belg_maize$precip_cv_MAM_ssp585)
plot_list <- names(precip_cv_Belg_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/MAM/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(precip_cv_Belg_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(15, 81))
  dev.off()
}
figure_list <- list(Hist_Belg_maize$precip_cv_MAM, Fut_Belg_maize$precip_cv_MAM_ssp585)
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
  ggsave(p, file=paste0("output/maize/MAM/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmax
tmax_clim_Belg_maize <- c(Hist_Belg_maize$tmax_clim_MAM, Fut_Belg_maize$tmax_clim_MAM_ssp585)
plot_list <- names(tmax_clim_Belg_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/MAM/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(tmax_clim_Belg_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(10, 40))
  dev.off()
}
figure_list <- list(Hist_Belg_maize$tmax_clim_MAM, Fut_Belg_maize$tmax_clim_MAM_ssp585)
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
  ggsave(p, file=paste0("output/maize/MAM/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmin
tmin_clim_Belg_maize <- c(Hist_Belg_maize$tmin_clim_MAM, Fut_Belg_maize$tmin_clim_MAM_ssp585)
plot_list <- names(tmin_clim_Belg_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/MAM/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(tmin_clim_Belg_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 30))
  dev.off()
}
figure_list <- list(Hist_Belg_maize$tmin_clim_MAM, Fut_Belg_maize$tmin_clim_MAM_ssp585)
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
  ggsave(p, file=paste0("output/maize/MAM/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Belg climate plots CHANGE ####

# precip_clim
precip_clim_Belg_change <- Changes_maize_MAM_ssp585$precip_clim_MAM_change_ssp585
writeRaster(precip_clim_Belg_change, "output/maize/MAM/maize_precip_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/MAM/maize_precip_clim_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(precip_clim_Belg_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-70, 25))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_clim_Belg_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    #breaks = seq(-250, 250, 100),
    breaks = c(-70, -35, 0, 35, 70),
    limits=c(-70,70), direction = -1) +
  labs(fill = "[mm]") +
  ggplot_template_MAM
print(p)
ggsave(p, file=paste0("output/maize/MAM/maize_precip_clim_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)


# precip_cv
precip_cv_Belg_change <- Fut_Belg_maize$precip_cv_MAM_ssp585 - Hist_Belg_maize$precip_cv_MAM
names(precip_cv_Belg_change) <- "precip_cv_MAM_change_ssp585"
writeRaster(precip_cv_Belg_change, "output/maize/MAM/maize_precip_cv_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/MAM/maize_precip_cv_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(precip_cv_Belg_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-5, 52))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_cv_Belg_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(-50, 50, 25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-50,52)) +
  labs(fill = "[%]") +
  ggplot_template_MAM
print(p)
ggsave(p, file=paste0("output/maize/MAM/maize_precip_cv_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmax
tmax_clim_Belg_change <- Changes_maize_MAM_ssp585$tmax_MAM_change_ssp585
writeRaster(tmax_clim_Belg_change, "output/maize/MAM/maize_tmax_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/MAM/maize_tmax_clim_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(tmax_clim_Belg_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(1, 2))
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
ggsave(p, file=paste0("output/maize/MAM/maize_tmax_clim_MAM_change_fig.png"), width = 4, height = 3, dpi = 300)

# Plot the boxplots per region per woreda for tmax
tmax_clim_Belg_change_reg_woreda <- exact_extract(tmax_clim_Belg_change,st_as_sf(FSRP_Woreda_vect_maize_Belg),include_cols=c("ADM1_EN","ADM3_EN"))
tmax_clim_Belg_change_reg_woreda<- bind_rows(tmax_clim_Belg_change_reg_woreda)
n<- length(tmax_clim_Belg_change_reg_woreda$ADM3_EN)
ggplot(tmax_clim_Belg_change_reg_woreda, aes(x=value, y=ADM3_EN, color=palette.colors(n))) + 
  geom_boxplot() +
  facet_wrap(~ADM1_EN) + theme_bw()

# tmin
tmin_clim_Belg_change <- Changes_maize_MAM_ssp585$tmin_MAM_change_ssp585
writeRaster(tmin_clim_Belg_change, "output/maize/MAM/maize_tmin_clim_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/MAM/maize_tmin_clim_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(tmin_clim_Belg_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(1, 2))
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
ggsave(p, file="output/maize/MAM/maize_tmin_clim_MAM_change_fig.png", width = 4, height = 3, dpi = 300)



#### Meher climate plots ####

AEZ_maize_raster <- rasterize(AEZ_maize_vect, zonal_climate_JJAS_stats, "MAEZ") # rasterize AEZ layer

# precip clim
precip_clim_Meher_maize <- c(Hist_Meher_maize$precip_clim_JJAS, Fut_Meher_maize$precip_clim_JJAS_ssp585)
plot_list <- names(precip_clim_Meher_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/JJAS/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(precip_clim_Meher_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 2005))
  dev.off()
}
figure_list <- list(Hist_Meher_maize$precip_clim_JJAS, Fut_Meher_maize$precip_clim_JJAS_ssp585)
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
  ggsave(p, file=paste0("output/maize/JJAS/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# precip cv
precip_cv_Meher_maize <- c(Hist_Meher_maize$precip_cv_JJAS, Fut_Meher_maize$precip_cv_JJAS_ssp585)
plot_list <- names(precip_cv_Meher_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/JJAS/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(precip_cv_Meher_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(6, 100))
  dev.off()
}
figure_list <- list(Hist_Meher_maize$precip_cv_JJAS, Fut_Meher_maize$precip_cv_JJAS_ssp585)
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
  ggsave(p, file=paste0("output/maize/JJAS/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmax
tmax_clim_Meher_maize <- c(Hist_Meher_maize$tmax_clim_JJAS, Fut_Meher_maize$tmax_clim_JJAS_ssp585)
plot_list <- names(tmax_clim_Meher_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/JJAS/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(tmax_clim_Meher_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(10, 40))
  dev.off()
}
figure_list <- list(Hist_Meher_maize$tmax_clim_JJAS, Fut_Meher_maize$tmax_clim_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(10, 40, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(10,40)) +
    labs(fill = "[°C]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/JJAS/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tmin
tmin_clim_Meher_maize <- c(Hist_Meher_maize$tmin_clim_JJAS, Fut_Meher_maize$tmin_clim_JJAS_ssp585)
plot_list <- names(tmin_clim_Meher_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/JJAS/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(tmin_clim_Meher_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 30))
  dev.off()
}
figure_list <- list(Hist_Meher_maize$tmin_clim_JJAS, Fut_Meher_maize$tmin_clim_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      breaks = seq(0, 30, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,30)) +
    labs(fill = "[°C]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/JJAS/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Meher climate plots CHANGE ####

# precip_clim
precip_clim_Meher_change <- Changes_maize_JJAS_ssp585$precip_clim_JJAS_change_ssp585
writeRaster(precip_clim_Meher_change, "output/maize/JJAS/maize_precip_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/JJAS/maize_precip_clim_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(precip_clim_Meher_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-17, 253))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_clim_Meher_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-250, 250, 100),
    #breaks = c(-50, -25, 0, 25, 50),
    limits=c(-260,253), direction = -1) +
  labs(fill = "[mm]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=paste0("output/maize/JJAS/maize_precip_clim_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# precip_cv
precip_cv_Meher_change <- Fut_Meher_maize$precip_cv_JJAS_ssp585 - Hist_Meher_maize$precip_cv_JJAS
names(precip_cv_Meher_change) <- "precip_cv_JJAS_change_ssp585"
writeRaster(precip_cv_Meher_change, "output/maize/JJAS/maize_precip_cv_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/JJAS/maize_precip_cv_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(precip_cv_Meher_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-5, 60))
dev.off()

p <- ggplot() +
  geom_spatraster(data = precip_cv_Meher_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(-50, 50, 25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-60,60)) +
  labs(fill = "[%]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file=paste0("output/maize/JJAS/maize_precip_cv_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmax
tmax_clim_Meher_change <- Changes_maize_JJAS_ssp585$tmax_JJAS_change_ssp585
writeRaster(tmax_clim_Meher_change, "output/maize/JJAS/maize_tmax_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/JJAS/maize_tmax_clim_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(tmax_clim_Meher_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(0, 1.5))
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
ggsave(p, file=paste0("output/maize/JJAS/maize_tmax_clim_JJAS_change_fig.png"), width = 4, height = 3, dpi = 300)


# tmin
tmin_clim_Meher_change <- Changes_maize_JJAS_ssp585$tmin_JJAS_change_ssp585
writeRaster(tmin_clim_Meher_change, "output/maize/JJAS/maize_tmin_clim_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/JJAS/maize_tmin_clim_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(tmin_clim_Meher_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(0, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tmin_clim_Meher_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(0.5, 2, 0.25),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(0.5, 2)) +
  labs(fill = "[°C]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/JJAS/maize_tmin_clim_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)



#### Annual Extremes plots ####

AEZ_maize_raster <- rasterize(AEZ_maize_vect, zonal_climate_extr_stats, "MAEZ") # rasterize AEZ layer

# r01
r01_maize <- c(Hist_Extreme_maize$r01, Fut_Extreme_maize$r01_ssp585)
plot_list <- names(r01_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(r01_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(18, 170))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$r01, Fut_Extreme_maize$r01_ssp585)
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
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# r10
r10_maize <- c(Hist_Extreme_maize$r10, Fut_Extreme_maize$r10_ssp585)
plot_list <- names(r10_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(r10_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(8, 100))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$r10, Fut_Extreme_maize$r10_ssp585)
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
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# r20
r20_maize <- c(Hist_Extreme_maize$r20, Fut_Extreme_maize$r20_ssp585)
plot_list <- names(r20_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(r20_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(1, 43))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$r20, Fut_Extreme_maize$r20_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      #breaks = seq(0, 50, 10),
      breaks = c(1, 10, 20, 30, 40, 50),
      limits=c(1,50)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# sdii
sdii_maize <- c(Hist_Extreme_maize$sdii, Fut_Extreme_maize$sdii_ssp585)
plot_list <- names(sdii_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(sdii_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(6, 23))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$sdii, Fut_Extreme_maize$sdii_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 25, 5),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,25)) +
    labs(fill = "[mm/day]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# tr
tr_maize <- c(Hist_Extreme_maize$tr, Fut_Extreme_maize$tr_ssp585)
plot_list <- names(tr_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(tr_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 365))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$tr, Fut_Extreme_maize$tr_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      #breaks = seq(0, 150, 50),
      breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(1,365)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# txge35
txge35_maize <- c(Hist_Extreme_maize$txge35, Fut_Extreme_maize$txge35_ssp585)
plot_list <- names(txge35_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(txge35_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 340))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$txge35, Fut_Extreme_maize$txge35_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "bl_yl_rd",
      #breaks = seq(0, 50, 10),
      breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(1,350)) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# dtr
dtr_maize <- c(Hist_Extreme_maize$dtr, Fut_Extreme_maize$dtr_ssp585)
plot_list <- names(dtr_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(dtr_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(4, 16))
  dev.off()
}
figure_list <- list(Hist_Extreme_maize$dtr, Fut_Extreme_maize$dtr_ssp585)
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
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# cdd - Belg
cdd_MAM_maize <- c(Hist_CDD_CDW_Belg_maize$cdd_MAM, Fut_CDD_CDW_Belg_maize$cdd_MAM_ssp585)
plot_list <- names(cdd_MAM_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(cdd_MAM_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(19, 66))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Belg_maize$cdd_MAM, Fut_CDD_CDW_Belg_maize$cdd_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(20, 70, 10),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(19,70), direction = -1) +
    labs(fill = "[days]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

## cdd - Meher
cdd_JJAS_maize <- c(Hist_CDD_CDW_Meher_maize$cdd_JJAS, Fut_CDD_CDW_Meher_maize$cdd_JJAS_ssp585)
plot_list <- names(cdd_JJAS_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(cdd_JJAS_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(2, 122))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Meher_maize$cdd_JJAS, Fut_CDD_CDW_Meher_maize$cdd_JJAS_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(0, 125, 25),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(0,125), direction = -1) +
    labs(fill = "[days]") +
    ggplot_template_JJAS
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

# cwd - Belg
cwd_MAM_maize <- c(Hist_CDD_CDW_Belg_maize$cwd_MAM, Fut_CDD_CDW_Belg_maize$cwd_MAM_ssp585)
plot_list <- names(cwd_MAM_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(cwd_MAM_maize[i], AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(3, 20))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Belg_maize$cwd_MAM, Fut_CDD_CDW_Belg_maize$cwd_MAM_ssp585)
for(i in figure_list) {
  p <- ggplot() +
    geom_spatraster(data = i) +
    scale_fill_whitebox_c(
      palette = "deep",
      breaks = seq(5, 20, 2.5),
      #breaks = c(1, 50, 100, 150, 200, 250, 300, 350),
      limits=c(3,20), direction = 1) +
    labs(fill = "[days]") +
    ggplot_template_MAM
  print(p)
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}

## cwd - Meher
cwd_JJAS_maize <- c(Hist_CDD_CDW_Meher_maize$cwd_JJAS, Fut_CDD_CDW_Meher_maize$cwd_JJAS_ssp585)
plot_list <- names(cwd_JJAS_maize)
for(i in plot_list) {
  png(file = paste0("output/maize/extremes/maize_", i, "_box.png"), pointsize=10, width=2800, height=1920, res=300)
  boxplot(cwd_JJAS_maize[i], AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = gsub('_',' ', i), ylim = c(0, 110))
  dev.off()
}
figure_list <- list(Hist_CDD_CDW_Meher_maize$cwd_JJAS, Fut_CDD_CDW_Meher_maize$cwd_JJAS_ssp585)
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
  ggsave(p, file=paste0("output/maize/extremes/maize_", names(i), "_fig.png"), width = 4, height = 3, dpi = 300)
}



#### Annual Extremes CHANGE plots ####

# r01
r01_change <- Changes_maize_extr_ssp585$r01_ann_change_ssp585
writeRaster(r01_change, "output/maize/extremes/maize_r01_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_r01_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(r01_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-1, 28))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r01_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-30, 30, 10),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-30,30)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_r01_change_fig.png", width = 4, height = 3, dpi = 300)


# r10
r10_change <- Changes_maize_extr_ssp585$r10_ann_change_ssp585
writeRaster(r10_change, "output/maize/extremes/maize_r10_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_r10_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(r10_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-1, 12))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r10_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-12,12)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_r10_change_fig.png", width = 4, height = 3, dpi = 300)


# r20
r20_change <- Changes_maize_extr_ssp585$r20_ann_change_ssp585
writeRaster(r20_change, "output/maize/extremes/maize_r20_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_r20_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(r20_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-1, 10))
dev.off()

p <- ggplot() +
  geom_spatraster(data = r20_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-10,10)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_r20_change_fig.png", width = 4, height = 3, dpi = 300)


# sdii
sdii_change <- Changes_maize_extr_ssp585$sdii_ann_change_ssp585
writeRaster(sdii_change, "output/maize/extremes/maize_sdii_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_sdii_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(sdii_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-2.5, 1))
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
ggsave(p, file="output/maize/extremes/maize_sdii_change_fig.png", width = 4, height = 3, dpi = 300)


# tr
tr_change <- Changes_maize_extr_ssp585$tr_ann_change_ssp585
writeRaster(tr_change, "output/maize/extremes/maize_tr_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_tr_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(tr_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(0, 182))
dev.off()

p <- ggplot() +
  geom_spatraster(data = tr_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    #breaks = seq(0, 100, 50),
    breaks = c(1, 25, 50, 75, 100, 125, 150, 175),
    limits=c(1,182)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_tr_change_fig.png", width = 4, height = 3, dpi = 300)


# txge35
txge35_change <- Changes_maize_extr_ssp585$txge35_ann_change_ssp585
writeRaster(txge35_change, "output/maize/extremes/maize_txge35_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_txge35_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(txge35_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(0, 78))
dev.off()

p <- ggplot() +
  geom_spatraster(data = txge35_change) +
  scale_fill_whitebox_c(
    palette = "muted",
    #breaks = seq(0, 100, 50),
    breaks = c(1, 25, 50, 75),
    limits=c(1,78)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_txge35_change_fig.png", width = 4, height = 3, dpi = 300)


# dtr
dtr_change <- Changes_maize_extr_ssp585$dtr_ann_change_ssp585
writeRaster(dtr_change, "output/maize/extremes/maize_dtr_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_dtr_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(dtr_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-1, 1))
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
ggsave(p, file="output/maize/extremes/maize_dtr_change_fig.png", width = 4, height = 3, dpi = 300)


## cdd - Belg
cdd_MAM_change <- Changes_maize_MAM_ssp585$cdd_MAM_change_ssp585
writeRaster(cdd_MAM_change, "output/maize/extremes/maize_cdd_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_cdd_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(cdd_MAM_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-11, 4))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cdd_MAM_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-10, 10, 5),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-11, 11)) +
  labs(fill = "[days]") +
  ggplot_template_MAM
print(p)
ggsave(p, file="output/maize/extremes/maize_cdd_MAM_change_fig.png", width = 4, height = 3, dpi = 300)


## cdd - Meher
cdd_JJAS_change <- Changes_maize_JJAS_ssp585$cdd_JJAS_change_ssp585
writeRaster(cdd_JJAS_change, "output/maize/extremes/maize_cdd_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_cdd_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(cdd_JJAS_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-24, 2))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cdd_JJAS_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-20, 20, 10),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-25, 25)) +
  labs(fill = "[days]") +
  ggplot_template_JJAS
print(p)
ggsave(p, file="output/maize/extremes/maize_cdd_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)


## cwd - Belg
cwd_MAM_change <- Changes_maize_MAM_ssp585$cwd_MAM_change_ssp585
writeRaster(cwd_MAM_change, "output/maize/extremes/maize_cwd_MAM_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_cwd_MAM_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(cwd_MAM_change, AEZ_maize_Belg_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-2, 3))
dev.off()

p <- ggplot() +
  geom_spatraster(data = cwd_MAM_change) +
  scale_fill_whitebox_c(
    palette = "bl_yl_rd",
    breaks = seq(-3, 3, 1),
    #breaks = c(-200, -100, 0, 100, 200),
    limits=c(-3, 3), direction = -1) +
  labs(fill = "[days]") +
  ggplot_template_MAM
print(p)
ggsave(p, file="output/maize/extremes/maize_cwd_MAM_change_fig.png", width = 4, height = 3, dpi = 300)


## cwd - Meher
cwd_JJAS_change <- Changes_maize_JJAS_ssp585$cwd_JJAS_change_ssp585
writeRaster(cwd_JJAS_change, "output/maize/extremes/maize_cwd_JJAS_change.tif", overwrite=TRUE)

png(file = "output/maize/extremes/maize_cwd_JJAS_change_box.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(cwd_JJAS_change, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = "change", ylim = c(-3, 26))
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
ggsave(p, file="output/maize/extremes/maize_cwd_JJAS_change_fig.png", width = 4, height = 3, dpi = 300)




#### zonal stats for change ####

## climate change per AEZ (Belg)
zonal_clim_MAM_change_stats <- c(Changes_maize_MAM_ssp585, precip_cv_Belg_change)

zonal_clim_MAM_change_stats_min <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_maize_Belg), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_max <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_maize_Belg), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_mean <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_maize_Belg), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_clim_MAM_change_stats_sd <- exact_extract(zonal_clim_MAM_change_stats, st_as_sf(AEZ_maize_Belg), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_clim_MAM_change_stats_AEZ <- cbind(zonal_clim_MAM_change_stats_min, zonal_clim_MAM_change_stats_max, zonal_clim_MAM_change_stats_mean, zonal_clim_MAM_change_stats_sd)

zonal_clim_MAM_change_stats_AEZ_final <- zonal_clim_MAM_change_stats_AEZ[!duplicated(as.list(zonal_clim_MAM_change_stats_AEZ))]
write.csv(zonal_clim_MAM_change_stats_AEZ_final, "output/maize/MAM/zonal_clim_MAM_change_stats_AEZ_maize.csv")


## climate change per AEZ (Meher)
zonal_clim_JJAS_change_stats <- c(Changes_maize_JJAS_ssp585, precip_cv_Meher_change)

zonal_clim_JJAS_change_stats_min <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_maize_vect), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_max <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_maize_vect), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_mean <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_maize_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_clim_JJAS_change_stats_sd <- exact_extract(zonal_clim_JJAS_change_stats, st_as_sf(AEZ_maize_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_clim_JJAS_change_stats_AEZ <- cbind(zonal_clim_JJAS_change_stats_min, zonal_clim_JJAS_change_stats_max, zonal_clim_JJAS_change_stats_mean, zonal_clim_JJAS_change_stats_sd)

zonal_clim_JJAS_change_stats_AEZ_final <- zonal_clim_JJAS_change_stats_AEZ[!duplicated(as.list(zonal_clim_JJAS_change_stats_AEZ))]
write.csv(zonal_clim_JJAS_change_stats_AEZ_final, "output/maize/JJAS/zonal_clim_JJAS_change_stats_AEZ_maize.csv")


## extremes change per AEZ
zonal_extreme_change_stats <- Changes_maize_extr_ssp585

zonal_extreme_change_stats_min <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_maize_vect), 'min', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_max <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_maize_vect), 'max', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_mean <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_maize_vect), 'mean', append_cols = "MAEZ", progress = FALSE)
zonal_extreme_change_stats_sd <- exact_extract(zonal_extreme_change_stats, st_as_sf(AEZ_maize_vect), 'stdev', append_cols = "MAEZ", progress = FALSE)

zonal_extreme_change_stats_AEZ <- cbind(zonal_extreme_change_stats_min, zonal_extreme_change_stats_max, zonal_extreme_change_stats_mean, zonal_extreme_change_stats_sd)

zonal_extreme_change_stats_AEZ_final <- zonal_extreme_change_stats_AEZ[!duplicated(as.list(zonal_extreme_change_stats_AEZ))]
write.csv(zonal_extreme_change_stats_AEZ_final, "output/maize/extremes/zonal_extreme_change_stats_AEZ_maize.csv")




#### drought frequency ####

drought <- rast("input/Adama_data/drought_frequency2.tif")
crs(drought)  <- "epsg:4326"
names(drought) <- 'drought_frequency'

# subset & mask imagery to shape polygons
drought_maize <- resample(drought, r_dummy) #downsampling to climate pixels
drought_maize <- crop(drought_maize, AEZ_maize_vect, mask=TRUE) # spatial subset + mask = clip to extent + mask out all pixels raster
plot(drought_maize)
writeRaster(drought_maize, "output/maize/Drought_maize.tif", overwrite=TRUE)


png(file = "output/maize/maize_drought_frequency.png", pointsize=10, width=2800, height=1920, res=300)
boxplot(drought_maize, AEZ_maize_raster, notch=FALSE, col=viridis(6), ylab = names(drought_maize), ylim = c(0, 4))
dev.off()

p <- ggplot() +
  geom_spatraster(data = drought_maize) +
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") +
  geom_spatvector(data = FSRP_Woreda_vect_maize, fill = NA, color = "brown") +
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
ggsave(p, file=paste0("output/maize/maize_drought_frequency_fig.png"), width = 4, height = 3, dpi = 300)



