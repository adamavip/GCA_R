library(rgdal)
library(sf)
library(rgeos)

library(tidyverse)
library(tidyterra)
library(ggspatial)
library(RColorBrewer)
library(paletteer)
library(viridis)
library(ggpattern)

library(terra)
library(exactextractr) #fast data extraction
library(raster)

setwd("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023")

#### Load vector data ####

AEZ_coffee <- sf::st_read("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Outputs/coffee/coffee/AEZ_coffee_shp.shp") %>%
  st_transform(4326)
AEZ_coffee_vect <- vect(AEZ_coffee)
# Load the coffee shapefile
fsrp_woreda<- sf::st_read("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/output/coffee/FSRP_Woreda_vect_coffee_socioecon.shp"
) %>%
  st_transform(4326)
fsrp_woreda_vect <- vect(fsrp_woreda)
plot(fsrp_woreda_vect)

fsrp_w<-fsrp_woreda
summary_tab<- fsrp %>% filter(MAEZ.x %in% c("M2","H2","SH1","SH2")) %>%  group_by(ADM3_EN,MAEZ.1) %>% summarise(across(.cols=c("health_AEZ","pop_AEZ_me"),list(mean=mean)))
fsrp<- summary_tab
mapping <- list(M2="crosshatch", H2="diagonal", SH1="dots", SH2="grid")
fsrp$patterns <- sapply(fsrp$MAEZ.1, function(x) mapping[[x]])

ggplot(fsrp) +
  geom_sf_pattern(aes(fill = health_AEZ_mean, pattern_type = MAEZ.1),
                      pattern= "magick",
                      pattern_aspect_ratio = 1.75,
                      colour= 'black') +
  scale_fill_whitebox_c(palette ="high_relief")+
  scale_pattern_type_discrete(choices = c("vertical2", "vertical3","verticalbricks","verticalleftshingle" ))+ 
  theme_bw()
  

df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
  geom_col_pattern(aes(fill = outcome, pattern_type = trt), 
                   pattern = 'magick', pattern_angle = c(15,35,45)) +
  scale_pattern_type_manual(values = c("diagonal", "horizontal", "vertical")) +
  theme(legend.key.size = unit(1.5, 'cm'))


# Load Ethiopia shapefile
Eth <- sf::st_read("Input/admin", layer = "Ethiopia_2013_UTM", quiet = TRUE) %>%
  st_transform(4326)
Eth_vect <- vect(Eth)
plot(Eth_vect)

# regions .shp
Eth_regions <- sf::st_read("Input/admin", layer = "Eth_Region_2013_UTM", quiet = TRUE) %>%
  st_transform(4326) %>%
  dplyr::select(REGIONNAME) %>% 
  dplyr::mutate(REGIONNAME = str_replace(REGIONNAME, "/", " "))
Eth_vect_reg <- vect(Eth_regions)



  

## ggplot template
ggplot_template_JJAS <-
  list(geom_sf(data = Eth_regions, fill = NA, color = "darkgrey"),
       geom_sf(data = fsrp_woreda, fill = NA, color = "blue"),
       geom_sf(data = Eth, fill = NA, color = "black"),
       theme_bw(),
       theme(plot.background = element_rect(fill = "transparent"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"), legend.position = c(.86, .85)),
       annotation_north_arrow(location = "lb", height = unit(1, "cm"), style = north_arrow_fancy_orienteering),
       coord_sf(),
       annotate("rect", xmin=43, xmax=44.5, ymin=2 , ymax=2.5, alpha=0.2, color="blue", fill="transparent",linewidth=1.3), 
       annotate("text", x = 46.6, y = 2.25, label = "FSRP Districts", size = 6, color='blue')
  )
ggplot_template_JJAS_bis <-
  list(geom_sf(data = Eth_regions, fill = NA, color = "darkgrey"),
       geom_sf(data = fsrp_woreda, fill = NA, color = "blue"),
       geom_sf(data = Eth, fill = NA, color = "black"),
       theme_bw(),
       theme(plot.background = element_rect(fill = "transparent"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white")),
       annotation_north_arrow(location = "lb", height = unit(1, "cm"), style = north_arrow_fancy_orienteering),
       coord_sf(),
       annotate("rect", xmin=38, xmax=40, ymin=2 , ymax=2.5, alpha=0.2, color="blue", fill="transparent",linewidth=1.3), 
       annotate("text", x = 43.2, y = 2.25, label = "FSRP Districts", size = 6, color='blue')
  )






## load dummy for resampling
r_dummy <- rast("./input/AGRA_tiff_nomask/eth/historical_annual/precp_annual_clim_EastAfr_1981T2021.nc")

r_dummy_croped <- crop(r_dummy, AEZ_coffee_vect, mask=T)
p <- ggplot() +
  geom_spatraster(data=r_dummy_croped)  +
  scale_fill_whitebox_c(palette = "viridi")+
  ggplot_template_JJAS +
  theme_bw()
print(p)

## Plot coffee agroecologies + fsrp districts

cols <- c("WB"="#000000",
          "A1"="#fef0d9", "SA1"="#fed8a4", "SM1"="#fdb77a", "M1"="#fc8d59", "SH1"="#ec603f", "H1"="#d33122", "PH1"="#b30000",
          "A2"="#ffffcc", "SA2"="#d7efaa", "SM2"="#a9dc8e", "M2"="#78c679", "SH2"="#48af60", "H2"="#208f4a", "PH2"="#006837",
          "SM3"="#e8f1fa", "M3"="#b0d2e8", "SH3"="#3e8ec4", "H3"="#08306b") # define colors for AEZ maps

#AEZ_coffee_sel<- AEZ_coffee %>% filter(MAEZ %in% c("M2","H2","SH1","SH2"))
p <- ggplot() +
  geom_spatvector(data = AEZ_coffee, aes(fill = MAEZ), color = NA) +
  geom_spatvector(data = Eth_vect_reg, fill = NA, color = "darkgrey") + 
  geom_spatvector(data = Eth_vect, fill = NA, color = "black") +
  geom_spatvector(data = fsrp_woreda_vect, fill = NA, color = "blue") +
  coord_sf()+
  annotate("rect", xmin=38, xmax=40, ymin=2 , ymax=2.5, alpha=0.2, color="blue", fill="transparent",linewidth=1.5)+
  annotate("text", x = 42.5, y = 2.25, label = "FSRP Districts", size = 4, color='blue')+
  scale_fill_manual(values = cols, breaks=c('A1', 'SA1', 'SM1', 'M1', 'SH1', 'H1', 'PH1',
                                            'A2', 'SA2', 'SM2', 'M2', 'SH2', 'H2', 'PH2',
                                            'SM3', 'M3', 'SH3', 'H3', 'WB')) +
  labs(fill = "AEZ",x=NULL,y=NULL) +
  guides(fill=guide_legend(ncol=2)) +
  theme_bw() +
  theme(plot.background = element_rect(fill = NA), 
        axis.text = element_text(size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.84, .74),
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_text(size=15),
        legend.text = element_text(size=12),
        legend.background = element_blank()) +
  #annotation_scale(location = "br", line_width = 1, height = unit(0.1, "cm")) +
  annotation_north_arrow(location = "lb", height = unit(1, "cm"), style = north_arrow_fancy_orienteering)
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "AEZs_FSRPs", ".png"), width = 7, height = 5, dpi = 300)



#### Load the annual data
# Rainfall
Hist_Ann_prep_coffee = rast("./input/AGRA_tiff_nomask/eth/historical_annual/precp_annual_clim_EastAfr_1981T2021.nc")
Fut_Ann_prep_coffee = rast("./input/AGRA_tiff_nomask/eth/future_annual/precp_model_annual_clim_EastAfr.nc")
Fut_Ann_prep_coffee <- resample(Fut_Ann_prep_coffee,r_dummy)
#CV
Hist_Ann_cv_coffee <- rast("./input/AGRA_tiff_nomask/eth/historical_annual/precp_annual_cv_EastAfr_1981T2021.nc")
Hist_Ann_cv_coffee <- resample(Hist_Ann_cv_coffee,r_dummy)
Fut_Ann_cv_coffee <- rast("./input/AGRA_tiff_nomask/eth/future_annual/precp_model_annual_cv_EastAfr.nc")
Fut_Ann_cv_coffee<- resample(Fut_Ann_cv_coffee,r_dummy)

# Temp
temp_list <-c("./input/AGRA_tiff_nomask/eth/historical_annual/tmin_annual_clim_EastAfr.nc","./input/AGRA_tiff_nomask/eth/historical_annual/tmax_annual_clim_EastAfr.nc")
Hist_Ann_temp_coffee <- rast(temp_list)
Hist_Ann_temp_coffee<- resample(Hist_Ann_temp_coffee,r_dummy)

# Calculate or load annual changes
changes_ann_prep_coffee<- Fut_Ann_prep_coffee - Hist_Ann_prep_coffee
changes_ann_prep_coffee_585 <- changes_ann_prep_coffee["pr_ssp=_3"]
#changes_ann_prep_coffee_585 <- resample(changes_ann_prep_coffee_585,r_dummy)
changes_ann_cv_coffee<- Fut_Ann_cv_coffee - Hist_Ann_cv_coffee
changes_ann_cv_coffee_585<- changes_ann_cv_coffee["pr_ssp=_3"]
changes_ann_cv_coffee_585 <- resample(changes_ann_cv_coffee_585,r_dummy)
changes_ann_tmin_coffee<- rast("./input/AGRA_tiff_nomask/eth/change/tasmin_model_annual_change_EastAfr.nc")
changes_ann_tmin_coffee_585 <-changes_ann_tmin_coffee["tasmin_ssp=_2"] 
changes_ann_tmin_coffee_585 <- resample(changes_ann_tmin_coffee_585,r_dummy)
changes_ann_tmax_coffee<- rast("./input/AGRA_tiff_nomask/eth/change/tasmax_model_annual_change_EastAfr.nc")
changes_ann_tmax_coffee_585 <-changes_ann_tmax_coffee["tasmax_ssp=_2"]
changes_ann_tmax_coffee_585 <- resample(changes_ann_tmax_coffee_585,r_dummy)





# Stack data 
rf_temp_ann_hist_chang<- c(Hist_Ann_prep_coffee,changes_ann_prep_coffee_585,Hist_Ann_cv_coffee,changes_ann_cv_coffee_585,Hist_Ann_temp_coffee["tmin"],changes_ann_tmin_coffee_585,Hist_Ann_temp_coffee["tmax"],changes_ann_tmax_coffee_585)
names(rf_temp_ann_hist_chang)<-c('rf', 'rf_change','cv','cv_change', 'tmin','tmin_change','tmax','tmax_change')

# crop data
rf_temp_ann_hist_chang <- crop(rf_temp_ann_hist_chang,AEZ_coffee_vect, mask=T)
writeRaster(rf_temp_ann_hist_chang, "../clirp/input/rf_temp_ann_hist_chang.tif",overwrite=T)


# Plot and save annual data
## RF
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=rf)) +
  scale_fill_whitebox_c(
    palette = "pi_y_g") +
  labs(fill = "[mm]",x=NULL,y=NULL) +
  ggplot_template_JJAS_bis+
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15))) 
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "rf", "_fig.png"), width = 6, height = 4, dpi = 300)

## RF change
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=rf_change)) +
  scale_fill_whitebox_c(
    palette = "deep") +
  labs(fill = "[mm]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "rf_change", "_fig.png"), width = 6, height = 4, dpi = 300)


# CV
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=cv)) +
  scale_fill_whitebox_c(
    palette ="high_relief" ) +
  labs(fill = "[%]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "cv", "_fig.png"), width = 6, height = 4, dpi = 300)

# CV change
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=cv_change)) +
  scale_fill_whitebox_c(
    palette ="viridi" ) +
  labs(fill = "[%]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "cv_change", "_fig.png"), width = 6, height = 4, dpi = 300)


# Tmin 
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=tmin)) +
  scale_fill_whitebox_c(
    palette ="muted" ) +
  labs(fill = "[°C]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "tmin", "_fig.png"), width = 6, height = 4, dpi = 300)
# Tmin change
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=tmin_change)) +
  scale_fill_whitebox_c(
    palette ="bl_yl_rd" ) +
  labs(fill = "[°C]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "tmin_change", "_fig.png"), width = 6, height = 4, dpi = 300)

#Tmax
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=tmax)) +
  scale_fill_whitebox_c(
    palette ="muted" ) +
  labs(fill = "[°C]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "tmax", "_fig.png"), width = 6, height = 4, dpi = 300)
# Tmax change
p <- ggplot() +
  geom_spatraster(data = rf_temp_ann_hist_chang, aes(fill=tmax_change)) +
  scale_fill_whitebox_c(
    palette ="bl_yl_rd" ) +
  labs(fill = "[°C]",x=NULL,y=NULL) +
  guides(fill = guide_colorbar(title.theme=element_text(size = 15),label.theme = element_text(size = 15)))+
  ggplot_template_JJAS_bis
print(p)
ggsave(p, file=paste0("output/coffee/Annual/coffee_", "tmax_change", "_fig.png"), width = 6, height = 4, dpi = 300)




# Automate saving
plot_list<- names(rf_temp_ann_hist_chang)
legend_label<-""
pal<-""
for(i in plot_list) {
  if(str_detect(i,pattern = "rf")){
    legend_label<-"[mm]"
    pal<-"pi_y_g"
  }
  else if(str_detect(i,pattern = "cv")){
    legend_label<-"[%]"
    pal<-"paired"
  }
  else {
    legend_label<-"[°C]"
    pal<- "RdYlBu"
  }
  p <- ggplot() +
    geom_spatraster(data=rf_temp_ann_hist_chang,fill=i) +
    scale_fill_whitebox_c(
      palette = pal) +
    labs(fill = legend_label) +
    ggplot_template_JJAS
  #print(p)
  ggsave(p, file=paste0("output/coffee/Annual/coffee_", i, "_fig.png"), width = 4, height = 3, dpi = 300)
}


# Extract data at woreda level
rf_temp_ann_hist_chang_aez_reg_woreda <- exact_extract(rf_temp_ann_hist_chang,st_as_sf(fsrp_woreda_vect),fun="mean",append_cols=c("MAEZ.x","ADM1_EN","ADM3_EN"))
rf_temp_ann_hist_chang_aez_reg_woreda<- bind_rows(rf_temp_ann_hist_chang_aez_reg_woreda)
rf_temp_ann_hist_chang_aez_reg_woreda_coffee<- rf_temp_ann_hist_chang_aez_reg_woreda%>% 
  filter(MAEZ.x %in% c("M2","H2","SH1","SH2"))# Choose only AEZ inside coffee growing areas
write.csv(rf_temp_ann_hist_chang_aez_reg_woreda_coffee,"./output/coffee/rf_temps_aez_reg_woreda.csv")


### Laod extremes ####
# Historical Extremes
Hist_Extreme_list1 <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_1", pattern = ".tif", full.names = TRUE)
Hist_Extreme1 <-rast(Hist_Extreme_list1) #stack into one dataset
crs(Hist_Extreme1)  <- "epsg:4326"
names(Hist_Extreme1) <- c('r01', 'r10', 'r20', 'sdii', 'tnlt4', 'tr')

Hist_Extreme_list2 <- list.files("input/AGRA_tiff_nomask/eth/historical_extremes_2", pattern = ".tif", full.names = TRUE)
Hist_Extreme2 <-rast(Hist_Extreme_list2) #stack into one dataset
crs(Hist_Extreme2)  <- "epsg:4326"
names(Hist_Extreme2) <- c('dtr', 'txge35')

Hist_cdd_coffee<- rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/input/AGRA_tiff_nomask/eth/historical_extremes_nc/cdd_JJAS_clim_EastAfr.nc")
names(Hist_cdd_coffee)<-"cdd_jjas"

# resampling
Hist_Extreme1_coffee <- resample(Hist_Extreme1, r_dummy) #downsampling to climate pixels
Hist_Extreme2_coffee <- resample(Hist_Extreme2, r_dummy) #downsampling to climate pixels
Hist_cdd_coffee <- resample(Hist_cdd_coffee,r_dummy)
Hist_Extreme_coffee <- c(Hist_Extreme1_coffee, Hist_Extreme2_coffee,Hist_cdd_coffee)



# Changes
Changes_list <- list.files("input/AGRA_tiff_nomask/eth/change", pattern = ".nc", full.names = TRUE)
Changes1 <-rast(Changes_list) #stack into one dataset
crs(Changes1)  <- "epsg:4326"


changes_list <- sapply(sources(Changes1), tools::file_path_sans_ext) # Remove the file extension
changes_list <- sapply(changes_list, basename) # fetch the basename of each file
ssp245_names <- paste0(changes_list,"_ssp245") 
ssp585_names <- paste0(changes_list,"_ssp585") 
## Create interleaved element list
idx <- order(c(seq_along(ssp245_names), seq_along(ssp585_names)))
ssp_names<- unlist(c(ssp245_names,ssp585_names))[idx]
names(Changes1) <- ssp_names


Changes2 <- rast("./input/AGRA_tiff_nomask/eth/future_extremes/txege35_model_annual_change_EastAfr.nc")
crs(Changes2)  <- "epsg:4326"
names(Changes2) <- c('txge35_ann_change_ssp245', 'txge35_ann_change_ssp585')

# Resampling
Changes1_coffee <- resample(Changes1, r_dummy) #downsampling to climate pixels
Changes2_coffee <- resample(Changes2, r_dummy) #downsampling to climate pixels

Changes_coffee <- c(Changes1_coffee, Changes2_coffee)
plot(Changes_coffee)


# Stack rainfall extremes
rainfall_extremes_hist_chang<- c(Hist_Extreme_coffee["r01"],Changes_coffee["r1_model_annual_change_EastAfr_ssp585"],
                                 Hist_Extreme_coffee["r10"],Changes_coffee["r10_model_annual_change_EastAfr_ssp585"],
                                 Hist_Extreme_coffee["r20"],Changes_coffee["r20_model_annual_change_EastAfr_ssp585"],
                                 Hist_Extreme_coffee["cdd_jjas"],Changes_coffee["cdd_model_jjas_change_EastAfr_ssp585"],
                                 Hist_Extreme_coffee["sdii"],Changes_coffee["sdii_model_annual_change_EastAfr_ssp585"]
                                 )


# Extract rainfall extremes per AEZ, Region and District for coffee areas
rainfall_extremes_hist_chang_aez_reg_woreda <- exact_extract(rainfall_extremes_hist_chang,st_as_sf(fsrp_woreda_vect),fun="mean",append_cols=c("MAEZ.x","ADM1_EN","ADM3_EN"))
rainfall_extremes_hist_chang_aez_reg_woreda<- bind_rows(rainfall_extremes_hist_chang_aez_reg_woreda)
rainfall_extremes_hist_chang_aez_reg_woreda_coffee<- rainfall_extremes_hist_chang_aez_reg_woreda%>% 
  filter(MAEZ.x %in% c("M2","H2","SH1","SH2"))# Choose only AEZ inside coffee growing areas
write.csv(rainfall_extremes_hist_chang_aez_reg_woreda_coffee,"./output/coffee/rf_extremes_aez_reg_woreda.csv")

# Stack temperature extremes
temp_extremes_hist_chang<- c(Hist_Extreme_coffee["txge35"],Changes_coffee["txge35_ann_change_ssp585"],
                             subset(Hist_Extreme_coffee["tr"],1:1),subset(Changes_coffee["tr_model_annual_change_EastAfr_ssp585"],2:2),
                                 Hist_Extreme_coffee["dtr"],Changes_coffee["dtr_model_annual_change_EastAfr_ssp585"]
                             )

temp_extremes_hist_chang_aez_reg_woreda <- exact_extract(temp_extremes_hist_chang,st_as_sf(fsrp_woreda_vect),fun="mean",append_cols=c("MAEZ.x","ADM1_EN","ADM3_EN"))
temp_extremes_hist_chang_aez_reg_woreda<- bind_rows(temp_extremes_hist_chang_aez_reg_woreda)
temp_extremes_hist_chang_aez_reg_woreda_coffee <- temp_extremes_hist_chang_aez_reg_woreda%>% 
  filter(MAEZ.x %in% c("M2","H2","SH1","SH2"))# Choose only AEZ inside coffee growing areas
write.csv(temp_extremes_hist_chang_aez_reg_woreda_coffee,"./output/coffee/temp_extremes_aez_reg_woreda.csv")


# Bar plots of CWD and CDD per FSRP
re <- readxl::read_xlsx("output/coffee/Annual/extremes_FSRP.xlsx", sheet = "rf_extremes") %>% as_tibble()
re<-re %>% dplyr::select(c("Woreda","cwd_Change","cdd_Change"))
re_shaped<- re %>% gather("extVariable","Value",-Woreda)
ggplot(re_shaped, aes(x=Woreda,y=Value,fill=extVariable)) +
  geom_bar(stat = "identity", position = "dodge", width = rel(0.5)) +
  labs(x = "Category", y = "Value", fill = "Variable") +
  theme(axis.text.x.bottom = element_text(angle = 90))+
  scale_fill_manual(values = c("cdd_Change" = "blue", "cwd_Change" = "red"))+
  theme_bw()

