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
library(ggplot2)
# library(gdalUtilities)
library(rayshader) #3D plots

setwd("C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023")

# Changes
Changes_list <- list.files("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_projections/", pattern = "_model_annual_change_EastAfr.nc", full.names = TRUE)
Changes_list <- Changes_list[1:8]
ann_Changes <-rast(Changes_list) #stack into one dataset
crs(ann_Changes)  <- "epsg:4326"
changes_list <- sapply(Changes_list, tools::file_path_sans_ext) # Remove the file extension
changes_list <- sapply(changes_list, basename) # fetch the basename of each file
ssp245_names <- paste0(changes_list,"_ssp245") 
ssp585_names <- paste0(changes_list,"_ssp585") 
## Create interleaved element list
idx <- order(c(seq_along(ssp245_names), seq_along(ssp585_names)))
ssp_names<- unlist(c(ssp245_names,ssp585_names))[idx]
names(ann_changes) <- c("dtr_model_annual_change_EastAfr_ssp245", "dtr_model_annual_change_EastAfr_ssp585",   
                        "precp_model_annual_change_EastAfr_ssp245", "precp_model_annual_change_EastAfr_ssp585", 
                        "r1_model_annual_change_EastAfr_ssp245","r1_model_annual_change_EastAfr_ssp585",    
                        "r10_model_annual_change_EastAfr_ssp245","r10_model_annual_change_EastAfr_ssp585",   
                        "r20_model_annual_change_EastAfr_ssp245","r20_model_annual_change_EastAfr_ssp585",   
                        "sdii_model_annual_change_EastAfr_ssp245","sdii_model_annual_change_EastAfr_ssp585",  
                        "tasmax_model_annual_change_EastAfr_ssp245","tasmax_model_annual_change_EastAfr_ssp585",
                        "tasmin_model_annual_change_EastAfr_ssp245","tasmin_model_annual_change_EastAfr_ssp585")

#load the prep, tmin and tmax data
prep_ann_changes = rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_projections/precp_model_annual_change_EastAfr.nc")
prep_ann_changes
names(prep_ann_changes)<-c("prep_ann_changes_ssp245","prep_ann_changes_ssp585")

tmin_ann_changes = rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_projections/tasmin_model_annual_change_EastAfr.nc")
tmin_ann_changes
names(tmin_ann_changes)<-c("tmin_ann_changes_ssp245","tmin_ann_changes_ssp585")

tmax_ann_changes = rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_projections/tasmax_model_annual_change_EastAfr.nc")
tmax_ann_changes
names(tmax_ann_changes)<-c("tmax_ann_changes_ssp245","tmax_ann_changes_ssp585")

# Calculate cv changes
cv_prep_ann_h = rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_observed/precp_annual_cv_EastAfr.nc")
cv_prep_ann_f = rast("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/AGRA/outputs/full_data/outnc_projections/precp_model_annual_cv_EastAfr.nc")
cv_prep_ann_f = resample(cv_prep_ann_f,cv_prep_ann_h) # downscale future data
cv_prep_ann_changes = cv_prep_ann_f -cv_prep_ann_h
cv_prep_ann_changes = subset(cv_prep_ann_changes,2:3) #extract the first two bands
names(cv_prep_ann_changes) = c("cv_prep_ann_changes_ssp245","cv_prep_ann_changes_ssp585")

# Export the cv changes raster
writeRaster(cv_prep_ann_changes["cv_prep_ann_changes_ssp585"],"C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/Python/Future/Precipitation/FIGURES/rf_cv_changes_ssp585.tif", overwrite=TRUE)

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
legend("topleft", legend = "ShapeFile 1", bty = "n", col="black", lwd=1)

# Add the second shapefile on top of the first one, but only draw borders (no fill)
plot(fsrp_woreda, add = TRUE, border = "red", lwd = 0.5)
legend("topright", legend = "ShapeFile 2", bty ="n" ,col="red" ,lwd=0.5)


# stack rasters
changes =  c(prep_ann_changes["prep_ann_changes_ssp585"],tmin_ann_changes["tmin_ann_changes_ssp585"],tmax_ann_changes["tmax_ann_changes_ssp585"],tmin_ann_changes["tmin_ann_changes_ssp585"])


# Extract change prep, tmin and tmax data per region and per FSRP district
ann_changes_reg_woreda <- exact_extract(changes,st_as_sf(fsrp_woreda_vect),include_cols=c("ADM1_EN","ADM3_EN"))
ann_changes_reg_woreda<- bind_rows(ann_changes_reg_woreda)
write.csv(ann_changes_reg_woreda, "C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/changes_reg_woreda.csv")

# Extract cv changes data per region and per FSRP district
cv_prep_ann_changes_reg_woreda<-exact_extract(cv_prep_ann_changes,st_as_sf(fsrp_woreda_vect),include_cols=c("ADM1_EN","ADM3_EN")) 
cv_prep_ann_changes_reg_woreda<- bind_rows(cv_prep_ann_changes_reg_woreda)

# Merge cv with climate variable data
cv_prep_ann_changes_reg_woreda<- cv_prep_ann_changes_reg_woreda %>%dplyr::select(-c("cv_prep_ann_changes_ssp245","coverage_fraction")) #remove coverage
ann_changes_reg_woreda_final<- ann_changes_reg_woreda%>%inner_join(cv_prep_ann_changes_reg_woreda)
ann_changes_reg_woreda_final<- dplyr::select(ann_changes_reg_woreda_final,-coverage_fraction)
write.csv(ann_changes_reg_woreda_final, "C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/changes_reg_woreda.csv")


# Load the RF data
rf_cv<- read.csv("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/ann_changes_prep_cv.csv")


# Plot prep changes for Oromia
g0<-subset(rf_cv, Region=="Oromia")%>%
  filter(Vars=="prep")%>%
  ggplot(aes(x=rf_cv, y=Woreda))+
  geom_boxplot(width=.5)+
  scale_color_manual(values = c("#636363"))+
  #scale_color_brewer(type = "diverging",palette = "Spectral")+
  #scale_color_gradient(low = "blue", high = "red")+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "none",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=23, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=13),
        axis.text.y=element_text(color="black", size=13),
        axis.line=element_line(color="black", linewidth =0.23)) +
  labs(title="Oromia",x="Annual Changes in Rainfall (mm)",y="FSRP Districts")
g0
ggsave(g0, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/output/coffee/Annual/rf_changes_oromia.png", width = 8, height = 6, dpi = 300, limitsize = FALSE)




# Plot cv changes for Oromia
g1<-subset(rf_cv, Region=="Oromia")%>%
  filter(Vars=="cv")%>%
  ggplot(aes(x=rf_cv, y=Woreda))+
  geom_boxplot(width=.5)+
  scale_color_manual(values = c("#636363"))+
  #scale_color_brewer(type = "diverging",palette = "Spectral")+
  #scale_color_gradient(low = "blue", high = "red")+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "none",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=23, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=13),
        axis.text.y=element_text(color="black", size=13),
        axis.line=element_line(color="black", linewidth =0.23)) +
  labs(title="Oromia",x="Annual Changes in Rainfall CV (%)",y="FSRP Districts")
g1
ggsave(g1, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/output/coffee/Annual/rf_cv_changes_oromia.png", width = 8, height = 6, dpi = 300, limitsize = FALSE)

# Plot prep changes for Other regions
g2<-subset(rf_cv, Region!="Oromia")%>%
  filter(Vars=="cv")%>%
  ggplot(aes(x=rf_cv, y=Woreda))+
  geom_boxplot(width=.5, aes(color=Region))+
  scale_color_manual(values = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99"))+
  #scale_color_gradient(low = "blue", high = "red")+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  labs(title="Others", x="Annual Changes in Rainfall CV (%)",y="FSRP Districts")+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "right",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=22, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=15),
        axis.text.y=element_text(color="black", size=15),
        axis.line=element_line(color="black", size=0.23)) 
  
g2
ggsave(g2, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/rf_cv_changes_otherRegions.png", width = 6, height = 4, dpi = 300, limitsize = FALSE)


g2<-subset(rf_cv, Region=="Oromia")%>%
  filter(Vars=="prep")%>%group_by(Woreda)%>%
  summarize(mean_cv=mean(rf_cv))%>%
  ggplot(aes(x=mean_cv,y=Woreda,color=mean_cv))+
  geom_point(size=2)+
  scale_color_viridis()+
  geom_vline(xintercept = 0, size=0.25, 
  color="red", linetype="dashed")+ theme_bw()+
  labs(x="Annual Changes in rainfall (mm) ",y="FSRP Districts",color="Changes (mm)")
 
g2_3d<- g2 %>%
  plot_gg(flat_transparent_bg = TRUE)
g2_3d
g2_3d + render_snapshot("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/cv_changes.png", clear=TRUE)

# Boxplot for prep and cv for Oromia
rf_cv_oromia<- subset(rf_cv, Region=="Oromia") #extract data for Oromia
g3<-ggplot(rf_cv_oromia, aes(x=rf_cv, y=Woreda)) +
  geom_boxplot(width=.5, aes(color=Vars))+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "none",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=10, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=15),
        axis.text.y=element_text(color="black", size=15),
        axis.line=element_line(color="black", size=0.23)) +
  labs(x="Annual Changes in Rainfall(mm) and CV(%) ",y="FSRP Districts")
g3
# Boxplot for prep and cv for other regions
rf_cv_others<- subset(rf_cv, Region!="Oromia") #extract data for Others
g4<-ggplot(rf_cv_others, aes(x=rf_cv, y=Woreda)) +
  geom_boxplot(width=.5, aes(color=Region))+
  facet_wrap(~Vars)+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        legend.position = "top",
        plot.title = element_text(hjust=0, size=10, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=8),
        axis.text.y=element_text(color="black", size=8),
        axis.line=element_line(color="black", size=0.23)) +
  labs(x="Annual Changes in Rainfall(mm) and CV(%) ",y="FSRP Districts")
ggsave(g4, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/rainfall_changes_reg_woreda.png", width = 12, height = 6, dpi = 300, limitsize = FALSE)


# Load temps data for Oromia
temp <- read.csv("C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/ann_changes_temps.csv")
colnames(temp)<- c("Regions","Woreda","Temp","Temperature")
g4<-subset(temp, Regions=="Oromia")%>%
  ggplot(aes(x=Temp, y=Woreda))+
  geom_boxplot(width=.5, aes(color=Temperature))+
  xlim(1,1.8)+
  scale_color_manual(values=c("green","#998ec3"))+
  #scale_color_gradient(low = "blue", high = "red")+
  theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "none",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=22, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=13),
        axis.text.y=element_text(color="black", size=13),
        axis.line=element_line(color="black", linewidth =0.23)) +
  labs(title="Oromia", x="Annual Changes in temperatures (°C)",y="FSRP Districts")
g4
ggsave(g4, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/Code/R/GCA_2023/output/coffee/Annual/temp_changes_oromia.png", width = 8, height = 6, dpi = 300, limitsize = FALSE)



# Load temps data for other regions
g5<-subset(temp, Regions!="Oromia")%>%
  ggplot(aes(x=Temp, y=Woreda))+
  geom_boxplot(width=.5, aes(color=Temperature, fill=Regions))+
  scale_color_manual(values=c("green","#998ec3"))+
  scale_fill_manual(values=c("#fef0d9", "#fdcc8a", "#fc8d59","#d7301f"))+
  xlim(1,1.8)+
  #scale_color_gradient(low = "blue", high = "red")+
  geom_vline(xintercept = 0, size=0.25, 
             color="red", linetype="dashed")+ theme_bw() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=18),
        legend.position = "right",
        legend.title = element_text(size =20),
        legend.text = element_text(size=18),
        plot.title = element_text(hjust=0, size=22, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=15),
        axis.text.y=element_text(color="black", size=15),
        axis.line=element_line(color="black", size=0.23))  +
  labs(title="Others", x="Annual Changes in temperatures (°C)",y="FSRP Districts")
g5
ggsave(g5, file="C:/Users/Adama/OneDrive - CIMMYT/Documents/CIMMYT/Projets/Coffee_VC/Results/R/coffee/coffee/temp_changes_otherRegions.png", width = 6, height = 4, dpi = 300, limitsize = F)
