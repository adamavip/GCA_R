library(ggplot2)
library(tidyverse)
library(reshape2)
library(readxl)
library(ggpubr)
library(gridGraphics)
library(gridExtra)
library(dplyr)
#getwd()
setwd("D:/May_2022/Project works/CIMMYT_2023/Data")
##Computing means

I<- read_excel("Clim.xlsx", sheet = "Maize_B")
I
I$Region<-as.factor(I$Region)
I$District<-as.factor(I$District)
I$Var<-as.factor(I$Var)

a<-ggplot(data = I, aes(x=Temp, y=District, color=Var)) + 
  geom_boxplot(aes(fill=Var))+
  fill_palette()
a
a<-a+geom_vline(xintercept = 0, size=0.25, 
                color="red", linetype="dashed")

a<-a+facet_wrap(.~Region,  scales = "free_y")

a
a<-a+ xlab('Tmax change [oC]') + 
  ylab('FSRP districts') + xlim(1.1, 1.8)
a
a<-a+theme_bw()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        legend.position = "top",
        plot.title = element_text(hjust=0, size=10, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=8),
        axis.text.y=element_text(color="black", size=8),
        axis.line=element_line(color="black", size=0.23))
a
ggsave(a, file="output/maize/MAM/Tmax_C.png", 
       width = 12, height = 6, dpi = 300, limitsize = FALSE)


#########RF_CV#######
I<- read_excel("Clim.xlsx", sheet = "Sheet2")
I
I$Region<-as.factor(I$Region)
I$District<-as.factor(I$District)
I$Var<-as.factor(I$Var)

a<-ggplot(data = I, aes(x=Vars, y=District)) + 
  geom_boxplot(aes(fill=Var))
  
a<-a+scale_fill_manual(values=c(2,3))
a
a<-a+geom_vline(xintercept = 0, size=0.25, 
                color="red", linetype="dashed")
a<-a+facet_wrap(.~Region,  scales = "free_y")

a
a<-a+ xlab('CV [%] and RF [mm] change') + 
  ylab('FSRP districts') + xlim(-50, 40)
a
a<-a+theme_bw()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        legend.position = "top",
        plot.title = element_text(hjust=0, size=10, color=1),
        legend.box.background = element_rect(colour = "white"),
        strip.background = element_rect(colour = "black", 
                                        fill = "white"),
        axis.text.x=element_text(color="black", size=8),
        axis.text.y=element_text(color="black", size=8),
        axis.line=element_line(color="black", size=0.23))
a
ggsave(a, file="output/maize/MAM/RF_CV_C.png", 
       width = 12, height = 6, dpi = 300, limitsize = FALSE)


