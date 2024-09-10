# This script runs a cross-sectional analysis of Inequality (within and between) and provides nuances discussed in the paper.

rm(list=ls())
library(tidyverse)
library(WDI)
library(sf)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(wesanderson)
library(boot)
library(gridExtra)
library(viridis)
library(ggrepel)

set.seed(123) ## For Bootstrapping
source("D:/Paper2/Script/HelperFunctions.R")

## Change Path
setwd("D:/Paper2/Data")

## Load Inequality Data
data = getdata("D:/Paper2/Data")

## Datasub for Scale 0.5 and Year 2015
datasub = data %>% filter(Scale == 0.5) %>% filter(Year == 2015)

## Append WB Data
datasub = append2015WBdata(datasub,"D:/Paper2/Data")

datasub = datasub [complete.cases(datasub),]
datasubcountries = datasub %>% pull(Country)

cor_ineq_land = ggplot(datasub,aes(x=Inequality_wtn,y=Inequality_btwn,color=land)) + geom_point(size=0.9) + theme_bw() + 
labs(y=expression(Inequality[between]),x=expression(Inequality[within])) + 
theme(axis.title=element_text(size=16),axis.text=element_text(size=13)) + geom_smooth(method="lm",se=F,color="red") + 
stat_cor(label.x.npc = 0.7,label.y.npc = 0.03,size=3,col="gray20",p.digits=0) + 
geom_abline(slope=1,intercept=0,color="gray")  + geom_text_repel(aes(label=Country)) + scale_color_viridis(name=expression("(log) Area " * km^2)) + 
theme(legend.position=c(0.2,0.7))

f0 = "D:/Paper2/Figures/SI_corineq_land_1.pdf"
ggsave(f0,cor_ineq_land,width=12,height=12,useDingbats=FALSE,family="sans")

plt1 = ggplot(datasub,aes(x= land,y=Inequality_wtn,label=Country,col=Urbanization)) + 
geom_point() +theme_bw() + labs(y = expression(Inequality[within]),x="log Land Area") + 
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),legend.position="none") + 
stat_cor(label.x.npc = "left",label.y.npc = "top",size=5,col="gray",p.digits=2,method="spearman") + 
scale_color_viridis(option="B")+ ylim(0,1)

plt2 = ggplot(datasub,aes(x=land,y=Inequality_btwn,label=Country,col=Urbanization)) + 
geom_point() +theme_bw() + labs(y = expression(Inequality[between]),x="log Land Area") + 
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),legend.position=c(0.2,0.6)) + 
stat_cor(label.x.npc = "left",label.y.npc = "top",size=5,col="gray",p.digits=2,method="spearman") + 
scale_color_viridis(option="B") + ylim(0,1)

dev.new(width = 8,height=4)
outputplot = grid.arrange(plt1,plt2,ncol=2)
f1 = "D:/Paper2/Figures/SI_corineq_land_2.pdf"
ggsave(f1,outputplot,width=10,height=4,useDingbats=FALSE,family="sans")

