# This script clusters countries based on their time trajectories of Between and Within Inequality
rm(list=ls())
library(reshape2)
library(tidyverse)
library(cluster)
library(ggplot2)
library(viridis)
library(ggrepel)
library(sf)
library(gridExtra)
source("D:/Paper2/Script/HelperFunctions.R")
setwd("D:/Paper2/Data")

data = getdata("D:/Paper2/Data")

## Datasub for Scale 0.5 and Year 2015
data = data %>% filter(Scale == 0.5) %>% select(-c(Scale))
head(data)

#BETWEEN_INEQUALITY
clustdata = data[,c(1,2,4,5)]
clustdata = dcast(clustdata, Country + GID_0 ~ Year, value.var="Inequality_btwn")
foest  = rep(NA,nrow(clustdata))
soest  = rep(NA,nrow(clustdata))
r2  = rep(NA,nrow(clustdata))
for(i in 1:nrow(clustdata)){
  sub= clustdata[i,]
  sub = as.numeric(sub[,-c(1,2)])
  year = 2000:2019
  mod = lm(sub~poly(year,2))
  smod = summary(mod)
  coefs = coefficients(mod)
  if(as.numeric(smod$coefficients[2,4])<0.05){
  	foest[i] = as.numeric(coefs[2])
  }else{
	foest[i] = as.numeric(coefs[2])
  }
  if(as.numeric(smod$coefficients[3,4])<0.05){
  	soest[i] = as.numeric(coefs[3])
  }else{
	soest[i] = as.numeric(coefs[3])
  }
  r2[i] = smod$r.squared
 }

pltdf= data.frame(Country=clustdata[,1],GID=clustdata[,2],foest,soest)
sum(pltdf$foest > 0 & pltdf$soest > 0)

clustout2= ggplot(pltdf,aes(foest,soest,label=Country,col=r2)) +
geom_point(size=2,alpha=0.75) + 
geom_text_repel(data=subset(pltdf,Country %in% c("Iraq","Angola","Vietnam","Thailand","Cambodia","Libya","Malaysia","India","China","Nigeria","Syria","Yemen")),col="gray20",size=4) +
xlab("First-order Trend Estimate") + ylab("Second-order Trend Estimate") + scale_color_viridis(option="C") +
theme_bw() + 
labs(col=expression(R^2)) + 
theme(legend.position=c(0.9,0.3),axis.text=element_text(size=10),axis.title=element_text(size=14)) + xlim(-0.3,0.5) +
annotate(geom="text", x=0.3, y=0.18, label="Increasing at Increasing Rate",
              color="gray30",size=4,fontface =2) +
annotate(geom="text", x=0.3, y=-0.27, label="Increasing at Decreasing Rate",
              color="gray30",size=4,fontface =2) +
annotate(geom="text", x=-0.18, y=0.18, label="Decreasing at Decreasing Rate",
              color="gray30",size=4,fontface =2) +
annotate(geom="text", x=-0.18, y=-0.27, label="Decreasing at Increasing Rate",
              color="gray30",size=4,fontface =2)+
geom_vline(xintercept=0,lty=2) +
geom_hline(yintercept=0,lty=2) +
ggtitle(expression(Inequality[between])) +
theme(plot.title = element_text(size = 20, face = "bold"))
##devsize = 10.222222  8.055556

#Within_INEQUALITY
clustdata = data[,c(1,2,3,5)]
clustdata = dcast(clustdata, Country + GID_0 ~ Year, value.var="Inequality_wtn")
foest  = rep(NA,nrow(clustdata))
soest  = rep(NA,nrow(clustdata))
r2  = rep(NA,nrow(clustdata))
for(i in 1:nrow(clustdata)){
  sub= clustdata[i,]
  sub = as.numeric(sub[,-c(1,2)])
  year = 2000:2019
  mod = lm(sub~poly(year,2))
  smod = summary(mod)
  coefs = coefficients(mod)
  if(as.numeric(smod$coefficients[2,4])<0.05){
  	foest[i] = as.numeric(coefs[2])
  }else{
	foest[i] = as.numeric(coefs[2])
  }
  if(as.numeric(smod$coefficients[3,4])<0.05){
  	soest[i] = as.numeric(coefs[3])
  }else{
	soest[i] = as.numeric(coefs[3])
  }
 	r2[i] = smod$r.squared
 }

pltdf= data.frame(Country=clustdata[,1],GID=clustdata[,2],foest,soest)
sum(pltdf$foest > 0 & pltdf$soest > 0)

clustout1= ggplot(pltdf,aes(foest,soest,label=Country,col=r2)) +
geom_point(size=2,alpha=0.75) + 
geom_text_repel(data=subset(pltdf,Country %in% c("Iraq","Malawi","Somalia","Angola","Vietnam","Thailand","Cambodia","Libya","Malaysia","India","China","Nigeria","Syria","Yemen")),col="gray20",size=4) +
xlab("First-order Trend Estimate") + ylab("Second-order Trend Estimate") + scale_color_viridis(option="C") +
theme_bw() + 
labs(col=expression(R^2)) + 
theme(legend.position=c(0.9,0.3),axis.text=element_text(size=10),axis.title=element_text(size=14)) + xlim(-0.3,0.5) +
annotate(geom="text", x=0.3, y=0.18, label="Increasing at Increasing Rate",
              color="gray30",size=4,fontface=2) +
annotate(geom="text", x=0.3, y=-0.27, label="Increasing at Decreasing Rate",
              color="gray30",size=4,fontface=2) +
annotate(geom="text", x=-0.18, y=0.18, label="Decreasing at Decreasing Rate",
              color="gray30",size=4,fontface=2) +
annotate(geom="text", x=-0.18, y=-0.27, label="Decreasing at Increasing Rate",
              color="gray30",size=4,fontface=2)+
geom_vline(xintercept=0,lty=2) +
geom_hline(yintercept=0,lty=2) + 
ggtitle(expression(Inequality[within])) +
theme(plot.title = element_text(size = 20, face = "bold"))

grid.arrange(clustout1,clustout2,ncol=2)

## Multi-scale:
data = read.csv("./InequalityData/Inequality_scale.csv") %>% select(-c(1))
gadm = read_sf("./GADM/gadm36_0.shp") %>% as.data.frame(.) %>% select(-geometry)
colnames(gadm)[2] = "Country"
gadm = gadm[,-3]
data = data %>% left_join(gadm, by="Country")

mstrend = function(clustdata,scale){
  clustdata$Scale = as.character(clustdata$Scale)
  clustdata = subset(clustdata,Scale==as.character(scale))
  foest  = rep(NA,nrow(clustdata))
  soest  = rep(NA,nrow(clustdata))
  r2  = rep(NA,nrow(clustdata))
  for(i in 1:nrow(clustdata)){
    sub= clustdata[i,]
    sub = as.numeric(sub[,-c(1,2,3)])
    year = 2000:2019
    mod = lm(sub~poly(year,2))
    smod = summary(mod)
    coefs = coefficients(mod)
    if(as.numeric(smod$coefficients[2,4])<0.05){
      foest[i] = as.numeric(coefs[2])
    }else{
      foest[i] = as.numeric(coefs[2])
    }
    if(as.numeric(smod$coefficients[3,4])<0.05){
      soest[i] = as.numeric(coefs[3])
    }else{
      soest[i] = as.numeric(coefs[3])
    }
    r2[i] = smod$r.squared
  }
  
  pltdf= data.frame(Country=clustdata[,1],GID=clustdata[,2],foest,soest)
  out1 = sum(pltdf$foest > 0 & pltdf$soest > 0)*100/dim(pltdf)[1]
  out2 = sum(pltdf$foest > 0)*100/dim(pltdf)[1]
  return(c(out1,out2))
}

#Between

clustdata = data[,c(1,2,4,5,6)]
clustdata = dcast(clustdata, Country + GID_0 + Scale ~ Year, value.var="Inequality_btwn")
outdfbw = data.frame()
for(j in seq(0.1,5,0.1)){
  outdfbw = rbind(outdfbw,c(j,mstrend(clustdata,j)))
}
colnames(outdfbw) = c("Scale","Increasing at Increasing Rates","Increasing")

# Within

clustdata = data[,c(1,2,3,5,6)]
clustdata = dcast(clustdata, Country + GID_0 + Scale ~ Year, value.var="Inequality_wtn")
outdfw = data.frame()
for(j in seq(0.1,5,0.1)){
  outdfw = rbind(outdfw,c(j,mstrend(clustdata,j)))
}
colnames(outdfw) = c("Scale","Increasing at Increasing Rates","Increasing")

outdfw1 = outdfw %>% pivot_longer(!Scale,names_to="Inequality")
head(outdfw1)

pltw = ggplot(outdfw1,aes(x=Scale,y=value,color=Inequality)) + geom_point() + ylim(0,100) + theme_bw() + 
xlab(expression(Spatial * " " * Scale * " " * (degree))) + ylab("% Countries") + 
  theme(legend.position=c(0.7,0.2),axis.text=element_text(size=12),axis.title=element_text(size=14),
        legend.text=element_text(size=12)) 

# Between

clustdata = data[,c(1,2,4,5,6)]
clustdata = dcast(clustdata, Country + GID_0 + Scale ~ Year, value.var="Inequality_btwn")
outdfbw = data.frame()
for(j in seq(0.1,5,0.1)){
  outdfbw = rbind(outdfbw,c(j,mstrend(clustdata,j)))
}
colnames(outdfbw) = c("Scale","Increasing at Increasing Rates","Increasing")

outdfbw1 = outdfbw %>% pivot_longer(!Scale,names_to="Inequality")
head(outdfbw1)

pltbw = ggplot(outdfbw1,aes(x=Scale,y=value,color=Inequality)) + geom_point() + ylim(0,100) + theme_bw() + 
  xlab(expression(Spatial * " " * Scale * " " * (degree))) + ylab("% Countries") + 
  theme(legend.position=c(0.7,0.2),axis.text=element_text(size=12),axis.title=element_text(size=14),
        legend.text=element_text(size=12))  


dev.new(width= 14,height=  12)
#out= grid.arrange(clustout1,clustout2,pltw,pltbw,nrow=2,ncol=2)
out = grid.arrange(clustout1,clustout2,pltw,pltbw,
  widths = c(2, 2),
  layout_matrix = rbind(c(1, 2),
                        c(1, 2),
				c(3, 4))
)
ggsave("D:/Paper2/Figures/Figure3.pdf",out,width=16,height=12)


















