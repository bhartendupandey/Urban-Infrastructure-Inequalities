rm(list=ls())
gc()
setwd("D:/Paper2/")
source("./Script/HelperFunctions.R")
library(sf)
library(ggplot2)
sf_use_s2(FALSE)
library(Hmisc)
library(tidyverse)
data11=read_sf("./Script/05degby05deg_GADM.gpkg","05degby05deg_gadm")

dfiles = list.files("./Data/Fishnet_Arc_DMSP/Compiled/",pattern="*.gpkg$",full.names=T)
vfiles = list.files("./Data/Fishnet_Arc_VIIRS_25/Compiled/",pattern="*.gpkg$",full.names=T)
lfiles = list.files("./Data/Fishnet_Arc_LS/Compiled/",pattern="*.gpkg$",full.names=T)

outdf = data.frame()
degres = seq(0.1,5,0.1)

for(i in 1:50){
  #i = 15
  dmsp = read_sf(dfiles[i])
  #colnames(dmsp)[15:90] = colnames(data11)[101:176]
  viirs = read_sf(vfiles[i])
  #colnames(viirs)[15:46] = colnames(data11)[69:100]
  lscan = read_sf(lfiles[i])
  #colnames(lscan)[15:94] = colnames(data11)[177:256]

  viirs = st_drop_geometry(viirs)
  lscan = st_drop_geometry(lscan)
  data = merge(dmsp, viirs,by=colnames(dmsp)[1:14])
  data = merge(data,lscan,by=colnames(dmsp)[1:14])
  st_geometry(data) <- "geometry"
  ########################################################
  ### Change VIIRS Column Names to include years
  # Get relevant VIIRS Column Indices and Names
  
  colindex = grep("V_",colnames(data))
  cols=colnames(data)[colindex]
  out = rep(NA,length(cols))
  Years=2000:2019
  # Replace these relevant VIIRS columns by years
  for(y in 1:8){
    index = substr(cols,3,3) %in% as.character(y)
    out[index] = gsub(paste("_",as.character(y),"_",sep=""),paste("_",as.character(Years[12+y]),"_",sep=""),cols[index])
  }
  # Reset Column Names
  colnames(data)[colindex] = out
  data = data%>% group_by(GID_0) %>% summarize(out= n()) %>% st_drop_geometry(.) %>% as.data.frame(.)
  ouut = data.frame(Country = data$GID_0,NumCountries = data$out,Scale = degres[i])
  outdf = rbind(outdf,ouut)
  print(i)
  flush.console()
}

write.csv(outdf,"D:/Paper2/Data/OutputData/scale_numcountries.csv")


outdf= read.csv("D:/Paper2/Data/OutputData/scale_numcountries.csv")
numcountries = outdf %>% group_by(Scale) %>% summarize(NumCountries=sum(NumCountries > 10))


plt = ggplot(numcountries, aes(x=Scale,y=NumCountries)) + geom_point() + theme_bw() +
 labs(y="Number of Countries (>10 grid cells)",x=expression(Spatial * " " * Scale * " "* (degree))) + 
  theme(axis.title=element_text(size=16),axis.text=element_text(size=13))
ggsave("D:/Paper2/Figures/Numcountries.pdf",plt)
