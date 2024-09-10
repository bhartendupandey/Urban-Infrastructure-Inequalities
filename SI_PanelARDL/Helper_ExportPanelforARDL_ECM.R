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

data = appendlongrunWBdata(data,"D:/Paper2/Data")

colnames(data)
datab = balanced(as.data.frame(data),"Country","Year")
dim(datab)
write.csv(datab,
"D:/Paper2/Data/OutputData/panel.csv")
