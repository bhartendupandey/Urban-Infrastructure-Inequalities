# This script applies the mann-kendall test to changes in the national-scale between and within inequalities.

rm(list=ls())
gc()
library(sf)
library(reshape2)
#library(cluster)
#library(factoextra)
library(trend)
library(ggplot2)
library(WDI)
library(tidyverse)
#library(plm)
library(stargazer)
library(trend)
library(viridis)
library(cowplot)
library(gridExtra)
library(aTSA)
library(tseries)
library(rworldmap)
source("D:/Paper2/Script/HelperFunctions.R")
# Read Inequality Data

set.seed(123) ## For Bootstrapping
## Change Path
setwd("D:/Paper2/Data")

## Check for Consistency
## Add Country Data
data = getdata("D:/Paper2/Data")

## Datasub for Scale 0.5 and Year 2015
data = data %>% filter(Scale == 0.5)%>% select(-c(Scale))

# 165 countries, While generating Inequality.csv countries were omitted with fewer regions. 
countries = unique(data$Country)
length(countries)

#############################################################################################################
############################################Mann Kendall Test################################################
# Between Regions Inequality Mann Kendall Test

mk_btwn = rep(NA,165)
mk_btwn_tau = rep(NA,165)

for (i in 1:165){
	country = countries[i]
	subdat = subset(data,Country==country)
	mk_btwn[i] = mk.test(subdat$Inequality_btwn)$p.value
	mk_btwn_tau[i] = as.numeric(mk.test(subdat$Inequality_btwn)$estimates[3])
}

# Within Regions Inequality Mann Kendall Test
mk_wtn = rep(NA,165)
mk_wtn_tau = rep(NA,165)
for (i in 1:165){
	country = countries[i]
	subdat = subset(data,Country==country)
	mk_wtn[i] = mk.test(subdat$Inequality_wtn)$p.value
	mk_wtn_tau[i] = as.numeric(mk.test(subdat$Inequality_wtn)$estimates[3])
}


pltdf = data.frame(Countries=countries,mk_btwn,mk_wtn,mk_btwn_tau,mk_wtn_tau)

# Result
# 115 countries out of 165 show a statistically significant trend
possig = dim(pltdf[pltdf$mk_wtn < 0.05 & pltdf$mk_wtn_tau>0,])[1] 
negsig = dim(pltdf[pltdf$mk_wtn < 0.05 & pltdf$mk_wtn_tau<0,])[1]
posinsig = dim(pltdf[pltdf$mk_wtn > 0.05 & pltdf$mk_wtn_tau>0,])[1]
neginsig = dim(pltdf[pltdf$mk_wtn > 0.05 & pltdf$mk_wtn_tau<0,])[1]
print(possig)

# Result
# 140 countries out of 165 show a statistically significant trend, 88.48% countries
possig = dim(pltdf[pltdf$mk_btwn < 0.05 & pltdf$mk_btwn_tau>0,])[1] 
negsig = dim(pltdf[pltdf$mk_btwn < 0.05 & pltdf$mk_btwn_tau<0,])[1]
posinsig = dim(pltdf[pltdf$mk_btwn > 0.05 & pltdf$mk_btwn_tau>0,])[1]
neginsig = dim(pltdf[pltdf$mk_btwn > 0.05 & pltdf$mk_btwn_tau<0,])[1]
print(possig)

############################ PLOT

cols= viridis(165,option="D")

p = ggplot(pltdf,aes(y=mk_wtn,x=mk_wtn_tau)) + geom_point(size=1) + labs(y="p-value",x=expression(tau)) + theme_classic() + 
	theme(axis.text=element_text(size=12),axis.title=element_text(size=15)) + 
	annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.05,fill=cols[possig],alpha=0.6,color="gray20")  + 
      annotate("rect", xmin = 0, xmax = 1, ymin = 0.05, ymax = 1 , fill= cols[posinsig],alpha=0.6,color="gray20") + 
      annotate("rect", xmin = -1, xmax = 0, ymin = 0.05, ymax = 1, fill= cols[neginsig],alpha=0.6,color="gray20") + 
      annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 0.05, fill= cols[negsig],alpha=0.6,color="gray20") + geom_point(size=1) +
	scale_y_sqrt(breaks=c(0,0.01, 0.05,0.1, 1)) + ggtitle(expression(Inequality[within]))
p

df= data.frame(x=rep(1,165),y=1:165,col=cols)
colorramp= ggplot(df,aes(x,y,fill=y)) + geom_tile(alpha=0.6)  + theme_classic() + 
	theme(legend.position="none",
	axis.text.x=element_blank(),
	axis.ticks.x=element_blank(),
	axis.line.x=element_blank(),
	axis.line.y=element_blank(),
	axis.title.y=element_text(size=14)) + 
	scale_fill_viridis()  + 
	scale_y_continuous(breaks=c(possig,negsig,posinsig,neginsig)) + 
	labs(x="",y="Number of Countries")

p1 = plot_grid(p, colorramp, align = "h", ncol= 2, rel_widths = c(10.5/12, 1.5/12))

possig1 = dim(pltdf[pltdf$mk_btwn < 0.05 & pltdf$mk_btwn_tau>0,])[1] 
negsig1 = dim(pltdf[pltdf$mk_btwn < 0.05 & pltdf$mk_btwn_tau<0,])[1]
posinsig1 = dim(pltdf[pltdf$mk_btwn > 0.05 & pltdf$mk_btwn_tau>0,])[1]
neginsig1 = dim(pltdf[pltdf$mk_btwn > 0.05 & pltdf$mk_btwn_tau<0,])[1]

p = ggplot(pltdf,aes(y=mk_btwn,x=mk_btwn_tau)) +  labs(y="p-value",x=expression(tau)) + theme_classic() + 
	theme(axis.text=element_text(size=12),axis.title=element_text(size=15)) + 
	annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 0.05,fill=cols[possig1],alpha=0.6,color="gray20")  + 
      annotate("rect", xmin = 0, xmax = 1, ymin = 0.05, ymax = 1 , fill= cols[posinsig1],alpha=0.6,color="gray20") + 
      annotate("rect", xmin = -1, xmax = 0, ymin = 0.05, ymax = 1, fill= cols[neginsig1],alpha=0.6,color="gray20") + 
      annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 0.05, fill= cols[negsig1],alpha=0.6,color="gray20") + geom_point(size=1) +
	scale_y_sqrt(breaks=c(0,0.01, 0.05,0.1, 1))+ ggtitle(expression(Inequality[between]))
p

df= data.frame(x=rep(1,165),y=1:165,col=cols)
colorramp= ggplot(df,aes(x,y,fill=y)) + geom_tile(alpha=0.6)  + theme_classic() + 
	theme(legend.position="none",
	axis.text.x=element_blank(),
	axis.ticks.x=element_blank(),
	axis.line.x=element_blank(),
	axis.line.y=element_blank(),
	axis.title.y=element_text(size=14)) + 
	scale_fill_viridis()  + 
	scale_y_continuous(breaks=c(possig1,negsig1,posinsig1,neginsig1)) + 
	labs(x="",y="Number of Countries")



p2 = plot_grid(p, colorramp, align = "h", ncol= 2, rel_widths = c(10.5/12, 1.5/12))

pltout = grid.arrange(p1,p2,ncol=2)
ggsave("D:/Paper2/Figures/SI_mktrendtest_12_5.pdf",pltout,width=12,height=5,useDingbats=FALSE,family="sans")

#########################################################################################################
###################################ADF TEST##########################################################
# ADF results discussed in the paper.

# Between Regions Inequality ADF

datamap = data[,c("GID_0","Country")]
datamap = datamap %>% distinct()

datamap$UnitRootBetween = NA

adf_btwn = rep(NA,165)

for (i in 1:165){
	country = countries[i]
	subdat = subset(data,Country==country)
	adf_btwn[i] = adf.test(subdat$Inequality_btwn)$p.value
	if(adf_btwn[i] > 0.05){
		datamap$UnitRootBetween[datamap$Country == country] = "Yes"
	}else{
		datamap$UnitRootBetween[datamap$Country == country] = "No"

	}
}
sum(adf_btwn > 0.05) / 165

#no of countries with unit root
sum(adf_btwn > 0.05)

# Result
# 157 countries out of 165 show a statistically significant trend, 88.48% countries

# Within Regions Inequality ADF Test
adf_wtn = rep(NA,165)
datamap$UnitRootWithin = NA

for (i in 1:165){
	country = countries[i]
	subdat = subset(data,Country==country)
	adf_wtn[i] = adf.test(subdat$Inequality_wtn)$p.value
	if(adf_wtn[i] > 0.05){
		datamap$UnitRootWithin[datamap$Country == country] = "Yes"
	}else{
		datamap$UnitRootWithin[datamap$Country == country] = "No"
	}
}
sum(adf_wtn > 0.05) / 165
sum(adf_wtn > 0.05)

sum(adf_wtn > 0.05 & adf_btwn > 0.05)

# Result
# 121 countries out of 165 show a statistically significant trend, 73.33% countries

## Plot datamap


sPDF <- joinCountryData2Map(datamap,joinCode = "ISO3",nameJoinColumn = "GID_0")
f0 = "D:/Paper2/Figures/SI_UnitRoot_BtwnIneq.pdf"

mapDevice(device="pdf",file=f0) #create world map shaped window
map1 = mapCountryData(sPDF,nameColumnToPlot='UnitRootBetween',
catMethod="categorical",borderCol="white",oceanCol="lightblue",missingCountryCol="gray",
mapTitle="Unit Root: Between Inequality",colourPalette=viridis(2,option="E"))
dev.off()
f1 = "D:/Paper2/Figures/SI_UnitRoot_WtnIneq.pdf"
mapDevice(device="pdf",file=f1)
map2 = mapCountryData(sPDF,nameColumnToPlot='UnitRootWithin',
catMethod="categorical",borderCol="white",oceanCol="lightblue",missingCountryCol="gray",
mapTitle="Unit Root: Within Inequality",colourPalette=viridis(2,option="E"))
dev.off()










