rm(list=ls())
gc()
library(ggplot2)
library(tidyverse)
library(rcartocolor)
library(sf)
library(rfinterval)
library(ggpubr)
library(gridExtra)

source("D:/Paper2/Script/HelperFunctions.R")
setwd("D:/Paper2/Data")

datab = gettraining("D:/Paper2/Data")
datab$GDP = datab$GDP / datab$Population ## GDP is per-capita

# Train Model for upto 2015; INEQUALITY BETWEEN
countries = as.character(unique(datab$Country))
datats = NULL

for (j in 2000:2015){
	dataf = NULL
	for (i in 1:length(countries)){
		country = countries[i]
		sub = subset(datab,Country==country)
		pred = sub[sub$Year==j,"Inequality_wtn"]
		GDP = sub[sub$Year==j ,"GDP"]
		Urbanization = sub[sub$Year==j,"Urbanization"]
		region = sub[sub$Year==j,"region"]
		land = sub[sub$Year==j,"land"]
		dataf = rbind(dataf,data.frame(Country=country,pred,GDP=log(GDP),Urbanization,region,land))
	}
datats = rbind(datats,dataf)
}

datats = datats[complete.cases(datats),]


# TEST RF Interval based training, TRAINING ACCURACY, CHECK for BIAS
# https://www.tandfonline.com/doi/abs/10.1080/00031305.2019.1585288?journalCode=utas20 
set.seed(123)
rffitint = rfinterval(pred~GDP + Urbanization + land+region, train_data=datats,test_data=datats,method= "quantreg",symmetry = T)
summary(lm(rffitint$testPred~rffitint$test_data$pred))

#Test Model for Time series Predictions
# 2016

datatstest = preptestdata1(2016,datab)
datatspredict2016mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=datatstest,method="quantreg",symmetry =T)
datatspredict2016 = datatspredict2016mod$testPred

#Plot predicted 2016 versus actual 2016
pltdf_2016 = data.frame(Predicted=datatspredict2016,Observed=datatstest$pred)
plt_2016= ggplot(pltdf_2016,aes(x=Observed,y=Predicted)) + geom_point(col="gray60") + theme_bw() + geom_smooth(method="lm") + 
stat_regline_equation(label.x.npc = "left",label.y.npc = "top",aes(label =  after_stat(adj.rr.label)),size=5) + 
theme(axis.title=element_text(size=16),axis.text = element_text(size=14))

#2017

datatstest = preptestdata1(2017,datab)
datatspredict2017mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=datatstest,method="quantreg",symmetry =T)
datatspredict2017 = datatspredict2017mod$testPred

#Plot predicted 2017 versus actual 2017
pltdf_2017 = data.frame(Predicted=datatspredict2017,Observed=datatstest$pred)
plt_2017= ggplot(pltdf_2017,aes(x=Observed,y=Predicted)) + geom_point(col="gray60") + theme_bw() + geom_smooth(method="lm") + 
stat_regline_equation(label.x.npc = "left",label.y.npc = "top",aes(label =  after_stat(adj.rr.label)),size=5) + 
theme(axis.title=element_text(size=16),axis.text = element_text(size=14))


#2018

datatstest = preptestdata1(2018,datab)
datatspredict2018mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=datatstest,method="quantreg",symmetry =T)
datatspredict2018 = datatspredict2018mod$testPred

#Plot predicted 2018 versus actual 2018
pltdf_2018 = data.frame(Predicted=datatspredict2018,Observed=datatstest$pred)
plt_2018= ggplot(pltdf_2018,aes(x=Observed,y=Predicted)) + geom_point(col="gray60") + theme_bw() + geom_smooth(method="lm") + 
stat_regline_equation(label.x.npc = "left",label.y.npc = "top",aes(label =  after_stat(adj.rr.label)),size=5) + 
theme(axis.title=element_text(size=16),axis.text = element_text(size=14))


#2019

datatstest = preptestdata1(2019,datab)
datatspredict2019mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=datatstest,method="quantreg",symmetry =T)
datatspredict2019 = datatspredict2019mod$testPred

#Plot predicted 2019 versus actual 2019
pltdf_2019 = data.frame(Predicted=datatspredict2019,Observed=datatstest$pred)
plt_2019= ggplot(pltdf_2019,aes(x=Observed,y=Predicted)) + geom_point(col="gray60") + theme_bw() + geom_smooth(method="lm") + 
stat_regline_equation(label.x.npc = "left",label.y.npc = "top",aes(label =  after_stat(adj.rr.label)),size=5) + 
theme(axis.title=element_text(size=16),axis.text = element_text(size=14))

figout = grid.arrange(plt_2016,plt_2017,plt_2018,plt_2019)
ggsave("D:/Paper2/Figures/rf_forecasting model_validation_within.pdf",figout)

#SSP1

sspurban = getsspUrbn("D:/Paper2/Data")
sspgdppc = getsspGDPpc("D:/Paper2/Data")

Forecast_wtn("SSP1",datats,sspurban,sspgdppc,2030,datab)
Forecast_wtn("SSP2",datats,sspurban,sspgdppc,2030,datab)
Forecast_wtn("SSP3",datats,sspurban,sspgdppc,2030,datab)
Forecast_wtn("SSP4",datats,sspurban,sspgdppc,2030,datab)
Forecast_wtn("SSP5",datats,sspurban,sspgdppc,2030,datab)

Forecast_wtn("SSP1",datats,sspurban,sspgdppc,2040,datab)
Forecast_wtn("SSP2",datats,sspurban,sspgdppc,2040,datab)
Forecast_wtn("SSP3",datats,sspurban,sspgdppc,2040,datab)
Forecast_wtn("SSP4",datats,sspurban,sspgdppc,2040,datab)
Forecast_wtn("SSP5",datats,sspurban,sspgdppc,2040,datab)

Forecast_wtn("SSP1",datats,sspurban,sspgdppc,2050,datab)
Forecast_wtn("SSP2",datats,sspurban,sspgdppc,2050,datab)
Forecast_wtn("SSP3",datats,sspurban,sspgdppc,2050,datab)
Forecast_wtn("SSP4",datats,sspurban,sspgdppc,2050,datab)
Forecast_wtn("SSP5",datats,sspurban,sspgdppc,2050,datab)