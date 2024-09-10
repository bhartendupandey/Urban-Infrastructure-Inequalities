rm(list=ls())
library(sf)
library(reshape2)
library(trend)
library(ggplot2)
library(WDI)
library(tidyverse)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)

source("D:/Paper2/Script/HelperFunctions.R")
setwd("D:/Paper2/Data")

data = getdata("D:/Paper2/Data")

## Datasub for Scale 0.5 and Year 2015
data = data %>% filter(Scale == 0.5) %>% select(-c(Scale))
head(data)

#wdi_pop_dat <- WDI(indicator = c("SP.POP.TOTL","SP.URB.TOTL","SP.URB.TOTL.IN.ZS"), start = 2000, end = 2019, extra = TRUE)
#write.csv(wdi_pop_dat,"./WBDATA_LR/Population.csv")
wdi_pop_dat = read.csv("./WBDATA_LR/Population.csv")[,-1]
population =wdi_pop_dat %>%
	filter(region != "Aggregates") %>%
	mutate(Population=SP.POP.TOTL,Urbanization=SP.URB.TOTL.IN.ZS) %>%
	select(c("country","year","Population","Urbanization","region","iso2c","iso3c"))%>%
	rename(CountryWB = country,Year=year,GID_0=iso3c)

data  = data %>% 
	left_join(population,  by = c("GID_0","Year"))

#gdp_dat <- WDI(indicator = c("NY.GDP.PCAP.PP.KD"), start = 2000, end = 2019, extra = TRUE)
#write.csv(gdp_dat,"./WBDATA_LR/GDP.csv")
gdp_dat = read.csv("./WBDATA_LR/GDP.csv")

gdp = gdp_dat %>%
  filter(region != "Aggregates") %>%
	mutate(GDP_pc=NY.GDP.PCAP.PP.KD) %>%
	select(c("year","GDP_pc","iso3c"))%>%
	rename(Year=year,GID_0=iso3c)

data  = data %>% 
	left_join(gdp,  by = c("GID_0","Year"))

sol = getSOL("D:/Paper2/Data")

data  = data %>% 
	left_join(sol,  by = c("Country","Year"))
colnames(data)


#DATA

datatp = data %>% 
	filter(Year %in% c(2000,2019)) %>% 
	select(Country,Year,Inequality_wtn,Inequality_btwn,Urbanization,GDP_pc,region,SOL) %>%
	mutate(GDP_pc = log(GDP_pc)) %>%
	mutate(SOL = log(SOL))

datatp = dcast(melt(datatp, id.vars=c("Country", "Year")), Country~variable+Year)
datatp[,c(2:9,12:13)] = apply(datatp[,c(2:9,12:13)],2,factor2numeric)
datatp$I_w = (datatp[,3] - datatp[,2])#/datatp[,2]
datatp$I_b = (datatp[,5] - datatp[,4])#/datatp[,4]
datatp$Urbanization = (datatp[,7] - datatp[,6])#/datatp[,6]
datatp$GDP_pc = (datatp[,9] - datatp[,8])#/datatp[,8]
datatp$SOL = (datatp[,13] - datatp[,12])#/datatp[,12]
datatp$region = datatp[,10]

datatp = datatp[,c(1,14:19)]
datatp = datatp[complete.cases(datatp),]
head(datatp)

###################### TEST LONG RUN CHANGE
################### Within

mod_w = lm(I_w ~ Urbanization + GDP_pc,data=datatp)
summary(mod_w)
car::vif(mod_w)
coeftest(mod_w , vcov = vcovHC(mod_w , type="HC1"))
cov1         = vcovHC(mod_w, type = "HC1")
robust_se1    <- sqrt(diag(cov1))
# Adjust F statistic 
wald_results1 <- waldtest(mod_w, vcov = cov1)

mod_w1 = lm(I_w ~ Urbanization + GDP_pc + SOL,data=datatp)
summary(mod_w1)
car::vif(mod_w1)
coeftest(mod_w1, vcov = vcovHC(mod_w1, type="HC1"))
cov2 = vcovHC(mod_w1, type = "HC1")
robust_se2    <- sqrt(diag(cov2))
# Adjust F statistic 
wald_results2 <- waldtest(mod_w1, vcov = cov2)

################### Between

mod_b = lm(I_b ~ Urbanization + GDP_pc,data=datatp)
summary(mod_b)
car::vif(mod_b)
coeftest(mod_b , vcov = vcovHC(mod_b, type="HC1"))
cov3         = vcovHC(mod_b, type = "HC1")
robust_se3    <- sqrt(diag(cov3))
wald_results3 <- waldtest(mod_b, vcov = cov3)

mod_b1 = lm(I_b ~ Urbanization + GDP_pc + SOL,data=datatp)
summary(mod_b1)
car::vif(mod_b1)
coeftest(mod_b1 , vcov = vcovHC(mod_b1, type="HC1"))
cov4         = vcovHC(mod_b1, type = "HC1")
robust_se4    <- sqrt(diag(cov4))
wald_results4 <- waldtest(mod_b1, vcov = cov4)

fileout = "D:/Paper2/Figures/SI_DifferenceReg_revised.html"
stargazer(mod_w,mod_w1,mod_b,mod_b1, type = "html",
          se        = list(robust_se1,robust_se2,robust_se3,robust_se4),
		omit.stat = c("f","adj.rsq"),
	    covariate.labels = c("Urbanization","(log) GDP per-capita",
	    "(log) Sum of Lights"),
	    dep.var.labels = c("Within Inequality","Between Inequality"),out=fileout)


