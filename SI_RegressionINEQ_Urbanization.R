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
library(lmtest)
library(sandwich)
library(stargazer)

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

data = datasub [complete.cases(datasub),]

###################################
# PCA
###################################

prdat = data[,c("Urbanization","GDP_pc")]
prdat$GDP_pc = log(prdat$GDP_pc)
summary(prdat)
out = prcomp(scale(prdat)) #88.09 % explained by urbanization and GDP_pc
data1 = cbind(data,predict(out))

###################################
# Bootstrap Regression: Within
###################################
filew="./OutputData/BS_1000iter_Regression_Inequality_within.csv"
out_wtn = lm(Inequality_wtn~ PC1 + land,data=data1)
summary(out_wtn)
anova(out_wtn)

anova(out_wtn)[,2]/sum(anova(out_wtn)[,2])
sum(anova(out_wtn)[1,2])/sum(anova(out_wtn)[,2])

if(!file.exists(filew)){
model_coef <- function(data, index){
  coef(lm(Inequality_wtn ~ PC1 + land,data=data, subset = index))
}

bsout=boot(data1, model_coef, 1000)

coef = as.numeric(bsout$t0)
serror= apply(bsout$t,2,sd)
coefh = coef + 1.96*serror
coefl = coef - 1.96*serror

outputw = data.frame(Name=c("Intercept","Urbanization_GDP_pc","Land"),coef,serror,coefh,coefl)
write.csv(outputw,filew)}else{
outpuwt=read.csv(filew)
}

###################################
# Bootstrap Regression: Between
###################################
fileb="./OutputData/BS_1000iter_Regression_Inequality_between.csv"

out_btwn = lm(Inequality_btwn~ PC1 + land,data=data1)
summary(out_btwn)
anova(out_btwn)
anova(out_btwn)[,2]/sum(anova(out_btwn)[,2])
sum(anova(out_btwn)[1,2])/sum(anova(out_btwn)[,2])

if(!file.exists(fileb)){
model_coef <- function(data, index){
  coef(lm(Inequality_btwn~PC1 + land,data=data, subset = index))
}

set.seed(123)
bsout=boot(data1, model_coef, 1000)

coef = as.numeric(bsout$t0)
serror= apply(bsout$t,2,sd)
coefh = coef + 1.96*serror
coefl = coef - 1.96*serror

outputb = data.frame(Name=c("Intercept","Urbanization_GDP_pc","Land"),coef,serror,coefh,coefl)
write.csv(outputb,fileb)}else{
outputb=read.csv(filew)
}


##

# Adjust standard errors
cov1         <- vcovHC(out_wtn, type = "HC1")
robust_se1    <- sqrt(diag(cov1))

# Adjust F statistic 
wald_results1 <- waldtest(out_wtn, vcov = cov1)


# Adjust standard errors
cov2         <- vcovHC(out_btwn, type = "HC1")
robust_se2    <- sqrt(diag(cov2))

# Adjust F statistic 
wald_results2 <- waldtest(out_btwn, vcov = cov1)

tabout = "D:/Paper2/Figures/SI_LM_out.html"
stargazer(out_wtn,out_btwn,out=tabout,se=list(robust_se1,robust_se2),omit.stat="f",add.lines = list(c("F Statistic (df = 2; 144)", "170.897***", "177.814***")))



