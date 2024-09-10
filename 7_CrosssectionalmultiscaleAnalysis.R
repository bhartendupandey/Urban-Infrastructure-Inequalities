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
source("D:/Paper2/Script/HelperFunctions.R")

set.seed(123) ## For Bootstrapping
## Change Path
setwd("D:/Paper2/Data")

## Check for Consistency
## Add Country Data
data = getdata("D:/Paper2/Data")

## Datasub for Scale 0.5 and Year 2015
datasub = data %>% filter(Scale == 0.5) %>% filter(Year == 2015)
datasub = append2015WBdata(datasub,"D:/Paper2/Data")

datasub = datasub[complete.cases(datasub),]
datasubcountries = datasub %>% pull(Country)
dim(datasub)
## Cross-sectional Analysis 2015

## Comparing Inequality Within versus Inequality Between,

cor_ineq = ggplot(datasub,aes(x=Inequality_wtn,y=Inequality_btwn)) + geom_point(size=0.9) + theme_bw() + 
  labs(y=expression(Inequality[between]),x=expression(Inequality[within])) + 
  theme(axis.title=element_text(size=16),axis.text=element_text(size=13)) + geom_smooth(method="lm",se=F,color="red") + 
  stat_cor(label.x.npc = 0.5,label.y.npc = 0.03,size=3,col="gray20",p.digits=0) + 
  geom_abline(slope=1,intercept=0,color="gray") + ylim(0,0.8) 

datascale = data %>% filter((Year==2015) & Country %in% datasubcountries) %>% group_by(Scale) %>% 
  summarize(Correlation = cor(Inequality_wtn,Inequality_btwn),
            Pvalue = cor.test(Inequality_wtn,Inequality_btwn)$p.value)

msinset = ggplot(datascale,aes(y=Correlation,x=Scale)) + geom_point(alpha=0.5) +  theme_classic() + 
  labs(y=expression(italic(R)), x=expression(Spatial * " " * Scale * " "* (degree))) +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10))

fig2a = cor_ineq + inset_element(msinset, left = 0.01, bottom = 0.60, right = 0.40, top = 0.95)
fig2a

### Plot %countries where Ineq within exceeds Ineq between

test = data %>% filter((Year==2015) & Country %in% datasubcountries) %>% group_by(Scale) %>% summarize(out = sum(Inequality_wtn > Inequality_btwn))
bversusw = ggplot(test,aes(x=Scale,y = out*100/147)) + geom_point() + theme_bw() + 
labs(y=expression("% Countries with: " * I[within]>I[between]), x=expression(Spatial * " " * Scale * " "* (degree))) +
theme(axis.title=element_text(size=16),axis.text=element_text(size=13))
ggsave("D:/Paper2/Figures/bversusw.pdf",bversusw,width=6,height=6)



## Comparing Inequality Within and Inequality Between, with Urbanization Levels and GDP-pc levels

wtntab = datasub %>% select(Country,Year,Inequality_wtn,Urbanization,GDP_pc,region) %>% rename(Inequality = Inequality_wtn) %>% mutate(Type="Within") %>% mutate(Plttype="Urbanization")
btwntab = datasub %>% select(Country,Year,Inequality_btwn,Urbanization,GDP_pc,region)%>% rename(Inequality = Inequality_btwn) %>% mutate(Type="Between") %>% mutate(Plttype="(log) GDP Per-capita")
plttab = rbind(wtntab,btwntab)

uplot = ggplot(plttab,aes(x= Urbanization,y=Inequality,group=Type,color=Type)) + 
  geom_point(size=0.9) + 
  geom_smooth(method="lm",se=F) + 
  theme_bw() + 
  labs(y = "Inequality",x="Urbanization") + 
  theme(axis.title=element_text(size=16),axis.text=element_text(size=13),legend.position=c(0.85,0.85)) +
  scale_color_manual(values=wes_palette(n=2, name="BottleRocket1",type="discrete")) + ylim(0,1) +
  stat_cor(label.x.npc = 0.75,label.y.npc = 0.1,size=3,p.digits=0,show.legend = F)


datascale = append2015WBdata(data %>% filter((Year==2015) & Country %in% datasubcountries),"D:/Paper2/Data")%>% 
  group_by(Scale) %>% 
  summarize(Within = cor(Inequality_wtn,Urbanization),
            Between = cor(Inequality_btwn,Urbanization)) %>% 
  pivot_longer(!Scale,names_to="Type")

msinset1 = ggplot(datascale,aes(y=value,x=Scale,color=Type)) + geom_point(alpha=0.5) +  theme_classic() + 
  labs(y=expression(italic(R)), x=expression(Spatial * " " * Scale * " "* (degree))) +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.position="none") + 
  scale_color_manual(values=wes_palette(n=2, name="BottleRocket1",type="discrete"))

fig2b = uplot + inset_element(msinset1, left = 0.01, bottom = 0.60, right = 0.40, top = 0.95)
fig2b
######

gplot = ggplot(plttab,aes(x= log(GDP_pc),y=Inequality,group=Type,color=Type)) + 
  geom_point(size=0.9) + 
  geom_smooth(method="lm",se=F) + 
  theme_bw() + 
  labs(y = "Inequality",x="(log) GDP Per-capita") + 
  theme(axis.title=element_text(size=16),axis.text=element_text(size=13),legend.position=c(0.85,0.85)) +
  scale_color_manual(values=wes_palette(n=2, name="BottleRocket1",type="discrete")) + ylim(0,1) +
  stat_cor(label.x.npc = 0.75,label.y.npc = 0.1,size=3,p.digits=0,show.legend = F)

datascale = append2015WBdata(data %>% filter((Year==2015) & Country %in% datasubcountries),"D:/Paper2/Data")%>% 
  group_by(Scale) %>% 
  summarize(Within = cor(Inequality_wtn,log(GDP_pc)),
            Between = cor(Inequality_btwn,log(GDP_pc))) %>% 
  pivot_longer(!Scale,names_to="Type")

msinset1 = ggplot(datascale,aes(y=value,x=Scale,color=Type)) + geom_point(alpha=0.5) +  theme_classic() + 
  labs(y=expression(italic(R)), x=expression(Spatial * " " * Scale * " "* (degree))) +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.position="none") + 
  scale_color_manual(values=wes_palette(n=2, name="BottleRocket1",type="discrete"))

fig2c = gplot + inset_element(msinset1, left = 0.01, bottom = 0.60, right = 0.40, top = 0.95)
fig2c

###################################
# PCA
###################################
prdat = datasub[,c("Urbanization","GDP_pc")]
prdat$GDP_pc = log(prdat$GDP_pc)
summary(prdat)
out = prcomp(scale(prdat)) #88.09 % explained by urbanization and GDP_pc
datasub1 = cbind(datasub,predict(out))  
    
out_wtn = lm(Inequality_wtn~ PC1 + land,data=datasub1)
summary(out_wtn)
anova(out_wtn)

anova(out_wtn)[,2]/sum(anova(out_wtn)[,2])
sum(anova(out_wtn)[1,2])/sum(anova(out_wtn)[,2])

model_coef <- function(data, index){
  coef(lm(Inequality_wtn ~ PC1 + land,data=data, subset = index))
}

bsout=boot(datasub1, model_coef, 1000)
coef = as.numeric(bsout$t0)
serror= apply(bsout$t,2,sd)
coefh = coef + 1.96*serror
coefl = coef - 1.96*serror
  
outputw = data.frame(Name=c("Intercept","Urbanization and\n (log) GDP Per-Capita","Land"),coef,serror,coefh,coefl)
outputw = outputw[-1,]

wplot = ggplot(outputw,aes(Name,coef)) + geom_point(size=5) +
  geom_errorbar(data=outputw, mapping=aes(x=Name, ymin=coefl, ymax=coefh), width=0.1, size=1) + coord_flip() + theme_bw()+
  labs(x="",y= "Estimates",title=expression(Inequality[within])) + ylim(0,0.1) +
  theme(axis.text.y=element_text(size=14),axis.title=element_text(size=15),axis.text.x=element_text(size=14))
wplot

### AllScales

datasubscale = append2015WBdata(data %>% filter((Year==2015) & Country %in% datasubcountries),"D:/Paper2/Data") 

allscales = data.frame()
for(deg in seq(0.1,5,0.1)){
  prdata = subset(datasubscale,as.character(datasubscale$Scale) == as.character(deg))
  prdat1 =  prdata[,c("Urbanization","GDP_pc")]
  prdat1$GDP_pc = log(prdat1$GDP_pc)
  out = prcomp(scale(prdat1))
  datasub1 = cbind(prdata, predict(out))
  bsout=boot(datasub1, model_coef, 1000)
  coef = as.numeric(bsout$t0)
  serror= apply(bsout$t,2,sd)
  coefh = coef + 1.96*serror
  coefl = coef - 1.96*serror
  outputw = data.frame(Name=c("Intercept","Urbanization and\n (log) GDP Per-Capita","Land"),coef,serror,coefh,coefl)
  outputw = outputw[-1,]
  outputw$Scale = deg
  allscales = rbind(allscales,outputw)
}

colnames(allscales) = c("Name","coef","serror","coefh","coefl","Scale")

msinset2 = ggplot(allscales,aes(Scale,coef,color=Name)) + geom_point(size=2) + theme_classic()+
  labs(y="Coefficient", x=expression(Spatial * " " * Scale * " "* (degree))) +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.position=c(.3,0.2),
  legend.title=element_blank()) +  
  theme(axis.text.y=element_text(size=14),axis.title=element_text(size=15),axis.text.x=element_text(size=14))

fig2d = wplot + inset_element(msinset2 , left = 0.5, bottom = 0.05, right = 0.9, top = 0.5)
fig2d

####
prdat = datasub[,c("Urbanization","GDP_pc")]
prdat$GDP_pc = log(prdat$GDP_pc)
summary(prdat)
out = prcomp(scale(prdat)) #88.09 % explained by urbanization and GDP_pc
datasub1 = cbind(datasub,predict(out))  
    
out_btwn = lm(Inequality_btwn~ PC1 + land,data=datasub1)
summary(out_btwn)
anova(out_btwn)

anova(out_btwn)[,2]/sum(anova(out_btwn)[,2])
sum(anova(out_btwn)[1,2])/sum(anova(out_btwn)[,2])

model_coef1 <- function(data, index){
  coef(lm(Inequality_btwn ~ PC1 + land,data=data, subset = index))
}

bsout=boot(datasub1, model_coef1, 1000)
coef = as.numeric(bsout$t0)
serror= apply(bsout$t,2,sd)
coefh = coef + 1.96*serror
coefl = coef - 1.96*serror
  
outputb = data.frame(Name=c("Intercept","Urbanization and\n (log) GDP Per-Capita","Land"),coef,serror,coefh,coefl)
outputb = outputb[-1,]

bplot = ggplot(outputb,aes(Name,coef)) + geom_point(size=5) +
  geom_errorbar(data=outputb, mapping=aes(x=Name, ymin=coefl, ymax=coefh), width=0.1, size=1) + coord_flip() + theme_bw()+
  labs(x="",y= "Estimates",title=expression(Inequality[between])) + ylim(0,0.1) +
  theme(axis.text.y=element_text(size=14),axis.title=element_text(size=15),axis.text.x=element_text(size=14))


### AllScales

allscales = data.frame()
for(deg in seq(0.1,5,0.1)){
  prdata = subset(datasubscale,as.character(datasubscale$Scale) == as.character(deg))
  prdat1 =  prdata[,c("Urbanization","GDP_pc")]
  prdat1$GDP_pc = log(prdat1$GDP_pc)
  out = prcomp(scale(prdat1))
  datasub1 = cbind(prdata, predict(out))
  bsout=boot(datasub1, model_coef1, 1000)
  coef = as.numeric(bsout$t0)
  serror= apply(bsout$t,2,sd)
  coefh = coef + 1.96*serror
  coefl = coef - 1.96*serror
  outputw = data.frame(Name=c("Intercept","Urbanization and\n (log) GDP Per-Capita","Land"),coef,serror,coefh,coefl)
  outputw = outputw[-1,]
  outputw$Scale = deg
  allscales = rbind(allscales,outputw)
}

colnames(allscales) = c("Name","coef","serror","coefh","coefl","Scale")

msinset3 = ggplot(allscales,aes(Scale,coef,color=Name)) + geom_point(size=2) + theme_classic()+
  labs(y="Coefficient", x=expression(Spatial * " " * Scale * " "* (degree))) +
  theme(axis.title=element_text(size=12),axis.text=element_text(size=10),legend.position=c(.35,0.2),
  legend.title=element_blank()) +  
  theme(axis.text.y=element_text(size=14),axis.title=element_text(size=15),axis.text.x=element_text(size=14))

fig2e = bplot + inset_element(msinset3 , left = 0.55, bottom = 0.05, right = 0.95, top = 0.5)
fig2e

ggsave("D:/Paper2/Figures/Fig2a.pdf",fig2a,width=5,height=5)
ggsave("D:/Paper2/Figures/Fig2b.pdf",fig2b,width=5,height=5)
ggsave("D:/Paper2/Figures/Fig2c.pdf",fig2c,width=5,height=5)
ggsave("D:/Paper2/Figures/Fig2d.pdf",fig2d,width=12,height=7)
ggsave("D:/Paper2/Figures/Fig2e.pdf",fig2e,width=12,height=7)
