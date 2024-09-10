# getoutdf_b_w function returns country-level data on historic and future Between or Within Inequality
rm(list=ls())
library(ggrepel)
library(tidyverse)
library(rcartocolor)
install.packages("ggExtra")
library(ggExtra)
library(gridExtra)
setwd("D:/Paper2/Data")
cols = carto_pal(9, "Safe")

#source("D:/Paper2/Script/HelperFunctions.R")
#outdf = getoutdf_b_w()
#within = outdf[[1]]
#between = outdf[[2]]
#write.csv(within,"./OutputData/WithinForecasts.csv")
#write.csv(between,"./OutputData/BetweenForecasts.csv")
within = read.csv("./OutputData/WithinForecasts.csv")[,-1]
between = read.csv("./OutputData/BetweenForecasts.csv")[,-1]

giniproj = read.csv("./GiniRao/NRao_et_al_GiniProjections_2018/Gini_projections_SSPs.csv")
giniproj = reshape2::melt(giniproj,id.vars = c("scenario","year"),variable.name = "Country",value.name="Gini")
colnames(giniproj) = c("Scenario","Year","GID_0","Gini")

withingini = merge(within,giniproj,by=c("Scenario","Year","GID_0"))
withingini50 = withingini %>% filter(Year==2050) %>% filter(Scenario=="SSP2") %>% mutate(Gini = Gini/100)

withingini50$Region = recode(withingini50$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
withingini50$Region = factor(withingini50$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))
withingini50$Type ="Within"
colnames(withingini50)[6] = "Inequality"

betweengini = merge(between,giniproj,by=c("Scenario","Year","GID_0"))
betweengini50 = betweengini %>% filter(Year==2050) %>% filter(Scenario=="SSP2") %>% mutate(Gini = Gini/100)
colnames(betweengini50)[6] = "Inequality"

betweengini50 $Region = recode(betweengini50 $Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
betweengini50$Region = factor(betweengini50$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))
betweengini50$Type ="Between"

pltdat = rbind(withingini50,betweengini50)

bw= ggplot(betweengini50) + geom_point(aes(x=Gini,y=Inequality,color=Region),size=2) + theme_bw() +
geom_text_repel(aes(x=Gini,y=Inequality,color=Region,label=Country),show.legend=F) + 
scale_color_manual(values = cols) + 
labs(y=expression(Inequality[between]*": (2050)"),x="Income Inequality: Gini Index (2050)")+
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),strip.text = element_text(size = 20)) +
facet_wrap(~Type)+ geom_smooth(aes(x=Gini,y=Inequality),se=T)


wtn = ggplot(withingini50) + geom_point(aes(x=Gini,y=Inequality,color=Region),size=2) + theme_bw() +
geom_text_repel(aes(x=Gini,y=Inequality,color=Region,label=Country),show.legend=F) + 
scale_color_manual(values = cols) + 
labs(y=expression(Inequality[within]*": (2050)"),x="Income Inequality: Gini Index (2050)")+
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),strip.text = element_text(size = 20)) +
facet_wrap(~Type) + geom_smooth(aes(x=Gini,y=Inequality),se=T)

## Change

withinchange= withingini %>% filter(Year %in% c(2019,2050)) %>% filter(Scenario=="SSP2") %>% mutate(Gini = Gini/100)%>% 
pivot_wider(names_from=c(Year),values_from=c(Inequality_wtn,Gini)) %>% 
mutate(CInequality_wtn=Inequality_wtn_2050-Inequality_wtn_2019) %>% 
mutate(CGini = Gini_2050 - Gini_2019)
withinchange= withinchange[complete.cases(withinchange),]
withinchange$Type = "Within"


wtnc=ggplot(withinchange,aes(x=CGini,y=CInequality_wtn,color=Region)) + geom_point(size=2,show.legend=F) + theme_bw() +
geom_text_repel(aes(label=Country),show.legend=F) + 
scale_color_manual(values = cols) + 
labs(y=expression(Delta* " "*Inequality[within]*": (2019-2050)"),x=expression(Delta * " Income Inequality: Gini Index (2019-2050)"))+
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),strip.text = element_text(size = 20)) +
facet_wrap(~Type) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")


betweenchange= betweengini %>% filter(Year %in% c(2019,2050)) %>% filter(Scenario=="SSP2") %>% mutate(Gini = Gini/100)%>% 
pivot_wider(names_from=c(Year),values_from=c(Inequality_btwn,Gini)) %>% 
mutate(CInequality_btwn=Inequality_btwn_2050-Inequality_btwn_2019) %>% 
mutate(CGini = Gini_2050 - Gini_2019)
betweenchange= betweenchange[complete.cases(betweenchange),]
betweenchange$Type = "Between"

btwnc = ggplot(betweenchange,aes(x=CGini,y=CInequality_btwn,color=Region)) + geom_point(size=2,show.legend=F) + theme_bw() +
geom_text_repel(aes(label=Country),show.legend=F) + 
scale_color_manual(values = cols) + 
labs(y=expression(Delta* " "*Inequality[between]*": (2019-2050)"),x=expression(Delta * " Income Inequality: Gini Index (2019-2050)"))+
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),strip.text = element_text(size = 20)) +
facet_wrap(~Type) + geom_hline(yintercept=0,linetype="dashed") + geom_vline(xintercept=0,linetype="dashed")

outplt = grid.arrange(bw,btwnc,wtn,wtnc,ncol=2,nrow=2)


ggsave("D:/Paper2/Figures/Infra_Inc_Ineq_variability_v1.pdf",outplt ,
width=16,height=12,useDingbats=FALSE,family="sans")
