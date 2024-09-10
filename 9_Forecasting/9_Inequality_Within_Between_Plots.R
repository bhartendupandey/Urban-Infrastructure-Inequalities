# getoutdf_b_w function returns country-level data on historic and future Between or Within Inequality
rm(list=ls())
library(viridis)
library(grid)
library(GGally)
library(tidyverse)
#remotes::install_github("Nowosad/rcartocolor")
library(rcartocolor)

cols = carto_pal(9, "Safe")

source("D:/Paper2/Script/HelperFunctions.R")
outdf = getoutdf_b_w()
within = outdf[[1]]
between = outdf[[2]]
dataforalterative = NULL

for (i in 1:5){

within_ssp2 = within %>% filter(Scenario==paste("SSP",i,sep=""))  %>%  group_by(Region,Year) %>% summarize(Predicted = mean(Inequality_wtn)) %>% ungroup()
between_ssp2 = between %>% filter(Scenario==paste("SSP",i,sep="")) %>% group_by(Region,Year) %>% summarize(Predicted = mean(Inequality_btwn)) %>% ungroup()

ssp2_f = within_ssp2 %>% left_join(between_ssp2,by=c("Region","Year")) %>% filter(Year>2018)
ssp2_f$Type = NA
ssp2_f$Type[ssp2_f$Year<2020] = "Observed"
ssp2_f$Type[ssp2_f$Year>=2020] = "Forecasted"


start = ssp2_f %>% filter(Year == 2019) %>% select(-Type) %>% select(-Year)
colnames(start)[2:3] = c("Ref.x","Ref.y")
if(i==1){
Initial_ = start
colnames(Initial_) = c("Region","X","Y")
Initial_$Type = "2019"
Final_  = ssp2_f %>% filter(Year == 2050) %>% select(Region,Predicted.x,Predicted.y)
colnames(Final_) = c("Region","X","Y")
Final_$Type = paste("SSP",i,"-2050",sep="")
outdf_ = rbind(Initial_,Final_)

dataforalterative = rbind(dataforalterative, outdf_)
}else{
Final_  = ssp2_f %>% filter(Year == 2050) %>% select(Region,Predicted.x,Predicted.y)
colnames(Final_) = c("Region","X","Y")
Final_$Type = paste("SSP",i,"-2050",sep="")
outdf_ = Final_
dataforalterative = rbind(dataforalterative, outdf_)
}
}

### Regionwise Plot More Clear
###########################
#### Within
pltdatalt = reshape2::dcast(dataforalterative,Region ~Type,value.var="X")
pltdatalt$Region = recode(pltdatalt$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
#Remove the following line for v3
pltdatalt$Region = factor(pltdatalt$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))

outputs = dataforalterative %>% filter(Type==2019)
linedat = data.frame(Region=rep(outputs$Region,5),X=rep(outputs$X,5),Y=rep(outputs$Y,5),XX = c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9)))
linedat$Region = recode(linedat$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
#Remove the following line for v3
linedat$Region = factor(linedat$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))


pltdatalt = pltdatalt %>% select(-`2019`)
regions = pltdatalt$Region

pltt = ggparcoord(pltdatalt,
    	columns = 2:6, groupColumn = 1, showPoints = TRUE, scale="globalminmax",alpha=0) + 
      theme_bw() + labs(x="Scenario",y=expression(Inequality[within])) + 
      scale_color_manual(values=cols) +  geom_point(size=4,shape=19) + theme(legend.position="none") +
	scale_y_continuous(breaks = scales::pretty_breaks(n = 2))+ ylim(0.2,0.7) + facet_wrap(~Region) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=14),axis.text=element_text(size=12)) +
	theme(strip.text.x = element_text(size = 14)) + geom_line(linetype=2)

pltwtnn = pltt + geom_line(data=linedat,aes(x=XX,y=X,group=Region),linetype=1,alpha=0.75) + 
theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_line( size=.1, color="gray"))

pltwtnn

###########################
#### Between

pltdatalt = reshape2::dcast(dataforalterative,Region ~Type,value.var="Y")
pltdatalt$Region = recode(pltdatalt$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
pltdatalt$Region = factor(pltdatalt$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))


outputs = dataforalterative %>% filter(Type==2019)
linedat = data.frame(Region=rep(outputs$Region,5),X=rep(outputs$X,5),Y=rep(outputs$Y,5),XX = c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9)))
linedat$Region = recode(linedat$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
linedat$Region = factor(linedat$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))

pltdatalt = pltdatalt %>% select(-`2019`)
regions = pltdatalt$Region

pltt = ggparcoord(pltdatalt,
    	columns = 2:6, groupColumn = 1, showPoints = TRUE, scale="globalminmax",alpha=0) + 
      theme_bw() + labs(x="Scenario",y=expression(Inequality[between])) + ylim(0.2,0.7) + 
      scale_color_manual(values=cols) +  geom_point(size=4,shape=19) + theme(legend.position="none") + facet_wrap(~Region) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title=element_text(size=14),axis.text=element_text(size=12)) +
	theme(strip.text.x = element_text(size = 14)) + geom_line(linetype=2)

pltbtwn = pltt + geom_line(data=linedat,aes(x=XX,y=Y,group=Region),linetype=1,alpha=0.75) + 
theme(panel.grid.major.y = element_blank(),panel.grid.major.x = element_line( size=.1, color="gray"))

plt3_ = grid.arrange(pltwtnn,pltbtwn,ncol=2)
plt3_
#ggsave("D:/Paper2/Figures/SSP1_5_within_betwen_2019_2050.pdf",plt3_,
#width=12,height=7,useDingbats=FALSE,family="sans")


##################################################### HISTORIC #############################################################

regplot = within %>% group_by(Region,Year) %>% summarize(Predicted=mean(Inequality_wtn),PredictedSD = sd(Inequality_wtn)) %>% ungroup()
regplot$alpha=0
regplot[regplot$Year < 2019,"alpha"] = 0.5
regplot[regplot$Year >= 2019,"alpha"] = 1
regplot$Region = recode(regplot$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
regplot$Region = factor(regplot$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))
regplot_w = regplot


regplot = between %>% group_by(Region,Year) %>% summarize(Predicted=mean(Inequality_btwn),PredictedSD = sd(Inequality_btwn)) %>% ungroup()
regplot$alpha=0
regplot[regplot$Year < 2019,"alpha"] = 0.5
regplot[regplot$Year >= 2019,"alpha"] = 1
regplot$Region = recode(regplot$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
regplot$Region = factor(regplot$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))
regplot_b = regplot

historic = regplot_w %>% left_join(regplot_b,by=c("Region","Year")) %>% filter(Year<2020)

dat_labels <- historic %>% group_by(Region) %>% summarize(
  x=mean(Predicted.x),
  y=mean(Predicted.y) + 0.03,
  label_value=round(cor.test(Predicted.x,Predicted.y,method="spearman")$estimate,2)
)
cols = carto_pal(9, "Safe")
bw_correlations_historic=ggplot(historic,aes(Predicted.x,Predicted.y,group=Region,color=Region)) +
geom_point(size=2.5) + geom_smooth(se=F) +
xlim(0.3,0.7) + ylim(0.1,0.75) + 
geom_text(data=dat_labels, aes(x=x, y=y,label=label_value),show.legend=F) + 
theme_bw() + labs(x=expression(Inequality[within]),y=expression(Inequality[between])) + 
theme(axis.title=element_text(size=16),axis.text=element_text(size=13),legend.position=c(0.1,0.7))+
geom_abline(intercept = 0, slope = 1,col="gray") + scale_color_manual(values=cols)

ggsave("D:/Paper2/Figures/bw_correlations_historic.pdf",bw_correlations_historic,
width=6,height=6,useDingbats=FALSE,family="sans")

### Forecasted

within_ssp2 = within %>% filter(Scenario=="SSP2")  %>%  group_by(Region,Year) %>% summarize(Predicted = mean(Inequality_wtn)) %>% ungroup()
between_ssp2 = between %>% filter(Scenario=="SSP2") %>% group_by(Region,Year) %>% summarize(Predicted = mean(Inequality_btwn)) %>% ungroup()

# Across All SSPs Variability
within_sd = within %>% group_by(Region,Year,Scenario) %>% summarize(out= mean(Inequality_wtn)) %>% ungroup() %>% group_by(Region,Year) %>%
	summarize(PredictedSD = sd(out)) %>% ungroup()

between_sd = between %>% group_by(Region,Year,Scenario) %>% summarize(out= mean(Inequality_btwn)) %>% ungroup() %>% group_by(Region,Year) %>%
	summarize(PredictedSD = sd(out)) %>% ungroup()

withinr = within_ssp2 %>% left_join(within_sd,by=c("Region","Year"))
betweenr = between_ssp2 %>% left_join(between_sd,by=c("Region","Year"))

withinr$Low = withinr$Predicted - withinr$PredictedSD
withinr$High = withinr$Predicted + withinr$PredictedSD

betweenr$Low = betweenr$Predicted - betweenr$PredictedSD
betweenr$High = betweenr$Predicted + betweenr$PredictedSD

withinr$Type = "Within"
betweenr$Type = "Between"
regplot = rbind(withinr,betweenr)

regplot$Region = recode(regplot$Region, China = "CHN", India = "IND",`East Asia & Pacific` = "EAP",
`Europe & Central Asia` = "ECA",`Latin America & Caribbean`="LAC",`Middle East & North Africa`="MENA",`North America` = "NAM",`South Asia` = "SA",`Sub-Saharan Africa` = "SSA")
#Remove the following line for a previous version
regplot$Region = factor(regplot$Region,levels = c("NAM","ECA","MENA","IND","CHN","SA","EAP","LAC","SSA"))

SSP2_and_variability = ggplot() +  
#ggplot() +  
#geom_point(data=subset(regplot,Type=="Within"),aes(x=Year,y=Predicted,group=Region,color=Type),size=1) +
geom_line(data=subset(regplot,Type=="Within"),aes(x=Year,y=Predicted,group=Region,color=Type)) +
#geom_point(data=subset(regplot,Type=="Between"),aes(x=Year,y=Predicted,group=Region,color=Type),size=1) +  
geom_line(data=subset(regplot,Type=="Between"),aes(x=Year,y=Predicted,group=Region,color=Type)) +  
geom_ribbon(data=subset(regplot,Type=="Within"),aes(x=Year,y=Predicted,ymin = Low, ymax = High,group=Region,fill=Type),alpha=0.25,col=NA) + 
geom_ribbon(data=subset(regplot,Type=="Between"),aes(x=Year,y=Predicted,ymin = Low, ymax = High,group=Region,fill=Type),alpha=0.25,col=NA) + 
theme_bw() +  facet_wrap(~Region,nrow=3) + labs(y="Inequality") + 
theme(legend.position="bottomright",
	axis.title=element_text(size=20),
	axis.text=element_text(size=14),
	axis.text.x = rotatedAxisElementText(90,'x'),
	strip.text.x = element_text(size = 18, angle = 0))

ggsave("D:/Paper2/Figures/SSP2_and_variability_v1.pdf",SSP2_and_variability,
width=8,height=8,useDingbats=FALSE,family="sans")


