getColIndex = function(adata1,Year,NTLFlag){
  if(NTLFlag == "DMSP"){
    colm = which(grepl(as.character(Year),colnames(adata1)) & grepl("mean",colnames(adata1)) & grepl("DV",colnames(adata1)))##Mean Column
    cols = which(grepl(as.character(Year),colnames(adata1)) & grepl("stdev",colnames(adata1)) & grepl("DV",colnames(adata1)))##SD Column 
    colp = which(grepl(as.character(Year),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("L",colnames(adata1)))## Population Column
  }else if(NTLFlag == "VIIRS"){
    colm = which(grepl(as.character(Year),colnames(adata1)) & grepl("mean",colnames(adata1)) & grepl("V_",colnames(adata1)))##Mean Column
    cols = which(grepl(as.character(Year),colnames(adata1)) & grepl("stdev",colnames(adata1)) & grepl("V_",colnames(adata1)))##SD Column 
    colp = which(grepl(as.character(Year),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("L",colnames(adata1)))## Population Column
  }else{
    stop("Check Inputs")
  }
  return(c(colm,cols,colp))
}


getIneqs = function(data,VIIRSthreshold=25){
  start = 2000
  end = 2019
  adata = st_drop_geometry(data)
  countrieslist = unique(adata$NAME_0)
  Years = start:end
  
  outputdfw = NULL
  outputdfb = NULL
  
  for(count in 1:length(countrieslist)) {
    adata1 = subset(adata, NAME_0 == countrieslist[count])
    ## Calculate Inequality if more than 10 grid cells exist
    if (nrow(adata1) > 10) {
      Ineqw = start:end
      Ineqb = start:end
      ## Use Inequality Estimates from 2012 and 2013 to adjust for systematic difference between DMSP and VIIRS
      Ineqw_2012_V = NA
      Ineqw_2012_DV = NA
      Ineqw_2013_V = NA
      Ineqw_2013_DV = NA
      Ineqb_2012_V = NA
      Ineqb_2012_DV = NA
      Ineqb_2013_V = NA
      Ineqb_2013_DV = NA
      ## Iterate over years
      for (j in 1:length(Ineqw)) {
        if (j < 13) {
          Year = Years[j]
          coli = getColIndex(adata1,Year, "DMSP")
          df = data.frame(IM = adata1[, coli[1]],
                          IS = adata1[, coli[2]],
                          P = adata1[, coli[3]])
          df = df[complete.cases(df), ]
          df$mS = sqrt((1 - (df$IM / 63)) * (df$IM / 63)) ## Maximum Heterogeneity
          df$I = (df$IS / 63) / df$mS ## Observed/Maximum Heterogeneity
          out = wtd.mean(df$I, weights = df$P) ## Population Weighted Mean
          Ineqw[j] = out
          wmom = wtd.mean(df$IM / 63, weights = df$P) ## Weighted Mean of Mean
          wsdom = sqrt(wtd.var(df$IM / 63, weights = df$P)) # Weighted SD of Mean
          Ineqb[j] = wsdom / sqrt(wsdom * (1 - wsdom)) # Observed / Maximum Heterogeneity
        } else if (j %in% c(13, 14)) {
          Year = Years[j]
          coli = getColIndex(adata1,Year, "DMSP")
          df = data.frame(IM = adata1[, coli[1]],
                          IS = adata1[, coli[2]],
                          P = adata1[, coli[3]])
          df = df[complete.cases(df), ]
          df$mS = sqrt((1 - (df$IM / 63)) * (df$IM / 63))
          df$I = (df$IS / 63) / sqrt((1 - (df$IM / 63)) * (df$IM / 63))
          out = wtd.mean(df$I, weights = df$P)
          wmom = wtd.mean(df$IM / 63, weights = df$P) ## Weighted Mean of Mean
          wsdom = sqrt(wtd.var(df$IM / 63, weights = df$P)) # Weighted SD of Mean
          out1 = wsdom / sqrt(wsdom * (1 - wsdom))#  Observed / Maximum Heterogeneity
          if (Year == 2012) {
            Ineqw_2012_DV = out
            Ineqb_2012_DV = out1
          } else{
            Ineqw_2013_DV = out
            Ineqb_2013_DV = out1
          }
          coli = getColIndex(adata1,Year, "VIIRS")
          df = data.frame(IM = adata1[, coli[1]],
                          IS = adata1[, coli[2]],
                          P = adata1[, coli[3]])
          df = df[complete.cases(df), ]
          df$mS = sqrt((1 - (df$IM / VIIRSthreshold)) * (df$IM / VIIRSthreshold))
          df$I = (df$IS / VIIRSthreshold) / sqrt((1 - (df$IM / VIIRSthreshold)) * (df$IM /VIIRSthreshold))
          out = wtd.mean(df$I, weights = df$P)
          wmom = wtd.mean(df$IM / VIIRSthreshold, weights = df$P)
          wsdom = sqrt(wtd.var(df$IM / VIIRSthreshold, weights = df$P))
          out1 = wsdom / sqrt(wsdom * (1 - wsdom))
          if (Year == 2012) {
            Ineqw_2012_V = out
            Ineqb_2012_V = out1
          } else{
            Ineqw_2013_V = out
            Ineqb_2013_V = out1
          }
        } else{
          Year = Years[j]
          coli = getColIndex(adata1,Year, "VIIRS")
          df = data.frame(IM = adata1[, coli[1]],
                          IS = adata1[, coli[2]],
                          P = adata1[, coli[3]])
          df = df[complete.cases(df), ]
          df$mS = sqrt((1 - (df$IM / VIIRSthreshold)) * (df$IM / VIIRSthreshold))
          df$I = (df$IS / VIIRSthreshold) / sqrt((1 - (df$IM / VIIRSthreshold)) * (df$IM / VIIRSthreshold))
          out = wtd.mean(df$I, weights = df$P)
          Ineqw[j] = out
          wmom = wtd.mean(df$IM / VIIRSthreshold, weights = df$P)
          wsdom = sqrt(wtd.var(df$IM / VIIRSthreshold, weights = df$P))
          out = wsdom / sqrt(wsdom * (1 - wsdom))
          Ineqb[j] = out
        }
      }
      
      V_I = c(Ineqw_2012_V, Ineqw_2013_V)
      DV_I = c(Ineqw_2012_DV, Ineqw_2013_DV)
      V_Iadj = (-1 * mean(V_I - DV_I)) + V_I
      Ineqw[13:14] = V_Iadj
      Ineqw[15:20] = (-1 * mean(V_I - DV_I)) + Ineqw[15:20]
      Ineqwdf = data.frame(Year = start:end, Inequality_wtn = Ineqw)
      Ineqwdf$Country = countrieslist[count]
      outputdfw = rbind(outputdfw, Ineqwdf)
      
      V_I = c(Ineqb_2012_V, Ineqb_2013_V)
      DV_I = c(Ineqb_2012_DV, Ineqb_2013_DV)
      V_Iadj = (-1 * mean(V_I - DV_I)) + V_I
      Ineqb[13:14] = V_Iadj
      Ineqb[15:20] = (-1 * mean(V_I - DV_I)) + Ineqb[15:20]
      Ineqbdf = data.frame(Year = start:end, Inequality_btwn = Ineqb)
      Ineqbdf$Country = countrieslist[count]
      outputdfb = rbind(outputdfb, Ineqbdf)
    }
  }
  return(list(outputdfw,outputdfb))
}


# https://stackoverflow.com/questions/25671246/r-remove-rows-from-panel-while-keeping-the-panel-balanced

balanced<-function(data, ID, TIME, VARS, required=c("all","shared")) {
  if(is.character(ID)) {
    ID <- match(ID, names(data))
  }
  if(is.character(TIME)) {
    TIME <- match(TIME, names(data))
  }
  if(missing(VARS)) { 
    VARS <- setdiff(1:ncol(data), c(ID,TIME))
  } else if (is.character(VARS)) {
    VARS <- match(VARS, names(data))
  }
  required <- match.arg(required)
  idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
  timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
  complete <- complete.cases(data[, VARS])
  tbl <- table(idf[complete], timef[complete])
  if (required=="all") {
    keep <- which(rowSums(tbl==1)==ncol(tbl))
    idx <- as.numeric(idf) %in% keep
  } else if (required=="shared") {
    keep <- which(colSums(tbl==1)==nrow(tbl))
    idx <- as.numeric(timef) %in% keep
  }
  return(data[idx, ])
}

getdata = function(rootdatapath){
  setwd(rootdatapath)
  data = read.csv("./InequalityData/Inequality_scale.csv") %>% select(-c(1))
  gadm = read_sf("./GADM/gadm36_0.shp") %>% as.data.frame(.) %>% select(-geometry)
  colnames(gadm)[2] = "Country"
  gadm = gadm[,-3]
  data = data %>% left_join(gadm, by="Country")
  return(data)
}

append2015WBdata = function(data,rootdatapath){
  setwd(rootdatapath)
  file_pop = "./WBDATA/WDI_POP.csv"
  if(!file.exists(file_pop)){
          wdi_pop_dat = WDI(indicator = c("SP.POP.TOTL","SP.URB.TOTL","SP.URB.TOTL.IN.ZS"), start = 2015, end = 2015, extra = TRUE)
          write.csv(wdi_pop_dat,file_pop)
  }else{
          wdi_pop_dat = read.csv(file_pop,head=T)
  }
  population =wdi_pop_dat %>%
          filter(region != "Aggregates") %>%
          mutate(Population=SP.POP.TOTL,Urbanization=SP.URB.TOTL.IN.ZS) %>%
          select(c("country","year","Population","Urbanization","region","iso2c","iso3c"))%>%
          rename(CountryWB = country,Year=year,GID_0=iso3c)

  data  = data %>% 
          left_join(population,  by = c("GID_0","Year"))

  file_gdp = "./WBDATA/WDI_GDP.csv"
  if(!file.exists(file_gdp)){
          gdp_dat <- WDI(indicator = c("NY.GDP.PCAP.PP.KD"), start = 2015, end = 2015, extra = TRUE)
          write.csv(gdp_dat,file_gdp)
  }else{
          gdp_dat = read.csv(file_gdp,head=T)
  }

  gdp = gdp_dat %>%
    filter(region != "Aggregates") %>%
          mutate(GDP_pc=NY.GDP.PCAP.PP.KD) %>%
          select(c("year","GDP_pc","iso3c"))%>%
          rename(Year=year,GID_0=iso3c)

  data  = data %>% 
          left_join(gdp,  by = c("GID_0","Year"))

  file_lnd = "./WBDATA/WDI_Land.csv"
  if(!file.exists(file_lnd)){
          lnd_dat <- WDI(indicator = c("AG.LND.TOTL.K2"), start = 2015, end = 2015, extra = TRUE)
          write.csv(lnd_dat,file_lnd)
  }else{
          lnd_dat = read.csv(file_lnd,head=T)
  }

  lnd = lnd_dat %>%
    filter(region != "Aggregates") %>%
          mutate(land=AG.LND.TOTL.K2) %>%
          select(c("year","land","iso3c"))%>%
          rename(Year=year,GID_0=iso3c)
  lnd$land = log(lnd$land)

  data  = data %>% 
          left_join(lnd,  by = c("GID_0","Year"))
  return(data)
}

getSOL = function(datarootpath){
setwd(datarootpath)
dmsp = read_sf("./Fishnet_Arc_DMSP/Compiled/0_5degby0_5deg.gpkg")
countrieslist = unique(dmsp$NAME_0)
dmsp = dmsp[,c(which(colnames(dmsp)=="GID_0"),which(colnames(dmsp)=="NAME_0"),which(grepl("sum",colnames(dmsp)) & grepl("DV",colnames(dmsp))))] %>% st_drop_geometry(.)
dmsp = dmsp %>%
  group_by(GID_0,NAME_0) %>%
  summarise(across(everything(), \(x) sum(x,na.rm=T))) 

viirs = read_sf("./Fishnet_Arc_VIIRS_25/Compiled/0_5degby0_5deg_25.gpkg")
viirs = viirs [,c(which(colnames(viirs)=="GID_0"),which(grepl("sum",colnames(viirs)) & grepl("V_",colnames(viirs))))] %>% st_drop_geometry(.)
viirs = viirs %>%
  group_by(GID_0) %>%
  summarise(across(everything(), \(x) sum(x,na.rm=T))) 
colnames(viirs)[2:9] = paste("V_",2012:2019,"sum",sep="")

adata= dmsp %>% left_join(viirs,by=c("GID_0"))
countrieslist = unique(dmsp$NAME_0)
outputdf = NULL

for(count in 1: length(countrieslist)){
  adata1 = subset(adata,NAME_0==countrieslist[count])
  #WithinInequality
  Ineq = 2000:2019
  Ineq_2012_V = NA
  Ineq_2012_DV = NA
  Ineq_2013_V = NA
  Ineq_2013_DV = NA
  for(j in 1:length(Ineq)){
    if(j < 13){
      i = 2000:2019
      i = i[j]
      colm = grepl(as.character(i),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("DV",colnames(adata1))
      Ineq[j] = as.numeric(adata1[,colm])
    }else if(j %in% c(13,14)){
      i = 2000:2019
      i = i[j]
      colm = grepl(as.character(i),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("DV",colnames(adata1))
      if(i==2012){
        Ineq_2012_DV = as.numeric(adata1[,colm])
      }else{
        Ineq_2013_DV = as.numeric(adata1[,colm])
      }
      i = 2000:2019
      i = i[j]
      colm = grepl(as.character(i),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("V_",colnames(adata1))
      if(i==2012){
        Ineq_2012_V = as.numeric(adata1[,colm])
      }else{
        Ineq_2013_V = as.numeric(adata1[,colm])
      }
      
    }else{
      i = 2000:2019
      i = i[j]
      colm = grepl(as.character(i),colnames(adata1)) & grepl("sum",colnames(adata1)) & grepl("V_",colnames(adata1))
      Ineq[j] = as.numeric(adata1[,colm])
    }    
  }
  
  V_I = c(Ineq_2012_V,Ineq_2013_V)
  DV_I= c(Ineq_2012_DV,Ineq_2013_DV)
  
  V_Iadj = (-1 * mean(V_I - DV_I)) + V_I
  
  Ineq[13:14] = V_Iadj
  Ineq[15:20] = (-1 * mean(V_I - DV_I)) + Ineq[15:20]
  
  Ineq = data.frame(Year=2000:2019,SOL=Ineq)
  Ineq$Country = countrieslist[count]
  outputdf = rbind(outputdf,Ineq)
}
outputdf = outputdf[complete.cases(outputdf),]
return(outputdf)
}

factor2numeric = function(x){return(as.numeric(as.character(x)))}

appendlongrunWBdata = function(data,datarootfolder){
setwd(datarootfolder)
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

sol = getSOL(datarootfolder)

data  = data %>% 
	left_join(sol,  by = c("Country","Year"))
return(data)
}

gettraining = function(rootdatafolder,scle=0.5){
data = getdata(rootdatafolder)
data = data %>% filter(Scale == scle) %>% select(-c(Scale))

wdi_pop_dat = read.csv("./WBDATA_LR/Population.csv")[,-1]
population =wdi_pop_dat %>%
	filter(region != "Aggregates") %>%
	mutate(Population=SP.POP.TOTL,Urbanization=SP.URB.TOTL.IN.ZS) %>%
	select(c("country","year","Population","Urbanization","region","iso2c","iso3c"))%>%
	rename(CountryWB = country,Year=year,GID_0=iso3c)
data  = data %>% 
	left_join(population,  by = c("GID_0","Year"))


gdp_dat = read.csv("./WBDATA_LR/WDI_GDP_NOTPC.csv")
gdp = gdp_dat %>%
  filter(region != "Aggregates") %>%
	mutate(GDP=NY.GDP.MKTP.PP.KD) %>%
	select(c("year","GDP","iso3c"))%>%
	rename(Year=year,GID_0=iso3c)
data  = data %>% 
	left_join(gdp,  by = c("GID_0","Year"))

sol = getSOL(rootdatafolder)
data  = data %>% 
	left_join(sol,  by = c("Country","Year"))

file_lnd = "./WBDATA/WDI_Land.csv"
lnd_dat = read.csv(file_lnd,head=T)
lnd = lnd_dat %>%
    filter(region != "Aggregates") %>%
    mutate(land=AG.LND.TOTL.K2) %>%
    select(c("land","iso3c"))%>%
    rename(GID_0=iso3c)
lnd$land = log(lnd$land)
data  = data %>% left_join(lnd,  by = c("GID_0"))
datab = balanced(as.data.frame(data),"Country","Year")
return(datab)
}

preptestdata = function(year,datab){
datatstest = NULL
for (j in year){
	dataftest = NULL

	for (i in 1:length(countries)){
		country = countries[i]
		sub = subset(datab,Country==country)
		pred = sub[sub$Year==j,"Inequality_btwn"]
		GDP = sub[sub$Year==j ,"GDP"]
		Urbanization = sub[sub$Year==j,"Urbanization"]
		region = sub[sub$Year==j,"region"]
		land = sub[sub$Year==j,"land"]
		dataftest = rbind(dataftest,data.frame(Country=country,pred,GDP=log(GDP),Urbanization,region,land))
	}
datatstest = rbind(datatstest,dataftest)
}
return(datatstest)
}

preptestdata1 = function(year,datab){
datatstest = NULL
for (j in year){
	dataftest = NULL

	for (i in 1:length(countries)){
		country = countries[i]
		sub = subset(datab,Country==country)
		pred = sub[sub$Year==j,"Inequality_wtn"]
		GDP = sub[sub$Year==j ,"GDP"]
		Urbanization = sub[sub$Year==j,"Urbanization"]
		region = sub[sub$Year==j,"region"]
		land = sub[sub$Year==j,"land"]
		dataftest = rbind(dataftest,data.frame(Country=country,pred,GDP=log(GDP),Urbanization,region,land))
	}
datatstest = rbind(datatstest,dataftest)
}
return(datatstest)
}
### CREATE A WDI CONSISTENT SSP_GDP SERIES

getsspGDP = function(datarootfolder){
  setwd(datarootfolder)
  file_gdp = "./WBDATA_LR/WDI_GDP_NOTPC.csv" # This file is not GDP per capita
  if(!file.exists(file_gdp)){
          gdp_dat <- WDI(indicator = c("NY.GDP.MKTP.PP.KD"), start = 2000, end = 2019, extra = TRUE)
          write.csv(gdp_dat,file_gdp)
  }else{
          gdp_dat = read.csv(file_gdp,head=T)
  }

  gdp_dat = subset(gdp_dat,year==2010)# Use this to REBASE
  gdp = gdp_dat %>%
    filter(region != "Aggregates") %>%
          mutate(GDP_PPP=NY.GDP.MKTP.PP.KD) %>%
          select(c("year","GDP_PPP","iso3c"))%>%
          rename(Year=year,GID_0=iso3c)

  gdp = gdp[complete.cases(gdp),]

  ssp = read.csv("./SspDb_country_data_2013-06-12.csv/SspDb_country_data_2013-06-12.csv")

  ssp_gdp = subset(ssp,VARIABLE == "GDP|PPP" & MODEL == "IIASA GDP") # FROM NCAR
  ssp_gdp$SCENARIO = substr(ssp_gdp$SCENARIO,1,4)
  ssp_gdp = ssp_gdp[,c(2,3,18:36)]

  ssp1gdp = subset(ssp_gdp,SCENARIO=="SSP1")
  ssp2gdp = subset(ssp_gdp,SCENARIO=="SSP2")
  ssp3gdp = subset(ssp_gdp,SCENARIO=="SSP3")
  ssp4gdp = subset(ssp_gdp,SCENARIO=="SSP4")
  ssp5gdp = subset(ssp_gdp,SCENARIO=="SSP5")
  # Calculate Ratios

  for(i in 4:21){
    ssp1gdp[,i] = ssp1gdp[,i] / ssp1gdp[,3]
    ssp2gdp[,i] = ssp2gdp[,i] / ssp2gdp[,3]
    ssp3gdp[,i] = ssp3gdp[,i] / ssp3gdp[,3]
    ssp4gdp[,i] = ssp4gdp[,i] / ssp4gdp[,3]
    ssp5gdp[,i] = ssp5gdp[,i] / ssp5gdp[,3]
  }
  ssp1gdp[,3] = 1
  ssp2gdp[,3] = 1
  ssp3gdp[,3] = 1
  ssp4gdp[,3] = 1
  ssp5gdp[,3] = 1
  sspgdp = rbind(ssp1gdp,ssp2gdp,ssp3gdp,ssp4gdp,ssp5gdp)
  sspgdp  = merge(sspgdp,gdp,by.x="REGION",by.y="GID_0")

  for(i in 3:21){
    sspgdp[,i] = sspgdp[,i] * sspgdp[,23]
  }
  sspgdp = sspgdp[,-c(22,23)]
  colnames(sspgdp)[3:21] = paste("G_",seq(2010,2100,by=5),sep="")
  return(sspgdp)
}


getsspUrbn = function(datarootfolder){
  setwd(datarootfolder)
  ssp = read.csv("./SspDb_country_data_2013-06-12.csv/SspDb_country_data_2013-06-12.csv")
  ssp_urban = subset(ssp,VARIABLE == "Population|Urban|Share") # FROM NCAR
  ssp_urban = ssp_urban[,c(2,3,seq(18,36,2))]
  colnames(ssp_urban)[3:12] = paste("U_",seq(2010,2100,by=10),sep="")
  ssp_urban$SCENARIO = substr(ssp_urban$SCENARIO,1,4)
  return(ssp_urban)
  }

  getsspPop= function(datarootfolder){
  setwd(datarootfolder)
  ssp = read.csv("./SspDb_country_data_2013-06-12.csv/SspDb_country_data_2013-06-12.csv")
  ssp_pop = subset(ssp,VARIABLE == "Population") # FROM NCAR
  ssp_pop = ssp_pop[,c(2,3,seq(18,36,2))]
  colnames(ssp_pop)[3:12] = paste("P_",seq(2010,2100,by=10),sep="")
  ssp_pop$SCENARIO = substr(ssp_pop$SCENARIO,1,4)
  head(ssp_pop)
  return(ssp_pop)
}


getsspGDPpc = function(datarootfolder){
  setwd(datarootfolder)
  file_gdp = "./WBDATA_LR/WDI_GDP_NOTPC.csv" # This file is not GDP per capita
  if(!file.exists(file_gdp)){
          gdp_dat <- WDI(indicator = c("NY.GDP.MKTP.PP.KD"), start = 2000, end = 2019, extra = TRUE)
          write.csv(gdp_dat,file_gdp)
  }else{
          gdp_dat = read.csv(file_gdp,head=T)
  }

  gdp_dat = subset(gdp_dat,year==2010)# Use this to REBASE
  gdp = gdp_dat %>%
    filter(region != "Aggregates") %>%
          mutate(GDP_PPP=NY.GDP.MKTP.PP.KD) %>%
          select(c("year","GDP_PPP","iso3c"))%>%
          rename(Year=year,GID_0=iso3c)

  gdp = gdp[complete.cases(gdp),]

  ssp = read.csv("./SspDb_country_data_2013-06-12.csv/SspDb_country_data_2013-06-12.csv")

  ssp_gdp = subset(ssp,VARIABLE == "GDP|PPP" & MODEL == "IIASA GDP") # FROM NCAR
  ssp_gdp$SCENARIO = substr(ssp_gdp$SCENARIO,1,4)
  ssp_gdp = ssp_gdp[,c(2,3,18:36)]

  ssp1gdp = subset(ssp_gdp,SCENARIO=="SSP1")
  ssp2gdp = subset(ssp_gdp,SCENARIO=="SSP2")
  ssp3gdp = subset(ssp_gdp,SCENARIO=="SSP3")
  ssp4gdp = subset(ssp_gdp,SCENARIO=="SSP4")
  ssp5gdp = subset(ssp_gdp,SCENARIO=="SSP5")
  # Calculate Ratios

  for(i in 4:21){
    ssp1gdp[,i] = ssp1gdp[,i] / ssp1gdp[,3]
    ssp2gdp[,i] = ssp2gdp[,i] / ssp2gdp[,3]
    ssp3gdp[,i] = ssp3gdp[,i] / ssp3gdp[,3]
    ssp4gdp[,i] = ssp4gdp[,i] / ssp4gdp[,3]
    ssp5gdp[,i] = ssp5gdp[,i] / ssp5gdp[,3]
  }
  ssp1gdp[,3] = 1
  ssp2gdp[,3] = 1
  ssp3gdp[,3] = 1
  ssp4gdp[,3] = 1
  ssp5gdp[,3] = 1
  sspgdp = rbind(ssp1gdp,ssp2gdp,ssp3gdp,ssp4gdp,ssp5gdp)

  sspgdp  = merge(sspgdp,gdp,by.x="REGION",by.y="GID_0")

  for(i in 3:21){
    sspgdp[,i] = sspgdp[,i] * sspgdp[,23]
  }
  sspgdp = sspgdp[,-c(22,23)]
  colnames(sspgdp)[3:21] = paste("G_",seq(2010,2100,by=5),sep="")

  ssp_pop = subset(ssp,VARIABLE == "Population" & MODEL == "IIASA-WiC POP") # FROM NCAR
  ssp_pop = ssp_pop[,c(2,3,seq(18,36,2))]
  colnames(ssp_pop)[3:12] = paste("P_",seq(2010,2100,by=10),sep="")
  ssp_pop$SCENARIO = substr(ssp_pop$SCENARIO,1,4)
  sspout  = merge(sspgdp,ssp_pop,by=c("SCENARIO","REGION"))

  gdpcols = colnames(sspout)[seq(3,11,2)]
  popcols = colnames(sspout)[seq(22,26,1)]
  for(i in 1:5){
    sspout[,gdpcols[i]] = sspout[,gdpcols[i]]/ (sspout[,popcols[i]] * 1000000)
  }
  sspout= sspout[,c("SCENARIO","REGION",gdpcols,popcols)]
  return(sspout)
}

Forecast_btwn = function(SSP,datats,sspurban,sspgdppc,year,datab){
	ssp = merge(sspurban,sspgdppc,by=c("SCENARIO","REGION"))
	refdat = subset(datab,Year==2019)
	refdat = refdat[,c("Country","GID_0","land","region","Inequality_wtn", "Inequality_btwn")]
	out = merge(refdat,ssp,by.x="GID_0",by.y="REGION")
	predat =  subset(out,SCENARIO==SSP)
	
	predat1 = predat[,c("Country","Inequality_btwn",paste("G_",year,sep=""),paste("U_",year,sep=""),"region","land","GID_0")]
	colnames(predat1)[3:4] = c("GDP","Urbanization")
	predat1$GDP = log(predat1$GDP)

	mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=predat1,method="quantreg",symmetry =T)
	predictSSP1_2030 = mod$testPred
	predictSSP1_2030_LB = mod$quantreg_interval$lower
	predictSSP1_2030_UB = mod$quantreg_interval$upper

	preout = data.frame(Country=predat1$Country,Region=predat1$region,Initial=predat1$Inequality_btwn,Predicted=predictSSP1_2030,Predicted_LB=predictSSP1_2030_LB,Predicted_UB=predictSSP1_2030_UB,GID_0=predat1$GID_0)
	fileout = paste(SSP,"_",year,".csv",sep="")
	write.csv(preout,paste0("./OutputData/Forecasts/Between/",fname=fileout,sep=""))
}

Forecast_wtn = function(SSP,datats,sspurban,sspgdppc,year,datab){
	ssp = merge(sspurban,sspgdppc,by=c("SCENARIO","REGION"))
	refdat = subset(datab,Year==2019)
	refdat = refdat[,c("Country","GID_0","land","region","Inequality_wtn", "Inequality_btwn")]
	out = merge(refdat,ssp,by.x="GID_0",by.y="REGION")
	predat =  subset(out,SCENARIO==SSP)
	
	predat1 = predat[,c("Country","Inequality_wtn",paste("G_",year,sep=""),paste("U_",year,sep=""),"region","land","GID_0")]
	colnames(predat1)[3:4] = c("GDP","Urbanization")
	predat1$GDP = log(predat1$GDP)

	mod = rfinterval(pred~GDP + Urbanization + land+region,train_data=datats,test_data=predat1,method="quantreg",symmetry =T)
	predictSSP1_2030 = mod$testPred
	predictSSP1_2030_LB = mod$quantreg_interval$lower
	predictSSP1_2030_UB = mod$quantreg_interval$upper

	preout = data.frame(Country=predat1$Country,Region=predat1$region,Initial=predat1$Inequality_wtn,Predicted=predictSSP1_2030,Predicted_LB=predictSSP1_2030_LB,Predicted_UB=predictSSP1_2030_UB,GID_0=predat1$GID_0)
	fileout = paste(SSP,"_",year,".csv",sep="")
	write.csv(preout,paste0("./OutputData/Forecasts/Within/",fname=fileout,sep=""))
}

getoutdf_b_w = function(){
library(tidyverse)
library(ggplot2)
#library(gghighlight)
library(gridExtra)
library(sf)

## INITIAL DATA
source("D:/Paper2/Script/HelperFunctions.R")
datab = gettraining("D:/Paper2/Data")
datab$Scenario=NA
datab = datab[,c("Country","GID_0","region","Scenario","Year","Inequality_wtn")]
colnames(datab)[3] = "Region"
Scenario = rep(paste("SSP",1:5,sep=""),each=nrow(datab))
datab = rbind(datab,datab,datab,datab,datab)
datab$Scenario = Scenario
files = list.files(path="./OutputData/Forecasts/Within",pattern="*.csv")
scenario = as.character(t(as.data.frame(strsplit(files,"_")))[,1])
Year = as.numeric(t(as.data.frame(strsplit(as.character(t(as.data.frame(strsplit(files,"_")))[,2]),".csv"))))
files = list.files(path="./OutputData/Forecasts/Within",pattern="*.csv",full.names=T)
outdf = NULL
for(i in 1:length(files)){
	csv = read.csv(files[i])[,-1]
	#csv[,4:6] = csv[,4:6]-csv[,3]
	csv$Scenario = scenario[i]
	csv$Year = Year[i]
	outdf = rbind(outdf,csv)
}

outdf = outdf[,c("Country","GID_0","Region","Scenario","Year","Predicted")]
colnames(outdf)[6] = "Inequality_wtn"
outdf = rbind(datab,outdf)
outdf$Region= as.character(outdf$Region)
outdf[outdf$Country=="India","Region"] = "India"
outdf[outdf$Country=="China","Region"] = "China"
outdfw = outdf

###########################################################################################################
# BETWEEN INEQUALITY

## INITIAL DATA
datab = gettraining("D:/Paper2/Data")
datab$Scenario=NA
datab = datab[,c("Country","GID_0","region","Scenario","Year","Inequality_btwn")]
colnames(datab)[3] = "Region"
Scenario = rep(paste("SSP",1:5,sep=""),each=nrow(datab))
datab = rbind(datab,datab,datab,datab,datab)
datab$Scenario = Scenario

files = list.files(path="./OutputData/Forecasts/Between",pattern="*.csv")
scenario = as.character(t(as.data.frame(strsplit(files,"_")))[,1])
Year = as.numeric(t(as.data.frame(strsplit(as.character(t(as.data.frame(strsplit(files,"_")))[,2]),".csv"))))
files = list.files(path="./OutputData/Forecasts/Between",pattern="*.csv",full.names=T)
outdf = NULL

for(i in 1:length(files)){
	csv = read.csv(files[i])[,-1]
	#csv[,4:6] = csv[,4:6]-csv[,3]
	csv$Scenario = scenario[i]
	csv$Year = Year[i]
	outdf = rbind(outdf,csv)
}

outdf = outdf[,c("Country","GID_0","Region","Scenario","Year","Predicted")]
colnames(outdf)[6] = "Inequality_btwn"

outdf = rbind(datab,outdf)
head(outdf)
outdf$Region= as.character(outdf$Region)
outdf[outdf$Country=="India","Region"] = "India"
outdf[outdf$Country=="China","Region"] = "China"
outdfb = outdf
return(list(outdfw,outdfb))
}

rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}
