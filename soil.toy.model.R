library(tidyverse)
library(lubridate)
library(anytime)
library(ggpubr)
# library(tictoc)
#* In this V5 version, I am introducing a snowpack and snow melt functionality 
#* Based on snowpack threshold and snowmelt threshold
#* ALSO CHANGED THE DRYING FUNCTION




Desktop.path <- setwd("C:/Ashif/OneDrive - USNH/Research Stuffs/NEON_data/")
# mac.path <- setwd("~/Library/CloudStorage/OneDrive-USNH/Research Stuffs/NEON_data/")
# home.path <- setwd("C:/OneDrive/OneDrive - USNH/Research Stuffs/NEON_data/")










# tic("Time Required to run the Model:")

#* Creating the Initial Dataframe with initial input data (theta_obs, precipitation, 
#* Air temperature) and water balance parameters with blank data


p1.wat.bal <- data.frame()
# p2.wat.bal <- data.frame()
# p3.wat.bal <- data.frame()
# p4.wat.bal <- data.frame()
# p5.wat.bal <- data.frame()
# 


terr.aq <- read.csv("LatestData/NEON.Terr.Aq.Paired.csv")
# filter(usgs.availability == "Y") %>%
# filter(secprecip.availability == "Y")


site.metadata <- read.csv("NEON_Field_Site_Metadata_20220412.csv")



terrsites.new <- terr.aq$siteID.terrestrial

mysite <- "ORNL"

# print(k)
# mysite <- terrsites.new[k]
# print(mysite)
mysite.aq <- terr.aq$siteID.aquatic[terr.aq$siteID.terrestrial == mysite]
mysite.state <- terr.aq$state[terr.aq$siteID.terrestrial == mysite]

filenames.swc <- list.files(path = "LatestData/daily.soil.swc/",  
                            pattern = mysite,
                            full.names = TRUE)

filenames.airtemp <- list.files(path = "LatestData/daily.air.temp/",  
                                pattern = mysite,
                                full.names = TRUE)
filenames.dlength <- list.files(path = "LatestData/daylength/",  
                                pattern = mysite,
                                full.names = TRUE)

filenames.swc.sensorpos <- list.files(path = "LatestData/soil.swc.sensorpos/",  
                                      pattern = mysite,
                                      full.names = TRUE)

filenames.discharge.neon <- list.files(path = "LatestData/daily.discharge/",  
                                       pattern = mysite.aq,
                                       full.names = TRUE)



filenames.discharge.usgs <- list.files(path = "LatestData/daily.discharge.usgs/",  
                                       pattern = mysite.aq,
                                       full.names = TRUE)



filenames.soiltemp <- list.files(path = "LatestData/daily.soil.temp/",  
                                 pattern = mysite,
                                 full.names = TRUE)

filenames.soilco2 <- list.files(path = "LatestData/daily.soil.co2/",  
                                pattern = mysite,
                                full.names = TRUE)




meg.pit <- read.csv("LatestData/soil.properties/soil.megapit/mgp_perbiogeosample.csv")%>% 
  filter(siteID == mysite) %>% 
  filter(biogeoBottomDepth <=100)

bden <- read.csv("LatestData/soil.properties/soil.megapit/mgp_perbulksample.csv") %>% 
  filter(siteID == mysite) %>% 
  filter(bulkDensBottomDepth <=30)


soilOC <- mean(meg.pit$estimatedOC, na.rm=T)


soil.temp <- read.csv(filenames.soiltemp) %>% 
  mutate(day = anydate(day))  %>% 
  # complete(day = seq.Date(min(as.Date(dlength$day)), max(as.Date(dlength$day)), by = "day")) %>% 
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31")) %>% 
  select(c(day,ends_with("501"))) %>% 
  fill(stemp.001_501, .direction = 'down')

soil.co2 <- read.csv(filenames.soilco2)  %>% 
  mutate(day = anydate(day)) %>% 
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31"))


field.latitude <- site.metadata$field_latitude[site.metadata$field_site_id == mysite]


dlength <- read.csv(filenames.dlength) %>%
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31")) %>%
  mutate(day = anytime(day)) %>% 
  select(-daylength, -frac.day) %>% 
  mutate(DOY = c(1:365,1:366,1:365,1:365)) %>% 
  mutate(delta = -23.44*cos((360/365)*(DOY + 10)*pi/180) ) %>% 
  mutate(daylength= (1/pi)*acos(-tan(field.latitude*pi/180)*tan(delta*pi/180)) )




soil.swc <- read.csv(filenames.swc) %>% 
  mutate(day = anydate(day)) %>% 
  complete(day = seq.Date(min(as.Date(dlength$day)), max(as.Date(dlength$day)), by = "day")) %>% 
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31"))


#* Reading Precipitation data for NEON sites. Two types of precipitation: 
#* Primary and secondary. Secondary data is of best quality and is used on 
#* priority basis. For sites where secondary data is not available, primary
#* data is used. The condion below checks for secondary precipitation data for
#* a given NEON site, if it finds secondary precipitation data, it reads right away.
#* If the scondary precip data is unavailable, primary precipitation data is read instead.


if(file.exists(paste0("LatestData/daily.sec.precip/",
                      "sec.precip.daily.",mysite,
                      ".csv"))) {
  precip <- read.csv(paste0("LatestData/daily.sec.precip/",
                            "sec.precip.daily.",mysite,
                            ".csv"))
} else {
  precip <- read.csv(paste0("LatestData/daily.pri.precip/",
                            "pri.precip.daily.",mysite,
                            ".csv"))
}


precip <- precip %>% 
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31")) %>% 
  mutate(yr = year(day))


airtemperature <- read.csv(filenames.airtemp) %>% 
  mutate(day = anytime(day)) %>% 
  complete(day = seq.Date(min(as.Date(dlength$day)), max(as.Date(dlength$day)), by = "day")) %>% 
  filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31")) %>%
  fill(airtemp, .direction = 'down') %>% 
  fill(airtemp, .direction = 'up')


### Reading USGS Discharge (if available for the site)
if(file.exists(paste0("LatestData/daily.discharge.usgs/",
                      "daily.discharge.usgs.",mysite.aq,
                      ".csv"))) {
  discharge.usgs <- read.csv(paste0("LatestData/daily.discharge.usgs/",
                                    "daily.discharge.usgs.",mysite.aq,
                                    ".csv")) %>% 
    mutate(day = anytime(dateTime)) %>%
    complete(day = seq.Date(min(as.Date(dlength$day)), max(as.Date(dlength$day)), by = "day")) %>% 
    filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31"))
} else {
  discharge.usgs <- data.frame(discharge.mm.perday = NA)
}



### Reading NEON Discharge (If Available for the site)
if(file.exists(paste0("LatestData/daily.discharge/",
                      "discharge.daily.",mysite.aq,
                      ".csv"))) {
  discharge.neon <- read.csv(paste0("LatestData/daily.discharge/",
                                    "discharge.daily.",mysite.aq,
                                    ".csv")) %>% 
    mutate(day = anytime(day)) %>%
    complete(day = seq.Date(min(as.Date(dlength$day)), max(as.Date(dlength$day)), by = "day")) %>% 
    filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31"))
} else {
  discharge.neon <- data.frame(day = NA,
                               discharge.liter.per.sec = NA,
                               discharge.mm.perday = NA)
}

swc.sensorpos <- read.csv(filenames.swc.sensorpos)



#********** Reading soil Porosity ************
filenames.porosity <- list.files(path = "LatestData/soil.blkden.porosity/",  
                                 pattern = mysite,
                                 full.names = TRUE)
porosity <- read.csv(filenames.porosity)

por.1.501 <- porosity$porosity[porosity$HOR.VER == 1.501]
rooting.depth <- 1 ### assuming 1 meter

#************ Defining the sampling space for drainage. fc. wp ******

# d.rate.p1.all <- seq(from = 0.012, to = 1, by = 0.01)
# p1.f.c.all <- seq(from = 0.10, to = 0.45, by = 0.01)
# # p1.w.p.all <- seq(from = w.p.lower, to = w.p.upper, by = 0.01)
# 
# p1.w.p.all <- seq(from = 0.05, to = 0.30, by = 0.01)
# 
# sat.rand.p1.all <- seq(from = 0.35, to = 0.60, by = 0.01)

#### ebd of the input files. 



source("NEON_RCodes/water.balance/water.balance.func.R")
# source("NEON_RCodes/DIC.func.R")
source("NEON_RCodes/resp.func.R")
source("NEON_RCodes/efflux.func.R")


p1.wat.bal <- water.balance.func(mysite, 0.35, 0.15, 0.48, 0.10)


# # dic.soil <- DIC.func(mysite, 3)
# soil_respiration <- data.frame()
# resp.rate.random <- 0.000030
# 
# day <- 1
# 
# respiration <- soil.resp.func(mysite, resp.rate.random, 1461)
# outputs <- effl.func(mysite, 1461)
# 
# 
# efflux <- as.data.frame(outputs[1])  
# dic <- as.data.frame(outputs[2])
# 
# dd <- format(efflux, scientific=F)





##### Soil Respiration
library(tidyverse)
resp.rate <- 0.000030




source("NEON_RCodes/gcpersqm.to_ppm.R")
soil_respiration <- data.frame()

site.name <- "ORNL"

  
  filenames.soiltemp <- list.files(path = "LatestData/daily.soil.temp/",  
                                   pattern = site.name,
                                   full.names = TRUE)
  filenames.soilco2 <- list.files(path = "LatestData/daily.soil.co2/",  
                                  pattern = site.name,
                                  full.names = TRUE)
  
  meg.pit <- read.csv("LatestData/soil.properties/soil.megapit/mgp_perbiogeosample.csv")%>% 
    filter(siteID == site.name) %>% 
    filter(biogeoBottomDepth <=100)
  
  bden <- read.csv("LatestData/soil.properties/soil.megapit/mgp_perbulksample.csv") %>% 
    filter(siteID == site.name) %>% 
    filter(bulkDensBottomDepth <=30)
  
  
  soilOC <- mean(meg.pit$estimatedOC, na.rm=T)
  
  
  soil.temp <- read.csv(filenames.soiltemp) %>% 
    mutate(day = anydate(day))  %>% 
    filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31")) %>% 
    select(c(day,ends_with("501"))) %>% 
    fill(stemp.001_501, .direction = 'down')
  
  
  soil.co2 <- read.csv(filenames.soilco2)  %>% 
    mutate(day = anydate(day)) %>% 
    filter(day >= as.Date("2019-01-01") & day <= as.Date("2022-12-31"))
  
  
  
  
  porosity <- 1- ( mean(bden$bulkDensExclCoarseFrag, na.rm = T)/2.65 )
  
  
  ### Defining Parameters
  b = 0.75
  theta.op <- 0.65 * porosity
  ns <- 2
  k.theta <- 0.10
  
  clay.c <- mean(meg.pit$clayTotal, na.rm=T) / 100
  
  # a <- ifelse(clay.c <= 0.016, 0, ifelse(clay.c > 0.016 & clay.c <=0.37, (2.8*clay.c - 0.046), 1))
  
  a <- if(clay.c <= 0.016) {
    0} else if(clay.c > 0.37) {
      1} else{(2.8*clay.c - 0.046)
      }
  
  
  
  soil_respiration[1:1461,"day"] <- seq(as.Date("2019-01-01"), as.Date("2022-12-31"), by = "day")
  soil_respiration[1:1461,"siteID"] <- site.name
  
  
  for (j in 1:1461) {
    
    soil_respiration[j,"fm.plot1"] <- ifelse(p1.wat.bal[j,"theta.pred"] < theta.op,
                                             ( ((k.theta + theta.op)/(k.theta + p1.wat.bal[j,"theta.pred"]) ) * (p1.wat.bal[j,"theta.pred"]/theta.op)^(1+a*ns) ),
                                             ( (porosity - p1.wat.bal[j,"theta.pred"])/(porosity - theta.op) )^b
    )
    
    
    
  }
  
  
  soil_respiration[1:1461, "soil.temp.plot1"] <- soil.temp$stemp.001_501
  
  soil_respiration[1:1461, "obs.soil.co2.plot1.ppm"] <- soil.co2$sco2.001_501
  soil_respiration[1:1461, "DOY"] <- seq(1,1461, 1)
  
  SOC.gcpersqm <- 10*max(bden$bulkDensBottomDepth) * 
    mean(bden$bulkDensExclCoarseFrag, na.rm=T) * 
    mean(meg.pit$estimatedOC, na.rm=T)
  
  
  
  # for (j in 1:1461) {
  # 
  #   soil_respiration[j, "mod.resp.rate.p1.gc.persqm"] <-  SOC.gcpersqm * resp.rate * soil_respiration[j,"fm.plot1"] * 2^((soil_respiration[j,"soil.temp.plot1"] - 20)/10)
  # 
  #   soil_respiration[j, "mod.soil.co2.plot1.ppm"] <- gc_persqm_to_ppm(soil_respiration[j,"mod.resp.rate.p1.gc.persqm"],
  #                                                                     soil_respiration[j,"soil.temp.plot1"] )
  # 
  # }
  
    # approx(soil_respiration$day, soil_respiration$soil.temp.plot1, as.Date("2019-01-03"))$y
  
  
  params<-c(Resp.rate.mod= 0.000030,
            efflux.rate = 0.009,
            drain.rate = 0.006)
  
  
  state <-c( soil.co2.gC.per.sqm= 3,   #1
             # SOC.gc.per.sqm= SOC.gcpersqm, #2
            soil.co2.ppm=6054,   #3
            co2.efflux.gc.persqm =1,  #4
            soil.remaining.co2.gas.gc.per.sqm=2,  #5
            soil.remaining.co2.ppm = 4036,  #6
            drain.DIC.loss.gc.persqm = 0,  #7
            final.co2.gc.persqm = 2  #8
            )
  
  
  soil.C.model<-function(t,state,parameters)
  {
    with(as.list(c(state,parameters)),{
      
      current_temp <- approx(soil_respiration$DOY, soil_respiration$soil.temp.plot1, t)$y
      current.fm <- approx(soil_respiration$DOY, soil_respiration$fm.plot1, t)$y
      # atm.co2 <- 420 ###ppm
      
      # dSOC <- - SOC.gc.per.sqm * Resp.rate.mod * current.fm * 2^((current_temp - 20)/10)  #1
      dsoil.co2.gC.per.sqm <- SOC.gcpersqm * Resp.rate.mod * current.fm * 2^((current_temp - 20)/10)  #2
      dsoil.co2.ppm <- gc_persqm_to_ppm(dsoil.co2.gC.per.sqm,current_temp)  #3
      dco2.efflux.gc.persqm <- -efflux.rate * ( dsoil.co2.ppm - 420)  #4
      dsoil.remaining.co2.gas.gc.per.sqm <- dsoil.co2.gC.per.sqm - dco2.efflux.gc.persqm  #5
      dsoil.remaining.co2.ppm <- gc_persqm_to_ppm(dsoil.remaining.co2.gas.gc.per.sqm, current_temp)  #6
      
      ddrain.DIC.loss.gc.persqm <- -dsoil.remaining.co2.gas.gc.per.sqm * drain.rate
      dfinal.co2.gc.persqm <- dsoil.remaining.co2.gas.gc.per.sqm - ddrain.DIC.loss.gc.persqm
      

      list(c(dsoil.co2.gC.per.sqm,dsoil.co2.ppm,
             dco2.efflux.gc.persqm,dsoil.remaining.co2.gas.gc.per.sqm, dsoil.remaining.co2.ppm,
             ddrain.DIC.loss.gc.persqm, dfinal.co2.gc.persqm))
    })
  }
  
  
  # times <-seq(as.Date("2019-01-01"), as.Date("2022-12-31"), by = 1)
  
  times <- seq(1, 365, 1)
  
  library(deSolve)
  
  gg <- ode(state,times,soil.C.model,params)
  
  
  out <- as.data.frame(ode(state,times,soil.C.model,params))
  head(out)
  
  # plot(gg)
  
  
  plot(diff(out$co2.efflux.gc.persqm))
  plot(diff(out$soil.co2.gC.per.sqm))
  plot(diff(out$soil.remaining.co2.ppm))
  plot(diff(out$soil.remaining.co2.gas.gc.per.sqm))
  plot(diff(out$drain.DIC.loss.gc.persqm))
  
  
  makesense <- out %>% 
    select(time, SOC.gc.per.sqm, soil.co2.gC.per.sqm, co2.efflux.gc.persqm,
           soil.remaining.co2.gas.gc.per.sqm, drain.DIC.loss.gc.persqm, final.co2.gc.persqm)
    
  
  
  instantatenouis <- data.frame(efflux = diff(makesense$co2.efflux.gc.persqm)) 
  
  
  
  
  ######## NEW
  
  params<-c(Resp.rate.mod= 0.00030,
            efflux.rate = 0.0045,
            drain.rate = 0.006)
  
  
  state <-c(  SOC.gc.per.sqm= SOC.gcpersqm, #1 
    
            soil.pore.co2.gC.per.sqm= 3,   #2
             
             soil.pore.co2.ppm=6054,   #3
            soil.co2.efflux = 3,
             soil.pore.co2.after.efflux.ppm =2000,  #4
            
            soil.pore.co2.after.efflux.gc.per.sqm=1,  #5
            
            soil.pore.co2.after.Drain.gc.persqm = 1  #6
  )
  
  
  soil.C.model<-function(t,state,parameters)
  {
    with(as.list(c(state,parameters)),{
      
      current_temp <- approx(soil_respiration$DOY, soil_respiration$soil.temp.plot1, t)$y
      current.fm <- approx(soil_respiration$DOY, soil_respiration$fm.plot1, t)$y
      
      dSOC <- -SOC.gc.per.sqm * Resp.rate.mod * current.fm * 2^((current_temp - 20)/10)  #1
      dsoil.pore.co2.gC.per.sqm <-  -dSOC  #2
      dsoil.pore.co2.ppm <- gc_persqm_to_ppm(dsoil.pore.co2.gC.per.sqm,current_temp)  #3
      
      dsoil.co2.efflux <- efflux.rate * ( dsoil.pore.co2.ppm - 420)
      
      dsoil.pore.co2.after.efflux.ppm <- dsoil.pore.co2.ppm  -efflux.rate * ( dsoil.pore.co2.ppm - 420)  #4
      
      dsoil.pore.co2.after.efflux.gc.per.sqm <- ppm_to_gc_persqm(dsoil.pore.co2.after.efflux.ppm, current_temp) #5
      
      dsoil.pore.co2.after.Drain.gc.persqm <-  dsoil.pore.co2.after.efflux.gc.per.sqm -dsoil.pore.co2.after.efflux.gc.per.sqm * drain.rate  #6

      
      list(c(dSOC, dsoil.pore.co2.gC.per.sqm, dsoil.pore.co2.ppm, dsoil.co2.efflux, 
             dsoil.pore.co2.after.efflux.ppm, dsoil.pore.co2.after.efflux.gc.per.sqm,
             dsoil.pore.co2.after.Drain.gc.persqm))
    })
  }
  
  
  # times <-seq(as.Date("2019-01-01"), as.Date("2022-12-31"), by = 1)
  
  times <- seq(1, 1461, 1)
  
  library(deSolve)
  
  gg <- ode(state,times,soil.C.model,params)
  pp <-   lsoda(state,times,soil.C.model,params)

  
  out <- as.data.frame(lsoda(state,times,soil.C.model,params))
  head(out)
  
  plot(gg)
  plot(pp)
  
  plot(diff(out$soil.pore.co2.ppm))
  plot(diff(out$soil.pore.co2.after.efflux.ppm))
  plot(diff(out$soil.pore.co2.after.Drain.gc.persqm))
  plot(diff(out$soil.co2.efflux))
  # plot(diff(out$drain.DIC.loss.gc.persqm))
  

  ddd <- soil.co2[1:1460,] %>% 
    fill(sco2.001_501, .direction = 'down')
  
  
  
  
  roro <- out %>% 
    # complete(soil.pore.co2.after.efflux.ppm, fill = "down")
  drop_na(soil.pore.co2.after.efflux.ppm)
  
  
  par(mfrow=c(1,1), oma=c(0,0, 3,0))
  plot (1:1460,diff(out$soil.pore.co2.ppm),type="l",main="[modeled]",
        xlab="time", ylab="efflux",lwd=2)
  plot (1:1461,soil.co2$sco2.001_501,type="l",main="NEON",
        xlab="time",ylab="efflux",lwd=2)

  
  rtrt <- data.frame(mod.sco2 = diff(out$soil.pore.co2.after.efflux.ppm),
                     neon.sco2 = ddd$sco2.001_501)
  
  
  plot(rtrt$neon.sco2,
     rtrt$mod.sco2)  
