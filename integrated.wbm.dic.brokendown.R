library(tidyverse)
library(lubridate)
library(anytime)
library(ggpubr)
# library(tictoc)
#* In this V5 version, I am introducing a snowpack and snow melt functionality 
#* Based on snowpack threshold and snowmelt threshold
#* ALSO CHANGED THE DRYING FUNCTION




# Desktop.path <- setwd("C:/Ashif/OneDrive - USNH/Research Stuffs/NEON_data/")
mac.path <- setwd("~/Library/CloudStorage/OneDrive-USNH/Research Stuffs/NEON_data/")
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
  
  mysite <- "WREF"
  
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
  
  
  p1.wat.bal <- water.balance.func(mysite, 0.35, 0.15, 0.48, 0.01)
   
  
  # dic.soil <- DIC.func(mysite, 3)
  soil_respiration <- data.frame()
  resp.rate.random <- 0.0000003
  
  day <- 1
  
  respiration <- soil.resp.func(mysite, resp.rate.random, 5)
  outputs <- effl.func(mysite, 5)


  efflux <- as.data.frame(outputs[1])  
  dic <- as.data.frame(outputs[2])
  
 dd <- format(efflux, scientific=F)
# write.csv(eff,"NEON_RCodes/cbalance.debug.csv")

  
  
  
  
  blah <- efflux %>% 
    select(day, mod.resp.rate.p1.gc.persqm, mod.resp.rate.ppm, initial.soil.co2.ppm, initial.soil.co2.gcpersqm,
           soil.co2.afterD.gc.persqm, efflux.p1.gc.persqm.perday, efflux.p1.micromol.persqm.persec, mod.residual.soil.co2.gc.persqm,
           mod.residual.soil.co2.ppm, mass.balance.gc.persqm)
  
  
  
  
  ttt <- efflux %>% 
    select(day, mod.resp.rate.p1.gc.persqm, initial.soil.co2.gcpersqm, DIC.gC.persqm.perday,
           soil.co2.afterD.gc.persqm, efflux.p1.gc.persqm.perday, mod.residual.soil.co2.gc.persqm,
           mass.balance.gc.persqm)
  
  