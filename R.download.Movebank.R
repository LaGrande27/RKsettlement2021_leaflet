#################################################
###### Download movement data from Movebank #####
#################################################

# 1 # get movement data from Ecotone
# 2 # get movement data from Milsar
# 3 # merge Milsar & Ecotone
# 4 # clean up data
# 5 # simplify data, save 

# 6 # old: other way for Ecotone data: download from Ecotone panel


#################################################

## load required libraries ### -----
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, # always
               raster, #for function extent() and crop()
               move #for Movebank Login
               )

# other packages, perhaps for my old, manual way of downloading GPS data (from ecotone panel)
#packages(mapproj)
#packages(lubridate)
#packages(tidyverse)
#packages(beepr)
#packages(plotKML)
#packages(rgeos)
#packages(htmlwidgets)
#packages(htmltools)
#packages(reshape)
#packages(mapdata)
#packages(scales)
#packages(moveVis)
#packages(RCurl)
#packages(bitops)


#################################################

options(scipen = 999) #R avoids scientific style of numbers (options(scipen=0) reset to default)
SWI <- extent(4.955849, 9.242637, 45.56534, 47.88471)
EU <- extent(-21.72941, 27.72941, 32.7168, 60.29146)

##### choose parameter !!!!!!!!!!! ######## -------
week_duration <- 1 ##choose time period (amount of weeks), if the download should be limited to certain time period
movebank_username #<- your username
movebank_password #<- your password


setwd("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
curl <- movebankLogin(username=movebank_username,  password=movebank_password)




#gps.df1.0 <- subset(gps.df1, as.factor(NAME) %nin% as.factor(settlers2$TransmGSM))


############################################################################################################################
################################################## old birds, set #1 -------------------------------------------------------
{
settlers1 <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_1.csv", header=T, dec=".", sep=";")
settlers01 <- settlers1 %>% dplyr::select(bird_ID) %>% as.list()

#################### get movement data from ecotone #################### 
## define start end end timestamp from system time ## (if needed)
{
end <-  Sys.Date()
actualdate <- format(end, format="%Y%m%d%H%M%S")
timestamp_end <- paste0(actualdate, "000")

startingdate <- format(as.Date("2021-01-01"), format="%Y%m%d%H%M%S")
timestamp_start <- paste0(startingdate, "000")
}

## load data of all birds instead of selected birds because of problem downloading ###
#ecotone <- getMovebankData(study="Milvusmilvus_GSM_SOI", login=curl, moveObject=TRUE) #if below doesn't work
ecotone1 <- getMovebankData(study="Milvusmilvus_GSM_SOI", animalName = as.character(settlers01$bird_ID), login=curl, moveObject=TRUE)
### or ###
{
## load data of settlers in specified timeframe ###
ecotone1 <- settlers %>% dplyr::select(bird_ID) %>% as.list()
ecotone <- getMovebankData(study="Milvusmilvus_GSM_SOI", animalName = as.character(ecotone1$bird_ID), login=curl, moveObject=TRUE, timestamp_start=timestamp_start, timestamp_end=timestamp_end)
#ecotone<- crop(ecotone, extent)
#all_juv <- moveStack(juv_17, juv_16, juv_15)   ## stack all data into one moveStack for easier handling ##
}

gps1.e <- as.data.frame(ecotone1)

# einlesen der runtergeladenen Daten von Movebank als dataframe
gps0.e1 <- gps1.e %>% dplyr::select(bird_id='local_identifier', timestamp, long='location_long', lat='location_lat', TransmGSM='comments')
gps0.e1 <- subset(gps0.e1, bird_id %in% settlers01$bird_ID)
gps0.e1$bird_id <- as.character(gps0.e1$bird_id)
gps.e1 <- gps0.e1 %>% arrange(bird_id, timestamp) %>% droplevels




#################### get movement data from Milsar #################### 

## load data of settlers in specified timeframe ###
milsar1 <- getMovebankData(study="Milvusmilvus_Milsar_SOI_final", animalName = as.character(settlers01$bird_ID), login=curl, moveObject=TRUE)
gps1.m <- as.data.frame(milsar1) #different number of columns than Ecotone

# einlesen der runtergeladenen Daten von Movebank als dataframe
gps0.m1 <- gps1.m %>% dplyr::select(bird_id='local_identifier', timestamp, long='location_long', lat='location_lat', TransmGSM='tag_local_identifier')
gps0.m1 <- subset(gps0.m1, bird_id %in% settlers01$bird_ID)
gps0.m1$bird_id <- as.character(gps0.m1$bird_id)
gps.m1 <- gps0.m1 %>% arrange(bird_id, timestamp) %>% droplevels



#################### merge Milsar & Ecotone #################### 
gps1 <-rbind(gps.e1, gps.m1)



#################### clean up data #################### 

## transform df to spatial point df
coordinates(gps1) = cbind(gps1$long, gps1$lat)
class(gps1)


####### filter geographically (one outlier in China) ####### 
EU.extent <- extent(-21.72941, 27.72941, 32.7168, 60.29146)
gps1<- crop(gps1, EU.extent)


####### filter speed outliers ####### 

#set coord system for SPDF – 3035 ist in Europa flächentreu
CH.CRS = "+init=epsg:21781"
EU.CRS = "+init=epsg:3035"
latlong="+init=epsg:4326"

proj4string(gps1)
proj4string(gps1) = CRS(EU.CRS)
gps01 <- spTransform(gps1, CRS(EU.CRS))
gps.3035 <- as.data.frame(gps01) ## turn back into data frame

## calculate speed from step length and tdiff
jd1 <- gps.3035 %>% group_by(bird_id) %>% mutate(tdiff = lead(timestamp)-timestamp, dist= sqrt(((lead(coords.x1)-coords.x1)^2) + ((lead(coords.x2)-coords.x2)^2) ), speed=dist/as.numeric(tdiff, unit="secs") )

## filter speed outliers
jd01 <- jd1 %>% group_by(bird_id) %>% filter(speed<=33.3 & lag(speed<=33.3)) # filter positions with speeds higher than 120 km/h and subsequent positions
jd01$speed[jd01$tdiff<60] <- NA # set master reset position speed to NA, as it is rarely correct



#################### simplify data, rearrange, save #################### 

# for now, filter dates till end of 2020 + a few days for the KDE estimation
gps.tibble1 <- jd01 %>% dplyr::select(bird_id, TransmGSM, timestamp, long, lat) %>%
  filter(timestamp < "2021-01-10 00:00:00") 
gps.df1 <- as.data.frame(droplevels(gps.tibble1))

# rearrange to conform with KDE function
gps.df1$timestamp <- substr(gps.df1$timestamp, 1, 16)
gps.df1$DATE <- substr(gps.df1$timestamp, 1, 10)

gps.df01 <- gps.df1 %>% 
  dplyr::select(NAME='TransmGSM', DATE, TIME='timestamp', LON='long', LAT='lat', BIRD_ID='bird_id')
head(gps.df01)

# somehow some double rows
gps.df01 <- gps.df01 %>% unique

# save
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
write.table(gps.df01, paste0(pathKML, "/2015-2015-2020coordinates_birds1.csv"), row.names=F, sep=";")

}







############################################################################################################################
################################################## new birds, set 2 (constantly updated) -----------------------------------
{
settlers2 <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_2.csv", header=T, dec=".", sep=";")
settlers02 <- settlers2 %>% dplyr::select(bird_ID) %>% as.list()

#################### get movement data from ecotone #################### 

## load data of all birds instead of selected birds because of problem downloading ###
ecotone2 <- getMovebankData(study="Milvusmilvus_GSM_SOI", animalName = as.character(settlers02$bird_ID), login=curl, moveObject=TRUE)
gps2.e <- as.data.frame(ecotone2)

# einlesen der runtergeladenen Daten von Movebank als dataframe
gps0.e2 <- gps2.e %>% dplyr::select(bird_id='local_identifier', timestamp, long='location_long', lat='location_lat', TransmGSM='comments')
gps0.e2 <- subset(gps0.e2, bird_id %in% settlers02$bird_ID)
gps0.e2$bird_id <- as.character(gps0.e2$bird_id)
gps.e2 <- gps0.e2 %>% arrange(bird_id, timestamp) %>% droplevels



#################### get movement data from Milsar #################### 

## load data of settlers in specified timeframe ###
milsar2 <- getMovebankData(study="Milvusmilvus_Milsar_SOI_final", animalName = as.character(settlers02$bird_ID), login=curl, moveObject=TRUE)
gps2.m <- as.data.frame(milsar2) #different number of columns than Ecotone

# einlesen der runtergeladenen Daten von Movebank als dataframe
gps0.m2 <- gps2.m %>% dplyr::select(bird_id='local_identifier', timestamp, long='location_long', lat='location_lat', TransmGSM='tag_local_identifier')
gps0.m2 <- subset(gps0.m2, bird_id %in% settlers02$bird_ID)
gps0.m2$bird_id <- as.character(gps0.m2$bird_id)
gps.m2 <- gps0.m2 %>% arrange(bird_id, timestamp) %>% droplevels


#################### merge Milsar & Ecotone #################### 
gps2 <-rbind(gps.e2, gps.m2)



#################### clean up data #################### 
## transform df to spatial point df
coordinates(gps2) = cbind(gps2$long, gps2$lat)
class(gps2)

####### filter geographically (one outlier in China) ####### 
EU.extent <- extent(-21.72941, 27.72941, 32.7168, 60.29146)
gps2<- crop(gps2, EU.extent)

####### filter speed outliers ####### 
#set coord system for SPDF – 3035 ist in Europa flächentreu
CH.CRS = "+init=epsg:21781"
EU.CRS = "+init=epsg:3035"
latlong="+init=epsg:4326"

proj4string(gps2)
proj4string(gps2) = CRS(EU.CRS)
gps2.2 <- spTransform(gps2, CRS(EU.CRS))

## turn back into data frame
gps.30352 <- as.data.frame(gps2.2)

## calculate speed from step length and tdiff
jd2 <- gps.30352 %>% group_by(bird_id) %>% mutate(tdiff = lead(timestamp)-timestamp, dist= sqrt(((lead(coords.x1)-coords.x1)^2) + ((lead(coords.x2)-coords.x2)^2) ), speed=dist/as.numeric(tdiff, unit="secs") )

## filter speed outliers
jd02 <- jd2 %>% group_by(bird_id) %>% filter(speed<=33.3 & lag(speed<=33.3)) # filter positions with speeds higher than 120 km/h and subsequent positions
jd02$speed[jd02$tdiff<60] <- NA # set master reset position speed to NA, as it is rarely correct



#################### simplify data, rearrange, save #################### 

# no time filter here. End = date of last download of data
gps.tibble2 <- jd02 %>% dplyr::select(bird_id, TransmGSM, timestamp, long, lat)
gps.df2 <- as.data.frame(droplevels(gps.tibble2))
# all unique rows, I checked  ( nrow(gps.df2); nrow(unique(gps.df2)) )

# rearrange to conform with KDE function
gps.df2$timestamp <- substr(gps.df2$timestamp, 1, 16)
gps.df2$DATE <- substr(gps.df2$timestamp, 1, 10)

gps.df02 <- gps.df2 %>% 
  dplyr::select(NAME='TransmGSM', DATE, TIME='timestamp', LON='long', LAT='lat', BIRD_ID='bird_id')
head(gps.df02)

# somehow some double rows
gps.df02 <- gps.df02 %>% unique

# save
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
write.table(gps.df02, paste0(pathKML, "/2015-2021coordinates_birds2.csv"), row.names=F, sep=";")

}











################################################## old approach ------------------------------------------------------------
#################################################
###################### old: #####################
########## other way for Ecotone data: ##########
########## download from Ecotone panel ##########
#################################################
{
  ##################
  #### Directory ###
  ##################
  # 1 # automatically download all files from ecotone website
  # 2 # aggregate all KMZ files into 1 dataset
  # 3 # Milsar dataset
  
####################### 1 #######################-------------------------------------------------
####### automatically download all files ########
############# from ecotone website ##############
#################################################

#set wd so data gets downloaded to the right place
setwd("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation/KMZ")

#list of settlers, for which the data has to be downloaded
settlers <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_1.csv", header=T, dec=".", sep=";")
#Ecotone settlers
settlerGSM.ecotone <- subset(settlers, stri_length(settlers$TransmGSM)<7)


# 1 # Find the right Index website #####

# enter user credentials
user     <- "guestswikites"
password <- "swikites"
credentials <- paste0(user, ":", password, "@", sep="")
web.site <- "telemetry.ecotone.pl/swikites/paths_positions/month/"

# years & months to be downloaded
year <- as.integer(c(2015:2021))
month <- c(1:12)                   ### ! if months not continuous, change reformatting function for rearranging Milsar data ! ###
month <- c(1)
# think about downloading also January of the following year (e.g. interested in 2019 & 2020: download also Jan 2021)
# to have a complete data for month December (in example Dec 2020)
# (otherwise the last days of december (4-10 days) are always missing, because the code will not compute KDE home ranges without enough
# days for the moving window (4-10 days)).
# Later: cut data after 31.12. of the last required year

# if downloading selected birds / years:
# additional years for selected birds, when the normal 2 years seem to not have enough settlement/potato information
selected.birds <- c("KIWS13", "KIWS37", "KIWS47", "KIWS60", "KIWS61", "KIWS75", "SWIK07", 
                    "SWIK12", "SWIK16", "SWIK35", "SWIK49", "SWIK69", "SWIK73", "SWIK88", "SWIK95",
                    "WSIK12", "WSIK18", "WSIK31", "WSIK39", "WSIK39", "WSIK40", "WSIK56", "WSIK57",
                    "WSIK58", "WSIK73", "SWIK88", "KIWS75", "SWIK52", "WSIK68", "WSIK85", "WSIK50",
                    "WSIK55", "WSIK68", "SWIK46", "KIWS31", "KIWS31", "KIWS48", "KIWS51", "KIWS53",
                    "KIWS93", "SWIK23", "SWIK55", "SWIK55", "SWIK73", "WSIK25", "WSIK87", "WSIK92",
                    "WSIK96")
selected.years <- c(2018, 2018, 2018, 2018, 2018, 2018, 2017,
                    2016, 2016, 2016, 2018, 2017, 2020, 2018, 2017, 
                    2019, 2017, 2018, 2019, 2020, 2018, 2017, 2017,
                    2017, 2018, 2017, 2018, 2020, 2017, 2020, 2017, 
                    2017, 2017, 2016, 2017, 2018, 2018, 2018, 2018, 
                    2018, 2017, 2017, 2018, 2017, 2017, 2018, 2018, 
                    2018)

for (i in seq_along(year)){
  # construct path to data
  path <- paste0("http://", credentials, web.site, year[i], sep="")
  
  for (j in seq_along(month)){
    if (nchar(month[j])==1){
      url <- paste0(path, "0", month[j], "/", sep="")
    } else {
      url <- paste0(path, month[j], "/", sep="")
    }
    
    # 2 # Download the correct birds from this year & month #####
    
    # read HTML document & find list of links for download
    doc <- htmlParse(readLines(url), asText=TRUE)
    links <- xpathSApply(doc, "//a/@href")
    
    #first: is the bird in the wanted time interval?
    ## for selected birds
    #for(k in seq_along(selected.birds)) {
    #if(selected.years[k] == year[i]){
    #bird <- paste0("*", selected.birds[k], "-*", sep="")
    
    ## for birds in settler list
    for(k in seq_along(settlerGSM.ecotone$TransmGSM)) {
      if(settlerGSM.ecotone$Sett_year[k]==year[i] | settlerGSM.ecotone$Sett_year[k]-1 == year[i]){
        bird <- paste0("*", settlerGSM.ecotone$TransmGSM[k], "-*", sep="")
        
        if(length(links[grepl(bird, links)])>0){
          # if bird present at that time, select the wanted birds
          wanted <- links[grepl(bird, links)]
          GetMe <- paste(url, wanted, sep = "")
          
          # download all relevant data into working directory
          lapply(seq_along(GetMe), 
                 function(x) download.file(GetMe[x], wanted[x], mode = "wb"))
        } else next
      } else next
    }
  }
}


#### DON'T forget to also download the telemetry.bio website data via movebank!!! ####
# see below in step 3, or see Patrick Scherler's modified code above #




####################### 2 #######################-------------------------------------------------
#### aggregate all KMZ files into 1 dataset #####
#################################################

pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")

# list the kmz files in a given folder path
KMZs <- list.files(path=paste0(pathKML, "/KMZ"), pattern="*.kmz", full.names=FALSE)

#convert KMZ files into folder with KML files
sapply(KMZs, function(x) unzip(zipfile=paste0(pathKML, "/KMZ/", x, sep=""), 
                               exdir=paste0(pathKML, "/KML", sep="")))

#open those KML files and extract BirdID, Coordinates, Date & Time
KMLs <- list.files(path=paste0(pathKML, "/KML", sep=""), pattern="*.kml", full.names=FALSE)
list.coord <- sapply(KMLs, function(x){
  #open the kml as text file
  kml.text <- readLines(con = paste0(pathKML, "/KML/", x, sep=""))
  
  #locating lines that use certain key words
  re <- "<coordinates> *([^<]+?) *<\\/coordinates>"
  coords <- grep(re, kml.text)
  
  re2 <- "Date and time of GPS position:"
  datetime <- grep(re2, kml.text)
  
  re3 <- "Name:"
  Name <- grep(re3, kml.text)
  
  kml.coordinates <- matrix(0,length(coords),6,dimnames=list(c(),c("NAME","DATE","TIME","LON","LAT","ELEV")))
  
  #Run a loop for each element in the coords vector and
  #collect the results into a matrix with ID, TIME and coordinates
  for(i in 1:length(coords)){
    sub.coords <- coords[i]
    temp1 <- gsub("<coordinates>"," ",kml.text[sub.coords])
    temp2 <- gsub("</coordinates>"," ",temp1)
    coordinates <- as.numeric(unlist(strsplit(temp2,",")))
    
    sub.ID <- datetime[i]
    TIME <- substr(gsub("<b>Date and time of GPS position:</b>"," ",kml.text[sub.ID]), start=32, stop=47)
    DATE <- substr(TIME, start=1, stop=10)
    
    sub.Name <- Name[i]
    NAME <- substr(gsub(paste("<b>Name:</b>"),"",kml.text[sub.Name]), start=7, stop=12)
    
    kml.coordinates[i,] <- matrix(c(NAME, DATE, TIME, coordinates), ncol=6)
  }
  kml.coordinates <- kml.coordinates[,-6]  #delete ELEV
})
df <- do.call("rbind", list.coord)

#final dataset with birdID, coords, date & time
df
write.table(df, paste0(pathKML, "/KML_coordinates.csv"), row.names=F)




####################### 3 #######################-------------------------------------------------
##############  Milsar dataset ##################
#################################################

# download milsar data manually from the movebank website
# into folder "/Kernel Density Estimation/Milsar"

# list all csv files in milsar folder
milsar.raw <- list.files(path=paste0(pathKML, "/Milsar"), pattern="*.csv", full.names=FALSE)

# rearrange and crop to fit ecotone data
reformatted <- lapply(milsar.raw, function(x){
  #rearrange data to fit the ecotone data
  kml.text <- read.table(paste0(pathKML, "/Milsar/", x, sep=""), header=T, dec=".", sep=c(",","\""))
  kml.text <- kml.text %>% dplyr::select(timestamp, location.long, location.lat, tag.local.identifier)
  kml.text <- kml.text[complete.cases(kml.text),]
  kml.text$date <- substr(kml.text$timestamp, start=1, stop=10)
  kml.text$timestamp <- substr(kml.text$timestamp, start=1, stop=16)
  kml.text <- kml.text[, c(4,5,1,2,3)] #reorder columns
  data.table::setnames(kml.text, old = c('tag.local.identifier','date','timestamp','location.long','location.lat'), new = c('NAME','DATE','TIME','LON','LAT'))
  
  #crop to needed time spans
  settlerGSM.milsar <- subset(settlers, stri_length(settlers$TransmGSM)>7 & bird_ID==paste0(substr(x,1,3)))
  kml.text$YEAR <- substr(kml.text$DATE, start=1, stop=4)
  kml.text$MONTH <- as.numeric(substr(kml.text$DATE, start=6, stop=7))
  #kml.text <- subset(kml.text, MONTH>=month[1] & MONTH<=month[length(month)])
  kml.text <- subset(kml.text, YEAR==settlerGSM.milsar$Sett_year |  YEAR==settlerGSM.milsar$Sett_year-1 | YEAR==settlerGSM.milsar$Sett_year-2)
  kml.text <- kml.text[,-c(6:7)]
})

milsar <- do.call("rbind", reformatted)

# remove one year of bad data
milsar1 <- subset(milsar, NAME!="16329F1C")
milsar2 <- subset(milsar, NAME=="16329F1C" & substr(milsar$DATE, start=1, stop=4)!=2018)
milsar <- rbind(milsar1, milsar2)

write.table(milsar, paste0(pathKML, "/Milsar_coordinates.csv"), row.names=F)



# merge ecotone & milsar data
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
ecotone <- read.table(paste0(pathKML, "/KML_coordinates.csv"), header=T, dec=".", sep=" ")
milsar <- read.table(paste0(pathKML, "/Milsar_coordinates.csv"), header=T, dec=".", sep=" ")

coord1 <- rbind(ecotone, milsar)
coord1$DATE <- as.POSIXct(coord1$DATE)
coord1$TIME <- as.POSIXct(coord1$TIME, format="%Y-%m-%d %H:%M")

#write.table(coord, paste0(pathKML, "/CoordinatesEcotMils.csv"), row.names=F) #somehow problems with import

}