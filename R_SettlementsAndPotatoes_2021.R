# checks if package is installed - if yes loads it (library/require), if not downloads it then loads it
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
# for shiny app
pacman::p_load(dplyr, # always
               ggplot2,
               lemon, #for function facet_rep_wrap()
               lubridate, #time stuff
               viridis, #viridis color scale
               shiny, #shiny app
               #ggmap, #if using maps with download tiles in shiny output
               #rgdal, 
               #purrr,
               leaflet, #map making
               sf, #transforming data into WGS84 coordinates
               htmltools) #if using browsable in leaflet to make legend size smaller
# for the rest
pacman::p_load(sp, #for SpatialPoints()
               adehabitatHR, #for Kernel Density Estimation
               parallel, #for using more than 1 core
               tidyr, #for using gather(), rearranging data
               stringr, #for using str_sub()
               geosphere #for function distm() in standardisation factor step
               #stringi, #for stri_length function
               #XML
               )  

options(scipen = 999) #R avoids scientific style of numbers (options(scipen=0) reset to default)

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis



##################
#### Directory ###
##################-------------------------------------------------
# 1 # load in data, prepare
# 2 # filter to only relevant years
# 4 # perform Kernel Density Estimation
# 5 # insert migration dates 
# 5 # add standardisation factor
# 7 # insert nest coordinates
# 8 # make a shiny app


####################### 1 #######################
############ load in data, prepare ##############
#################################################-------------------------------------------------


# bird batch 1 ------------------------------------------------------------------------------------------
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
gpsss <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1.csv"), header=T, dec=".", sep=";")
#these are all available years 2015-2020 for each kite, downloaded from Movebank
#the data has been filtered to exclude spatial outliers (China) and speed outliers

gpsss$TIME <- as.POSIXct(gpsss$TIME, format="%Y-%m-%d %H:%M")
gpsss$DATE <- as.Date(gpsss$DATE)
str(gpsss)
summary(gpsss) 

#NA in 7 cases in gpsss$TIME --> replace!
{
rownames(gpsss) <- 1:nrow(gpsss) #new rownames to be able to assign values to NAs
gpsss$TIME[125216] <- "2016-03-27 03:00:00" #SWIK38 2016-03-27 03:00:00
gpsss$TIME[197955] <- "2016-03-27 02:00:00" #SWIK31 2016-03-27 02:00:00
gpsss$TIME[240666] <- "2016-03-27 03:00:00" #SWIK52 2016-03-27 03:00:00
gpsss$TIME[311548] <- "2016-03-27 02:00:00" #SWIK46 2016-03-27 02:00:00
gpsss$TIME[1090947] <- "2017-03-26 02:00:00" #WSIK58 2017-03-26 02:00:00
}
write.table(gpsss, paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), row.names=F, sep=";")




# bird batch 2 ------------------------------------------------------------------------------------------
gpsss.b2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2.csv"), header=T, dec=".", sep=";")
#these are all available years 2015-2020 for each kite, downloaded from Movebank
#the data has been filtered to exclude spatial outliers (China) and speed outliers

gpsss.b2$TIME <- as.POSIXct(gpsss.b2$TIME, format="%Y-%m-%d %H:%M")
gpsss.b2$DATE <- as.Date(gpsss.b2$DATE)
str(gpsss.b2)
summary(gpsss.b2) 

#NA in some cases in gpsss.b2$TIME --> replace!
{
  rownames(gpsss.b2) <- 1:nrow(gpsss.b2) #new rownames to be able to assign values to NAs
  gpsss.b2$TIME[54874] <- "2016-03-27 02:00:00" #SWIK55 2016-03-27 02:00:00
  gpsss.b2$TIME[58547] <- "2017-03-26 03:00:00" #SWIK55 2017-03-26 03:00:00
}
write.table(gpsss.b2, paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), row.names=F, sep=";")




# ------------------------------------------------------------------------------------------
#  get list of years in each Kite used previously
#  to use as filter, because using all years will be too much information and take too long to compute
#ya <- setkde %>% dplyr::select(ID,YEAR) %>% unique
#write.table(ya, paste0(pathKML, "/list_of_relevant_years_forKDE_perBird.csv"), row.names=F)


# min max DATE for each kite
mm <- gpsss %>% group_by(NAME) %>% summarise(min=min(DATE), max=max(DATE))
mm2 <- gpsss.b2 %>% group_by(NAME) %>% summarise(min=min(DATE), max=max(DATE))






####################### 4 #######################
###### perform Kernel Density Estimation ########
#################################################-------------------------------------------------
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")

#resulting dataset has:
# BIRD-ID
# START DATE = first day of data that is used for KDE (as reference point for Google Earth visualization)
# NR_POINTS = nr of coordinates for the time period analysed (minimum = minimum_datapoints_per_day = 2)
# AREA_KM2 = area of Kernel Home Range in square kilometers that a bird occupied during x days
# H = smoothing factor
# CENTER.LON_X & CENTER.LAT_Y = centroid of Kernel Home Range (average of all LAT/LON coordinates, unweighted)

# defining variables
kde.percent = 90
days.stationary = 3             # moving window approach with a X day window
minimum.datapoints.per.daylight = 2  # minimum data points during the day. I chose 2 because in winter there is only 1 point every 6h, so at 12:00 and 18:00
first.GPS.point = 9             # GPS points considered should be between 9:00:00
last.GPS.point = 20             # and 20:00:00 (to avoid getting only roosting points -> falsely small home range)
#kde.cutoff.CH = 6              # using country-dependent home range cut-offs to account for different habitat qualities
#kde.cutoff.CZ = 30             # cutoff.CH used for FR, CH, AT, DE; cutoff.CZ for CZ
# CZ: https://link.springer.com/article/10.1007/s10336-020-01811-7 (100 km2 / 3.69 pairs = 27 km2)

# function to calculate KDEs with 5 day-moving window approach
calc.KDE.5dayMovingWindow <- function(coord) {
  
  # remove days without sufficient number of data points (2 per day during daylight)
  nrlocs <- coord %>% group_by(DATE) %>% 
    filter(hour(TIME) >= first.GPS.point & hour(TIME) <= last.GPS.point) %>% 
    summarize(nr_pos = n()) %>% 
    filter(nr_pos >= minimum.datapoints.per.daylight)
  sub_coord <- subset(coord, DATE %in% nrlocs$DATE)
  sub_coord$DATE <- as.Date(sub_coord$DATE, format="%Y-%m-%d") #definitely use "format", because 
  #otherwise the conversion from POSIXct to Date will result in false date, e.g. 2019-03-12 instead of 2019-03-11 !
  
  # Initilizing matrix for storing results
  results = as.data.frame(matrix(NA, nrow=length(unique(sub_coord$DATE)), ncol=7))
  colnames(results)<-c("ID", "START_DATE", "NR_POINTS", "AREA_KM2", "H", "CENTER.LON_X", "CENTER.LAT_Y")
  
  for (i in seq_along(sub_coord$DATE)) {
    
    if(i==1) {
      j=1 # start indexing
      
      # determine start and end date
      start <- sub_coord$DATE[i]
      end <- start + lubridate::hours((days.stationary-1)*24)
      filtercoord <- sub_coord %>% filter(between(sub_coord$DATE, start, end))
      
      # if one day in the moving window is missing, jump to next date
      if (length(unique(filtercoord$DATE))<days.stationary) next else {
        
        # Transform CRS into UTM for Europe, output=meters
        sp <- SpatialPoints(filtercoord[4:5])
        proj4string(sp) = CRS("+init=epsg:4326")
        coords.utm <- spTransform(sp, CRS("+init=epsg:3035"))
        
        # apply KDE only if no error message (error would be: grid too small -> only with very extended home ranges; unimportant for this)
        ud <- kernelUD(coords.utm, h="href")  #calculate the utilization distribution
        x <- try(getverticeshr(ud, percent=kde.percent, unin = 'm', unout='km2')$area, silent=T) #find the 90% utilization distribution
        #getverticeshr$area measures the area of the vector home range (with smoother contour), as opposed to
        #kernel.area, which measures the area covered by the rasterized home range (pixels!)
        
        if('try-error' %in% class(x)) next else {
          # save values in new df
          results[j,1] <- paste0(filtercoord$NAME[1])
          results[j,2] <- paste0(start)
          results[j,3] <- nrow(filtercoord)
          results[j,4] <- round(x, 2)
          results[j,5] <- round(ud@h$h, 2)          # smoothing factor from the utilization distribution
          results[j,6] <- round(mean(filtercoord$LON), 6)
          results[j,7] <- round(mean(filtercoord$LAT), 6)
        }
      }
    }
    
    else if (sub_coord$DATE[i]!=sub_coord$DATE[i-1]) {   #after day 1 jump from day to day, not row to row
      
      # use only 6 days of data
      start <- sub_coord$DATE[i]
      end <- start + lubridate::hours((days.stationary-1)*24)
      filtercoord <- sub_coord %>% filter(between(sub_coord$DATE, start, end))
      
      # if one day in the moving window is missing, jump to next date
      if (length(unique(filtercoord$DATE))<days.stationary) next else {
        
        # Transform CRS into UTM for Europe, output=meters
        sp <- SpatialPoints(filtercoord[4:5])
        proj4string(sp) = CRS("+init=epsg:4326")
        coords.utm <- spTransform(sp, CRS("+init=epsg:3035"))
        
        j = j+1 #use index fo successful loops for assignment of results to rows
        
        # apply KDE only if no error message
        ud <- kernelUD(coords.utm, h="href")  #calculate the utilization distribution
        x <- try(getverticeshr(ud, percent=kde.percent, unin = 'm', unout='km2')$area, silent=T) #find the 90% utilization distribution
        
        if('try-error' %in% class(x)) next else {
          # save values in new df
          results[j,1] <- paste0(filtercoord$NAME[1])
          results[j,2] <- paste0(start)
          results[j,3] <- nrow(filtercoord)
          results[j,4] <- round(x, 2)
          results[j,5] <- round(ud@h$h, 2)          # smoothing factor from the utilization distribution
          results[j,6] <- round(mean(filtercoord$LON), 6)
          results[j,7] <- round(mean(filtercoord$LAT), 6)
        }
      }
    } else next
  }
  results <- results[complete.cases(results), ]
}





# bird batch 1 ------------------------------------------------------------------------------------------
#### Data ####
#coord0 <- read.table(paste0(pathKML, "/2015-2020coordinates_relYears.csv"), header=T, dec=".", sep=";", row.names = )
coord1 <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), header=T, dec=".", sep=";", row.names = )
coord1$TIME <- as.POSIXct(coord1$TIME, format="%Y-%m-%d %H:%M")


#### apply KDE function ####
#estimated duration of following command: >160 minutes
#using parallel cores somehow doesn't work, it never seems to finish calculating
system.time({
  KDE.settlers1 <- do.call(rbind, lapply(split(coord1, coord1$NAME), calc.KDE.5dayMovingWindow))
})

str(KDE.settlers1)
summary(KDE.settlers1)

# also filter to dates before 2021, so that for the year 2021 we'll know where to start
KDE.settlers1 <- subset(KDE.settlers1, as.Date(START_DATE)<"2021-01-01")
# can't have KDE with 0 km2 --#-- to not have problems with log(KDE) in shiny app: transform 0.00 km2 into 0.01 km2:
KDE.settlers1$AREA_KM2 <- ifelse(KDE.settlers1$AREA_KM2 == 0.00, 0.01, KDE.settlers1$AREA_KM2)


# dataname telling moving window length and minimum data points per day
#write.table(KDE.settlers1, paste0(pathKML, "/KDEsettlers_per_",days.stationary,"days_",minimum.datapoints.per.daylight,"ppd_birds1.csv"), row.names=F, sep=";")
write.table(KDE.settlers1, paste0(pathKML, "/KDE_AllSettlers_per_",days.stationary,"days_",minimum.datapoints.per.daylight,"ppd_birds1.csv"), row.names=F, sep=";")







# bird batch 2 ------------------------------------------------------------------------------------------
#### Data ####
#coord0 <- read.table(paste0(pathKML, "/2015-2020coordinates_relYears.csv"), header=T, dec=".", sep=";", row.names = )
coord2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), header=T, dec=".", sep=";", row.names = )
coord2$TIME <- as.POSIXct(coord2$TIME, format="%Y-%m-%d %H:%M")


#### apply KDE function ####
#estimated duration of following command: 17 minutes
#using parallel cores somehow doesn't work, it never seems to finish calculating
system.time({
  KDE.settlers2 <- do.call(rbind, lapply(split(coord2, coord2$NAME), calc.KDE.5dayMovingWindow))
})

str(KDE.settlers2)
summary(KDE.settlers2)

# can't have KDE with 0 km2 --#-- to not have problems with log(KDE) in shiny app: transform 0.00 km2 into 0.01 km2:
KDE.settlers2$AREA_KM2 <- ifelse(KDE.settlers2$AREA_KM2 == 0.00, 0.01, KDE.settlers2$AREA_KM2)


# dataname telling moving window length and minimum data points per day
#write.table(KDE.settlers1, paste0(pathKML, "/KDEsettlers_per_",days.stationary,"days_",minimum.datapoints.per.daylight,"ppd_birds1.csv"), row.names=F, sep=";")
write.table(KDE.settlers2, paste0(pathKML, "/KDE_AllSettlers_per_",days.stationary,"days_",minimum.datapoints.per.daylight,"ppd_birds2.csv"), row.names=F, sep=";")






####################### 5 #######################
############ insert migration dates #############
#################################################-------------------------------------------------
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
settlers <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_all.csv", header=T, dec=".", sep=";")
if ("using the original dataset (for graph 1+2 in shiny):") {
  # !!! changed from Steph's file: converted into CSV and !!! deleted comments !!! (otherwise problems with read.table)
  migr <- read.table(paste0(pathKML, "/Migration Dates (PhD Stephanie Witczak)/migration_dates.csv"), header=T, dec=".", sep=";")
} else if ("also use some self-inserted changes (for graph 3 in shiny, the points used for standardisation):") {
  # more changes: iserted mostly migration dates (roughly) for 2020 fall, sometimes also earlier
  gpsss
  migr <- read.table(paste0(pathKML, "/Migration Dates (PhD Stephanie Witczak)/migration_dates_LaraSomeNewDates2020.csv"), header=T, dec=".", sep=";")
}

#setkde <- read.table(paste0(pathKML, "/KDEsettlers_per_5days_2ppd.csv"), header=T, dec=".", sep=";")
#setkde.all <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd.csv"), header=T, dec=".", sep=";")

# match df with settlers list & clean up df
migr.settlers <- subset(migr, bird_id %in% settlers$bird_ID) #use only birds in settler file
migr.settlers <- migr.settlers[,-c(5,7,9,11,25)]  #shorten df, take observers out

# too many NAs, therefor: (#however there is some problem with it, doesn't quite work)
#migr.settlers <- subset(migr.settlers, !is.na(migration)) #take out rows without migration info
#migr.settlers <- subset(migr.settlers, migration=="migrant") #keep only migrants
# however: then some kites have no migration information, subset of that kite would be empty
# therefor I'm keeping the residents in the migration dataset. If the above subsets should be used,
# insert following command into lapply function before "sub.migr$on.migration.fall <- NA":
#if (nrow(sub.migr)<1) next else { 

#use only Steph's fall/spring start/end dates, not the alternative dates
migr.settlers <- subset(migr.settlers, select=c(1:7)) #keep only important rows

# transform julian dates in real dates
# ignore warnings of NAs
migr.settlers$fall_start <- as.Date(as.integer(paste0(migr.settlers$fall_start)), 
                                    origin=as.Date(paste0(migr.settlers$mig_year, "-01-01", sep="")))
migr.settlers$fall_end <- as.Date(as.integer(paste0(migr.settlers$fall_end)), 
                                  origin=as.Date(paste0(migr.settlers$mig_year, "-01-01", sep="")))
migr.settlers$spring_start <- as.Date(as.integer(paste0(migr.settlers$spring_start)), 
                                      origin=as.Date(paste0(migr.settlers$mig_year, "-01-01", sep="")))
migr.settlers$spring_end <- as.Date(as.integer(paste0(migr.settlers$spring_end)), 
                                    origin=as.Date(paste0(migr.settlers$mig_year, "-01-01", sep="")))

# insert TransmGSM into migration df (until now only bird_id)
# and information about settlement year
migr.settlers <- merge(migr.settlers, settlers[,c("bird_ID","TransmGSM","Sett_year")], by.x="bird_id", by.y="bird_ID")
summary(migr.settlers$TransmGSM)
str(migr.settlers)



# function to check if setkde$START_DATE is during migration, or a start or end date of migration
# command duration: ca. 1 minute
# function to use:
find.migration.dates <- function (setkde) {
  
  # Initilizing matrix for storing results
  #there is certainly a better & shorter way of storing the results, but this is the first thing that works
  results = as.data.frame(matrix(NA, nrow=length(unique(setkde$START_DATE)), ncol=10))
  colnames(results)<-c("ID", "START_DATE", "NR_POINTS", "AREA_KM2","H","CENTER.LON_X","CENTER.LAT_Y", "on.migration", "departure.day", "arrival.day")
  
  results$on.migration <- factor(NA, levels=c("yes", "no"))
  results$departure.day <- factor(NA, levels=c("yes", "no"))
  results$arrival.day <- factor(NA, levels=c("yes", "no"))
  
  # subset for migration data for each individual
  sub.migr <- subset(migr.settlers0, TransmGSM==setkde$ID[1])
  
  for (i in seq_along(setkde$START_DATE)) {
    
    date <- as.Date(setkde$START_DATE[i])
    
    # for some settlers there is no information on migration dates, therefor:
    sub.migr$on.migration.fall <- NA
    sub.migr$on.migration.spring <- NA
    sub.migr$departure.day <- NA
    sub.migr$arrival.day <- NA
    
    # there are several rows (one for each migration year) in Steph's data per Individual that need to be checked
    for (j in seq_along(sub.migr$mig_year)) {
      
      # FALL
      # first deal with NAs
      if (!is.na(sub.migr$fall_start[j]) & !is.na(sub.migr$fall_end[j])) {
        if (date >= sub.migr$fall_start[j] & date <= sub.migr$fall_end[j]) {
          sub.migr$on.migration.fall[j] <- 1
        }}
      
      # SPRING
      # first deal with NAs
      if (!is.na(sub.migr$spring_start[j]) & !is.na(sub.migr$spring_end[j])) {
        if (date >= sub.migr$spring_start[j] & date <= sub.migr$spring_end[j]) {
          sub.migr$on.migration.spring[j] <- 1
        }}
      
      # but that is not enough: sometimes only start but not end date of migration known. Also, 
      # some dates might not appear on the home range graph because of insufficient data points for that day.
      # Therefor determine if first day of moving window is a start or end date of migration
      
      # FALL start
      if (!is.na(sub.migr$fall_start[j]) & date == sub.migr$fall_start[j]) {
        sub.migr$departure.day[j] <- 1
      }
      
      # FALL end
      if (!is.na(date == sub.migr$fall_end[j]) & date == sub.migr$fall_end[j]) {
        sub.migr$arrival.day[j] <- 1
      }
      
      # SPRING start
      if (!is.na(sub.migr$spring_start[j]) & date == sub.migr$spring_start[j]) {
        sub.migr$departure.day[j] <- 1
      }
      
      # SPRING end
      if (!is.na(date == sub.migr$spring_end[j]) & date == sub.migr$spring_end[j]) {
        sub.migr$arrival.day[j] <- 1
      }
    }
    
    # check if the KDE moving window start date is in one of the different migr.year rows
    # is the day during migration?
    results$ID[i] <- paste(setkde$ID[1])
    results$START_DATE[i] <- paste(date)
    results$NR_POINTS[i] <- paste(setkde$NR_POINTS[i])
    results$AREA_KM2[i] <- paste(setkde$AREA_KM2[i])
    results$H[i] <- paste(setkde$H[i])
    results$CENTER.LON_X[i] <- paste(setkde$CENTER.LON_X[i])
    results$CENTER.LAT_Y[i] <- paste(setkde$CENTER.LAT_Y[i])
    
    if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])) {
      results$settlement.year[i] <- "sett_year"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])+1) {
      results$settlement.year[i] <- "1y_prior_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])+2) {
      results$settlement.year[i] <- "2y_prior_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])+3) {
      results$settlement.year[i] <- "3y_prior_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])+4) {
      results$settlement.year[i] <- "4y_prior_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])+5) {
      results$settlement.year[i] <- "5y_prior_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])-1) {
      results$settlement.year[i] <- "1y_after_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])-2) {
      results$settlement.year[i] <- "2y_after_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])-3) {
      results$settlement.year[i] <- "3y_after_sett"
    } else if (sub.migr$Sett_year[1]==as.integer(setkde$YEAR[i])-4) {
      results$settlement.year[i] <- "4y_after_sett"
    }
    
    if (sum(sub.migr$on.migration.fall, na.rm=T)==1 | sum(sub.migr$on.migration.spring, na.rm=T)==1) {
      results$on.migration[i] <- "yes"
    } else {results$on.migration[i] <- "no"}
    
    # is the day a start day of migration?
    if(sum(sub.migr$departure.day, na.rm=T)==1) {
      results$departure.day[i] <- "yes"
    } else {results$departure.day[i] <- "no"}
    
    # is the day an end day of migration?
    if(sum(sub.migr$arrival.day, na.rm=T)==1) {
      results$arrival.day[i] <- "yes"
    } else {results$arrival.day[i] <- "no"}
  }
  results
}



# bird batch 1 ------------------------------------------------------------------------------------------
setkde.all.1 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_birds1.csv"), header=T, dec=".", sep=";")
setkde1 <- setkde.all.1

summary(migr)
str(setkde1)
summary(setkde1)

setkde1$START_DATE <- as.Date(setkde1$START_DATE, format="%Y-%m-%d")
setkde1$DATE <- as.Date(paste0("2020-", substr(setkde1$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
setkde1$YEAR <- substr(setkde1$START_DATE, 1, 4)

migr.settlers0 <- subset(migr.settlers, TransmGSM %in% setkde1$ID) %>% droplevels


system.time({ # measure time it takes to perform function with 7 cores instead of 1 core
  setkde.migrationDates10 <- mclapply(split(setkde1, setkde1$ID), find.migration.dates, mc.cores = 7)
  setkde.migrationDates11 <- bind_rows(setkde.migrationDates10)
})
str(setkde.migrationDates11)
#setkde.migrationDates11$ID <- as.factor(setkde.migrationDates11$ID)
#setkde.migrationDates11$START_DATE <- as.Date(setkde.migrationDates11$START_DATE)
#setkde.migrationDates11$settlement.year <- factor(setkde.migrationDates11$settlement.year)

# if using setkde:
#write.table(setkde.migrationDates11, paste0(pathKML, "/KDEsettlers_per_5days_2ppd_m.csv"), row.names=F, sep=";")

# if using setkde.all:
write.table(setkde.migrationDates11, paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_birds1.csv"), row.names=F, sep=";")





# bird batch 2 ------------------------------------------------------------------------------------------
setkde.all.2 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_birds2.csv"), header=T, dec=".", sep=";")
setkde2 <- setkde.all.2

summary(migr)
str(setkde2)
summary(setkde2)

setkde2$START_DATE <- as.Date(setkde2$START_DATE, format="%Y-%m-%d")
setkde2$DATE <- as.Date(paste0("2020-", substr(setkde2$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
setkde2$YEAR <- substr(setkde2$START_DATE, 1, 4)

migr.settlers0 <- subset(migr.settlers, TransmGSM %in% setkde2$ID) %>% droplevels

system.time({ # measure time it takes to perform function with 7 cores instead of 1 core
  setkde.migrationDates20 <- mclapply(split(setkde2, setkde2$ID), find.migration.dates, mc.cores = 7)
  setkde.migrationDates21 <- bind_rows(setkde.migrationDates20)
})
str(setkde.migrationDates21)
#setkde.migrationDates21$ID <- as.factor(setkde.migrationDates21$ID)
#setkde.migrationDates21$START_DATE <- as.Date(setkde.migrationDates21$START_DATE)
#setkde.migrationDates21$settlement.year <- factor(setkde.migrationDates21$settlement.year)

# if using setkde:
#write.table(setkde.migrationDates21, paste0(pathKML, "/KDEsettlers_per_5days_2ppd_m.csv"), row.names=F, sep=";")

# if using setkde.all:
write.table(setkde.migrationDates21, paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_birds2.csv"), row.names=F, sep=";")




# merge both ------------------------------------------------------------------------------------------
m1 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_birds1.csv"), header=T, dec=".", sep=";")
m2 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_birds2.csv"), header=T, dec=".", sep=";")
m_all <- bind_rows(m1, m2) 
m_all <- m_all %>% arrange(as.character(ID), START_DATE)
write.table(m_all, paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_allbirds.csv"), row.names=F, sep=";")



# reduce to relevant years ------------------------------------------------
setkde.all <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_3days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")
setkde.all$YEAR <- substr(setkde.all$START_DATE, 1, 4)
rel_years <- read.table(paste0(pathKML, "/list_of_relevant_years_forKDE_perBird.csv"), header=T, dec=".", sep=";")

set_rel_years <- do.call(rbind, lapply(split(setkde.all, setkde.all$ID), function(setkde.all) {
  rel_y <- subset(rel_years, NAME==setkde.all$ID[1])
  new.setkde <- setkde.all %>% filter(YEAR %in% rel_y$YEAR)
  new.setkde
}))

write.table(set_rel_years, paste0(pathKML, "/KDE_AllSettlers_per_3days_2ppd_m_allbirds_relYears.csv"), row.names=F, sep=";")






####################### 7 #######################
########### insert nest coordinates #############
#################################################-------------------------------------------------
settlers <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_all.csv", header=T, dec=".", sep=";")
gpsss1 <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), header=T, dec=".", sep=";")
gpsss2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), header=T, dec=".", sep=";")
gpsss <- gpsss1
gpsss <- gpsss2

gpsss$YEAR <- substr(gpsss$DATE, 1, 4)


# list all years with KDE data per bird
df.s <- gpsss %>% dplyr::select(NAME, YEAR) %>% unique 

df.s.n <- merge(settlers, df.s, by.x="TransmGSM", by.y="NAME")  #all years

# for each kite and each year with KDE data, find the respective nest coordinates
for (i in seq_along(df.s.n$TransmGSM)) {
  if (df.s.n$YEAR[i] == 2017) {
    df.s.n$nest.x[i] <- df.s.n$nest_x_17[i]
    df.s.n$nest.y[i] <- df.s.n$nest_y_17[i]
    df.s.n$nest.ID[i] <- df.s.n$nest_ID_17[i]
  }
  if (df.s.n$YEAR[i] == 2018) {
    df.s.n$nest.x[i] <- df.s.n$nest_x_18[i]
    df.s.n$nest.y[i] <- df.s.n$nest_y_18[i]
    df.s.n$nest.ID[i] <- df.s.n$nest_ID_18[i]
  }
  if (df.s.n$YEAR[i] == 2019) {
    df.s.n$nest.x[i] <- df.s.n$nest_x_19[i]
    df.s.n$nest.y[i] <- df.s.n$nest_y_19[i]
    df.s.n$nest.ID[i] <- df.s.n$nest_ID_19[i]
  }
  if (df.s.n$YEAR[i] == 2020) {
    df.s.n$nest.x[i] <- df.s.n$nest_x_20[i]
    df.s.n$nest.y[i] <- df.s.n$nest_y_20[i]
    df.s.n$nest.ID[i] <- df.s.n$nest_ID_20[i]
  }
}


# clean up data
settlers.nest <- df.s.n %>% dplyr::select("bird_ID", "TransmGSM", "YEAR", "nest.x", "nest.y", "Sett_year", "nest.ID")
settlers.nest <- settlers.nest[complete.cases(settlers.nest), ]
  
# category for ggplot legend
settlers.nest$category <- as.factor("nest")

# designate which is the nest in settlement year
settlers.nest$settlement.year <- NA
settlers.nest$settlement.year <- factor(settlers.nest$settlement.year, levels=c("settlement year", 
                                                                                "1y after settling",
                                                                                "2y after settling",
                                                                                "3y after settling"))
for (i in seq_along(settlers.nest$bird_ID)) {
  if (settlers.nest$YEAR[i] == settlers.nest$Sett_year[i]) {
    settlers.nest$settlement.year[i] <- "settlement year"
  } else if (settlers.nest$YEAR[i] == settlers.nest$Sett_year[i]+1) {
    settlers.nest$settlement.year[i] <- "1y after settling" 
  } else if (settlers.nest$YEAR[i] == settlers.nest$Sett_year[i]+2) {
    settlers.nest$settlement.year[i] <- "2y after settling"
  } else if (settlers.nest$YEAR[i] == settlers.nest$Sett_year[i]+3) {
    settlers.nest$settlement.year[i] <- "3y after settling"
  }
}


# on the shiny app there is a problem if a nest is used several years in a row (it can be displayed only once)
# therefor: make column that includes information from all years
settlers.nest$unique <- duplicated(settlers.nest$nest.x+settlers.nest$nest.y) | duplicated(settlers.nest$nest.x+settlers.nest$nest.y, fromLast = TRUE)
sett_nest <- do.call(rbind, lapply(split(settlers.nest, settlers.nest$bird_ID), function(settlers.nest) {
  if (nrow(settlers.nest)>1) {
    for (i in seq_along(settlers.nest$YEAR)) {
      if (settlers.nest$unique[i]==T) {
        unique.s <- subset(settlers.nest, settlers.nest$unique==T)
        settlers.nest$YEARsPL[i] <- paste(as.character(unique.s$YEAR), collapse=", ")
        settlers.nest$settlement.yearsPL[i] <- paste(as.character(unique.s$settlement.year), collapse=", ")
        } else {
          settlers.nest$YEARsPL[i] <- paste(settlers.nest$YEAR[i])
          settlers.nest$settlement.yearsPL[i] <- paste(settlers.nest$settlement.year[i])
        }
    }
  } 
  else if (nrow(settlers.nest)==1) {
    settlers.nest$YEARsPL <- paste(settlers.nest$YEAR)
    settlers.nest$settlement.yearsPL <- paste(settlers.nest$settlement.year)
  }
  subset(settlers.nest, select=-c(unique))
}))


# final nesting data:
sett_nest <- sett_nest[with(sett_nest, order(TransmGSM, YEAR)),]
summary(sett_nest)
sett_nest

#write.table(settlers.nest, paste0(pathKML, "/list_of_nests_in_SettYear.csv"), row.names=F)
write.table(sett_nest, paste0(pathKML, "/list_of_nests_in2after_SettYear_birds1.csv"), row.names=F, sep=";")
#or
write.table(sett_nest, paste0(pathKML, "/list_of_nests_in2after_SettYear_birds2.csv"), row.names=F, sep=";")

# !! manually added: information to when was active (likely egg laying date; last possible day active; success/fledglings)
# ---> "list_of_nests_in2after_SettYear_withSuccessInfo.csv"


# merge both ------------------------------------------------------------------------------------------
nest1 <- read.table(paste0(pathKML, "/list_of_nests_in2after_SettYear_birds1.csv"), header=T, dec=".", sep=";")
nest2 <- read.table(paste0(pathKML, "/list_of_nests_in2after_SettYear_birds2.csv"), header=T, dec=".", sep=";")
nest_all <- rbind(nest1, nest2) 
nest_all <- nest_all %>% arrange(as.character(TransmGSM))
write.table(nest_all, paste0(pathKML, "/list_of_nests_in2after_SettYear_allbirds.csv"), row.names=F, sep=";")






# find for each settler coordinates of birth nest -------------------------
pathNEW <- "/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/"
settlers <- read.table(paste0(pathNEW, "Settlers_list_Lara_all.csv", sep=""), header=T, dec=".", sep=";")
master <- read.table(paste0(pathNEW, "Kernel Density Estimation/for birth nests of settlers/Life_histories_2015_2020-Tabelle 1.csv", sep=""), header=T, sep=";")
nests <- read.table(paste0(pathNEW, "Kernel Density Estimation/for birth nests of settlers/nest_list_2020_updated.csv", sep=""), header=T, dec=",", sep=";")
# tables master and nest have to be cleaned first (remove unknown or empty cells, also change weird letters (â), delete dots in nest names)

master <- master %>% dplyr::select(bird_id, transm_gsm, hatch_year, nest_of_origin_ID, nest_of_origin_name)

mergedsub <- merge(settlers[,c('bird_ID', 'TransmGSM')], master, by='TransmGSM', by.y='transm_gsm'); nrow(mergedsub)
mergedsub$nonunique <- mergedsub$bird_ID==mergedsub$bird_id
mergedsub <- subset(mergedsub, nonunique==T); nrow(mergedsub)

nests <- nests %>% dplyr::select(id, nest_name, x,y) %>% filter(!is.na(x))

subnests <- nests %>% filter(id %in% as.numeric(mergedsub$nest_of_origin_ID)); nrow(subnests)

mergednests <- merge(mergedsub, nests, by.x='nest_of_origin_ID', by.y='id'); nrow(mergednests)
mergednests <- mergednests %>% dplyr::select(-c('nest_name', 'nonunique','bird_id'))
mergednests <- mergednests %>% dplyr::select(bird_ID, TransmGSM, hatch_year, nest_of_origin_ID, nest_of_origin_name, x, y)
write.table(mergednests, paste0(pathNEW, "Kernel Density Estimation/for birth nests of settlers/list_of_Settlers_BirthNests.csv", sep=""), row.names = F, sep=";", dec=".")

# 

mergednests_sf <- mergednests %>% 
  st_as_sf(coords=c("x","y")) %>% 
  st_set_crs(4326) #this uses WGS84 projected (expected by leaflet)

leaflet() %>% 
  addProviderTiles(providers$OpenTopoMap, options = providerTileOptions(opacity = 0.7)) %>%
  addCircleMarkers(
    data=mergednests_sf,
    radius = 6,
    color = "red",
    popup = ~ paste0(TransmGSM)
  )


####################### 8 #######################
############### make a shiny app ################
#################################################-------------------------------------------------

pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
#non.migration.settl.Points <- read.table(paste0(pathKML, "/AllSettlers_nonMigrating_nonWinter_GPSpoints.csv"), header=T, dec=".", sep=";") #needs to be updated
locations <- read.table(paste0(pathKML, "/Settlement_Potatoes_Locations.csv"), header=T, dec=".", sep=";")
settlers <- read.table("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Settlers_list_Lara_all.csv", header=T, dec=".", sep=";")
settlers.nest <- read.table(paste0(pathKML, "/list_of_nests_in2after_SettYear_allbirds.csv"), header=T, dec=".", sep=";")
str(settlers.nest)
setkde5 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m_allbirds_relYears.csv"), header=T, dec=".", sep=";") #only relevant years
setkde5.all <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_5days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")
setkde4 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_4days_2ppd_m_allbirds_relYears.csv"), header=T, dec=".", sep=";") #only relevant years
setkde4.all <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_4days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")
setkde3 <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_3days_2ppd_m_allbirds_relYears.csv"), header=T, dec=".", sep=";") #only relevant years
setkde3.all <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_3days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")


# dataset: setkde ----------------------------------------------------
list.setkde <- list(setkde5=setkde5, setkde4=setkde4, setkde3=setkde3)
res1 <- lapply(list.setkde, function(setkde) {
  setkde$START_DATE <- as.Date(setkde$START_DATE, format="%Y-%m-%d")
  setkde$DATE <- as.Date(paste0("2020-", substr(setkde$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
  setkde$YEAR <- substr(setkde$START_DATE, 1, 4)
  setkde$YEAR <- factor(setkde$YEAR, levels=c("2015","2016","2017","2018","2019","2020","2021"))
  levels(setkde$settlement.year) <- list("3y before settling"="3y_prior_sett",
                                         "2y before settling"="2y_prior_sett",
                                         "1y before settling"="1y_prior_sett",
                                         "settlement year"="sett_year", 
                                         "1y after settling"="1y_after_sett",
                                         "2y after settling"="2y_after_sett",
                                         "3y after settling"="3y_after_sett")#, "<br>Color = ", color)
  setkde$julian <- julian(setkde$DATE, origin=as.Date("2020-01-01")) #as workaround for color legend
  setkde
})
#list2env(res1,.GlobalEnv) #this unnlists the listed dataframes and saves them under their original names !
setkde <- do.call(rbind, res1)
setkde$window.length <- lapply(strsplit(row.names(setkde), "\\."), '[[', 1) %>% substr(., 7,7)
setkde5 <- subset(setkde, window.length==5)
setkde4 <- subset(setkde, window.length==4)
setkde3 <- subset(setkde, window.length==3)

list.setkde_sf <- list(setkde5_sf=setkde5,setkde4_sf=setkde4,setkde3_sf=setkde3)
res2 <- lapply(list.setkde_sf, function(setkde) {
  setkde_sf <- setkde %>% 
    st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
    st_set_crs(4326) #this uses WGS84 projected (expected by leaflet)
  setkde_sf
})
setkde_sf <- do.call(rbind, res2)
setkde_sf$window.length <- lapply(strsplit(row.names(setkde_sf), "\\."), '[[', 1) %>% substr(., 7,7)


# dataset: setkde.all ----------------------------------------------------
list.setkde.all <- list(setkde5.all=setkde5.all, setkde4.all=setkde4.all, setkde3.all=setkde3.all)
res.all1 <- lapply(list.setkde.all, function(setkde.all) {
  setkde.all$START_DATE <- as.Date(setkde.all$START_DATE, format="%Y-%m-%d")
  setkde.all$DATE <- as.Date(paste0("2020-", substr(setkde.all$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
  setkde.all$YEAR <- substr(setkde.all$START_DATE, 1, 4)
  setkde.all$YEAR <- factor(setkde.all$YEAR, levels=c("2015","2016","2017","2018","2019","2020","2021"))
  levels(setkde.all$settlement.year) <- list("5y before settling"="5y_prior_sett",
                                             "4y before settling"="4y_prior_sett",
                                             "3y before settling"="3y_prior_sett",
                                             "2y before settling"="2y_prior_sett",
                                             "1y before settling"="1y_prior_sett",
                                             "settlement year"="sett_year", 
                                             "1y after settling"="1y_after_sett",
                                             "2y after settling"="2y_after_sett",
                                             "3y after settling"="3y_after_sett")
  setkde.all$julian <- julian(setkde.all$DATE, origin=as.Date("2020-01-01")) #as workaround for color legend
  setkde.all
})
#list2env(res.all1,.GlobalEnv) #this unnlists the listed dataframes and saves them under their original names !
setkde.all <- do.call(rbind, res.all1)
setkde.all$window.length <- lapply(strsplit(row.names(setkde.all), "\\."), '[[', 1) %>% substr(., 7,7)
setkde5.all <- subset(setkde.all, window.length==5)
setkde4.all <- subset(setkde.all, window.length==4)
setkde3.all <- subset(setkde.all, window.length==3)

list.setkde.all_sf <- list(setkde5.all_sf=setkde5.all, setkde4.all_sf=setkde4.all, setkde3.all_sf=setkde3.all)
res.all2 <- lapply(list.setkde.all_sf, function(setkde.all) {
  setkde.all_sf <- setkde.all %>% 
    st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
    st_set_crs(4326) #this uses WGS84 projected (expected by leaflet)
  setkde.all_sf
})
setkde.all_sf <- do.call(rbind, res.all2)
setkde.all_sf$window.length <- lapply(strsplit(row.names(setkde.all_sf), "\\."), '[[', 1) %>% substr(., 7,7)


#dataset: settlers.nest ----------------------------------------------------
settlers.nest$settlement.year <- factor(settlers.nest$settlement.year, levels=c("settlement year","1y after settling","2y after settling","3y after settling"))
settlers.nest_sf <- settlers.nest %>% 
  st_as_sf(coords=c("nest.x","nest.y")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)


#dataset: locations of Settlement + Potatoes ---------------------------------------
settlement_loc <- locations %>% filter(!is.na(LON)) %>%
  st_as_sf(coords=c("LON","LAT")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)
# for leaflet plot 2; the visualisation of all non-Spain/France-wintering, non-migration GPS points


# also activate ----------------------------------------------------
radius = 1500     #1.5 km radius around nest
make.visible = 2  #value to add to size of the points acc. to KDE size, but 1km2 would be too small to see well, so all values get +2


# function to display logarithmic scales with necessary decimal places only and with comma as thousands separator
plain <- function(x,...) format(x, ..., scientific = FALSE, drop0trailing = TRUE, big.mark=",")



########### 1 - user interface ###########

ui <- fluidPage(
  navbarPage(
    #tabsetPanel(
    "5 day-window Kernel home ranges",
    # Two tabs, one for a specific individual, the other as overview of home range development for all kites
    tabPanel("Individuals",
             # App title
             #titlePanel(""),
             
             fluidRow(
               # Build selection tool for changing the individual to be displayed
               column(5,
                      wellPanel(
                        selectInput(inputId = "ID", label = "Red Kite", choices = unique(settlers$TransmGSM),
                                    selected = "KISW01", multiple = F))
               ),
               # Change amount of data displayed
               column(3,
                      radioButtons(inputId = "AmountOfData",
                                   label = "Data",
                                   choices = list("Relevant years" = 1,
                                                  "All years" = 2),
                                   selected = 2)
               ),
               # Change cutoff/threshold value of home range. Only home ranges below that value will be considered for settlements and potatoes
               column(4,
                      sliderInput(inputId = "cutoff",
                                  label = "Max. home range size for map display",
                                  min = 3,
                                  max = 50,
                                  step = 1,
                                  value = 9)
               )
             ),
             
             # Displaying outputs
             # Output for selected individuals: Date vs. Home Range
             fluidRow(
               column(12,
                      h3("Home range sizes throughout the year")
               )
             ),
             # Sidebar layout with input and output definitions
             fluidRow(
               column(3,
                      # Change the moving window length
                      wellPanel(
                        h5("Outline migration dates"),
                        checkboxInput(inputId = "checkbox", 
                                      label = HTML("grey contour = on migration,<br />black outline = departure/arrival day with sufficient data"), 
                                      value=TRUE)
                      ),
                      checkboxInput(inputId = "checkbox.highlight.pot.pot",
                                    label = "Highlight points below cutoff (Mar-Jun)",
                                    value = T),
                      # show migration dates
                        radioButtons(inputId = "MovingWindowLength",
                                     label = "Moving Window Length",
                                     choices = list("5 days" = 3,
                                                    "4 days" = 2,
                                                    "3 days" = 1),
                                     selected = 1),
                      verbatimTextOutput("plot.Ind.potpot")
               ),
               column(9,
                      plotOutput(outputId = "plot.Individual",
                                 width = "auto", height = "350", 
                                 click = "plot_click_Ind",
                                 brush = "plot_brush_Ind"),
                      verbatimTextOutput("plot.Ind.click_brush")
                      )
             ),
             # Show data of points when clicking on them
             fluidRow(
               column(3, offset=0,
                      htmlOutput("warning")
               )
             ),
             
             # Output for selected individuals: Plotting home range centers on a map
             # Plots with overview graph left and zoom-graph right
             fluidRow(
               column(width = 12, #class = "well",
                      h3("Map of home ranges below threshold"),
                      leafletOutput("zoomplot", height = 550)
               )
             )
    ),
    tabPanel("All Red Kites",
             fluidRow(
               column(6,
                      wellPanel(
                        selectInput(inputId = "colouration.All", label=h5("Colour coding"), choices = c("Longitude"="Longitude",
                                                                                                        "Latitude"="Latitude",
                                                                                                        "Settlement Year"="Settlement Year"),
                                    selected = "Settlement Year", multiple = F)
                      )
               ),
               column(6,
                      wellPanel(
                        h5("Data Confidence"),
                        checkboxInput(inputId = "datasecurity",
                                      label=HTML("Highlight days with<br /><30 points (medium size, semi-transparent)<br />or <20 points (big, opaque)<br />for entire moving window length"),
                                      value = F)
                      )
               )
             ),
             fluidRow(
               column(12,
                      plotOutput(outputId = "plot.All",
                                 width = "auto", height = "3000",
                                 click = "plot_click_All",
                                 brush = "plot_brush_All"),
                      
                      #htmlOutput("info.All")
               )
             )
    )
  )
)


############### 2 - server ###############

server <- function(input, output, session){
  dataInput <- reactive({input$AmountOfData})
  WindowLengthInput <- reactive({input$MovingWindowLength})
  ### Panel 1 ----------------------------------------------------------------------------- ###
  
  # Depending on the type of data chosen, 
  # create a subset of data filtering for chosen bird.ID level(s)
  sub_setkde <- reactive({
    if(dataInput()==1){
      if(WindowLengthInput()==3) {setkde5[setkde5$ID == input$ID,]}
      else if (WindowLengthInput()==2) {setkde4[setkde4$ID == input$ID,]}
      else if (WindowLengthInput()==1) {setkde3[setkde3$ID == input$ID,]}
    }
    else if (dataInput()==2){
      if(WindowLengthInput()==3) {setkde5.all[setkde5.all$ID == input$ID,]}
      else if (WindowLengthInput()==2) {setkde4.all[setkde4.all$ID == input$ID,]}
      else if (WindowLengthInput()==1) {setkde3.all[setkde3.all$ID == input$ID,]}
    }
  })
  
  # different KDE cutoff for red kite breeding in Czech Republic (still activated for All kites plot)
  offset <- reactive({
    if (input$cutoff >15) {6} else {1.2}
  }) #this regulates how far away the text annotation is away from the geom_line
  
  # insert warning if necessary to observe 2021, or if settlement debatable
  settlerinfo <- reactive({
    settlers[settlers$TransmGSM == input$ID,]
  })
  
  ### --- Home Range during the year --- ###
  # Plot of one tagged inidivual; Date vs. Home Range
  output$plot.Individual <- renderPlot({
    session$resetBrush("plot_brush_Ind") #to avoid that the blue box of brushed points stays when changing plots
    
    p2 <- sub_setkde() %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      ylab(expression(Kernel~home~range~(km^2))) +
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      xlab(NULL) +
      ggtitle(paste0(input$ID)) + 
      annotation_logticks(side="l", size=0.4, colour="black") +
      theme(plot.title = element_text(hjust = 0.5),
            panel.border=element_blank(), axis.line=element_blank(),
            legend.position = "bottom", legend.key.width=unit(1.5,"cm"), legend.box = "vertical",
            axis.text.x  = element_text(hjust=-0.5)) +
      scale_shape_manual("", values = c("2021"=18, "2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2021"="longdash", "2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    #for showing migration dates as grey/black outlines
    if(input$checkbox==TRUE){
      p2 <- p2 +
        # function to use in ggplot for subsetting specific data
        #pick <- function(condition){ function(d) d %>% filter(!!enquo(condition)) }; geom_point(data=pick(on.migration=="yes"), col="grey", cex=0.95) +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    if(input$checkbox.highlight.pot.pot==TRUE){
      p2 <- p2 +
        geom_point(data=subset(sub_setkde(), DATE>="2020-03-01" & DATE <="2020-07-31" & 
                                 AREA_KM2<=input$cutoff & 
                                 settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling"), 
                   aes(shape=YEAR), cex=3, col="yellow", show.legend = F)
    }
    
    p2.5 <- p2 +
      geom_point(aes(col=settlement.year,
                     shape=YEAR), cex=0.9) +
      scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                         "4y before settling"="#9900CC",
                                         "3y before settling"="#440154FF", 
                                         "2y before settling"="#39568CFF", 
                                         "1y before settling"="#1F968BFF", 
                                         "settlement year"="#73D055FF", 
                                         "1y after settling"="#FDE725FF", 
                                         "2y after settling"="orange",
                                         "3y after settling"="chocolate"),
                          guide = guide_legend(override.aes = list(size=2)))    
    p2.5
  })
  
  output$warning <- renderText({
    if (settlerinfo()$status=="observe") {
      return(paste("<span style=\"color:red\">Observe carefully in 2021.</span>"))
    } else if (settlerinfo()$status=="debatable_settlement") {
      return(paste("<span style=\"color:red\">Settlement debatable,<br>use it for now.</span>"))
    }
  })
  
  # Find date and home range when clicking on/hovering over/brushing at points in plots
  output$plot.Ind.click_brush <- renderPrint({
    click <- nearPoints(sub_setkde(), input$plot_click_Ind, threshold=2, addDist = F) #showing points near click
    brush <- brushedPoints(sub_setkde(), input$plot_brush_Ind) #showing points near brush
    if(nrow(click)>0) {
      click <- click[order(click[,2] ), ] #order by START_DATE
      row.names(click) <- c(1:nrow(click))
      click[,c(2:4,6:7,11)] #keep only interesting rows, and only rows without factors
    } 
    else if(nrow(brush)>0) {
      brush <- brush[order(brush[,2] ), ] #order by START_DATE
      brush1 <- brush[, c(2:4,6:7,11)] #keep only interesting rows (for printing all rows in brush, not just min-max)
      row.names(brush1) <- c(1:nrow(brush))
      brush2 <- brush[, c(2:4,6:7)] #keep only interesting rows, and only rows without factors because of Min Max function
      Min <- summarize_all(brush2, min); row.names(Min) <- "min"
      Max <- summarize_all(brush2, max); row.names(Max) <- "max"
      minmax <- rbind(Min, Max)
      if(nrow(brush)>2) print(minmax)
      if(nrow(brush)<21) print(brush1)
    } 
    else {
      cat("Click on, or brush over the plot.\nBrushed points will be highlighted in the map below.\n\nPoints in March-June below cutoff: ", nrow(subset(sub_setkde(), DATE>="2020-03-01" & DATE <="2020-06-30" & AREA_KM2<=input$cutoff & settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling")), sep="")
      }
  })
  
  output$plot.Ind.potpot <- renderPrint({
    potpot <- subset(sub_setkde(), DATE>="2020-03-01" & DATE <="2020-07-31" & 
                       AREA_KM2<=input$cutoff & 
                       on.migration=="no" &
                       settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling")
    potpot <- potpot %>%dplyr::select(START_DATE)
    if(nrow(potpot) > 0) {
      row.names(potpot) <- c(1:nrow(potpot))
      potpot
      }
    })
  
  
  ### --- Home range centers below threshold on a map --- ###
  
  # Create a subset of data filtering for chosen bird.ID level(s) for this type of data
  # if necessary adjust country-specific kde.cutoff (in server function, first command)
  sub_setkde_cutoff <- reactive({
    if(dataInput()==1){
      setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff,]
    } else if (dataInput()==2){
      setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$AREA_KM2 <= input$cutoff,]    }
  }) #this is class "sf", it includes spatial WGS84 coordinates 
  
  # find nest coordinates for each kite
  sub_setkde_nest.set <- reactive({
    if(dataInput()==1){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID & settlers.nest_sf$settlement.year == "settlement year",]
    } else if (dataInput()==2){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID & settlers.nest_sf$settlement.year == "settlement year",]
    }
  }) # Nests of only settlement years
  sub_setkde_nest.all <- reactive({
    if(dataInput()==1){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID,]
    } else if (dataInput()==2){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID,]
    }
  }) # Nests for all years
  
  # for migration dates only (to use in leaflet plot for grey borders to indicate migration)
  sub_setkde_cutoff.m <- reactive({
    if(dataInput()==1){
      setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff & setkde_sf$on.migration=="yes",]
    } else if (dataInput()==2){
      setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$AREA_KM2 <= input$cutoff & setkde.all_sf$on.migration=="yes",]
    }
  })
  
  # for settlement & potato centers
  sub.sepo_loc <- reactive({
    settlement_loc[settlement_loc$TransmGSM == input$ID,]
  })
  
  # determining subset based on window length 
  windowlength <- reactive ({
    if(input$MovingWindowLength == 1) {3}
    else if(input$MovingWindowLength == 2) {4}
    else if(input$MovingWindowLength == 3) {5}
  })
  
  # highlight brushed points of ggplot above in the leaflet plot below
  activePoint <- reactiveVal()   # Create a reactive value to store the point we select
  observeEvent(input$plot_brush_Ind, {
    if(length(input$plot_brush_Ind)>0){
      near_Points <- brushedPoints(sub_setkde(), input$plot_brush_Ind)
      activePoint(as.Date(near_Points[,2])) # Extract just the start date of point and assign it to activePoint()
    }
  })   # Update the value of activePoint() when we detect an input$plotClick event
  highlightData <- reactive({
    setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$window.length == windowlength() & setkde.all_sf$START_DATE %in% activePoint(),]
  })  # Use that data in the leaflet plot
  
  # plot of KDE centers with area below cutoff (km2) on zoomable map
  output$zoomplot <- renderLeaflet({
    
    ### make colour palette for Date and Year -----------------------------------------------
    pal.date <- colorNumeric(palette = viridis(200), domain = c(0,366), reverse=T) #color legend for date (workaround with julian dates to have numeric values)
    pal.nest <- colorFactor(palette = c('#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
    pal.sepo <- colorFactor(palette = c("orange", "blue"), domain = NULL)
    # !!! order has to match level order of settlement.year and settlers.nest !!! #
    
    if(dataInput()==1){ # display only relevant years
      ### make colour palette for Date and Year -----------------------------------------------
      pal.year <- colorFactor(palette = c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
      
      ### legend for settlement.year, on.migration and nest location ### ----------------------
      colors <- c(rep("white",7),"red","white","blue","orange") #fill inside borders of circles
      labels <- c("-3 years", "-2 years", "-1 year", "settlement", "+1 year", "+2 years", "+3 years",
                  "nest", "on migration", "settlement", "potato")
      sizes <- c(rep(12,7),2,12,rep(2,2))
      shapes <- "circle"
      margin.top <- c(rep(5,7),8,5,rep(8,2)) #margin from circles to each other, distance from top
      margin.left <- c(rep(0,7),2,0,rep(2,2)) #margin from circles to left side (to center the "nest" circle)
      borders <- c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate',
                   'red', 'gray', 'blue','orange')
    } 
    else { # display all years
      #  ### make colour palette for Date and Year -----------------------------------------------
      pal.year <- colorFactor(palette = c('#CC00CC', '#9900CC', '#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
      
      ### legend for settlement.year, on.migration and nest location ### ----------------------
      colors <- c(rep("white",9),"red","white","blue","orange") #fill inside borders of circles
      labels <- c("-5 years", "-4 years", "-3 years", "-2 years", "-1 year", "settlement", "+1 year", "+2 years", "+3 years",
                  "nest", "on migration", "settlement", "potato")
      sizes <- c(rep(12,9),2,12,rep(2,2))
      shapes <- "circle"
      margin.top <- c(rep(5,9),8,5,rep(8,2)) #margin from circles to each other, distance from top
      margin.left <- c(rep(0,9),2,0,rep(2,2)) #margin from circles to left side (to center the "nest" circle)
      borders <- c('#CC00CC', '#9900CC', '#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate',
                   'red', 'gray', 'blue',' orange')
    }
    
    ### legend for settlement.year, on.migration and nest location ### ----------------------
    addLegendCustom <- function(map, labels, sizes, shapes, borders, opacity = 1, 
                                position = c("topright", "bottomright", "bottomleft", "topleft")){
      position <- match.arg(position)
      make_shapes <- function(sizes, borders, shapes) {
        shapes <- gsub("circle", "100%", shapes)
        paste0(colors, "; width:", sizes, "px; margin-top:",margin.top,"px;margin-left:",margin.left,
               "px;height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
      }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block;font-size: 10px;height: ", 
               sizes, "px;margin-top: 0px;line-height: ", 
               sizes, "px;'>", labels, "</div>")
      }
      legend_colors <- make_shapes(sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
    }
    
    
    ### legend for KDE size  ### ------------------------------------------------------------
    colors.Size <- "black" #fill inside borders of circles
    labels.Size <- c("1","6") #labelling for km2 home size
    sizes.Size <- c(0.5+make.visible,11+make.visible) #value to make small KDE areas (points) better visible
    margin.top.Size <- c(7,3) #margin from circles to each other, distance from top
    margin.left.Size <- c(3.5,0) #margin from circles to left side (to center the "nest" circle)
    borders.Size <- "black"
    addLegendCustom.Size <- function(map, labels.Size, sizes.Size, shapes, 
                                     borders.Size, opacity = 1, 
                                     position = c("topright", "bottomright", "bottomleft", "topleft")){
      position <- match.arg(position)
      make_shapes <- function(sizes.Size, borders.Size, shapes) {
        shapes <- gsub("circle", "50%", shapes)
        paste0(colors.Size, "; width:", sizes.Size, "px; margin-top:",margin.top.Size,"px;margin-left:",
               margin.left.Size,"px;height:", sizes.Size, "px; border:2px solid ", 
               borders.Size, "; border-radius:", shapes)
      }
      make_labels <- function(sizes.Size, labels.Size) {
        paste0("<div style='display: inline-block;font-size: 10px;height: ", 
               sizes.Size, "px;margin-top: 0px;line-height: ", 
               sizes.Size, "px;'>", labels.Size, " km<sup>2</sup>  KDE</div>")
      }
      legend_colors <- make_shapes(sizes.Size, borders.Size, shapes)
      legend_labels <- make_labels(sizes.Size, labels.Size)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
    }
    
    
    ### legend for Date coloration ### ------------------------------------------------------
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){
          as <- as.Date(cuts, origin="2020-01-01")#, format="%b-%d")
          format(as,"%b")
          #div(style="font-size: 10px;", month)
        } 
      }else{
        labelFormat(...)
      }
    }
    
    
    l1 <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }"
      ) %>%
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(opacity = 0.7) #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      ) %>%  
      addMapPane("nest", zIndex = 430 #puts nest on near-top of all layers
      ) %>%  
      addMapPane("settlements", zIndex = 435 #puts settlements on top of all layers
      ) %>%  
      addMapPane("KDE.points", zIndex = 425 #puts settlements on top of all layers
      ) %>%  
      addMapPane("migration", zIndex = 415 #puts settlements on top of all layers
      ) %>%  
      addMapPane("highlights", zIndex = 405 #puts settlements on top of all layers
      ) %>%  
      ## -- SETTLEMENT CENTERS -- ##
      addCircleMarkers(
        data=sub.sepo_loc(),
        radius = 3,
        color = ~pal.sepo(Type),
        group = "settlement and potatoes",
        stroke = FALSE, fillOpacity = 1,
        options = pathOptions(pane = "settlements"),
        popup = ~ paste0(Type, "<br>", Start," - ", End)
      ) %>% 
      addCircles(
        data=sub.sepo_loc(),
        radius = 1500,
        color = "black",
        group = "settlement and potatoes",
        weight=1,
        stroke = TRUE, fill=F,
        options = pathOptions(pane = "settlements")
      ) %>% 
      ## -- KDE CENTERS -- ##
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==3), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "3 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        options = pathOptions(pane = "KDE.points"),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==4), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "4 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        options = pathOptions(pane = "KDE.points"),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==5), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "5 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        options = pathOptions(pane = "KDE.points"),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      ## -- MIGRATION points underneath KDE CENTERS -- ##
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==3),
        group = "3 day-window",
        options = pathOptions(pane = "migration"),
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==4),
        group = "4 day-window",
        options = pathOptions(pane = "migration"),
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==5),
        group = "5 day-window",
        options = pathOptions(pane = "migration"),
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      ## -- NEST points -- ##
      #addCircleMarkers( #add nest for when only nest of settlement year should be displayed (as controlled by addLayersControl)
      #  data=sub_setkde_nest.set(), #lng=~nest.x, lat=~nest.y,
      #  radius = 3,
      #  group = "nest of settlement year",
      #  color = "red",
      #  stroke = FALSE, fillOpacity = 1,
      #  options = pathOptions(pane = "nest"),
      #  popup = ~ paste0("nest: ",settlement.year,"<br>",YEAR)
      #)  %>% 
      addCircleMarkers( #add nests for when nests of all years should be displayed (as controlled by addLayersControl)
        data=sub_setkde_nest.all(),
        radius = 3,
        group = "nests of all years",
        color = "red",
        stroke = FALSE, fillOpacity = 1,
        options = pathOptions(pane = "nest"),
        popup = ~ paste0("nest: ",settlement.yearsPL,"<br>",YEARsPL)
      )  %>% 
      addCircleMarkers( # coloured border around point as indicator for year 
        data=sub_setkde_nest.all(),
        radius = 3,
        group = "nests of all years",
        color = ~pal.nest(settlement.year),
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      ## -- CONTROLS -- ##
      addLayersControl( #control to define which nests are displayed
        baseGroups = c("3 day-window", "4 day-window", "5 day-window"),
        overlayGroups = c("settlement and potatoes"), #control to define if settlements & potoatoes are displayed
        options = layersControlOptions(collapsed = F),
        position = "topleft"
      ) %>% 
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = F)) %>% 
      addLegendCustom(labels, sizes, shapes, borders, position="topleft") %>% # legend for settlement.year, on.migration and nest location
      addLegendCustom.Size(labels.Size, sizes.Size, shapes, borders.Size, position="topleft") %>%  # legend for point size (AREA_KM2)
      addLegend(     # legend for date (viridis scale)
        data = sub_setkde_cutoff(),
        position = "bottomright", 
        pal = pal.date,
        values = ~julian,
        opacity = 1,
        bins=6,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      ) %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )
    
    if (length(input$plot_brush_Ind) > 0) { #points selected: highlight them in this leaflet plot
      l1 <- l1 %>% 
        addCircleMarkers( #highlight the points selected in first graph
          data=highlightData(),
          options = pathOptions(pane = "highlights"),
          radius = ~AREA_KM2+make.visible+6,
          fillColor = "orange",
          fillOpacity = 0.75,         
          stroke = F,
          popup = ~ paste0(START_DATE, "<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                           settlement.year,"<br>GPS points: ", NR_POINTS)
        )
      l1
    } else {l1} # No points selected; regular plot
    
    if (input$checkbox.highlight.pot.pot == T) {
      l1 <- l1 %>% 
        addCircleMarkers(
          data=subset(sub_setkde_cutoff(), DATE>="2020-03-01" & DATE <="2020-07-31" & 
                        AREA_KM2<=input$cutoff & 
                        on.migration =="no" &
                        settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling" &
                        window.length==3), 
          options = pathOptions(pane = "highlights"),
          group = "3 day-window",
          radius = ~AREA_KM2+make.visible+30,
          fillColor = "yellow",
          fillOpacity = 0.75,         
          stroke = F
        ) %>%
        addCircleMarkers(
          data=subset(sub_setkde_cutoff(), DATE>="2020-03-01" & DATE <="2020-07-31" & 
                        AREA_KM2<=input$cutoff & 
                        on.migration =="no" &
                        settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling" &
                        window.length==4), 
          options = pathOptions(pane = "highlights"),
          group = "4 day-window",
          radius = ~AREA_KM2+make.visible+30,
          fillColor = "yellow",
          fillOpacity = 0.75,         
          stroke = F
        ) %>%
        addCircleMarkers(
          data=subset(sub_setkde_cutoff(), DATE>="2020-03-01" & DATE <="2020-07-31" & 
                        AREA_KM2<=input$cutoff & 
                        on.migration =="no" &
                        settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling" &
                        window.length==5), 
          group = "5 day-window",
          radius = ~AREA_KM2+make.visible+30,
          fillColor = "yellow",
          fillOpacity = 0.75,         
          stroke = F
        )
      } else {l1}
  })
  
  
  
  
  ### Panel 2 ----------------------------------------------------------------------------- ###
  # Plot for all Individuals
  data.plot.All <- reactive({
    if(dataInput()==1) {
      if(WindowLengthInput()==1) {setkde5}
      else if (WindowLengthInput()==2) {setkde4}
      else if (WindowLengthInput()==3) {setkde3}
    }
    else if (dataInput()==2) {
      if(WindowLengthInput()==1) {setkde5.all}
      else if (WindowLengthInput()==2) {setkde4.all}
      else if (WindowLengthInput()==3) {setkde3.all}
    }
  })
  
  output$plot.All <- renderPlot({
    
    p5 <- data.plot.All() %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      ylab(expression(Kernel~home~range~(km^2))) +
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      xlab(NULL) +
      annotation_logticks(side="l", size=0.4, colour="black") +
      facet_rep_wrap(~ID, scales="fixed", ncol=4, nrow=20) +
      theme(panel.border=element_blank(), axis.line=element_blank(),
            #axis.text.x  = element_text(hjust=-0.5),
            legend.position = "bottom", legend.key.width=unit(1.5,"cm"), legend.box = "vertical") +
      scale_shape_manual("", values = c("2021"=18, "2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2021"="longdash", "2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    
    if(input$checkbox==TRUE){
      p5 <- p5 +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    if(input$datasecurity==TRUE){
      if(input$colouration.All=="Longitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=4, data=subset(data.plot.All(), NR_POINTS<20)) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LON_X), max(data.plot.All()$CENTER.LON_X))) +
          labs(col = "Longitude")
      }
      else if(input$colouration.All == "Latitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=4, data=subset(data.plot.All(), NR_POINTS<20)) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LAT_Y), max(data.plot.All()$CENTER.LAT_Y))) +
          labs(col = "Latitude")
      }
      else if(input$colouration.All == "Settlement Year"){
        p5.5 <- p5 +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=0.5, alpha=0.2) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=4, data=subset(data.plot.All(), NR_POINTS<20))
        
        if(dataInput()==1){
          p5.5 <- p5.5 +
            scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        } 
        else {
          p5.5 <- p5.5 +
            scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                               "4y before settling"="#9900CC",
                                               "3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        }
        
      }
    } 
    else if (input$datasecurity==FALSE) {
      if(input$colouration.All=="Longitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.9, alpha=0.9) +
          #scale_size(range=c(3,0.5), trans="log") +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LON_X), max(data.plot.All()$CENTER.LON_X))) +
          labs(col = "Longitude")
      }
      else if(input$colouration.All == "Latitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.9) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LAT_Y), max(data.plot.All()$CENTER.LAT_Y))) +
          labs(col = "Latitude")
      }
      else if(input$colouration.All == "Settlement Year"){
        if(dataInput()==1){
          p5.5 <- p5 +
            geom_point(aes(col=settlement.year,
                           shape=YEAR), cex=0.9) +
            scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        } 
        else {
          p5.5 <- p5 +
            geom_point(aes(col=settlement.year,
                           shape=YEAR), cex=0.9) +
            scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                               "4y before settling"="#9900CC",
                                               "3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        }
      }
    }
    
    p5.5
    
  })
}


############### 3 - start shinyApp ##############

shinyApp(ui = ui, server = server)



shiny::runGitHub('RKsettlement2021_ggmap', 'LaGrande27', ref="main") #1
shiny::runGitHub('RKsettlement2021_leaflet', 'LaGrande27', ref="old_2graphsOnly") #2
shiny::runGitHub('RKsettlement2021_leaflet', 'LaGrande27', ref="last_without_potatoes") #3
shiny::runGitHub('RKsettlement2021_leaflet', 'LaGrande27', ref="potatoes1") #4
shiny::runGitHub('RKsettlement2021_leaflet', 'LaGrande27', ref="potatoes2") #5


# with that data, make list of possible potatoes and settlement dates
# --> "Settlement_Potatoes_DatesAndLocations.csv"
# ! manually insert every date you know of, not only the ones that change with a different cutoff !


  




####################### 9 #######################
############# identify locations of #############
################## settlements ##################
#################################################-------------------------------------------------

pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
gpsss1 <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), header=T, dec=".", sep=";") #the GPS points
gpsss2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), header=T, dec=".", sep=";") #the GPS points
gpsss <- rbind(gpsss1, gpsss2)

setkde.all <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_5days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")
setkde_6km2 <- setkde.all %>% 
  filter(AREA_KM2<=6)
setkde_6km2$START_DATE <- as.Date(setkde_6km2$START_DATE)
setkde_9km2 <- setkde.all %>% 
  filter(AREA_KM2<=9)
setkde_9km2$START_DATE <- as.Date(setkde_9km2$START_DATE)

sepo0 <- read.table(paste0(pathKML, "/SettlementDates.csv"), header=T, dec=".", sep=";") #which dates to take
sepo <- cbind(lapply(sepo0[1], as.factor), sepo0[2:4], lapply(sepo0[5:16], as.Date), lapply(sepo0[17:18], as.Date), sepo0[19:20], lapply(sepo0[21:22], as.Date))
sepo_t1 <- sepo %>%  # reorder df, start dates first
  gather(key=Type, value=Start, spring_start_6, last_start_6, longest_start_6, spring_start_9, last_start_9, longest_start_9, recommended_start_date_settlement) %>% 
  dplyr::select(bird_ID, TransmGSM, exclude, Sett_year_Val, Type, Start) %>% 
  arrange(TransmGSM)
sepo_t2 <- sepo %>% #now also including column for end dates
  gather(key=Type2, value=End, spring_end_6, last_end_6, longest_end_6, spring_end_9, last_end_9, longest_end_9, recommended_end_date_settlement) %>% 
  dplyr::select(TransmGSM, Type2, End) %>% 
  dplyr::rename(TransmGSM.2 = TransmGSM) %>% arrange(TransmGSM.2) 
sepo_t <- cbind(sepo_t1, sepo_t2) %>% #merging both
  dplyr::select(bird_ID, TransmGSM, exclude, Sett_year_Val, Type, Start, End)
sepo_t$End.true <- sepo_t$End + 4 #because home range is calculated with GPS points from 5 days
sepo_t$Type2 <- sub("\\_.*", "", sepo_t$Type)
sepo_t$cutoff <- str_sub(sepo_t$Type,-1,-1)
sepo_t$Type2[sepo_t$Type2 %in% "recommended"] <- "settlement"
sepo_t$cutoff[sepo_t$cutoff %in% "t"] <- 6
sepo_t$cutoff[sepo_t$TransmGSM=="KIWS95" & sepo_t$Type2=="settlement"] <- 20 #Czech Republic bird needs higher cutoff
sepo_t$Type <- paste0(sepo_t$Type2, "_", sepo_t$cutoff)
sepo_t$TransmGSM <- factor(sepo_t$TransmGSM, levels=levels(gpsss$NAME)) #for the next function, otherwise warning about unequal factor levels
sepo_t #final


locs <- do.call(rbind, lapply(split(sepo_t, sepo_t$TransmGSM), function(sepo_t) {
  
  results = as.data.frame(matrix(NA, nrow=length(sepo_t$TransmGSM), ncol=7))
  colnames(results)<-c("bird_ID","TransmGSM","Type","LON","LAT","Start","End")
  
  currentKiteGPS <- subset(gpsss, NAME==sepo_t$TransmGSM[1]) # access correct points in GPS dataset
  currentKite6km2 <- subset(setkde_6km2, ID==sepo_t$TransmGSM[1]) # 
  currentKite9km2 <- subset(setkde_9km2, ID==sepo_t$TransmGSM[1]) # 
  
  for (i in seq_along(sepo_t$Type)) {
    
    results$bird_ID[i] <- paste0(sepo_t$bird_ID[i])
    results$TransmGSM[i] <- paste0(sepo_t$TransmGSM[i])
    results$Type[i] <- paste0(sepo_t$Type[i])
    results$Start[i] <- paste0(sepo_t$Start[i])
    results$End[i] <- paste0(sepo_t$End.true[i])
    
    if (!is.na(sepo_t$Start[i])) {
      if (sepo_t$cutoff[i]==6 | sepo_t$cutoff[i]==20) {
        filt.Date <- seq(sepo_t$Start[i], sepo_t$End[i], by="days")
        filt.Date.6km2_1 <- subset(filt.Date, filt.Date %in% currentKite6km2$START_DATE) # are all dates with home range <cutoff km2?
        # to the dates that are, add 4 days (home range = start date + 4 days -> 5 days moving window)
        filt.Date.6km2_5 <- filt.Date.6km2_1 + 4 #there is probably a better, faster way to do this, but I don't know it
        filt.Date.6km2_4 <- filt.Date.6km2_1 + 3
        filt.Date.6km2_3 <- filt.Date.6km2_1 + 2
        filt.Date.6km2_2 <- filt.Date.6km2_1 + 1 
        filt.Date.6km2 <- c(filt.Date.6km2_1, filt.Date.6km2_2, filt.Date.6km2_3, filt.Date.6km2_4, filt.Date.6km2_5) %>% unique
        
        if(sepo_t$TransmGSM[1]!="KIWS14") {
          results$LON[i] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date.6km2) %>% summarize(lon = mean(LON)) %>% round(digits=6)
          results$LAT[i] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date.6km2) %>% summarize(lat = mean(LAT)) %>% round(digits=6)
        } else if (sepo_t$TransmGSM[1]=="KIWS14") { #some points in June for that kite are not at all at the settlement place
          donttake <- as.Date(as.Date("2020-06-02"):as.Date("2020-06-15"), origin="1970-01-01")
          `%nin%` = Negate(`%in%`)
          results$LON[i] <- currentKiteGPS %>% filter(as.Date(DATE) %nin% donttake) %>% filter(as.Date(DATE) %in% filt.Date.6km2) %>% summarize(lon = mean(LON)) %>% round(digits=6)
          results$LAT[i] <- currentKiteGPS %>% filter(as.Date(DATE) %nin% donttake) %>% filter(as.Date(DATE) %in% filt.Date.6km2) %>% summarize(lat = mean(LAT)) %>% round(digits=6)
        }
      } else if (sepo_t$cutoff[i]==9) {
        filt.Date <- seq(sepo_t$Start[i], sepo_t$End.true[i], by="days")
        filt.Date.9km2_1 <- subset(filt.Date, filt.Date %in% currentKite9km2$START_DATE) # are all dates with home range <cutoff km2?
        # to the dates that are, add 4 days (home range = start date + 4 days -> 5 days moving window)
        filt.Date.9km2_5 <- filt.Date.9km2_1 + 4 #there is probably a better, faster way to do this, but I don't know it
        filt.Date.9km2_4 <- filt.Date.9km2_1 + 3
        filt.Date.9km2_3 <- filt.Date.9km2_1 + 2
        filt.Date.9km2_2 <- filt.Date.9km2_1 + 1 
        filt.Date.9km2 <- c(filt.Date.9km2_1, filt.Date.9km2_2, filt.Date.9km2_3, filt.Date.9km2_4, filt.Date.9km2_5) %>% unique
        
        results$LON[i] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date.9km2) %>% summarize(lon = mean(LON)) %>% round(digits=6)
        results$LAT[i] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date.9km2) %>% summarize(lat = mean(LAT)) %>% round(digits=6)
      }
    } else {
      results$LON[i] <- NA
      results$LAT[i] <- NA
    }
  }
  results
}))


str(locs) #somehow results in list-columns, no idea why...
result <- locs %>% unnest(c(LON, LAT))
View(result)
write.table(result, paste0(pathKML, "/Settlement_Potatoes_Locations_prep.csv"), row.names=F, sep=";")


# preparing for QGIS
settl <- subset(result, Type=="settlement_6" | Type=="settlement_20")
settl$Type <- "Settlement"
settl$Start <- as.Date(settl$Start); settl$End <- as.Date(settl$End)
#settl <- subset(settl, select=-c(Start,End))

# merging the centroid for one tagged couple (Brünisried_Wilersguet)
combine <- settl[which(settl$TransmGSM=="BC38EB1D" | settl$TransmGSM=="SWIK88"),] 
if("you wanna have one combined row for both but delete the individual rows:"){
  combine[nrow(combine) + 1,] = list(paste(combine$bird_ID[1],combine$bird_ID[2], sep=""),
                                     paste(combine$TransmGSM[1],combine$TransmGSM[2], sep="_"),
                                     paste(combine$Type[1]),
                                     mean(combine$LON),
                                     mean(combine$LAT),
                                     min(combine$Start),
                                     max(combine$End))
} else if ("both birds have a combination of both rows") { # favored solution
  combine[nrow(combine) + 1,] = list(paste(combine$bird_ID[1]),
                                     paste(combine$TransmGSM[1]),
                                     paste(combine$Type[1]),
                                     mean(combine$LON[c(1,2)]),
                                     mean(combine$LAT[c(1,2)]),
                                     combine$Start[1],
                                     combine$End[1])
  combine[nrow(combine) + 1,] = list(paste(combine$bird_ID[2]),
                                     paste(combine$TransmGSM[2]),
                                     paste(combine$Type[1]),
                                     mean(combine$LON[c(1,2)]),
                                     mean(combine$LAT[c(1,2)]),
                                     combine$Start[2],
                                     combine$End[2])
}
settlwoBaS88 <-settl %>% filter(TransmGSM!="BC38EB1D" & TransmGSM!="SWIK88")
settl2 <- rbind(settlwoBaS88, combine[c(3,4),]) 

#settl$centroid.ID <- paste0(settl$bird_ID, sep="_", substr(settl$Type,1,1))

write.table(settl2, paste0(pathKML, "/Settlement_Locations.csv"), row.names=F, sep=";")

### In QGIS ##
# import with normal CRS: 21781
# reproject to 21781
# calculator: new x & y coordinates (automatically Swiss CRS!)






###################### 10 #######################
############# identify locations of #############
#################### potatoes ###################
#################################################-------------------------------------------------
pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
sepo_used <- read.table(paste0(pathKML, "/Settlement_Potatoes_Locations_as_used.csv"), header=T, dec=".", sep=";")
pots <- read.table(paste0(pathKML, "/PotatoOptions.csv"), header=T, dec=".", sep=";")
pots <- pots[complete.cases(pots), ]
`%nin%` = Negate(`%in%`)
pots <- subset(pots, take=="yes" & TransmGSM %nin% subset(sepo_used$TransmGSM, sepo_used$Type=="Potato"))
moving.window <- 3 #days

## take random number out of list for each settler
random.potato <- do.call(rbind, lapply(split(pots, pots$TransmGSM), function(pots) {
  set.seed(15)
  randomrow <- pots[sample(nrow(pots), 1), ]
  randomrow
}))
ranpot <- random.potato %>% dplyr::select(bird_ID, TransmGSM, start, end) %>% droplevels
ranpot2 <- bind_cols(lapply(ranpot[1:2], as.factor), lapply(ranpot[3:4], as.Date))
ranpot2$end.true <- ranpot2$end + (moving.window-1) #because home range is calculated with GPS points in a 3 days moving window


## extract the associated GPS points
gpsss1 <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), header=T, dec=".", sep=";") #the GPS points
gpsss2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), header=T, dec=".", sep=";") #the GPS points
gpsss <- rbind(gpsss1, gpsss2)
setkde3.all_ <- read.table(paste0(pathKML , "/KDE_AllSettlers_per_3days_2ppd_m_allbirds.csv"), header=T, dec=".", sep=";")
setkde3.9km <- subset(setkde3.all_, AREA_KM2<=9)

potlocs <- do.call(rbind, lapply(split(ranpot2, ranpot2$TransmGSM), function(ranpot2) {
  
  results = as.data.frame(matrix(NA, nrow=length(ranpot2$TransmGSM), ncol=7))
  colnames(results)<-c("bird_ID","TransmGSM","Type","LON","LAT","Start","End")
  
  results$bird_ID[1] <- paste0(ranpot2$bird_ID[1])
  results$TransmGSM[1] <- paste0(ranpot2$TransmGSM[1])
  results$Type[1] <- "Potato"
  results$Start[1] <- paste0(ranpot2$start[1])
  results$End[1] <- paste0(ranpot2$end.true[1])
  
  currentKiteGPS <- subset(gpsss, NAME==paste0(ranpot2$TransmGSM[1])) # access correct points in GPS dataset
  currentKite3ppd9km2 <- subset(setkde3.9km, ID==paste0(ranpot2$TransmGSM[1]))
  filt.Date <- seq(ranpot2$start[1], ranpot2$end[1], by="days")
  filt.Date.1 <- subset(filt.Date, filt.Date %in% as.Date(currentKite3ppd9km2$START_DATE)) # are all dates with home range <cutoff km2?
  filt.Date.2 <- filt.Date.1 + 1
  filt.Date.3 <- filt.Date.1 + 2 
  filt.Date <- c(filt.Date.1, filt.Date.2, filt.Date.3) %>% unique
  
  
  if(ranpot2$TransmGSM[1]!="KIWS14") {
    results$LON[1] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date) %>% summarize(lon = mean(LON)) %>% round(digits=6)
    results$LAT[1] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Date) %>% summarize(lat = mean(LAT)) %>% round(digits=6)
  } else if (ranpot2$TransmGSM[1]=="KIWS14") { #some points in June for that kite are not at all at the settlement place
    start2 <- as.Date("2018-06-04"); end2 <- as.Date("2018-06-12")
    filt.Date2 <- seq(start2, end2, by="days")
    filt.Date2.1 <- subset(filt.Date2, filt.Date2 %in% as.Date(currentKite3ppd9km2$START_DATE)) # are all dates with home range <cutoff km2?
    filt.Date2.2 <- filt.Date2.1 + 1
    filt.Date2.3 <- filt.Date2.1 + 2 
    filt.Dates2 <- c(filt.Date2.1, filt.Date2.2, filt.Date2.3 ) %>% unique
    filt.Dates <- c(filt.Date, filt.Dates2)
    results$LON[1] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Dates) %>% summarize(lon = mean(LON)) %>% round(digits=6)
    results$LAT[1] <- currentKiteGPS %>% filter(as.Date(DATE) %in% filt.Dates) %>% summarize(lat = mean(LAT)) %>% round(digits=6)
  }
  results
}))
potlocs2 <- potlocs %>% unnest(c(LON, LAT))
write.table(potlocs2, paste0(pathKML, "/Potatoes_Locations_latestAdditions.csv"), row.names=F, sep=";")


## merge with settlement locations
# prep settlement csv
setts <- read.table(paste0(pathKML, "/Settlement_Locations.csv"), header=T, dec=".", sep=";")
setts <- setts[complete.cases(setts), ]

# if you want to use only the latest additions of settlements
sepo_used <- read.table(paste0(pathKML, "/Settlement_Potatoes_Locations_as_used 20210413.csv"), header=T, dec=".", sep=";")
`%nin%` = Negate(`%in%`)
setts0 <- subset(setts, TransmGSM %nin% subset(sepo_used$TransmGSM, sepo_used$Type=="Settlement"))

# merge with settlement locations
setpot <- rbind(setts0, potlocs2)
write.table(setpot, paste0(pathKML, "/Settlement_Potatoes_Locations_latestAdditions.csv"), row.names=F, sep=";")


setpot_x <- rbind(setts0, sepo_used)
write.table(setpot_x, paste0(pathKML, "/Settlement_Potatoes_Locations 20210414.csv"), row.names=F, sep=";")




#unused but useful stuff----------------------------------------------------------------
{
  #make a subset
  asd <- setkde
  movedata <- subset(setkde, ID=="KISW01" & AREA_KM2<10)
  
  library(moveVis)
  movedata$temp <- as.POSIXct(movedata$START_DATE) #important!
  movedata <- movedata %>% 
    mutate(colour = case_when(settlement.year=="3y before settling" ~ '#440154FF',
                              settlement.year=="2y before settling" ~ '#39568CFF',
                              settlement.year=="1y before settling" ~ '#1F968BFF',
                              settlement.year=="settlement year" ~ '#73D055FF',
                              settlement.year=="1y after settling" ~ '#FDE725FF',
                              settlement.year=="2y after settling" ~ 'orange',
                              settlement.year=="3y after settling" ~ '#chocolate', #changes colour according to life stages
                              TRUE ~ 'black'))
  df <- df2move(movedata, proj="+init=epsg:3857", #reproject, because base map is in that CRS
                x="CENTER.LON_X", y="CENTER.LAT_Y", time="temp", track_id="ID")
  df <- sp::spTransform(df, CRS("+init=epsg:3857")) 
  m <- align_move(df, unit = "days")
  
  frames <- frames_spatial(m, map_service = "osm", map_type = "topographic", alpha = 0.5,
                           path_legend = F, equidistant = F) %>% #, path_legend = F
    add_labels(x = "Longitude", y = "Latitude") %>%
    add_northarrow(position = "topright") %>%
    add_scalebar(position = "bottomright") %>%
    add_timestamps(m, type = "label") %>% #does it still show also hms?  ##  = substr(m,1,10)
    #add_timestamps(m, type = "label") %>%
    add_progress()
  length(frames) #how many frames
  frames[[100]] #see one frame
  #all <- frames
  
  pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
  
  animate_frames(all, out_file = "moveVis_all20152020.gif")
  animate_frames(frames, end_pause=1, res=200, overwrite=T,
                 out_file = paste0(pathKML, "/moveVisGIFS/moveVis_KISW01_20152020.gif"))
  
} #builds moveVis GIF for individual kite

{
  sub_setkde_cutoff <- sub_setkde_cutoff %>% 
    mutate(year.shape = case_when(YEAR==2020 ~ 16,
                                  YEAR==2019 ~ 17,
                                  YEAR==2018 ~ 15,
                                  YEAR==2017 ~ 18,
                                  YEAR==2016 ~ 25,
                                  YEAR==2016 ~ 16,
                                  TRUE ~ 8))
} #match value in one column to another column

{
# in ui
    htmlOutput("info.Individual") #,
# in server
    output$info.Individual <- renderText({
    
    transform_time <- function(e) {
      substr(as.POSIXct(e*60*60*24, origin = "1970-01-01"), 1, 10)
    }
    
    click_output <- function(df) {
      if(is.null(df)) return("NULL<br/>") else{
        HTML("Date: ", transform_time(df$x),
             ",  Home range: ", round(df$y, 2), " km<sup>2</sup>", "<br/>still method nearPoints() needed!<br/>")}}
    
    click_output_range <- function(df) {
      if(is.null(df)) return("NULL<br/>") else {
        paste0("Dates: ", transform_time(df$xmin), "-", transform_time(df$xmax),
               ",  Home range: ", round(df$ymin, 2), "-", round(df$ymax, 2), " km<sup>2</sup><br/>")}}
    
    paste0(
      "click: ", click_output(input$plot_click_Ind),
      "brush: ", click_output_range(input$plot_brush_Ind)
    )
  })
} #print output of click and brush in "all individuals" facet_wrap plot

{
  pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
  #setkde <- read.table(paste0(pathKML, "/KDEsettlers_per_",days.stationary,"days.csv"), header=T, dec=".", sep=" ")
  setkde <- read.table(paste0(pathKML, "/KDEsettlers_per_6days_2ppd.csv"), header=T, dec=".", sep=" ")
  #setkde <- read.table(paste0(pathKML, "/KDEsettlers_per_6days.csv"), header=T, dec=",", sep=";")
  str(setkde)
  
  setkde$START_DATE <- as.Date(setkde$START_DATE, format="%Y-%m-%d")
  setkde$DATE <- as.Date(paste0("2020-", substr(setkde$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
  setkde$YEAR <- substr(setkde$START_DATE, 1, 4)
  
  
  # function to display logarithmic scales with necessary decimal places only and with comma as thousands separator
  plain <- function(x,...) format(x, ..., scientific = FALSE, drop0trailing = TRUE, big.mark=",")
  
  ### ggplot base function ###
  ggplot.setkde <- setkde %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
    geom_hline(yintercept=6, size=0.1) +
    scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                            as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                            as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                 date_labels = "%b") +
    theme_classic() +
    scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
    labs(x=NULL, y=expression("Kernel home range (km"*italic(2)*")")) +
    annotate(geom="text", label="6 km2", x=as.Date("01-05", format="%m-%d"), y=kde.cutoff.CH-1.2, hjust=1, size=1.5, col="darkgrey") +
    annotation_logticks(side="l", size=0.4, colour="black") +
    facet_rep_wrap(~ID, scales="fixed") +
    theme(panel.border=element_blank(), axis.line=element_blank()) +
    theme(legend.position = "bottom", legend.key.width=unit(2,"cm"), legend.box = "vertical") +
    scale_shape_manual("", values = c("2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
    scale_linetype_manual("", values = c("2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
  
  
  
  ### plot with col = BIRD ID ###
  ind <- ggplot.setkde +
    geom_line(aes(col=ID, linetype=as.factor(YEAR)), size=0.4) + 
    geom_point(aes(col=ID, shape=as.factor(YEAR)), cex=0.85) +
    scale_colour_discrete(guide = FALSE)
  ggsave(paste0(pathKML, "/rplot.pdf"), ind, width=49.9, height=30)
  
  ### plot with col = indication of change in LONGITUDE ###
  lon <- ggplot.setkde +
    geom_line(aes(linetype=as.factor(YEAR)), col="grey", size=0.4) + 
    geom_point(aes(col=CENTER.LON_X, shape=as.factor(YEAR)), cex=0.85) +
    viridis::scale_color_viridis() +
    labs(col = "Longitude")
  ggsave(paste0(pathKML, "/rplot_spatial_LON.pdf"), lon, width=49.9, height=30)
  
  
  #### calculate distance between first and second point ####
  # to use as colour gradient in plots
  # distance in m
  for (i in seq_along(setkde$CENTER.LON_X)) {
    if (i==nrow(setkde)) {
      setkde$DIST_TO_NXT_PT_m[i] <- 0   #for last row of dataset, the distance to the next point is 0
    } else {
      if (setkde$ID[i]!=setkde$ID[i+1]) {
        setkde$DIST_TO_NXT_PT_m[i] <- 0   #for last row of each bird, the distance to the next point is 0
      } else {
        if (setkde$YEAR[i]!=setkde$YEAR[i+1]) {
          setkde$DIST_TO_NXT_PT_m[i] <- 0   #for last row of each year, the distance to the next point is 0
        } else {
          setkde$DIST_TO_NXT_PT_m[i] <- format(round(geosphere::distGeo(c(setkde$CENTER.LON_X[i], setkde$CENTER.LAT_Y[i]), c(setkde$CENTER.LON_X[i+1], setkde$CENTER.LAT_Y[i+1])), 2), scientific=F)}}}}
  setkde$DIST_TO_NXT_PT_m <- as.numeric(setkde$DIST_TO_NXT_PT_m)
  str(setkde)
  hist(setkde$DIST_TO_NXT_PT_m)
  hist(log(setkde$DIST_TO_NXT_PT_m))
  
  ### plot with col = indication of change in position ###
  dist <- ggplot.setkde +
    geom_line(aes(linetype=as.factor(YEAR)), col="grey", size=0.5) + 
    geom_point(aes(col=log(DIST_TO_NXT_PT_m), shape=as.factor(YEAR)), cex=1) +
    viridis::scale_color_viridis()
  ggsave(paste0(pathKML, "/rplot_spatial_DIST.pdf"), dist, width=49.9, height=30)
  } #old section 7: "inspect results" -- ggplots

{
  # to make legend font size smaller
  #library(htmltools)
  #browsable(
  #  tagList(list(
  #    tags$head(
  #      tags$style(
  #        ".leaflet .legend {
  #             font-size: 10px;
  #             }"
  #      )
  #    ),
  #    l1
  #  ))
  #)
  
} #make leaflet legend font-size smaller

{
  ####################### 6 #######################
  ########## add standardisation factor ###########
  #################################################-------------------------------------------------
  pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
  gpsss <- read.table(paste0(pathKML, "/2015-2020coordinates_cleaned.csv"), header=T, dec=".", sep=";")
  summary(gpsss); str(gpsss)
  # use steps above to get
  migr.settlers
  
  # define start and end date of times to remove from dataset (dates during migration, or in winter)
  for (i in seq_along(migr.settlers$bird_id)) {
    if (is.na(migr.settlers$fall_start[i])==F & is.na(migr.settlers$spring_end[i])==F) {
      migr.settlers$Start[i] <- migr.settlers$fall_start[i]
      migr.settlers$End[i] <- migr.settlers$spring_end[i]
    } else if (is.na(migr.settlers$fall_start[i])==T & is.na(migr.settlers$spring_end[i])==F) {
      if (is.na(migr.settlers$spring_start[i])==F) {
        migr.settlers$Start[i] <- migr.settlers$spring_start[i]
        migr.settlers$End[i] <- migr.settlers$spring_end[i]
      } else {
        migr.settlers$Start[i] <- NA
        migr.settlers$End[i] <- NA
      }
    } else if (is.na(migr.settlers$fall_start[i])==F & is.na(migr.settlers$spring_end[i])==T) {
      if (is.na(migr.settlers$fall_end[i])==F) {
        migr.settlers$Start[i] <- migr.settlers$fall_start[i]
        migr.settlers$End[i] <- migr.settlers$fall_end[i]
      } else {
        migr.settlers$Start[i] <- NA
        migr.settlers$End[i] <- NA
      }
    } else if (is.na(migr.settlers$fall_start[i])==T & is.na(migr.settlers$spring_end[i])==T) {
      migr.settlers$Start[i] <- NA
      migr.settlers$End[i] <- NA
    }
  } #keep julian Start-End dates for next command !
  
  non.migration.settlers <- do.call(rbind, lapply(split(gpsss, gpsss$NAME), function(gpsss) {
    sub_migr.settlers <- subset(migr.settlers, TransmGSM==gpsss$NAME[1] & is.na(Start)==F)
    if(nrow(sub_migr.settlers)>=1) {
      # define start and end date of times to remove from dataset (dates during migration, or in winter)
      filt.vals = unlist(apply(sub_migr.settlers, 1, function(a) a["Start"]:a["End"]), use.names = F)
      filt.vals1 <- as.Date(filt.vals, origin="1970-01-01")
      # keep only those dates in the dataset
      gpsss$DATE <- as.Date(gpsss$DATE)
      filtered.gpsss <- gpsss %>% filter(!(DATE %in% filt.vals1))
      filtered.gpsss
    } else {  # for kites that were resident all years:
      gpsss
    }
  }))
  
  write.table(non.migration.settlers, paste0(pathKML, "/AllSettlers_nonMigrating_nonWinter_GPSpoints.csv"), row.names=F, sep=";")
  non.migration.settlers <- read.table(paste0(pathKML, "/AllSettlers_nonMigrating_nonWinter_GPSpoints.csv"), header=T, dec=".", sep=";")
  
  # all the GPS points with the right dates are ready now
  
  # GPS points are in EPSG:4326 so far, which is in degrees. I tried different methods to
  # (i) Transform into projected CRS with unit metres, focus Europe (EPSG:3035) and calculate that average
  # (ii) according to this (geoographic midpoint): https://www.geomidpoint.com/calculation.html
  # (iii) various gcentroid, centroid and st_centroid functions
  #       - st_centroid didn't really work
  #       - gcentroid with CRS 4326 gave same result as normal averaging)
  #       - geoographic midpoint manual conversion gave almost same point as simple averaging (just a few house down a street)
  # THEREFOR: Staying with the simple averaging method
  
  setkde <- read.table(paste0(pathKML, "/KDEsettlers_per_5days_2ppd_m.csv"), header=T, dec=".", sep=";")
  setkde.all <- read.table(paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_m.csv"), header=T, dec=".", sep=";")
  
  setkde <- setkde.all
  
  # find center for each bird
  cent <- non.migration.settlers %>% 
    group_by(NAME) %>% 
    summarise(cent_lon = mean(LON), cent_lat = mean(LAT)) 
  
  # merge with KDE dataset
  merged <- merge(setkde, cent, by.x="ID", by.y="NAME")
  
  new <- merged %>% 
    mutate(dist_km = geosphere::distHaversine(cbind(CENTER.LON_X, CENTER.LAT_Y), cbind(cent_lon, cent_lat))/1000) %>% 
    group_by(ID) %>% 
    mutate(avg_dist2center=mean(dist_km)) %>% 
    dplyr::select(-c(dist_km, cent_lon, cent_lat))
  
  write.table(new, paste0(pathKML, "/KDEsettlers_per_5days_2ppd_standardisationFactor.csv"), row.names=F, sep=";")
  write.table(new, paste0(pathKML, "/KDE_AllSettlers_per_5days_2ppd_standardisationFactor.csv"), row.names=F, sep=";")
} #old section 6: "add standardisation factor"

{
  system.time({
  KDE.settlers1 <- do.call(rbind, lapply(split(coord1, coord1$NAME), function(coord) {
    
    # remove days without sufficient number of data points (2 per day during daylight)
    nrlocs <- coord %>% group_by(DATE) %>% 
      filter(hour(TIME) >= first.GPS.point & hour(TIME) <= last.GPS.point) %>% 
      summarize(nr_pos = n()) %>% 
      filter(nr_pos >= minimum.datapoints.per.daylight)
    sub_coord <- subset(coord, DATE %in% nrlocs$DATE)
    sub_coord$DATE <- as.Date(sub_coord$DATE, format="%Y-%m-%d") #definitely use "format", because 
    #otherwise the conversion from POSIXct to Date will result in false date, e.g. 2019-03-12 instead of 2019-03-11 !
    
    # Initilizing matrix for storing results
    results = as.data.frame(matrix(NA, nrow=length(unique(sub_coord$DATE)), ncol=7))
    colnames(results)<-c("ID", "START_DATE", "NR_POINTS", "AREA_KM2", "H", "CENTER.LON_X", "CENTER.LAT_Y")
    
    for (i in seq_along(sub_coord$DATE)) {
      
      if(i==1) {
        j=1 # start indexing
        
        # determine start and end date
        start <- sub_coord$DATE[i]
        end <- start + lubridate::hours((days.stationary-1)*24)
        filtercoord <- sub_coord %>% filter(between(sub_coord$DATE, start, end))
        
        # if one day in the moving window is missing, jump to next date
        if (length(unique(filtercoord$DATE))<days.stationary) next else {
          
          # Transform CRS into UTM for Europe, output=meters
          sp <- SpatialPoints(filtercoord[4:5])
          proj4string(sp) = CRS("+init=epsg:4326")
          coords.utm <- spTransform(sp, CRS("+init=epsg:3035"))
          
          # apply KDE only if no error message (error would be: grid too small -> only with very extended home ranges; unimportant for this)
          ud <- kernelUD(coords.utm, h="href")  #calculate the utilization distribution
          x <- try(getverticeshr(ud, percent=kde.percent, unin = 'm', unout='km2')$area, silent=T) #find the 90% utilization distribution
          #getverticeshr$area measures the area of the vector home range (with smoother contour), as opposed to
          #kernel.area, which measures the area covered by the rasterized home range (pixels!)
          
          if('try-error' %in% class(x)) next else {
            # save values in new df
            results[j,1] <- paste0(filtercoord$NAME[1])
            results[j,2] <- paste0(start)
            results[j,3] <- nrow(filtercoord)
            results[j,4] <- round(x, 2)
            results[j,5] <- round(ud@h$h, 2)          # smoothing factor from the utilization distribution
            results[j,6] <- round(mean(filtercoord$LON), 6)
            results[j,7] <- round(mean(filtercoord$LAT), 6)
          }
        }
      }
      
      else if (sub_coord$DATE[i]!=sub_coord$DATE[i-1]) {   #after day 1 jump from day to day, not row to row
        
        # use only 6 days of data
        start <- sub_coord$DATE[i]
        end <- start + lubridate::hours((days.stationary-1)*24)
        filtercoord <- sub_coord %>% filter(between(sub_coord$DATE, start, end))
        
        # if one day in the moving window is missing, jump to next date
        if (length(unique(filtercoord$DATE))<days.stationary) next else {
          
          # Transform CRS into UTM for Europe, output=meters
          sp <- SpatialPoints(filtercoord[4:5])
          proj4string(sp) = CRS("+init=epsg:4326")
          coords.utm <- spTransform(sp, CRS("+init=epsg:3035"))
          
          j = j+1 #use index fo successful loops for assignment of results to rows
          
          # apply KDE only if no error message
          ud <- kernelUD(coords.utm, h="href")  #calculate the utilization distribution
          x <- try(getverticeshr(ud, percent=kde.percent, unin = 'm', unout='km2')$area, silent=T) #find the 90% utilization distribution
          
          if('try-error' %in% class(x)) next else {
            # save values in new df
            results[j,1] <- paste0(filtercoord$NAME[1])
            results[j,2] <- paste0(start)
            results[j,3] <- nrow(filtercoord)
            results[j,4] <- round(x, 2)
            results[j,5] <- round(ud@h$h, 2)          # smoothing factor from the utilization distribution
            results[j,6] <- round(mean(filtercoord$LON), 6)
            results[j,7] <- round(mean(filtercoord$LAT), 6)
          }
        }
      } else next
    }
    results <- results[complete.cases(results), ]
  }))
})
  } #old command for KDE calculation (just having the do.call etc with the function description together, no big change!)

{
  ####################### 2 #######################
  ######## filter to only relevant years ##########
  #################################################-------------------------------------------------
  # this helps to filter out years of birth, or after settlement...
  # ... helps to not make the shiny graphs of home ranges too messy
  pathKML <- paste0("/Users/Lara/Documents/_Jobs/2020 Sempach/Settlement Protocol/QGIS settlement protocol/Kernel Density Estimation")
  rel_years <- read.table(paste0(pathKML, "/list_of_relevant_years_forKDE_perBird.csv"), header=T, dec=".", sep=";")
  
  # bird batch 1 ------------------------------------------------------------------------------------------
  gpsss <- read.table(paste0(pathKML, "/2015-2020coordinates_birds1_cleaned.csv"), header=T, dec=".", sep=";")
  gpsss$DATE <- as.POSIXct(gpsss$DATE)
  gpsss$TIME <- as.POSIXct(gpsss$TIME, format="%Y-%m-%d %H:%M")
  gpsss$BIRD_ID <- as.factor(gpsss$BIRD_ID)
  gpsss$YEAR <- substr(gpsss$DATE,1,4)
  
  # data of which years to take
  # otherwise it may take forever
  rel_years_birds1 <- subset(rel_years, NAME %in% gpsss$NAME)  %>% droplevels
  asd <- do.call(rbind, lapply(split(gpsss, gpsss$NAME), function (gpsss) {
    years2keep <- subset(rel_years_birds1, NAME==gpsss$NAME[1])
    new <- gpsss %>% filter(YEAR>=min(years2keep$YEAR) & YEAR<=max(years2keep$YEAR))
    new
  }))
  
  coord <- subset(asd, select=-c(YEAR))
  str(coord)
  summary(coord)
  
  write.table(coord, paste0(pathKML, "/2015-2020coordinates_birds1_relYears.csv"), row.names=F, sep=";")
  
  
  
  # bird batch 2 ------------------------------------------------------------------------------------------
  gpsss.b2 <- read.table(paste0(pathKML, "/2015-2021coordinates_birds2_cleaned.csv"), header=T, dec=".", sep=";")
  gpsss.b2$DATE <- as.POSIXct(gpsss.b2$DATE)
  gpsss.b2$TIME <- as.POSIXct(gpsss.b2$TIME, format="%Y-%m-%d %H:%M")
  gpsss.b2$BIRD_ID <- as.factor(gpsss.b2$BIRD_ID)
  gpsss.b2$YEAR <- substr(gpsss.b2$DATE,1,4)
  
  # data of which years to take
  # otherwise it may take forever
  rel_years_birds2 <- subset(rel_years, NAME %in% gpsss.b2$NAME)  %>% droplevels
  asd2 <- do.call(rbind, lapply(split(gpsss.b2, gpsss.b2$NAME), function (gpsss.b2) {
    years2keep <- subset(rel_years_birds2, NAME==gpsss.b2$NAME[1])
    new <- gpsss.b2 %>% filter(YEAR>=min(years2keep$YEAR) & YEAR<=max(years2keep$YEAR))
    new
  }))
  
  coord.b2 <- subset(asd2, select=-c(YEAR))
  str(coord.b2)
  summary(coord.b2)
  
  write.table(coord.b2, paste0(pathKML, "/2015-2021coordinates_birds2_relYears.csv"), row.names=F, sep=";")
} #old section 2: "filter to only relevant years"

{library(leaderCluster)
  setkde5.0 <- setkde5.all %>% filter(ID=="KIWS37" & AREA_KM2<=6 & on.migration=="no" &
                                        settlement.year!="settlement year" & settlement.year!="1y after settling" & settlement.year!="2y after settling" & settlement.year!="3y after settling" &
                                        DATE>="2020-03-01" & DATE>="2020-06-30") %>% 
    dplyr::select(CENTER.LAT_Y, CENTER.LON_X)
  setkde5.0sf <- setkde5.0 %>% 
    st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
    st_set_crs(4326) #this uses WGS84 (expected by leaflet)
  
  clust <- leaderCluster(setkde5.0, 0.3, weights = rep(1, nrow(setkde5.0)),# max_iter = 10L,
                         distance = "haversine") #, p = 2)
  clustc <- as.data.frame(clust$cluster_centroids); colnames(clustc) <- c("lat", "long")
  clustsf <- clustc %>% 
    st_as_sf(coords=c("long","lat")) %>% 
    st_set_crs(4326) #this uses WGS84 (expected by leaflet)
  
  
  leaflet() %>%
    addProviderTiles(providers$OpenTopoMap,
                     options = providerTileOptions(opacity = 0.7) #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
    ) %>% 
    addMapPane("cluster", zIndex = 415 #puts settlements on top of all layers
    ) %>%  
    addMapPane("kdecenters", zIndex = 405 #puts settlements on top of all layers
    ) %>%  
    addCircleMarkers(
      data=setkde5.0sf,
      color = "grey",
      radius = 5,
      fillOpacity = 0.6,
      stroke=F,
      options = pathOptions(pane = "kdecenters"),
      clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                            disableClusteringAtZoom=11)
    ) %>% 
    addCircleMarkers(
      data=clustsf,
      color = "red",
      radius = 3,
      stroke=F,
      options = pathOptions(pane = "cluster"),
      fillOpacity = 1
    ) %>% 
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "kilometers",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )} #calculating clusters with "leaderCluster"-function

{
setpotfilt <- do.call(rbind, lapply(split(setpot, setpot$TransmGSM), function(setpot) {
  if(nrow(setpot)==2) {
    setpot$dist_km[c(1,2)] <- raster::pointDistance(c(setpot$LON[1], setpot$LAT[1]), c(setpot$LON[2], setpot$LAT[2]), lonlat=T) / 1000
    setpot
  }
}))
} #calculating distance between potato and settlement

