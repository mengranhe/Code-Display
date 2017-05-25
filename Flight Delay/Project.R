#### 36-662 Data Mining Final Project ####
#### This file is used to clean data and write file ####

library(dplyr)
library(data.table)
library(sqldf)
library(ggplot2)
library(magrittr)
library(Holidays)
library(rapportools)
library(rvest)

dtf15 <- read.csv("flights2015.csv", header = TRUE)
# dtf16 <- read.csv("flights2016.csv", header = TRUE)
dtf16_visible <- read.csv("flights2016_visible.csv", header = TRUE)
dtf16_guess <- read.csv("flights2016_guess.csv", header = TRUE)

tr15 <- dtf15[, -which(names(dtf15) %in% c("UNIQUE_CARRIER", "AIRLINE_ID", "ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID","ORIGIN_CITY_MARKET_ID",
                  "ORIGIN_CITY_NAME", "ORIGIN_STATE_FIPS","ORIGIN_STATE_NM","ORIGIN_WAC",
                  "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID","DEST_CITY_MARKET_ID","DEST_CITY_NAME","DEST_STATE_FIPS","DEST_STATE_NM",
                  "DEST_WAC","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", "WHEELS_ON", "TAXI_IN",              
                  "ARR_TIME","ARR_DELAY", "ARR_DELAY_NEW","ARR_DEL15", "ARR_DELAY_GROUP", "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
                  "ACTUAL_ELAPSED_TIME","AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY",       
                  "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME", "TOTAL_ADD_GTIME","LONGEST_ADD_GTIME"))]


# "DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", "WHEELS_ON", "TAXI_IN",              
# "ARR_TIME","ARR_DELAY", "ARR_DELAY_NEW","ARR_DEL15", "ARR_DELAY_GROUP", "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
# "ACTUAL_ELAPSED_TIME","AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY",       
# "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME", "TOTAL_ADD_GTIME","LONGEST_ADD_GTIME"  

#write.csv(tr15, file = "training.csv", row.names = FALSE)

te16 <- dtf16_visible[, -which(names(dtf16_visible) %in% c("UNIQUE_CARRIER", "AIRLINE_ID", "ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID","ORIGIN_CITY_MARKET_ID",
                                           "ORIGIN_CITY_NAME","ORIGIN_STATE_FIPS","ORIGIN_STATE_NM","ORIGIN_WAC",
                                           "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID","DEST_CITY_MARKET_ID","DEST_CITY_NAME","DEST_STATE_FIPS","DEST_STATE_NM",
                                           "DEST_WAC","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", "WHEELS_ON", "TAXI_IN",              
                                           "ARR_TIME","ARR_DELAY", "ARR_DELAY_NEW","ARR_DEL15", "ARR_DELAY_GROUP", "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
                                           "ACTUAL_ELAPSED_TIME","AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY",       
                                           "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME", "TOTAL_ADD_GTIME","LONGEST_ADD_GTIME"))]


#write.csv(te16, file = "testing.csv", row.names = FALSE)


guess16 <- dtf16_guess[, -which(names(dtf16_guess) %in% c("UNIQUE_CARRIER", "AIRLINE_ID","DEP_DEL15", "ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID","ORIGIN_CITY_MARKET_ID",
                                                              "ORIGIN_CITY_NAME","ORIGIN_STATE_FIPS","ORIGIN_STATE_NM","ORIGIN_WAC",
                                                              "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID","DEST_CITY_MARKET_ID","DEST_CITY_NAME","DEST_STATE_FIPS","DEST_STATE_NM",
                                                              "DEST_WAC","DEP_TIME", "DEP_DELAY","DEP_DELAY_NEW","DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", "WHEELS_ON", "TAXI_IN",              
                                                              "ARR_TIME","ARR_DELAY", "ARR_DELAY_NEW","ARR_DEL15", "ARR_DELAY_GROUP", "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
                                                              "ACTUAL_ELAPSED_TIME","AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY","SECURITY_DELAY",       
                                                              "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME", "TOTAL_ADD_GTIME","LONGEST_ADD_GTIME"))]
#write.csv(guess16, file = "predicting.csv", row.names = FALSE)

######################################### Weather Data #########################################
## Airport Code and Names
airportCode = fread("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",
                    data.table = FALSE)[,-1]
airportCodeShort = subset(airportCode, subset =  V5 %in% unique(rbind(dtf15,dtf16_visible)$DEST), 
                          select = c(V2, V5)) %>% arrange(V2)
# manually add STL airport since it appeard in the testing dataset
# write.csv(airportCodeShort, file = "airportCodeShort.csv")

# Link to weather data
# https://www1.ncdc.noaa.gov/pub/orders/CDO3599347325480.txt
weather = fread("weatherData.txt", data.table = FALSE, sep = ",")
weather = weather[, -grep("^V[[:digit:]]{,2}$", colnames(weather))]
# Downloaded PHX and STL weather data from the same website, rbind below
PHXwea = read.csv("PHXweather.txt", sep=",")
STLwea = read.csv("STLweather.txt", sep=",")
PHXwea = PHXwea[, colnames(PHXwea) %in% colnames(weather)]
STLwea = STLwea[, colnames(STLwea) %in% colnames(weather)]
PHXwea = cbind("PHX", PHXwea)
STLwea = cbind("STL", STLwea)
colnames(PHXwea)[1] = colnames(weather)[1]
colnames(STLwea)[1] = colnames(weather)[1]
weather = rbind(weather, PHXwea, STLwea)

stationdata = read.fwf("stationlist.txt", skip = 22, widths = c(7, 6, 30))
colnames(stationdata) = c("USAF", "WBAN", "StationName")
#write.table(stationdata, "stationcsv.csv", sep = "\t")

weatherwithname = merge(stationdata, weather, by = "WBAN", all.y = TRUE)
weatherwithname$shortname = gsub('[/-]', ' ', substr(weatherwithname$StationName, 1,16))

airportCodeShort$shortname = gsub('[/-]', ' ', (toupper(substr(airportCodeShort$V2,1,16))))
#George Bush Intercontinental Houston Airport
airportCodeShort$shortname[airportCodeShort$shortname == "GEORGE BUSH INTE"] = "G BUSH INTERCONT"   #G BUSH INTERCONTINENTAL AP/HO
#General Edward Lawrence Logan International Airport
airportCodeShort$shortname[airportCodeShort$shortname == "GENERAL EDWARD L"] = "GEN E L LOGAN IN"   #GEN E L LOGAN INTERNATIONAL A
#Hartsfield Jackson Atlanta International Airport
airportCodeShort$shortname[airportCodeShort$shortname == "HARTSFIELD JACKS"] = "ATLANTA HARTSFIE"   #ATLANTA HARTSFIELD INTL AP
#Detroit Metropolitan Wayne County Airport
airportCodeShort$shortname[airportCodeShort$shortname == "DETROIT METROPOL"] = "DETROIT METRO WA"   #DETROIT METRO WAYNE COUNTY AI

DELFLLrow = airportCodeShort[airportCodeShort$V5=="PBI",]
DELFLLrow[1,2] = "FLL"
DELRSWrow = airportCodeShort[airportCodeShort$V5=="PBI",]
DELRSWrow[1,2] = "RSW"

airportCodeShort = rbind(airportCodeShort, DELFLLrow, DELRSWrow)

weatherCode = merge(weatherwithname, airportCodeShort, by = "shortname")

#missing from the weather data
state = unique(rbind(dtf15, dtf16_visible)$DEST)
subset(state, !(state %in% unique(weatherCode$V5)))
# FLL and RSW use PBI data, which WBAN is 12844, fixed above by duplicating PBI airportCode as FLL and RSW
# PIT uses hourly data

# check if any station name in the weather data has the same short version
DELuniweashort = unique(weatherCode$V5)
#sapply(DELuniweashort, function(x){unique(weatherCode$StationName[weatherCode$V5 == x])})

# clean duplicated airports
dupairprt = c("CHARLOTTE DOUGLAS MUNICIPAL A", "DALLAS LOVE FIELD","MINNEAPOLIS-ST PAUL WOLD-CHAM",
              "PHILADELPHIA INTERNATIONAL AP", "PHOENIX SKY HARBOR INTL AP", "SAN FRANCISCO INTL AP")
#both CHARLOTTE/DOUGLAS INTERNATION and CHARLOTTE DOUGLAS MUNICIPAL A have same WBAN, this one is 999999
#both DALLAS LOVE FIELD and DALLAS LOVE FIELD AIRPORT have same WBAN, just keep the latter
#both MINNEAPOLIS-ST PAUL WOLD-CHAM and MINNEAPOLIS-ST PAUL INTERNATI have same WBAN, keep the latter
#both PHILADELPHIA INTERNATIONAL AP and PHILADELPHIA INTERNATIONAL AI have same WBAN, keep the latter
#both PHOENIX SKY HARBOR INTL AP and PHOENIX SKY HARBOR INTL AIRPO have same WBAN, keep the latter
#both SAN FRANCISCO INTL AP and SAN FRANCISCO INTERNATIONAL A have same WBAN, keep the latter

weatherCode = weatherCode[!(trimws(weatherCode$StationName) %in% dupairprt),]

sapply(DELuniweashort, function(x){unique(weatherCode$StationName[weatherCode$V5 == x])})
#check again, everycode has only one corresponding airport, OK!

# check with the manual comparison results
DELcodecheck = read.csv("airportShort_WBAN.csv")
 
for (ii in 1:31) {
   cat(unique(weatherCode$WBAN[weatherCode$V5 == DELcodecheck$V5[ii]]),";", DELcodecheck$WBAN[ii],
       unique(weatherCode$WBAN[weatherCode$V5 == DELcodecheck$V5[ii]]) == DELcodecheck$WBAN[ii],
       ii,"\n")
}

weatherCode = weatherCode[,-1]
colnames(weatherCode)[19:20] = c("Airport_NM", "Airport_ABR")
#write.table(weatherCode, "weatherCode.csv", sep = "\t")
rm(airportCode, DELcodecheck, DELFLLrow, DELRSWrow, stationdata, weatherwithname, 
   weather, airportCodeShort, PHXwea, dupairprt, state,ii, STLwea, DELuniweashort)

############### clean unnessary columns and recode some variables in weatherCode ###############
#weatherCode = read.csv("weatherCode.csv", sep="\t")
weatherCode = select(weatherCode, -WBAN,-USAF,-StationName,-contains("STN"),-Airport_NM,
                     -MAX,-MIN,-GUST,-MXSPD)
#change variable type and recode missing value in weatherCode dataset
weatherCode$YEAR = substr(weatherCode$YEARMODA,1,4)
weatherCode$SNDP = ifelse(weatherCode$SNDP == 999.9, 0, weatherCode$SNDP)  #999.9 means missing or no snow. recoded as 0
weatherCode$PRCP = gsub('[A-Z]', "", weatherCode$PRCP) #precipitation: remove any ending letter and recoded as numeric value
weatherCode$PRCP = ifelse(weatherCode$PRCP == 99.99, 0, weatherCode$PRCP)
weatherCode$PRCP = as.numeric(weatherCode$PRCP)
weatherCode$STP = ifelse(weatherCode$STP == 9999.9, weatherCode$SLP, weatherCode$STP) #if station pressure is missing, use sea level pressure
weatherCode$YEARMODA = as.Date(as.character(weatherCode$YEARMODA), "%Y%m%d")
colnames(weatherCode)[!is.na(match(colnames(weatherCode),c("STP", "VISIB", "WDSP", "PRCP", "SNDP")))] = 
                  c("Pressure","Visibility", "WindSpeed", "Precipitation", "Snowdepth")
weatherCode$FRSHTT = formatC(weatherCode$FRSHTT, width=6, flag="0")
#FRSHTT: Indicators (1 = yes, 0 = no/not reported) for the occurrence during the day of:
#Fog ('F' - 1st digit).
#Rain or Drizzle ('R' - 2nd digit).
#Snow or Ice Pellets ('S' - 3rd digit).
#Hail ('H' - 4th digit).
#Thunder ('T' - 5th digit).
#Tornado or Funnel Cloud ('T' - 6th digit).
weatherCode = cbind(weatherCode,sapply(1:6, function(ii){ifelse(substr(weatherCode$FRSHTT,ii,ii)==1,1,0)}))
colnames(weatherCode)[tail(seq_along(weatherCode),6)] = c("Fog","Rain","Snow","Hail","Thunder","Tornado")
weatherCode = select(weatherCode,-FRSHTT, -SLP)
#some rows have positive number in Precipitation or Snowdepth but 0 in FRSHTT, fix Rain and Snow col
weatherCode$Rain = ifelse(weatherCode$Precipitation>0,1,weatherCode$Rain)
weatherCode$Snow = ifelse(weatherCode$Snowdepth>0,1,weatherCode$Snow)
weatherCode = weatherCode[!duplicated(weatherCode),]

#################### recode pitweather.csv and remove unimportant variables ####################
pitwea = read.csv("pitweather.csv", header = TRUE, sep = "\t")
pitwea = select(pitwea, -Gust_Speed.mph., -X17,-X18,-X19,-X20, -Time.PIT., -Wind_Dir, -DEST)

pitwea$Time = as.integer(gsub("^0","",paste0(pitwea$Hour,pitwea$Min)))
pitwea$Date = as.Date(pitwea$Date)
pitwea$Temp.F. = as.numeric(ifelse(pitwea$Temp.F. == "-", NA, regmatches(pitwea$Temp.F., regexpr("-?\\d{,2}.\\d{,2}", pitwea$Temp.F.))))
#temp <- aggregate(pitwea$Temp.F., by = list(pitwea$Date), function(x) mean(x, na.rm = TRUE))
#names(temp) <- c("Date", "dailyTemp")
pitwea$Humidity = as.numeric(ifelse(pitwea$Humidity == "N/A%", NA, gsub("%", "", pitwea$Humidity)))
pitwea$Pressure.in. = as.numeric(ifelse(pitwea$Pressure.in. == "-", NA, regmatches(pitwea$Pressure.in., regexpr("\\d{,2}.\\d{,2}", pitwea$Pressure.in.))))
pitwea$Visibility.mi. = as.numeric(ifelse(pitwea$Visibility.mi. == "-", NA, regmatches(pitwea$Visibility.mi., regexpr("\\d{,2}.\\d{,2}", pitwea$Visibility.mi.))))
pitwea$Dew_Point.F. = as.numeric(ifelse(pitwea$Dew_Point.F. == "-", NA, regmatches(pitwea$Dew_Point.F., regexpr("-?\\d{,2}.\\d{,2}", pitwea$Dew_Point.F.))))
#What does Calm mean? - no wind
pitwea$Wind_Speed.mph. = as.numeric(ifelse(pitwea$Wind_Speed.mph. == "Calm" | 
                        pitwea$Wind_Speed.mph. == "-", 0, regmatches(pitwea$Wind_Speed.mph., regexpr("\\d{,2}.\\d{,2}", pitwea$Wind_Speed.mph.))))

pitwea$Precip.in. = as.numeric(ifelse(pitwea$Precip.in. == "N/A", 0, regmatches(pitwea$Precip.in., regexpr("\\d{,2}.\\d{,2}", pitwea$Precip.in.))))

#unique(pitwea$Conditions)
# [1] Partly Cloudy                Clear                        Scattered Clouds            
# [4] Mostly Cloudy                Overcast                     Light Freezing Rain         
# [7] Light Rain                   Rain                         Light Drizzle               
#[10] Light Snow                   Snow                         Heavy Snow                  
#[13] Haze                         Light Freezing Drizzle       Light Ice Pellets           
#[16] Fog                          Unknown                      Small Hail                  
#[19] Light Rain Showers           Thunderstorm                 Light Thunderstorms and Rain
#[22] Heavy Thunderstorms and Rain Thunderstorms and Rain       Heavy Rain                  
#[25] Patches of Fog               Shallow Fog                  Light Freezing Fog          
#[28] Mist    


pitwea$Snow = as.factor(pitwea$Conditions)
levels(pitwea$Snow) = list("1"=c("Light Snow", "Light Ice Pellets"), 
                           "2"=c("Snow","Small Hail"), "3"=c("Heavy Snow"))
pitwea$Rain = as.factor(pitwea$Conditions)
levels(pitwea$Rain) = list("1" = c("Light Drizzle","Light Freezing Drizzle"),
              "2"=c("Light Freezing Rain", "Light Rain","Light Thunderstorms and Rain","Light Rain Showers"), 
              "3"=c("Rain","Thunderstorms and Rain", "Thunderstorm"), "4"=c("Heavy Rain", "Heavy Thunderstorms and Rain"))
pitwea$Snow = as.factor(ifelse(is.na(pitwea$Snow),0,pitwea$Snow))
pitwea$Rain = as.factor(ifelse(is.na(pitwea$Rain),0,pitwea$Rain))
# We may have to use precipitation and snowdepth from the average daily data
pitwea = select(pitwea, -Events, -Conditions)

############################# update tr15, te16 with weather data ##############################
tr15$FL_DATE = gsub('-', "", tr15$FL_DATE)
te16$FL_DATE = gsub('-', "", te16$FL_DATE)
guess16$FL_DATE = gsub('-', "", guess16$FL_DATE)
tr15DEP = tr15[tr15$ORIGIN=="PIT",]
te16DEP = te16[te16$ORIGIN=="PIT",]
guess16 = cbind(guess16[,1:14],"DEP_DEL",guess16[,15:19])
# we want to focus on flights departured from PIT. We may use arrival information, but guess16 doesn't
# have arrival info. We need to download that for guess16 predictions.
# some features: how many flights have arrived before this departure; if this plane just arrived, when did it
# arrive; how many flights are departing at the same time

# combine all data to first add weather data
colnames(tr15DEP) == colnames(te16DEP) 
colnames(guess16) == colnames(tr15DEP)

tr15DEP = cbind(tr15DEP,"tr15DEP")
te16DEP = cbind(te16DEP, "te16DEP")
guess16 = cbind(guess16, "guess16")

colnames(tr15DEP)[ncol(tr15DEP)] = "DataSet"
colnames(te16DEP) = colnames(tr15DEP)
colnames(guess16) = colnames(tr15DEP)

allData = rbind(tr15DEP,te16DEP,guess16)
allData$FL_DATE = as.Date(allData$FL_DATE, "%Y%m%d")

minPos = function(x){which.min(x[x>0])}

# merge PITweather based on dates and time; for time, use the time right before the scheduled departure;
# if there is no same day closest time recorded, take the last record from previous day
DELstart = Sys.time()

FL_DATErow = which(colnames(allData)=="FL_DATE")
CRS_DEP_TIMErow = which(colnames(allData)=="CRS_DEP_TIME")
mergePIT = function(row) {
  pitSameDay = pitwea[pitwea$Date == row[FL_DATErow],]
  closesttime = minPos(as.numeric(row[CRS_DEP_TIMErow])-pitSameDay$Time)
  if (length(closesttime)==0){
    pitPreDay = pitwea[pitwea$Date == (as.Date(row[FL_DATErow])-1),]
    return(pitPreDay[nrow(pitPreDay),])
  }
  return(pitSameDay[closesttime,])
}
pitweaAllDEP = apply(allData, 1,mergePIT)
pitweaAllDEP = do.call(rbind,pitweaAllDEP)

DELend = Sys.time()
DELdur = DELend-DELstart 
DELdur #3.34428 mins

colnames(pitweaAllDEP) = paste0("DEP_",colnames(pitweaAllDEP))
allData = cbind(allData,pitweaAllDEP)

allData = merge(x = allData, y=weatherCode, by.x = c("YEAR","FL_DATE", "DEST"), 
                by.y = c("YEAR","YEARMODA","Airport_ABR"), all.x = TRUE, all.y=FALSE)
colnames(allData)[tail(seq_along(allData),13)] = paste0("ARV_", colnames(allData)[tail(seq_along(allData),13)])

# factorize ARV_snowdepth into 0,1,2,3 levels.
ARV_SnowdepNonZ = allData$ARV_Snowdepth[!is.na(allData$ARV_Snowdepth)&allData$ARV_Snowdepth!=0]
allData$ARV_Snow = cut(as.numeric(allData$ARV_Snowdepth), labels = c(0,1,2,3),
    breaks = c(-1,0,quantile(allData$ARV_Snowdepth[allData$ARV_Snowdepth!=0], 0.33, na.rm = TRUE), 
    quantile(allData$ARV_Snowdepth[allData$ARV_Snowdepth!=0], 0.67, na.rm = TRUE),
    max(allData$ARV_Snowdepth, na.rm = TRUE)))

# clean and format allData
allData$ARV_Rain = as.factor(allData$ARV_Rain)
allData$ARV_Fog = as.factor(allData$ARV_Fog)
allData$ARV_Hail = as.factor(allData$ARV_Hail)
allData$ARV_Thunder = as.factor(allData$ARV_Thunder)
allData$ARV_Tornado = as.factor(allData$ARV_Tornado)
allData$QUARTER = as.factor(allData$QUARTER)
allData$DAY_OF_WEEK = as.factor(allData$DAY_OF_WEEK)
allData$DISTANCE_GROUP = as.factor(allData$DISTANCE_GROUP)
allData = select(allData, -DEP_Dew_Point.F., -DEP_Precip.in., -DEP_Date, -DEP_Time, -DEP_Hour, -DEP_Min)

allData$FL_DATE_ARR = if_else(allData$CRS_ARR_TIME<=allData$CRS_DEP_TIME, allData$FL_DATE+1, allData$FL_DATE)
allData$CRS_DEP_TIME = ifelse(allData$CRS_DEP_TIME<1000, paste0(0,allData$CRS_DEP_TIME), as.character(allData$CRS_DEP_TIME))
allData$CRS_ARR_TIME = ifelse(allData$CRS_ARR_TIME<10, paste0("000", allData$CRS_ARR_TIME), ifelse(allData$CRS_ARR_TIME<1000, paste0(0,allData$CRS_ARR_TIME), as.character(allData$CRS_ARR_TIME)))
allData$DEP_Time = as.POSIXct(strptime(paste(allData$FL_DATE, allData$CRS_DEP_TIME), format="%Y-%m-%d %H%M"))
allData$ARR_Time = as.POSIXct(strptime(paste(allData$FL_DATE_ARR, allData$CRS_ARR_TIME), format="%Y-%m-%d %H%M"))
#allData$DEP_Hour = lubridate::hour(allData$DEP_Time)
allData$DEP_Hour = as.numeric(substr(allData$CRS_DEP_TIME,1,2))
allData$DEP_Day = ifelse(allData$DEP_Hour>=6&allData$DEP_Hour<12, "Morning", 
                  ifelse(allData$DEP_Hour>=12&allData$DEP_Hour<18, "Afternoon",
                  ifelse(allData$DEP_Hour>=18&allData$DEP_Hour<24, "Evening", "Midnight")))
allData$DEP_Day = factor(allData$DEP_Day, levels = c("Morning", "Afternoon", "Evening", "Midnight"))

rm(ARV_SnowdepNonZ, DELdur, DELend, DELstart, pitweaAllDEP, CRS_DEP_TIMErow, FL_DATErow, mergePIT, minPos)

# check missing values before splitting datasets.
apply(allData,2,function(x){sum(is.na(x))})

# there are 495 rows of data that response var DEP_DEL15 is missing. If using unsupervised data, we may
# create another dataset and give these rows value, so we will have more training/testing data.

# All missing PIT temp, humidity, visibility are all from the training data, create another version to 
# use the previous hour data for those NA's; 
# arrival data are missing 3 records, all from PHX, the new version uses the day before.

allDataNonMiss = allData
allDataNonMiss[is.na(allDataNonMiss$DEP_Temp.F.) & allDataNonMiss$FL_DATE == "2015-08-17",][,c(22,23,25)] = 
    pitwea[pitwea$Date == '2015-08-17' & pitwea$Hour == 11,][c(1,3,5)]
allDataNonMiss[is.na(allDataNonMiss$DEP_Temp.F.) & allDataNonMiss$FL_DATE == "2015-09-01",][,c(22,23,25)] = 
  pitwea[pitwea$Date == '2015-09-01' & pitwea$Hour == 11,][c(1,3,5)]
allDataNonMiss[is.na(allDataNonMiss$DEP_Temp.F.) & allDataNonMiss$FL_DATE == "2015-09-03",][,c(22,23,25)] = 
  pitwea[pitwea$Date == '2015-09-01' & pitwea$Hour == 10,][c(1,3,5)]
allDataNonMiss[is.na(allDataNonMiss$DEP_Visibility.mi.) & allDataNonMiss$FL_DATE == "2015-07-23",][,25] = 
  pitwea[pitwea$Date == '2015-07-23' & pitwea$Hour == 10,][c(5)]
allDataNonMiss[is.na(allDataNonMiss$ARV_TEMP),][,29:41] = 
  weatherCode[weatherCode$Airport_ABR == "PHX" & weatherCode$YEARMODA == "2015-09-30",][c(2:8,11:16)]

apply(allDataNonMiss,2,function(x){sum(is.na(x))})

tr15DEP = select(allData[allData$DataSet=="tr15DEP",], -DataSet)
te16DEP = select(allData[allData$DataSet=="te16DEP",], -DataSet)
guess16 = select(allData[allData$DataSet=="guess16",], -DataSet)

tr15DEPNonM = select(allDataNonMiss[allDataNonMiss$DataSet=="tr15DEP",], -DataSet)
te16DEPNonM = select(allDataNonMiss[allDataNonMiss$DataSet=="te16DEP",], -DataSet)
guess16NonM = select(allDataNonMiss[allDataNonMiss$DataSet=="guess16",], -DataSet)

save(tr15DEP, te16DEP, guess16, tr15DEPNonM, te16DEPNonM, guess16NonM, pitwea, weatherCode, allData, 
     allDataNonMiss, file="WeatherData.RData")


################################# add previous plane info 2015 #################################
pitdep = tr15DEPNonM
pitdep = pitdep[complete.cases(pitdep),]
pitdep$FL_DATE = as.character(pitdep$FL_DATE)
pitarr = dtf15[dtf15$DEST=="PIT", 
              -which(names(dtf15) %in% c("YEAR","QUARTER","DAY_OF_WEEK",
                                         "AIRLINE_ID", "ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID",
                                         "ORIGIN_CITY_MARKET_ID","ORIGIN_CITY_NAME", "ORIGIN_STATE_FIPS",
                                         "ORIGIN_STATE_ABR", "ORIGIN_STATE_NM","ORIGIN_WAC",
                                         "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID","DEST_CITY_MARKET_ID",
                                         "DEST_CITY_NAME","DEST_STATE_FIPS","DEST_STATE_NM",
                                         "DEST_STATE_ABR","DEST_WAC", "DEP_DELAY","DEP_DELAY_NEW",
                                         "DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", 
                                         "WHEELS_ON","ARR_DELAY_GROUP",
                                         "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
                                         "AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY",
                                         "SECURITY_DELAY","FLIGHTS",
                                         "DISTANCE_GROUP","FIRST_DEP_TIME", "TOTAL_ADD_GTIME",
                                         "LONGEST_ADD_GTIME", "CRS_DEP_TIME","LATE_AIRCRAFT_DELAY",
                                         "CRS_ELAPSED_TIME", "DISTANCE", "MONTH", "DAY_OF_MONTH",
                                         "UNIQUE_CARRIER", "DEP_TIME", "DEP_DEL15", "DEST", "FL_NUM",
                                         "ACTUAL_ELAPSED_TIME", "ORIGIN", "ARR_DELAY_NEW"))]
pitjoin = sqldf("
      SELECT d.*, a.CARRIER AS prev_CARRIER, a.CRS_ARR_TIME AS prev_CRS_ARR_TIME, 
                  a.ARR_TIME AS prev_actl_ARR_TIME, a.ARR_DELAY AS pre_actl_ARR_DELAY,
                  a.ARR_DEL15 AS prev_actl_ARR_DEL15, a.FL_DATE AS pre_FL_DATE, 
                  a.TAIL_NUM AS pre_TAIL_NUM, a.TAXI_IN AS pre_TAXI_IN
      FROM pitdep AS d 
      LEFT JOIN pitarr AS a ON 
                  d.FL_DATE=a.FL_DATE AND d.TAIL_NUM=a.TAIL_NUM AND prev_CRS_ARR_TIME<d.CRS_DEP_TIME
      GROUP BY d.FL_DATE, d.TAIL_NUM, d.CRS_DEP_TIME
      HAVING MAX(CASE WHEN prev_CRS_ARR_TIME IS NULL THEN 0.99 ELSE prev_CRS_ARR_TIME END);")

# quick check by making sure first several columns are not duplicated. sum to 0, so good; Quick check
# to make sure all carriers match, and nonmatch sum to 0, so good.
sum(duplicated(select(pitjoin, YEAR:DEP_Day)))
table(pitjoin[pitjoin$DEP_DEL15==1,'prev_actl_ARR_DEL15'])
table(pitjoin[pitjoin$DEP_DEL15==0,'prev_actl_ARR_DEL15'])
sum(pitjoin[!is.na(pitjoin$prev_CARRIER),]$CARRIER != pitjoin[!is.na(pitjoin$prev_CARRIER),]$prev_CARRIER)

# good. all flights that arrived late than planed dep time caused delay for PIT departure.
pitjoin$pre_actl_ARR_postCRSDEP = ifelse(pitjoin$prev_actl_ARR_TIME>as.numeric(pitjoin$CRS_DEP_TIME),1,0)
table(pitjoin[pitjoin$pre_actl_ARR_postCRSDEP==1, 'DEP_DEL15'])

pitjoin$pre_actl_extratime = difftime(strptime(pitjoin$CRS_DEP_TIME,"%H%M"),
                  strptime(formatC(pitjoin$prev_actl_ARR_TIME,width=4, flag="0"), "%H%M"), units="mins")
pitjoin$pre_actl_extraaftTaxi = pitjoin$pre_actl_extratime + pitjoin$pre_TAXI_IN

# among delayed planes, 75% of them only have 40 min or less to get ready for next departure. Also read 
# an article online, a plane needs about 34 min to get ready from one to another. Use 30 to differeciate 
# time. The reason have to factorize: we cannot assign a reasonable number to NA's, but you know there 
# will have enough time for planes to get ready.
quantile(pitjoin[pitjoin$DEP_DEL15==1&pitjoin$pre_actl_extratime>0,'pre_actl_extratime'],na.rm = TRUE)

pitjoin$pre_actl_extraFactor = cut(as.numeric(pitjoin$pre_actl_extratime), labels = c(10:0),
                                      breaks = c(-10000,5,seq(10,90,by=10),10000))

max(pitjoin[pitjoin$DEP_DEL15==1&pitjoin$pre_actl_extratime>0,'pre_actl_extratime'], na.rm = TRUE)
min(pitjoin[pitjoin$DEP_DEL15==0&pitjoin$pre_actl_extratime>0,'pre_actl_extratime'], na.rm = TRUE)
table(pitjoin$pre_actl_extraFactor)
ggplot(pitjoin, aes(x = as.numeric(pre_actl_extraFactor))) + 
  geom_histogram(binwidth = 1, aes(fill = as.factor(DEP_DEL15)), position = "identity", alpha = 0.65) +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")

quantile(pitjoin[pitjoin$DEP_DEL15==1&pitjoin$pre_actl_extraaftTaxi>0,'pre_actl_extraaftTaxi'],na.rm = TRUE)

pitjoin$pre_actl_extraFactortaxi = cut(as.numeric(pitjoin$pre_actl_extraaftTaxi), labels = c(10:0),
                                   breaks = c(-10000,5,seq(10,90,by=10),10000))

# how about too many flights planned to leave at the same time cause delay on flight system?
flightPerHr =  aggregate(FLIGHTS~FL_DATE+DEP_Hour, data=pitjoin, FUN=sum)
# aggregate(pitjoin$FLIGHTS, by=list(FL_DATE=pitjoin$FL_DATE, DEP_Hour=pitjoin$DEP_Hour), FUN=sum)
pitjoin = merge(x = pitjoin, y = flightPerHr, all.x = TRUE, by = c("FL_DATE", "DEP_Hour"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS.y"] = "SameHourFlights"
ggplot(pitjoin, aes(x = SameHourFlights)) + 
  geom_histogram(binwidth = 1, aes(fill = as.factor(DEP_DEL15)), position = "identity", alpha = 0.65) +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")
ggplot(pitjoin, aes(x = SameHourFlights)) + 
  geom_bar(aes(fill = as.factor(DEP_DEL15)), position = "fill") +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")
hist(pitjoin$SameHourFlights)

ggplot(pitjoin, aes(x = CARRIER)) + 
  geom_bar(aes(fill = as.factor(DEP_DEL15)), position = "fill") +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")

flightPerdaypart =  aggregate(FLIGHTS.x~FL_DATE+DEP_Day, data=pitjoin, FUN=sum)
# aggregate(pitjoin$FLIGHTS, by=list(FL_DATE=pitjoin$FL_DATE, DEP_Hour=pitjoin$DEP_Hour), FUN=sum)
pitjoin = merge(x = pitjoin, y = flightPerdaypart, all.x = TRUE, by = c("FL_DATE", "DEP_Day"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS.x.y"] = "SameDayPartFlights"
ggplot(pitjoin, aes(x = SameDayPartFlights)) + 
  geom_histogram(binwidth = 1, aes(fill = as.factor(DEP_DEL15)), position = "identity", alpha = 0.65) +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")
ggplot(pitjoin, aes(x = SameDayPartFlights)) + 
  geom_bar(aes(fill = as.factor(DEP_DEL15)), position = "fill") +
  theme_bw() +
  scale_fill_discrete(name = "Observed Y")
hist(pitjoin$SameDayPartFlights)

# what about the same hour/day/10min actual arrival number of planes?
dtf15forArrAgre = dtf15[dtf15$DEST=="PIT",]
dtf15forArrAgre$ARR_TIME = formatC(dtf15forArrAgre$ARR_TIME, width = 4, flag="0")
dtf15forArrAgre$ARR_TIME_Hour = substr(dtf15forArrAgre$ARR_TIME,1,2)
dtf15forArrAgre$ARR_TIME_Hour = ifelse(dtf15forArrAgre$ARR_TIME_Hour=="  ", "NA",dtf15forArrAgre$ARR_TIME_Hour)
dtf15forArrAgre$ARR_TIME_Hour = ifelse(dtf15forArrAgre$ARR_TIME_Hour=="24", "00",dtf15forArrAgre$ARR_TIME_Hour)
dtf15forArrAgre$ARR_TIME_Hour = as.numeric(dtf15forArrAgre$ARR_TIME_Hour)
dtf15forArrAgre$ARR_TIME_Daypart = ifelse(dtf15forArrAgre$ARR_TIME_Hour>=6&dtf15forArrAgre$ARR_TIME_Hour<12, "Morning", 
                                   ifelse(dtf15forArrAgre$ARR_TIME_Hour>=12&dtf15forArrAgre$ARR_TIME_Hour<18, "Afternoon",
                                   ifelse(dtf15forArrAgre$ARR_TIME_Hour>=18&dtf15forArrAgre$ARR_TIME_Hour<24, "Evening", "Midnight")))
arrflightPerHr = aggregate(FLIGHTS~FL_DATE+ARR_TIME_Hour, data=dtf15forArrAgre, FUN=sum)
arrflightPerHr$ARR_TIME_Hour = as.numeric(arrflightPerHr$ARR_TIME_Hour)

pitjoin = merge(x = pitjoin, y = arrflightPerHr, all.x = TRUE, by.x = c("FL_DATE", "DEP_Hour"),
                by.y = c("FL_DATE", "ARR_TIME_Hour"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS"] = "SameHourarrFlights"

arrflightDaypart = aggregate(FLIGHTS~FL_DATE+ARR_TIME_Daypart, data=dtf15forArrAgre, FUN=sum)
pitjoin = merge(x = pitjoin, y = arrflightDaypart, all.x = TRUE, by.x = c("FL_DATE", "DEP_Day"),
                by.y = c("FL_DATE", "ARR_TIME_Daypart"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS"] = "SameDayPartArrFlights"

# if arr plane number is NA, mean 0
pitjoin[,(ncol(pitjoin)-1):ncol(pitjoin)][is.na(pitjoin[,(ncol(pitjoin)-1):ncol(pitjoin)])] = 0

pitjoin$SameHourFlightsTOTAL = pitjoin$SameHourFlights + pitjoin$SameHourarrFlights
pitjoin$SameDayPartFlightsTOTAL = pitjoin$SameDayPartFlights + pitjoin$SameDayPartArrFlights

ggplot(pitjoin, aes(x = SameHourFlightsTOTAL)) + 
  geom_histogram(binwidth = 1, aes(fill = as.factor(DEP_DEL15)), position = "identity", alpha = 0.65) +
  theme_bw() + scale_fill_discrete(name = "Observed Y")
ggplot(pitjoin, aes(x = SameHourFlightsTOTAL)) + 
  geom_bar(aes(fill = as.factor(DEP_DEL15)), position = "fill") +
  theme_bw() + scale_fill_discrete(name = "Observed Y")
hist(pitjoin$SameHourFlightsTOTAL)

# if na, means the plane scheduled to departure hasn't been used for that day before, so it won't affect
pitjoin$pre_actl_ARR_postCRSDEP[is.na(pitjoin$pre_actl_ARR_postCRSDEP)] = 0
pitjoin$pre_actl_extraFactor[is.na(pitjoin$pre_actl_extraFactor)] = 0
pitjoin$pre_actl_extraFactortaxi[is.na(pitjoin$pre_actl_extraFactortaxi)] = 0
pitjoin$pre_actl_extraFactor = as.numeric(pitjoin$pre_actl_extraFactor)
pitjoin$pre_actl_extraFactortaxi = as.numeric(pitjoin$pre_actl_extraFactortaxi)

pitjoin = select(pitjoin, -prev_CARRIER, -prev_CRS_ARR_TIME, -prev_actl_ARR_TIME, -pre_actl_ARR_DELAY,
                 -prev_actl_ARR_DEL15, -pre_FL_DATE, -pre_TAIL_NUM, -pre_TAXI_IN, -pre_actl_extratime,
                 -pre_actl_extraaftTaxi, 
                 -SameHourFlights, -SameDayPartFlights, -SameHourarrFlights,
                 -SameDayPartArrFlights, -YEAR, -TAIL_NUM, -FL_NUM, -ORIGIN, -ORIGIN_STATE_ABR,
                 -DEST_STATE_ABR, -CRS_DEP_TIME, -CRS_ARR_TIME, -FLIGHTS.x.x, -DISTANCE_GROUP,
                 -FL_DATE_ARR, -DEP_Time, -ARR_Time, -ARV_Snowdepth)

apply(pitjoin,2,function(x){sum(is.na(x))})
any(is.na(pitjoin))

tr15DEPNonM = pitjoin

################################# add previous plane info 2016 #################################
te16DEPNonM %<>% subset(DEP_DEL15 %in% 0:1)
sum(colnames(te16DEPNonM) != colnames(guess16NonM))
pitdep = rbind(te16DEPNonM, guess16NonM)
pitdep$FL_DATE = as.character(pitdep$FL_DATE)
pitarr = dtf16_visible[dtf16_visible$DEST=="PIT", 
               -which(names(dtf16_visible) %in% c("YEAR","QUARTER","DAY_OF_WEEK",
                                          "AIRLINE_ID", "ORIGIN_AIRPORT_ID","ORIGIN_AIRPORT_SEQ_ID",
                                          "ORIGIN_CITY_MARKET_ID","ORIGIN_CITY_NAME", "ORIGIN_STATE_FIPS",
                                          "ORIGIN_STATE_ABR", "ORIGIN_STATE_NM","ORIGIN_WAC",
                                          "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID","DEST_CITY_MARKET_ID",
                                          "DEST_CITY_NAME","DEST_STATE_FIPS","DEST_STATE_NM",
                                          "DEST_STATE_ABR","DEST_WAC", "DEP_DELAY","DEP_DELAY_NEW",
                                          "DEP_DELAY_GROUP", "DEP_TIME_BLK","TAXI_OUT", "WHEELS_OFF", 
                                          "WHEELS_ON","ARR_DELAY_GROUP",
                                          "ARR_TIME_BLK","CANCELLED","CANCELLATION_CODE","DIVERTED",
                                          "AIR_TIME", "CARRIER_DELAY","WEATHER_DELAY","NAS_DELAY",
                                          "SECURITY_DELAY","FLIGHTS",
                                          "DISTANCE_GROUP","FIRST_DEP_TIME", "TOTAL_ADD_GTIME",
                                          "LONGEST_ADD_GTIME", "CRS_DEP_TIME","LATE_AIRCRAFT_DELAY",
                                          "CRS_ELAPSED_TIME", "DISTANCE", "MONTH", "DAY_OF_MONTH",
                                          "UNIQUE_CARRIER", "DEP_TIME", "DEP_DEL15", "DEST", "FL_NUM",
                                          "ACTUAL_ELAPSED_TIME", "ORIGIN", "ARR_DELAY_NEW"))]
pitjoin = sqldf("
                SELECT d.*, a.CARRIER AS prev_CARRIER, a.CRS_ARR_TIME AS prev_CRS_ARR_TIME, 
                a.ARR_TIME AS prev_actl_ARR_TIME, a.ARR_DELAY AS pre_actl_ARR_DELAY,
                a.ARR_DEL15 AS prev_actl_ARR_DEL15, a.FL_DATE AS pre_FL_DATE, 
                a.TAIL_NUM AS pre_TAIL_NUM, a.TAXI_IN AS pre_TAXI_IN
                FROM pitdep AS d 
                LEFT JOIN pitarr AS a ON 
                d.FL_DATE=a.FL_DATE AND d.TAIL_NUM=a.TAIL_NUM AND prev_CRS_ARR_TIME<d.CRS_DEP_TIME
                GROUP BY d.FL_DATE, d.TAIL_NUM, d.CRS_DEP_TIME
                HAVING MAX(CASE WHEN prev_CRS_ARR_TIME IS NULL THEN 0.99 ELSE prev_CRS_ARR_TIME END);")

# quick check by making sure first several columns are not duplicated. sum to 0, so good; Quick check
# to make sure all carriers match, and nonmatch sum to 0, so good.
sum(duplicated(select(pitjoin, YEAR:DEP_Day)))
sum(pitjoin[!is.na(pitjoin$prev_CARRIER),]$CARRIER != pitjoin[!is.na(pitjoin$prev_CARRIER),]$prev_CARRIER)

# good. all flights that arrived late than planed dep time caused delay for PIT departure.
pitjoin$pre_actl_ARR_postCRSDEP = ifelse(pitjoin$prev_actl_ARR_TIME>as.numeric(pitjoin$CRS_DEP_TIME),1,0)
table(pitjoin[pitjoin$pre_actl_ARR_postCRSDEP==1, 'DEP_DEL15'])

pitjoin$pre_actl_extratime = difftime(strptime(pitjoin$CRS_DEP_TIME,"%H%M"),
                                      strptime(formatC(pitjoin$prev_actl_ARR_TIME,width=4, flag="0"), "%H%M"), units="mins")
pitjoin$pre_actl_extraaftTaxi = pitjoin$pre_actl_extratime + pitjoin$pre_TAXI_IN

# among delayed planes, 75% of them only have 40 min or less to get ready for next departure. Also read 
# an article online, a plane needs about 34 min to get ready from one to another. Use 30 to differeciate 
# time. The reason have to factorize: we cannot assign a reasonable number to NA's, but you know there 
# will have enough time for planes to get ready.
pitjoin$pre_actl_extraFactor = cut(as.numeric(pitjoin$pre_actl_extratime), labels = c(10:0),
                                   breaks = c(-10000,5,seq(10,90,by=10),10000))

pitjoin$pre_actl_extraFactortaxi = cut(as.numeric(pitjoin$pre_actl_extraaftTaxi), labels = c(10:0),
                                       breaks = c(-10000,5,seq(10,90,by=10),10000))

# how about too many flights planned to leave at the same time cause delay on flight system?
flightPerHr =  aggregate(FLIGHTS~FL_DATE+DEP_Hour, data=pitjoin, FUN=sum)
# aggregate(pitjoin$FLIGHTS, by=list(FL_DATE=pitjoin$FL_DATE, DEP_Hour=pitjoin$DEP_Hour), FUN=sum)
pitjoin = merge(x = pitjoin, y = flightPerHr, all.x = TRUE, by = c("FL_DATE", "DEP_Hour"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS.y"] = "SameHourFlights"

flightPerdaypart =  aggregate(FLIGHTS.x~FL_DATE+DEP_Day, data=pitjoin, FUN=sum)
# aggregate(pitjoin$FLIGHTS, by=list(FL_DATE=pitjoin$FL_DATE, DEP_Hour=pitjoin$DEP_Hour), FUN=sum)
pitjoin = merge(x = pitjoin, y = flightPerdaypart, all.x = TRUE, by = c("FL_DATE", "DEP_Day"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS.x.y"] = "SameDayPartFlights"

# what about the same hour/day/10min actual arrival number of planes?
dtf16forArrAgre = dtf16_visible[dtf16_visible$DEST=="PIT",]
dtf16forArrAgre$ARR_TIME = formatC(dtf16forArrAgre$ARR_TIME, width = 4, flag="0")
dtf16forArrAgre$ARR_TIME_Hour = substr(dtf16forArrAgre$ARR_TIME,1,2)
dtf16forArrAgre$ARR_TIME_Hour = ifelse(dtf16forArrAgre$ARR_TIME_Hour=="  ", "NA",dtf16forArrAgre$ARR_TIME_Hour)
dtf16forArrAgre$ARR_TIME_Hour = ifelse(dtf16forArrAgre$ARR_TIME_Hour=="24", "00",dtf16forArrAgre$ARR_TIME_Hour)
dtf16forArrAgre$ARR_TIME_Hour = as.numeric(dtf16forArrAgre$ARR_TIME_Hour)
dtf16forArrAgre$ARR_TIME_Daypart = ifelse(dtf16forArrAgre$ARR_TIME_Hour>=6&dtf16forArrAgre$ARR_TIME_Hour<12, "Morning", 
                                    ifelse(dtf16forArrAgre$ARR_TIME_Hour>=12&dtf16forArrAgre$ARR_TIME_Hour<18, "Afternoon",
                                      ifelse(dtf16forArrAgre$ARR_TIME_Hour>=18&dtf16forArrAgre$ARR_TIME_Hour<24, "Evening", "Midnight")))
arrflightPerHr = aggregate(FLIGHTS~FL_DATE+ARR_TIME_Hour, data=dtf16forArrAgre, FUN=sum)
arrflightPerHr$ARR_TIME_Hour = as.numeric(arrflightPerHr$ARR_TIME_Hour)

pitjoin = merge(x = pitjoin, y = arrflightPerHr, all.x = TRUE, by.x = c("FL_DATE", "DEP_Hour"),
                by.y = c("FL_DATE", "ARR_TIME_Hour"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS"] = "SameHourarrFlights"

arrflightDaypart = aggregate(FLIGHTS~FL_DATE+ARR_TIME_Daypart, data=dtf16forArrAgre, FUN=sum)
pitjoin = merge(x = pitjoin, y = arrflightDaypart, all.x = TRUE, by.x = c("FL_DATE", "DEP_Day"),
                by.y = c("FL_DATE", "ARR_TIME_Daypart"))
colnames(pitjoin)[colnames(pitjoin)=="FLIGHTS"] = "SameDayPartArrFlights"

# if arr plane number is NA, mean 0
pitjoin[,(ncol(pitjoin)-1):ncol(pitjoin)][is.na(pitjoin[,(ncol(pitjoin)-1):ncol(pitjoin)])] = 0

pitjoin$SameHourFlightsTOTAL = pitjoin$SameHourFlights + pitjoin$SameHourarrFlights
pitjoin$SameDayPartFlightsTOTAL = pitjoin$SameDayPartFlights + pitjoin$SameDayPartArrFlights

ggplot(pitjoin[pitjoin$DEP_DEL15!="DEP_DEL",], aes(x = SameHourFlightsTOTAL)) + 
  geom_histogram(binwidth = 1, aes(fill = as.factor(DEP_DEL15)), position = "identity", alpha = 0.65) +
  theme_bw() + scale_fill_discrete(name = "Observed Y")
ggplot(pitjoin[pitjoin$DEP_DEL15!="DEP_DEL",], aes(x = SameHourFlightsTOTAL)) + 
  geom_bar(aes(fill = as.factor(DEP_DEL15)), position = "fill") +
  theme_bw() + scale_fill_discrete(name = "Observed Y")
hist(pitjoin$SameHourFlightsTOTAL)

# if na, means the plane scheduled to departure hasn't been used for that day before, so it won't affect
pitjoin$pre_actl_ARR_postCRSDEP[is.na(pitjoin$pre_actl_ARR_postCRSDEP)] = 0
pitjoin$pre_actl_extraFactor[is.na(pitjoin$pre_actl_extraFactor)] = 0
pitjoin$pre_actl_extraFactortaxi[is.na(pitjoin$pre_actl_extraFactortaxi)] = 0
pitjoin$pre_actl_extraFactor = as.numeric(pitjoin$pre_actl_extraFactor)
pitjoin$pre_actl_extraFactortaxi = as.numeric(pitjoin$pre_actl_extraFactortaxi)

pitjoin = select(pitjoin, -prev_CARRIER, -prev_CRS_ARR_TIME, -prev_actl_ARR_TIME, -pre_actl_ARR_DELAY,
                 -prev_actl_ARR_DEL15, -pre_FL_DATE, -pre_TAIL_NUM, -pre_TAXI_IN, -pre_actl_extratime,
                 -pre_actl_extraaftTaxi, 
                 -SameHourFlights, -SameDayPartFlights, -SameHourarrFlights,
                 -SameDayPartArrFlights, -YEAR, -TAIL_NUM, -FL_NUM, -ORIGIN, -ORIGIN_STATE_ABR,
                 -DEST_STATE_ABR, -CRS_DEP_TIME, -CRS_ARR_TIME, -FLIGHTS.x.x, -DISTANCE_GROUP,
                 -FL_DATE_ARR, -DEP_Time, -ARR_Time, -ARV_Snowdepth)

apply(pitjoin,2,function(x){sum(is.na(x))})
any(is.na(pitjoin))

te16DEPNonM = pitjoin[pitjoin$DEP_DEL15!="DEP_DEL",]
guess16NonM = pitjoin[pitjoin$DEP_DEL15=="DEP_DEL",]

###################################### add holiday info ########################################
url = "http://www.officeholidays.com/countries/usa/2015.php"
holi15 = read_html(url)
list15 = html_nodes(holi15, "table")[1]
table15 = html_table(list15, fill = TRUE)[[1]]
table15$Date = gsub("\n.+", "", table15$Date)
table15$Date = paste("2015",table15$Date, sep = " ")
table15$Date = as.Date(table15$Date, "%Y %B %d")
table15 =  table15[, -which(names(table15) %in% c("Year", "Comments"))]
table15$Date = as.character(table15$Date)

tr15DEPNonM = sqldf("select a.*, t.Holiday
                    from tr15DEPNonM as a
                    left join table15 as t on a.FL_DATE = t.Date;")

url = "http://www.officeholidays.com/countries/usa/2016.php"
holi16 = read_html(url)
list16 = html_nodes(holi16, "table")[1]
table16 = html_table(list16, fill = TRUE)[[1]]
table16$Date = gsub("\n.+", "", table16$Date)
table16$Date = paste("2016", table16$Date, sep = " ")
table16$Date = as.Date(table16$Date, "%Y %B %d")
table16 =  table16[, -which(names(table16) %in% c("Year", "Comments"))]
table16$Date = as.character(table16$Date)

te16DEPNonM = sqldf("select a.*, t.Holiday
                    from te16DEPNonM as a
                    left join table16 as t on a.FL_DATE = t.Date;")

guess16NonM = sqldf("select a.*, t.Holiday
                    from guess16NonM as a
                    left join table16 as t on a.FL_DATE = t.Date;")

# Replace NA in Holiday with NOTHOLIDAY
tr15DEPNonM$Holiday[is.na(tr15DEPNonM$Holiday)] = "NOTHOLIDAY"
te16DEPNonM$Holiday[is.na(te16DEPNonM$Holiday)] = "NOTHOLIDAY"
guess16NonM$Holiday[is.na(guess16NonM$Holiday)] = "NOTHOLIDAY"

# Create correct levels for Holiday
tr15DEPNonM$Holiday %<>% as.factor()
te16DEPNonM$Holiday %<>% as.factor()
guess16NonM$Holiday %<>% as.factor()
levels(tr15DEPNonM$Holiday) %<>% tocamel
levels(te16DEPNonM$Holiday) %<>% tocamel
levels(guess16NonM$Holiday) %<>% tocamel

trHoliday_levels = levels(tr15DEPNonM$Holiday)
teHoliday_levels = levels(te16DEPNonM$Holiday)
guessHoliday_levels = levels(guess16NonM$Holiday)
Holiday_levels = unique(c(trHoliday_levels, teHoliday_levels, guessHoliday_levels))

tr15DEPNonM$Holiday %<>% factor(levels = Holiday_levels)
te16DEPNonM$Holiday %<>% factor(levels = Holiday_levels)
guess16NonM$Holiday %<>% factor(levels = Holiday_levels)

###############################################################################
# Remove unneccesary columns
tr15DEPNonM %<>% select(-FL_DATE)
te16DEPNonM %<>% select(-FL_DATE)
guess16NonM %<>% select(-FL_DATE)

tr15DEPNonM$CARRIER %<>% as.factor()
te16DEPNonM$CARRIER %<>% as.factor()
guess16NonM$CARRIER %<>% as.factor()
trCarrier_levels = levels(tr15DEPNonM$CARRIER)
teCarrier_levels = levels(te16DEPNonM$CARRIER)
guessCarrier_levels = levels(guess16NonM$CARRIER)
Carrier_levels = unique(c(trCarrier_levels, teCarrier_levels, guessCarrier_levels))

tr15DEPNonM$CARRIER = factor(tr15DEPNonM$CARRIER, levels = Carrier_levels)
te16DEPNonM$CARRIER = factor(te16DEPNonM$CARRIER, levels = Carrier_levels)
guess16NonM$CARRIER %<>% factor(levels = Carrier_levels)

# Check for missing data
any(is.na(tr15DEPNonM))
any(is.na(te16DEPNonM))
any(is.na(guess16NonM))

# Calculate Base Rate
trBaseRate = prop.table(table(tr15DEPNonM$DEP_DEL15))[2]
teBaseRate = prop.table(table(te16DEPNonM$DEP_DEL15))[2]

### Create y and X matrices
y.train = as.factor(tr15DEPNonM$DEP_DEL15)
X.train = model.matrix(DEP_DEL15 ~ ., data = tr15DEPNonM)[,-1]
colnames(X.train) %<>% make.names(unique = TRUE)

y.test = as.factor(te16DEPNonM$DEP_DEL15)
X.test = model.matrix(DEP_DEL15 ~ ., data = te16DEPNonM)[,-1]
colnames(X.test) %<>% make.names(unique = TRUE)

# Check dimensions
length(y.train)
length(y.test)
dim(X.train)
dim(X.test)
