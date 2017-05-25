library(rvest)
library(dplyr)

hrperday = 50   #allow at most 50 rows for each day
dtstart = as.Date("2015/1/1")
dtend = as.Date("2016/12/31")
yeardate = seq(dtstart, dtend, by="days")
yeardate = gsub('/0','/',format(yeardate, '%Y/%m/%d'))
datelist = rep(seq(dtstart, dtend, by="days"), each = hrperday)
datelist = gsub('/0','/',format(datelist, '%Y/%m/%d'))

pitweather = data.frame(matrix(ncol = 20, nrow = length(yeardate)*hrperday))
colnames(pitweather)[1:16] = c("Time(PIT)", "Temp(F)", "Dew_Point(F)", "Humidity", 
                               "Pressure(in)", "Visibility(mi)", "Wind_Dir", "Wind_Speed(mph)", 
                               "Gust_Speed(mph)", "Precip(in)", "Events", "Conditions",
                               "Hour", "Min", "Date", "DEST")
pitweather$Date = datelist
pitweather$DEST = "PIT"

scrapestate = "PIT"

DELstart = Sys.time()

for (ii in 1:length(yeardate)) {
  #tryCatch({       #this line and the line down there can keep for loop running even with error
  
  scrapedate = yeardate[ii]  
  url = paste0("https://www.wunderground.com/history/airport/K",scrapestate,"/",scrapedate,"/DailyHistory.html")
  #url = "https://www.wunderground.com/history/airport/KPIT/2017/4/18/DailyHistory.html"
  
  doc = read_html(url)
  docLists = html_nodes(doc, 'table')[5]
  weathertable = html_table(docLists)[[1]]
  if (ncol(weathertable) != 13 & ncol(weathertable) != 12) {cat(ii)}
  # make sure no other extra columns were included in the table
  weathertable = select(weathertable, contains("Time ("),Temp., `Dew Point`, Humidity, Pressure, Visibility,
                        `Wind Dir`, `Wind Speed`, `Gust Speed`, Precip, Events, Conditions)
  
  weathertable$Hour = as.integer(format(strptime(weathertable[,1], "%I:%M %p" ), "%H"))
  weathertable$Min = as.integer(format(strptime(weathertable[,1], "%I:%M %p" ), "%M"))
  #weathertable = weathertable[!duplicated(weathertable$'Time (EST)'),]
  pitweatherRow = min(which(pitweather$Date == scrapedate))
  pitweatherEnd = pitweatherRow + nrow(weathertable) - 1
  
  pitweather[pitweatherRow:pitweatherEnd, 1:ncol(weathertable)] = weathertable
  
  #}, error = function(e){cat(ii)})
}

DELend = Sys.time()
DELduration = DELend - DELstart     
DELduration #Used 11.01156 mins

pitweather = pitweather[!is.na(pitweather$`Time(PIT)`),]

write.table(pitweather, "pitweather.csv", sep = "\t")
