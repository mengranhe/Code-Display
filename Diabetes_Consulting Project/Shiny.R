# library(ggplot2)
# library(Hmisc)
# library(foreign)
# ###Some preparation for shiny web 
# setwd("~/S17/36726/Diabeetus")
# rawdata <- read.spss("data.sav", to.data.frame = TRUE)
# rawdata$date <- as.Date(rawdata$date/86400, origin = "1582-10-14")
# 
# # Consistent missingness
# name <- c("coupleid", "date", "time", "d1sex", "p1sex", "d1racewhbl", "p1racewhbl", "isdhandler", "isphandler", "isdcc1r", 
#           "ispcc1r","isdcc2", "ispcc2", "isdemotional",
#           "ispemotional", "isdinstr", "ispinstr", "isdminimize", "ispminimize", "isdavoid", "ispavoid", "isdunsupp", 
#           "ispunsupp", "isdhappy", "isphappy", "isddepr", "ispdepr", "isdanger", "ispanger", "isdanx", "ispanx")
# rawdata3 <- rawdata[match(name, names(rawdata))]
# data3 <- rawdata3[complete.cases(rawdata3),]
# 
# #quantify isdcc2, ispcc2 into numeric variables
# quant <- function(x){
#   if(x == "none of the time"){
#     value = 1
#   }
#   else if(x == "a little of the time"){
#     value = 2
#   }
#   else if (x == "some of the time"){
#     value = 3
#   }
#   else if  (x == "most of the time"){
#     value = 4
#   }
#   else if (x == "all of the time"){
#     value = 5
#   }
#   return(value)
# }
# data3$isdcc2r <- sapply(data3$isdcc2, quant)
# data3$ispcc2r <- sapply(data3$ispcc2, quant)
# saveRDS(data3, file = "Shiny.Rdata")
# dtf <- readRDS(data3, file = "Shiny.Rdata")
# 
# g <- ggplot(data3[1:13,], aes(x = as.numeric(time))) + labs(title = "Psychological mood of couples", x = "Time(day)", y = "Happiness rating")
# g <- g + geom_point(aes(y = as.numeric(isdhappy), color = "isdhappy")) + geom_line(aes(y = as.numeric(isdhappy), color = "isdhappy"))
# g <- g + geom_point(aes(y = as.numeric(isphappy), color = "isphappy")) + geom_line(aes(y = as.numeric(isphappy), color = "isphappy"))
# g

dtf <- read.csv("https://raw.githubusercontent.com/atzechang/data/master/Diabetes/dtf.csv")
