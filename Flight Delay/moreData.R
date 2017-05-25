#### Add Holiday data and convert dependent variables#### 
library(dplyr)
library(magrittr)
library(Holidays)
library(rapportools)

# Load datasets
load("weatherData.RData")

library(rvest)
library(sqldf)
url = "http://www.officeholidays.com/countries/usa/2015.php"
holi15 = read_html(url)
list15 = html_nodes(holi15, "table")[1]
table15 = html_table(list15, fill = TRUE)[[1]]
table15$Date = gsub("\n.+", "", table15$Date)
table15$Date = paste("2015",table15$Date, sep = " ")
table15$Date = as.Date(table15$Date, "%Y %B %d")
table15 =  table15[, -which(names(table15) %in% c("Year", "Comments"))]

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

te16DEPNonM = sqldf("select a.*, t.Holiday
            from te16DEPNonM as a
                    left join table16 as t on a.FL_DATE = t.Date;")

guess16NonM = sqldf("select a.*, t.Holiday
            from guess16NonM as a
                    left join table16 as t on a.FL_DATE = t.Date;")

# Replace NA in Holiday with NOTHOLIDAY
tr15DEPNonM$Holiday[is.na(tr15DEPNonM$Holiday)] = "NOTHOLIDAY"
te16DEPNonM$Holiday[is.na(te16DEPNonM$Holiday)] = "NOTHOLIDAY"

# Create correct levels for Holiday
tr15DEPNonM$Holiday %<>% as.factor()
te16DEPNonM$Holiday %<>% as.factor()
levels(tr15DEPNonM$Holiday) %<>% tocamel
levels(te16DEPNonM$Holiday) %<>% tocamel

trHoliday_levels = levels(tr15DEPNonM$Holiday)
teHoliday_levels = levels(te16DEPNonM$Holiday)
Holiday_levels = unique(c(trHoliday_levels, teHoliday_levels))

tr15DEPNonM$Holiday %<>% factor(levels = Holiday_levels)
te16DEPNonM$Holiday %<>% factor(levels = Holiday_levels)

###############################################################################
# Create Holiday columns
# tr15holidays = lapply(allHolidays(), function(x) factor(as.numeric(isHoliday(tr15DEPNonM$FL_DATE, type = x)),
#                                                         levels = c(0, 1)))
# tr15holidaysDF = data.frame(tr15holidays)
# names(tr15holidaysDF) = paste0("HOL_", allHolidays())
# 
# te16holidays = lapply(allHolidays(), function(x) factor(as.numeric(isHoliday(te16DEPNonM$FL_DATE, type = x)),
#                                                         levels = c(0, 1)))
# te16holidaysDF = data.frame(te16holidays)
# names(te16holidaysDF) = paste0("HOL_", allHolidays())
# 
# guess16holidays = lapply(allHolidays(), function(x) factor(as.numeric(isHoliday(guess16NonM$FL_DATE, type = x)),
#                                                         levels = c(0, 1)))
# guess16holidaysDF = data.frame(guess16holidays)
# names(guess16holidaysDF) = paste0("HOL_", allHolidays())
# 
# tr15DEPNonM %<>% cbind(tr15holidaysDF)
# te16DEPNonM %<>% cbind(te16holidaysDF)
# guess16NonM %<>% cbind(guess16holidaysDF)

### Merge test and guessed data ###

# Remove unneccesary columns
tr15DEPNonM %<>% select(-c(YEAR, FL_DATE, FL_NUM, ORIGIN, ORIGIN_STATE_ABR, TAIL_NUM,
                           CRS_ARR_TIME, CRS_DEP_TIME, DEST_STATE_ABR, DISTANCE_GROUP, 
                           FLIGHTS, DEP_Time, ARR_Time,
                           ARV_Snowdepth, FL_DATE_ARR))

te16DEPNonM %<>% select(-c(YEAR, FL_DATE, FL_NUM, ORIGIN, ORIGIN_STATE_ABR, TAIL_NUM,
                           CRS_ARR_TIME, CRS_DEP_TIME, DEST_STATE_ABR, DISTANCE_GROUP, 
                           FLIGHTS, DEP_Time, ARR_Time,
                           ARV_Snowdepth, FL_DATE_ARR))

# Convert variables
# trTAIL_levels = levels(tr15DEPNonM$TAIL_NUM)
# teTAIL_levels = levels(te16DEPNonM$TAIL_NUM)
# TAIL_NUM_levels = unique(c(trTAIL_levels, teTAIL_levels))
# 
# tr15DEPNonM$TAIL_NUM %<>% factor(levels = TAIL_NUM_levels)
# te16DEPNonM$TAIL_NUM %<>% factor(levels = TAIL_NUM_levels)

# Subset non-missing y data
tr15DEPNonM %<>% subset(DEP_DEL15 %in% 0:1)
te16DEPNonM %<>% subset(DEP_DEL15 %in% 0:1)

# Check for missing data
sapply(tr15DEPNonM, function(x) sum(is.na(x)))
sapply(te16DEPNonM, function(x) sum(is.na(x)))

# Calculate Base Rate
trBaseRate = prop.table(table(tr15DEPNonM$DEP_DEL15))[2]
teBaseRate = prop.table(table(te16DEPNonM$DEP_DEL15))[2]

# Convert FL_DATE to factor
# tr15DEP$FL_DATE %<>% as.Date(format = "%Y%m%d")
# tr15DEP$CRS_DEP_TIME %<>% {as.POSIXct(paste(tr15DEP$FL_DATE, .), format = "%Y-%m-%d %H%M")}
# tr15DEP$CRS_ARR_TIME %<>% {as.POSIXct(paste(tr15DEP$FL_DATE, .), format = "%Y-%m-%d %H%M")}

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



