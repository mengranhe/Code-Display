library(Hmisc)
library(foreign)
library(ggplot2)
library(dplyr)

rawdata <- spss.get("total123isurveyby datewithsyntax-01-12--2017.sav")
rawdata$date <- as.Date(rawdata$date/86400, origin = "1582-10-14")

#120 couples did the survey
length(unique(rawdata$coupleid)) 

#create new dataset by removing all irrelevant variables
name <- c("coupleid", "date", "time", "d1sex", "p1sex", "d1racewhbl", "p1racewhbl", "isdhandler", "isphandler", "isdcc1r", 
          "ispcc1r","isdcc2", "ispcc2", "isdemotional", "ispemotional", "isdinstr", "ispinstr", "isdminimize", "ispminimize", "isdavoid", "ispavoid", "isdunsupp", 
          "ispunsupp", "isdhappy", "isphappy", "isddepr", "ispdepr", "isdanger", "ispanger", "isdanx", "ispanx")

rawdata2 <- rawdata[match(name, names(rawdata))]
rawdata2 <- rawdata2[complete.cases(rawdata2),]


#after selecting important variables:
survey <- rawdata[match(name, names(rawdata))]
bad <- survey[-which(complete.cases(survey)),]

handler <- function(x){
  if (is.na(x)){
    value = NA
  }
  else if(x == "noncommunal"){
    value = 0
  }
  else if(x == "communal"){
    value = 1
  }
  return (value)
}
survey$isdhandler <- sapply(survey$isdhandler, handler) 
survey$isphandler <- sapply(survey$isphandler, handler)
survey$isdcc1r <- sapply(survey$isdcc1r, handler) 
survey$ispcc1r <- sapply(survey$ispcc1r, handler) 

#additional indicator variables 
survey$view_of_diabetes <- as.numeric(survey$isdcc1r) + as.numeric(survey$ispcc1r)
survey$cchandler <- survey$isdhandler + survey$isphandler

#remove all missing response, 1343 entries left.
survey <- survey[complete.cases(survey),]


##############
#    EDA     #
##############
timerecorded <- lapply(split(survey$coupleid, survey$coupleid), function(x){length(x)})
#there are 44 out of 120 couples completing the daily diaries for 14 consecutive days,
#40 couples completing the daily diaries for 12 consecutive days. The shortest is 8 days,
#the longest is 16 days. (timerecorded + 1 because time starts from 0)
hist(as.numeric(timerecorded), main = "Frequency of Completing the Survey", 
     xlab = "Number of consecutive days", 
     ylim = c(0, 50), label = TRUE, col = "grey",
     breaks = seq(0,16,1))
# min(timerecorded[,2])
# max(timerecorded[,2])

#measure gender of couples by adding new indicator variable couple_gender_indicator, record any 
#missing value as NA
couple_gender <- function(x){
  if(is.na(x[135]) || is.na(x[136])){
    value = NA
  }
  else if(x[135] == "male" && x[136] == "female"){
    value = "Male Patient\nFemale Partner"
  }
  else if (x[135] == "female" && x[136] == "male"){
    value = "Female Patient\nMale Partner"
  }
  else if (x[135] == "male" && x[136] == "male"){
    value = "Male Patient\nMale Partner"
  }
  else if (x[135] == "female" && x[136] == "female"){
    value = "Female Patient\nFemale Partner"
  }
  return(value)
}
rawdata$couple_gender_indicator <- apply(rawdata, 1, couple_gender)

#create a barplot describing the distribution of couples' gender (first letter represents
#participant, second letter represents partner)
with(unique(rawdata[, c("coupleid", "couple_gender_indicator")]), 
     barchart(couple_gender_indicator, xlab = "Frequency", 
              ylab = "Gender of couple", main = "Distribution of Couples' Gender", 
              xlim = c(0, 70), col = "#A6192e")) 

#measure couples' race
couple_race <- function(x){
  if(is.na(x[137]) || is.na(x[138])){
    value = NA
  }
  else if (x[137] == "white" && x[138] == "white"){
    value = "Patient White\nPartner White"
  }
  else if (x[137] == "white" && x[138] == "black"){
    value = "Patient White\nPartner Black"
  }
  else if (x[137] == "black" && x[138] == "white"){
    value = "Patient Black\nPartner White"
  }
  else if (x[137] == "black" && x[138] == "black"){
    value = "Patient Black\nPartner Black"
  }
  return(value)
} 
rawdata$couple_race_indicator <- apply(rawdata, 1, couple_race)
#create a barplot describing the distribution of couples' gender (first letter represents
#participant, second letter represents partner)
with(unique(rawdata[, c("coupleid", "couple_race_indicator")]), 
     barchart(couple_race_indicator, xlab = "Count in Couples' Race", 
              ylab = "Couples' Race", main = "Distribution of Couples' Race", 
              xlim = c(0, 80), col = "#a6192e"))

####################################################################################################

### Plotting functions for distributions ###

# Create a scatterplot for relationship of two variables overall
# Variables should be vector length 1
# df is the data
# facets can be NULL or vector length 1 or 2
bar_hist_plots <- function(var, df, facets = NULL) {
  if (length(var) != 1) {
    stop("Wrong number of variables")
  }
  var1 <- var[1]
  subtitle_name <- NULL
  if (!is.null(facets)) {
    var3 <- facets[1]
    if (length(facets) == 2) {
      var4 <- facets[2]
    }
    if (length(facets) > 2) {
      stop("Too many facet variables")
    }
    subtitle_name <- paste("Facetted on ", paste(facets, collapse = " and "))
  }
  
  title_name <- paste0(var1, " in Overall Data")
  g <- ggplot(data = df) + 
    labs(x = var1, 
         title = title_name,
         subtitle = paste0("N = ", nrow(df))) +
    theme_bw() 
  
  if(class(df[, var1]) == "factor") {
    g <- g + geom_bar(aes_string(x = var1),
                      color = "black")
  } else if (class(df[, var1]) == "numeric") { 
    g <- g + geom_histogram(aes_string(x = var1),
                            binwidth = 1/6,
                            color = "black")
  }
  
  if (!is.null(facets)) {
    g <- g + facet_wrap(facets, labeller = label_both) + 
      labs(subtitle = paste0(subtitle_name, "; N = ", nrow(df)))
  }
  
  g <- g + theme(title = element_text(size = 16),
                 axis.text = element_text(size = 12))
  g
  return(g)
}

# Create a scatterplot for relationship of two variables overall
# Variables should be vector length 2
# df is the data
# facets can be NULL or vector length 1 or 2
scatterplots <- function(variables, df, facets = NULL){
  if (length(variables) != 2) {
    stop("Wrong number of variables")
  }
  var1 <- variables[1]
  var2 <- variables[2]
  subtitle_name <- NULL
  if (!is.null(facets)) {
    var3 <- facets[1]
    if (length(facets) == 2) {
      var4 <- facets[2]
    }
    if (length(facets) > 2) {
      stop("Too many facet variables")
    }
    subtitle_name <- paste("Facetted on", paste(facets, collapse = " and "))
  }
  
  title_name <- paste0(var1, " vs. ", var2, " in Overall Data")
  if (length(facets) == 1) {
    group_df <- group_by_(df, 
                          var1, var2, var3)
  } else if (length(facets) == 2){
    group_df <- group_by_(df, 
                          var1, var2, var3, var4)
  } else {
    group_df <- group_by_(df, 
                          var1, var2)
  }
  sum_df <- dplyr::summarize(group_df, count = n())
  g <- ggplot(data = sum_df) + 
    geom_text(aes_string(x = var1, y = var2, label = "count", 
                         size = "count")) +
    labs(x = var1, 
         y = var2, 
         title = title_name,
         size = "Number of\nResponses\nat Point", 
         subtitle = paste0("N = ", nrow(df))) +
    theme_bw() + 
    scale_size_continuous(range = c(3,8)) 
  
  if(class(df[, var1]) == "factor") {
    g <- g + scale_x_discrete(limits = c(levels(df[, var1])))
  } else if (class(df[, var1]) == "numeric") { 
    g <- g + scale_x_continuous(limits = c(1, 5))
  }
  
  if(class(df[, var2]) == "factor") {
    g <- g + scale_y_discrete(limits = c(levels(df[, var2])))
  } else if (class(df[, var2]) == "numeric") { 
    g <- g + scale_y_continuous(limits = c(1, 5))
  }
  
  if (!is.null(facets)) {
    g <- g + facet_wrap(facets, labeller = label_both) + 
      labs(subtitle = paste0(subtitle_name, "; N = ", nrow(df)))
  }
  
  g <- g + theme(title = element_text(size = 16),
                 axis.text = element_text(size = 12))
  g
  return(g)
}

# Create a plot for a variable for a couple across time
# Numeric id
# variable is a character string of variable name
# df is the data
univar_plots_over_time <- function(id, variable, df){
  couple_df <- df[which(id == df$coupleid), ]
  sub_name <- paste0("For Couple ID ", id)
  title_name <- paste0(variable, " over Survey Period by Couple")
  g <- ggplot(data = couple_df) + 
    geom_point(aes_string(x = "time", y = variable)) +
    labs(x = "Day Number", y = variable, 
         title = title_name, subtitle = sub_name) +
    scale_x_continuous(limits = c(0, max(14, max(df$time)))) +
    theme_bw()
  if(class(df[, variable]) == "factor") {
    g <- g + scale_y_discrete(limits = c(levels(couple_df[, variable])))
  } else if (class(df[, variable]) == "numeric") { 
    g <- g + scale_y_continuous(limits = c(1, 5))
  }
  
  g <- g + theme(title = element_text(size = 16),
                 axis.text = element_text(size = 12))
  g
  return(g)
}

# Create a plot of relationship of two variables for a couple
# Numeric id
# Variables should be a two-length vector
# df is the data
bivar_coup_plots <- function(id, variables, df){
  var1 <- variables[1]
  var2 <- variables[2]
  couple_df <- df[which(id == df$coupleid), ]
  sub_name <- paste0("For Couple ID ", id, 
                     "; Days Responded = ", nrow(couple_df))
  title_name <- paste0(var1, " vs. ", var2, " over Survey Period by Couple")
  group_df <- group_by_(couple_df, 
                        var1, var2)
  sum_df <- dplyr::summarize(group_df, count = n())
  g <- ggplot(data = sum_df) + 
    geom_text(aes_string(x = var1, y = var2, label = "count", 
                         size = "count")) +
    labs(x = var1, 
         y = var2, 
         title = title_name, subtitle = sub_name,
         size = "Number of\nResponses\nat Point") +
    theme_bw() + 
    scale_size_continuous(range = c(3,8))
  
  if(class(df[, var1]) == "factor") {
    g <- g + scale_x_discrete(limits = c(levels(df[, var1])))
  } else if (class(df[, var1]) == "numeric") { 
    g <- g + scale_x_continuous(limits = c(1, 5))
  }
  
  if(class(df[, var2]) == "factor") {
    g <- g + scale_y_discrete(limits = c(levels(df[, var2])))
  } else if (class(df[, var2]) == "numeric") { 
    g <- g + scale_y_continuous(limits = c(1, 5))
  }
  
  g <- g + theme(title = element_text(size = 16),
                 axis.text = element_text(size = 12))
  g
  return(g)
}


bar_hist_plots(c("isdhappy"), rawdata2)
bar_hist_plots(c("isddepr"), rawdata2)
bar_hist_plots(c("isdanger"), rawdata2)
bar_hist_plots(c("isdanger"), rawdata2, c("isdcc1r"))
bar_hist_plots(c("isdmeds"), rawdata2)
bar_hist_plots(c("isdcc1r"), rawdata2, c("p1sex"))


plot_names <- name[-c(1:3)]
pdf("onevar_EDA.pdf")
for(ii in plot_names) {
  jj <- bar_hist_plots(c(ii), rawdata2)
  plot(jj)
}
dev.off()

scatterplots(c("isdanger", "ispunsupp"), rawdata2)
scatterplots(c("isdanger", "isdhappy"), rawdata2)
scatterplots(c("isdhandler", "isdcc1r"), rawdata2)
scatterplots(c("isdunsupp", "ispanger"), rawdata2)
scatterplots(c("isdmeds", "ispmeds"), rawdata2)
scatterplots(c("isddiet", "ispdiet"), rawdata2)
scatterplots(c("isdtest", "isptest"), rawdata2)
scatterplots(c("isdunsupp", "isdcc1r"), rawdata2)
scatterplots(c("isdcc1r", "ispcc1r"), rawdata2, c("p1sex"))
scatterplots(c("isdcc1r", "ispcc1r"), rawdata2, c("p1sex", "d1sex"))
scatterplots(c("isdhappy", "isphappy"), rawdata2, c("p1sex"))
scatterplots(c("isdcc1r", "ispcc1r"), rawdata2, c("p1sex"))



univar_plots_over_time(67, "isdanger", rawdata2)
univar_plots_over_time(103, "isdcc1r", rawdata2)

bivar_coup_plots(45, c("isdanger", "ispanger"), rawdata2)
bivar_coup_plots(40, c("isdunsupp", "isdcc1r"), rawdata2)
bivar_coup_plots(32, c("isdmeds", "ispmeds"), rawdata2)

##################### patterns of missingness (data entries) ########################
library(VIM)
aggr_plot <- aggr(survey, col=c('navyblue','red'), numbers= TRUE, sortVars=TRUE, labels=names(survey), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

par(mfrow = c(1,2))
marginplot(survey[c("isdhappy", "isphappy")])
marginplot(data.frame(isdhappy = jitter(survey$isdhappy), isphappy = jitter(survey$isphappy)))
#If our assumption of MCAR data is correct, then we expect 
#the red and blue box plots to be very similar.
marginplot(survey[c("isdanger", "ispanger")])
marginplot(data.frame(isdanger = jitter(survey$isdanger), ispanger = jitter(survey$ispanger)))

vars <-  c("isdhappy", "isphappy","isdanger", "ispanger")
marginmatrix(survey[,vars], alpha=0.6)

################ idk stuff #####################
rawdata2[which(rawdata2$isddepr >= 4), ]$coupleid
length(unique(rawdata2[which(rawdata2$isddepr >= 4), ]$coupleid))
