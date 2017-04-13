library(homals)
library(PCAmixdata)
library(Hmisc)
library(lme4)
library(stats)
library(arm)
#setwd("~/Dropbox/CMU/36726 Statistical Practice"): wd for Joyce

rawdata <- spss.get("total123isurveyby datewithsyntax-01-12--2017.sav")
rawdata$date <- as.Date(rawdata$date/86400, origin = "1582-10-14")

# Consistent missingness
name <- c("coupleid", "date", "time", "d1sex", "p1sex", "d1racewhbl", "p1racewhbl", "isdhandler", "isphandler", "isdcc1r", 
          "ispcc1r","isdcc2", "ispcc2", "isdemotional",
          "ispemotional", "isdinstr", "ispinstr", "isdminimize", "ispminimize", "isdavoid", "ispavoid", "isdunsupp", 
          "ispunsupp", "isdhappy", "isphappy", "isddepr", "ispdepr", "isdanger", "ispanger", "isdanx", "ispanx")
rawdata3 <- rawdata[match(name, names(rawdata))]


#create pca dataset by removing all irrelevant variables
pca_name <- c("d1sex", "p1sex", "d1racewhbl", "p1racewhbl", "isdhandler", "isphandler", "isdcc1r", 
          "ispcc1r","isdcc2", "ispcc2", "isdemotional",
          "ispemotional", "isdinstr", "ispinstr", "isdminimize", "ispminimize", "isdavoid", "ispavoid", "isdunsupp", 
          "ispunsupp", "isdhappy", "isphappy", "isddepr", "ispdepr", "isdanger", "ispanger", "isdanx", "ispanx")

rawdata_pca <- rawdata[match(pca_name, names(rawdata))]
rawdata_pca <- rawdata_pca[complete.cases(rawdata3),]
rawdatatime <- rawdata3$time[complete.cases(rawdata3)]
try <- prcomp(rawdata_pca[rawdatatime == 0, -(1:10)],center = TRUE, scale. = TRUE)
#rawdata_pca$isdcc2 <- ordered(rawdata_pca$isdcc2)
#rawdata_pca$ispcc2 <- ordered(rawdata_pca$ispcc2)

#quantify isdcc2, ispcc2 into numeric variables
quant <- function(x){
  if(x == "none of the time"){
    value = 1
  }
  else if(x == "a little of the time"){
    value = 2
  }
  else if (x == "some of the time"){
    value = 3
  }
  else if  (x == "most of the time"){
    value = 4
  }
  else if (x == "all of the time"){
    value = 5
  }
  return(value)
}
rawdata_pca$isdcc2 <- as.factor(sapply(rawdata_pca$isdcc2, quant))
rawdata_pca$ispcc2 <- as.factor(sapply(rawdata_pca$ispcc2, quant))

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
#convert variable isdhandler "how do you handle problems" into indicator variable, 0 = noncommunal, 1 = communal
rawdata_pca$isdhandler <- as.factor(sapply(rawdata_pca$isdhandler, handler))
#convert variable isphandler "how do you handle problems" into indicator variable, 0 = noncommunal, 1 = communal
rawdata_pca$isphandler <- as.factor(sapply(rawdata_pca$isphandler, handler))
#convert variable isdcc1r into indicator variable: 0 = noncommunal, 1 = communal. (How did you view diabetes as "our problem")
rawdata_pca$isdcc1r <- as.factor(sapply(rawdata_pca$isdcc1r, handler))
rawdata_pca$ispcc1r <- as.factor(sapply(rawdata_pca$ispcc1r, handler))

############## PCA #################
#http://www.statmethods.net/advstats/factor.html
###### PCA in IVs #######
#https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/
#In factor analysis we model the observed variables as linear functions of the “factors.” 
#In principal components, we create new variables that are linear combinations of the observed variables.

iv_pca <- prcomp(rawdata_pca[, 5:10],center = TRUE, scale. = TRUE) #standardize the variables prior to the application of PCA
print(iv_pca)
plot(iv_pca, type = "l")  #scree plot: 4 PC's are enough to explain most of variability in the data
summary(iv_pca)
biplot(iv_pca, col = c("grey", "red"))  

#Data visualization 1
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(iv_pca, choices = 1:2, groups = as.factor(rawdata_pca$d1racewhbl), ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

#Data visualization 2
require(ggplot2)
theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(iv_pca$rotation, 
                       .names = row.names(iv_pca$rotation))
p + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")


#### Factor Analysis ####
fa1 <- PCAmix(X.quanti = rawdata_pca[, 11:28], X.quali = rawdata_pca[, 1:10], ndim = 2, graph = FALSE,
             rename.level = TRUE)

##Factor analysis on independent variables
fa2 <- factanal(rawdata_pca[,11:28], 6)  #6 factors can explain 52% of variance
print(fa2, digits=2, cutoff=.3, sort=TRUE)  #ab(loadings) smaller than 0.3 are supporessed
fa2$correlation  #correlation matrix of dependent variables


### homogeneity analysis for binary response #####
#https://www.r-bloggers.com/finding-patterns-amongst-binary-variables-with-the-homals-package/

##### Hierarchical models for each question #######
#"isdemotional", "ispemotional", "isdinstr", "ispinstr", "isdminimize", "ispminimize", "isdavoid", "ispavoid", "isdunsupp", 
#"ispunsupp", "isdhappy", "isphappy", "isddepr", "ispdepr", "isdanger", "ispanger", "isdanx", "ispanx"
#by checking BIC(), dhappy2 is better. there are some variations among coupleid. 
dhappy <- lmer(isdhappy ~ d1sex + p1sex + d1racewhbl + p1racewhbl + isdhandler+isphandler+isdcc1r+ ispcc1r+isdcc2 + ispcc2 + (1|coupleid), data = rawdata3)
phappy <- lmer(isphappy ~ d1sex + p1sex + d1racewhbl + p1racewhbl + time + (1 + time|coupleid), data = rawdata3)



demotional <- lmer(isdemotional ~ time + (1|coupleid), data = rawdata3)
pemotional <- lmer(ispemotional ~ time + (1|coupleid), data = rawdata3)

dinstr <- lmer(isdinstr ~ time + (1|coupleid), data = rawdata3)
pinstr <- lmer(ispinstr ~ time + (1|coupleid), data = rawdata3)

dminimize <- lmer(isdminimize ~ time + (1|coupleid), data = rawdata3)
pminimize <- lmer(ispminimize ~ time + (1|coupleid), data = rawdata3)

davoid <- lmer(isdavoid ~ time + (1|coupleid), data = rawdata3)
pavoid <- lmer(ispavoid ~ time + (1|coupleid), data = rawdata3)

dunsupp <- lmer(isdunsupp ~ time + (1|coupleid), data = rawdata3)
punsupp <- lmer(ispunsupp ~ time + (1|coupleid), data = rawdata3)

ddepr <- lmer(isddepr ~ time + (1|coupleid), data = rawdata3)
pdepr <- lmer(ispdepr ~ time + (1|coupleid), data = rawdata3)

danger<- lmer(isdanger ~ time + (1|coupleid), data = rawdata3)
panger<- lmer(ispanger ~ time + (1|coupleid), data = rawdata3)

danx<- lmer(isdanx ~ time + (1|coupleid), data = rawdata3)
panx<- lmer(ispanx ~ time + (1|coupleid), data = rawdata3)