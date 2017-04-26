setwd(dirname(parent.frame(2)$ofile))

library(sas7bdat)
library(plyr)
library(ggplot2)
library(mvtnorm)
library(dplyr)


iter <- 0
mins_df <- data.frame(dv = character(0), ivs = character(0), cc = character(0),
                      demog = character(0), inter = character(0), bic = numeric(0))
for (ii in list.files("./Model_Results_Fac")) {
  iter <- iter + 1
  var_name <- strsplit(ii, "_")[[1]][1]
  bic_name <- paste0(var_name,"_bic")
  models_name <- paste0(var_name,"_models")
  list_name <- paste0(var_name,"_list")
  eval(parse(text = paste0(bic_name, " <- read.sas7bdat('./Model_Results_Fac/", ii ,"')")))
  eval(parse(text = paste0(models_name, " <- read.csv('./SAS_Models_Fac/", models_name ,".csv', stringsAsFactors = FALSE)")))
  eval(parse(text = paste0(models_name, "$bic <- ", bic_name, "$Value" )))
  eval(parse(text = paste0(list_name, " <- list()")))
  eval(parse(text = paste0("mins_df <- rbind(mins_df, ", models_name,"[order(", models_name, "$bic), ][1, ])")))
  eval(parse(text = paste0(list_name,"$min <- ", models_name,"[order(", models_name, "$bic), ][1, ]")))
  eval(parse(text = paste0(list_name,"$min_head <- ", models_name,"[order(", models_name, "$bic), ][1:100, ]")))
  eval(parse(text = paste0(list_name, "$noint <- ", models_name, "[", models_name, 
                           "$ivs == 'ccd ccp d1sex p1sex d1racewhbl p1racewhbl ', ]")))
  eval(parse(text = paste0(list_name, "$full<-", models_name, "[nrow(", models_name,"), ]")))
}

rm(iter)
