setwd(dirname(parent.frame(2)$ofile))

library(sas7bdat)
library(plyr)
library(ggplot2)
library(mvtnorm)
library(dplyr)

full_bics <- read.sas7bdat("full_bics.sas7bdat")
full_times <- read.sas7bdat("full_times.sas7bdat")
full_time_stds <- read.sas7bdat("full_time_stds.sas7bdat")
full_times <- mutate(full_times, std1 = full_time_stds$Col100,
                     std2 = full_time_stds$Col101)

iter <- 0
mins_df <- data.frame(dv = character(0), ivs = character(0), cc = character(0),
                      demog = character(0), inter = character(0), bic = numeric(0))
for (ii in list.files("./Model_Results")) {
  iter <- iter + 1
  var_name <- strsplit(ii, "_")[[1]][1]
  bic_name <- paste0(var_name,"_bic")
  models_name <- paste0(var_name,"_models")
  list_name <- paste0(var_name,"_list")
  eval(parse(text = paste0(bic_name, " <- read.sas7bdat('./Model_Results/", ii ,"')")))
  eval(parse(text = paste0(models_name, " <- read.csv('./SAS_Models/", models_name ,".csv', stringsAsFactors = FALSE)")))
  eval(parse(text = paste0(models_name, "$bic <- ", bic_name, "$Value" )))
  eval(parse(text = paste0(list_name, " <- list()")))
  eval(parse(text = paste0("mins_df <- rbind(mins_df, ", models_name,"[order(", models_name, "$bic), ][1, ])")))
  eval(parse(text = paste0(list_name,"$min <- ", models_name,"[order(", models_name, "$bic), ][1, ]")))
  eval(parse(text = paste0(list_name,"$min_head <- ", models_name,"[order(", models_name, "$bic), ][1:100, ]")))
  eval(parse(text = paste0(list_name, "$noint <- ", models_name, "[", models_name, 
                           "$ivs == 'isdhandler isphandler isdcc1r ispcc1r isdcc2 ispcc2 d1sex p1sex d1racewhbl p1racewhbl', ]")))
  eval(parse(text = paste0(list_name, "$p1r <- ", models_name, "[", models_name, 
                           "$inter == 'isdhandler*p1racewhbl isphandler*p1racewhbl isdcc1r*p1racewhbl ispcc1r*p1racewhbl isdcc2*p1racewhbl ispcc2*p1racewhbl', ]")))
  eval(parse(text = paste0(list_name, "$d1r <- ", models_name, "[", models_name, 
                           "$inter == 'isdhandler*d1racewhbl isphandler*d1racewhbl isdcc1r*d1racewhbl ispcc1r*d1racewhbl isdcc2*d1racewhbl ispcc2*d1racewhbl', ]")))
  eval(parse(text = paste0(list_name, "$d1g <- ", models_name, "[", models_name, 
                           "$inter == 'isdhandler*d1sex isphandler*d1sex isdcc1r*d1sex ispcc1r*d1sex isdcc2*d1sex ispcc2*d1sex', ]")))
  eval(parse(text = paste0(list_name, "$p1g <- ", models_name, "[", models_name, 
                           "$inter == 'isdhandler*p1sex isphandler*p1sex isdcc1r*p1sex ispcc1r*p1sex isdcc2*p1sex ispcc2*p1sex', ]")))
  eval(parse(text = paste0(list_name, "$full<- data.frame(dv = '", 
                           var_name,
                           "', ivs = 'isdhandler isphandler isdcc1r ispcc1r isdcc2 ispcc2 isdhandler*p1racewhbl isphandler*p1racewhbl isdcc1r*p1racewhbl ispcc1r*p1racewhbl isdcc2*p1racewhbl ispcc2*p1racewhbl isdhandler*d1racewhbl isphandler*d1racewhbl isdcc1r*d1racewhbl ispcc1r*d1racewhbl isdcc2*d1racewhbl ispcc2*d1racewhbl isdhandler*d1sex isphandler*d1sex isdcc1r*d1sex ispcc1r*d1sex isdcc2*d1sex ispcc2*d1sex isdhandler*p1sex isphandler*p1sex isdcc1r*p1sex ispcc1r*p1sex isdcc2*p1sex ispcc2*p1sex'",
                           ", cc = 'isdhandler isphandler isdcc1r ispcc1r isdcc2 ispcc2'",
                           ", demog = 'd1sex p1sex d1racewhbl p1racewhbl'",
                           ", inter = 'isdhandler*p1racewhbl isphandler*p1racewhbl isdcc1r*p1racewhbl ispcc1r*p1racewhbl isdcc2*p1racewhbl ispcc2*p1racewhbl isdhandler*d1racewhbl isphandler*d1racewhbl isdcc1r*d1racewhbl ispcc1r*d1racewhbl isdcc2*d1racewhbl ispcc2*d1racewhbl isdhandler*d1sex isphandler*d1sex isdcc1r*d1sex ispcc1r*d1sex isdcc2*d1sex ispcc2*d1sex isdhandler*p1sex isphandler*p1sex isdcc1r*p1sex ispcc1r*p1sex isdcc2*p1sex ispcc2*p1sex'", 
                           ", bic = ", 
                           full_bics[iter,], ")")))
}

rm(iter)

var_names_all <- plyr::laply(sapply(list.files("./Model_Results"), strsplit, split = "_"), `[[`, 1)

pdf("TimeEffectPlots.pdf")
for (jj in 1:length(var_names_all)) {
  resamp <- rmvnorm(100, mean = c(full_times[(2 * jj - 1) : (2 * jj), "Estimate"]),
                    sigma = as.matrix(full_times[(2 * jj - 1) : (2 * jj), c("std1", "std2")]))
  plot_df <- data.frame(t = 0:13, 
                        t_pred = full_times[2 * jj - 1, "Estimate"] * 0:13 + 
                          full_times[2 * jj, "Estimate"] * (0:13) ^ 2)
  plot_df <- mutate(plot_df, upper = quantile(resamp[, 1], 0.975) * 0:13 + 
                      quantile(resamp[, 2], 0.975) * (0:13) ^ 2,
                    lower = quantile(resamp[, 1], 0.025) * 0:13 + 
                      quantile(resamp[, 2], 0.025)* (0:13) ^ 2)
  print(plot_df)
  p <- ggplot(data = plot_df) + 
    geom_line(aes(x = t, y = t_pred)) +
    geom_line(aes(x = t, y = upper), color = "blue") +
    geom_line(aes(x = t, y = lower), color = "blue") +
    theme_bw() +
    labs(x = "Time",
         y = var_names_all[jj],
         title = paste0("Fixed Effects of Time, ", var_names_all[jj]),
         subtitle = "95% CI in Blue")
  print(p)
}
dev.off()