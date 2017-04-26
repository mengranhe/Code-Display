this_dir <- dirname(parent.frame(2)$ofile)
setwd(this_dir)
library(plyr)

# dvs <- c('isdemotional', 'ispemotional', 'isdinstr', 'ispinstr', 'isdminimize', 'ispminimize', 'isdavoid', 'ispavoid', 'isdunsupp', 
#          'ispunsupp', 'isdhappy', 'isphappy', 'isddepr', 'ispdepr', 'isdanger', 'ispanger', 'isdanx', 'ispanx')
# 
# ivs <- c('isdhandler', 'isphandler', 'isdcc1r', 'ispcc1r','isdcc2', 'ispcc2')
# inter_vars <- c('d1sex', 'p1sex', 'd1racewhbl', 'p1racewhbl')

dvs <- c('f1', 'f2', 'f3', 'f4', 'f5', 'f6')

ivs <- c('ccd', 'ccp')
demo <-  c('d1sex', 'p1sex', 'd1racewhbl', 'p1racewhbl')

# model_gen <- function(dv, ivs, demog) {
#   library(utils)
#   demog_all <- paste(demog, collapse = " ")
#   x <- matrix(c(dv, demog_all, " ", demog_all, " "), 
#               ncol = 5)
#   colnames(x) <- c("dv", "ivs", "cc", "demog", "inter")
#   for (ii in 1:length(ivs)) {
#     cc_ivs_n <- t(combn(ivs, ii))
#     for (mm in 1:nrow(cc_ivs_n)){
#       new_model_cc <- cc_ivs_n[mm,]
#       full_model <- paste(c(new_model_cc, demog_all), collapse = " ")
#       x <- rbind(x, c(dv, full_model, paste(new_model_cc, collapse = " "),
#                       demog_all, " "))
#       for (jj in 1:ii) {
#         poss_inters <- t(combn(new_model_cc, jj))
#         for (kk in 1:nrow(poss_inters)) {
#           for (ll in demog) {
#             interactions <- sapply(poss_inters[kk,], paste, ll, sep = "*")
#             full_model <- paste(c(new_model_cc, demog_all, interactions), collapse = " ")
#             x <- rbind(x, c(dv, full_model, paste(new_model_cc, collapse = " "),
#                             demog_all, paste(interactions, collapse = " ")))
#           }
#         }
#       }
#     }
#   }
#   return(x)
# }

model_gen <- function(dv, ivs, demog) {
  library(utils)
  
  somefunc <- function(iv, inters, comb_list){
    x <- character(0)
    for (ii in 1:length(comb_list)) {
      for (jj in 1:nrow(comb_list[[ii]])) {
        if (length(comb_list[[ii]][jj, ]) == 0) {
          x <- c(x, "")
        } else {
          y <- paste(paste(iv, demog[comb_list[[ii]][jj, ]], sep = "*"), collapse = " ")
          x <- c(x, y)
        }
      }
    }
    return(x)
  }
  
  demog_all <- paste(demog, collapse = " ")
  all_models <- matrix(c(dv, demog_all, " ", demog_all, " "), 
              ncol = 5)
  colnames(all_models) <- c("dv", "ivs", "cc", "demog", "inter")
  inter_list <- list()
  for (ii in 0:length(demog)) {
    inter_list[[ii + 1]] <- t(combn(1:length(demog), ii))
  }
  for (ii in 1:length(ivs)) {
    cc_ivs_n <- t(combn(ivs, ii))
    for (mm in 1:nrow(cc_ivs_n)){
      new_model_cc <- cc_ivs_n[mm,]
      full_model <- paste(c(new_model_cc, demog_all), collapse = " ")
      for (jj in 1:ii) {
        poss_inters <- t(combn(new_model_cc, jj))
        for (kk in 1:nrow(poss_inters)) {
          xx <- alply(poss_inters[kk, , drop = FALSE], 2, somefunc, demog, inter_list)
          inters <- do.call(expand.grid, xx)
          iv_new <- apply(cbind(full_model, inters), 1, paste, collapse = " ")
          new_rows <- cbind(dv, iv_new, paste(c(new_model_cc), collapse = " "), 
                            demog_all, apply(cbind(inters), 1, paste, collapse = " "))
          colnames(new_rows) <- c("dv", "ivs", "cc", "demog", "inter")
          all_models <- rbind(all_models, new_rows)
        }
      }
    }
  }
  return(all_models)
}


for(dv in dvs) {
  test <- model_gen(dv, ivs, demo)
  write.csv(test, paste0("./SAS_Models_Fac/", dv, "_models.csv"), row.names = FALSE)
}