dtf <- read.csv("https://raw.githubusercontent.com/atzechang/data/master/Diabetes/sasdata.csv")[, -1]

# Last 16 columns are DVs, others are IVs. 
dvs <- dtf[, 14:31]
ivs <- dtf[, 1:13]
sas_mvar <- data.frame()
for(ii in dvs) {
  sas_mvar <- rbind(sas_mvar, dtf_ivs)
}
dvs_data <- unlist(dtf[, 14:31], )

tmp <- data.frame(rep(NA, 29988))
for(ii in 0:(ncol(dvs) - 1)){
  tmp <- cbind(tmp, ifelse(c(0:29987) %/% 1666 == ii, 1, 0))
}
tall_dtf <- cbind(ivs, tmp[, -1], dvs_data)

colnames(tall_dtf) <- c(colnames(dtf), "response")
rownames(tall_dtf) <- NULL
