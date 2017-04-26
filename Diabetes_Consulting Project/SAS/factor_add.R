this_dir <- dirname(parent.frame(2)$ofile)
setwd(this_dir)

sasdata <- read.csv("./sasdata.csv")

sasdata$f1 <- -0.56 * as.numeric(sasdata$isdhappy) +
  0.87 * as.numeric(sasdata$isdhappy) +
  0.80 * as.numeric(sasdata$isdanger) +
  0.67 * as.numeric(sasdata$isdanx) +
  0.30 * as.numeric(sasdata$isdunsupp)

sasdata$f2 <- -0.53 * as.numeric(sasdata$isphappy) +
  0.90 * as.numeric(sasdata$ispdepr) +
  0.78 * as.numeric(sasdata$ispanger) +
  0.62 * as.numeric(sasdata$ispanx)

sasdata$f3 <- 0.66 * as.numeric(sasdata$isdemotional) +
  0.85 * as.numeric(sasdata$isdinstr) +
  0.35 * as.numeric(sasdata$isdminimize)

sasdata$f4 <- 0.35 * as.numeric(sasdata$isdemotional) +
  0.80 * as.numeric(sasdata$ispemotional) +
  0.35 * as.numeric(sasdata$ispinstr)

sasdata$f5 <- 0.58 * as.numeric(sasdata$isdminimize) +
  0.64 * as.numeric(sasdata$ispminimize) +
  0.40 * as.numeric(sasdata$ispinstr)

sasdata$f6 <- 0.65 * as.numeric(sasdata$ispunsupp) +
  0.39 * as.numeric(sasdata$ispavoid) +
  0.35 * as.numeric(sasdata$isdunsupp)

sasdata$ccd <- as.numeric(sasdata$isdhandler) + 
  as.numeric(sasdata$isdcc1r) + 
  (as.numeric(sasdata$isdcc2) - 1) / 4

sasdata$ccp <- as.numeric(sasdata$isphandler) + 
  as.numeric(sasdata$ispcc1r) + 
  (as.numeric(sasdata$ispcc2) - 1) / 4

sasdata$X.1 <- NULL

write.csv(sasdata, "sasdata.csv", row.names  = FALSE)