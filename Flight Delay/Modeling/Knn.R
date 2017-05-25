library(dplyr)
library(class)

load("WeatherData.RData")

############## KNN data ##############
numcol = c("QUARTER", "DAY_OF_WEEK", "DEP_Snow", "DEP_Rain",
           "ARV_Fog", "ARV_Rain", "ARV_Snow", "ARV_Hail", "ARV_Thunder", "ARV_Tornado", "DEP_Day")
numcollen = length(numcol)

tr15DEPNonMknn = select(tr15DEPNonM, -CARRIER, -DEST)
tr15DEPNonMknn$Holiday = ifelse(tr15DEPNonMknn$Holiday=="NOTHOLIDAY", 0, 1)
te16DEPNonMknn = te16DEPNonM
te16DEPNonMknn$Holiday = ifelse(te16DEPNonMknn$Holiday=="NOTHOLIDAY", 0, 1)

for (ii in 1:numcollen){
  tr15DEPNonMknn[,colnames(tr15DEPNonMknn)==numcol[ii]] = 
    as.numeric(tr15DEPNonMknn[,colnames(tr15DEPNonMknn)==numcol[ii]])
  te16DEPNonMknn[,colnames(te16DEPNonMknn)==numcol[ii]] = 
    as.numeric(te16DEPNonMknn[,colnames(te16DEPNonMknn)==numcol[ii]])
}

te16DEPNonMknn = te16DEPNonMknn[,colnames(te16DEPNonMknn) %in% colnames(tr15DEPNonMknn)]

knntraining = select(tr15DEPNonMknn, -DEP_DEL15) 
knntest = select(te16DEPNonMknn, -DEP_DEL15)

knntrainingY = as.factor(tr15DEPNonMknn$DEP_DEL15)

for (ii in 1:35){
  knnhat = knn(knntraining, knntest, knntrainingY, k=ii)
  print(ii)
  print(table(knnhat, te16DEPNonMknn$DEP_DEL15))
  print(mean(knnhat != te16DEPNonMknn$DEP_DEL15))
}

knnScaletraining = scale(knntraining)
knnScaletest = scale(knntest)

knnfitprob = lapply(4:13, function(x){knn(knnScaletraining, knnScaletest, knntrainingY, k = x, prob = TRUE)})
knnfit = lapply(4:13, function(x){knn(knnScaletraining, knnScaletest, knntrainingY, k = x)})
knnmis = lapply(knnfit, function(x){mean(x != te16DEPNonMknn$DEP_DEL15)})

# k = 13 gives the best misclassfication rate

knn13prob = attr(knnfitprob[[10]], "prob")

save(knnfitprob, knnfit, knnmis, knn13prob, file = "knnresults.RData")

############## KNN with all 15 and 16 visable data ##############
sum(colnames(tr15DEPNonM) != colnames(te16DEPNonM))

alldataX = select(rbind(tr15DEPNonM, te16DEPNonM), -DEP_DEL15)
alldatay = as.factor(rbind(tr15DEPNonM, te16DEPNonM)$DEP_DEL15)

numcol = c("QUARTER", "DAY_OF_WEEK", "DEP_Snow", "DEP_Rain",
           "ARV_Fog", "ARV_Rain", "ARV_Snow", "ARV_Hail", "ARV_Thunder", "ARV_Tornado", "DEP_Day")
numcollen = length(numcol)

alldataX = select(alldataX, -CARRIER, -DEST)
alldataX$Holiday = ifelse(alldataX$Holiday=="NOTHOLIDAY", 0, 1)
guessX = guess16NonM[,colnames(guess16NonM) %in% colnames(alldataX)]
guessX$Holiday = ifelse(guessX$Holiday=="NOTHOLIDAY", 0, 1)

for (ii in 1:numcollen){
  alldataX[,colnames(alldataX)==numcol[ii]] = 
    as.numeric(alldataX[,colnames(alldataX)==numcol[ii]])
  guessX[,colnames(guessX)==numcol[ii]] = 
    as.numeric(guessX[,colnames(guessX)==numcol[ii]])
}

alldataXscale = scale(alldataX)
guessXscale = guessX
guessXscale[1:26] = scale(guessXscale[1:26])
guessXscale[29:33] = scale(guessXscale[29:33])

knnyhat = knn(alldataXscale, guessXscale, alldatay, k=13)

save(knnfitprob, knnfit, knnmis, knn13prob, file = "knnresults.RData")
save(knnyhat, file="knnAllDataYhat.RData")







