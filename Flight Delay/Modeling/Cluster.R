##Unsupervised Learning: Clustering
library(magrittr)
load("WeatherData.RData")

tr15DEPNonM%<>%subset(DEP_DEL15 %in% 0:1)
X_tr <- tr15DEPNonM[, -which(names(tr15DEPNonM) %in% c("YEAR", "FL_DATE", "DEST", "QUARTER", 
        "MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK", "CARRIER", "TAIL_NUM", "ORIGIN_STATE_ABR","FL_NUM", "ORIGIN", 
        "PRIGIN_STATE_ABR", "DEST_STATE_ABR", "FLIGHTS", "CRS_DEP_TIME","CRS_ARR_TIME","DISTANCE_GROUP",
        "DEP_Snow", "DEP_Rain", "DEP_Hour","DEP_Min","ARV_Rain","ARV_Hail", "ARV_Snow", "ARV_Thunder","ARV_Tornado", 
        "ARV_Fog", "FL_DATE_ARR", "DEP_Time","ARR_Time", "DEP_Day"))]

#Base rate: 86.23% of dep_del15 = 0, 13.77% of dep_del15 = 1.

#K-means
set.seed(0)
tr_kmean <- kmeans(X_tr[,-1], centers = 2, algorithm = "Lloyd")
kmean_cluster <- ifelse(tr_kmean$cluster == 1, 1, 0)
table(kmean_cluster, X_tr[,1])
mean(kmean_cluster != X_tr[,1]) #missclassification = 0.238191

#euclidean distance
diss <- dist(X_tr[,-1])


#K-medoids: same result as k_means
# library(cluster)
# tr_medoid <- pam(X_tr[,-1], k = 2, diss = FALSE, keep.diss = FALSE, keep.data = FALSE, 
#                  cluster.only = TRUE)
# tr_medoid <- ifelse(tr_medoid == 1, 0, 1)
# table(tr_medoid, X_tr[,1])
# mean(tr_medoid != X_tr[,1])

#Hierarchical Clustering
#single linkage
# single <- hclust(diss, method = "single")
# single_cut <- cutree(single, k = 2)
# plot(single)
# abline(h = mean(rev(single$height)[1:2]), col = "red")
# single_cut <- ifelse(single_cut == 1, 0, 1)
# table(single_cut, X_tr[,1])
# mean(single_cut != X_tr[,1]) #missclassification = 0.2098321

#average linkage
# avg <- hclust(diss, method = "average")
# avg_cut <- cutree(avg, k = 2)

#complete linkage
complete <- hclust(diss, method = "complete")
com_cut <- cutree(complete, k = 2)
plot(complete, labels = FALSE, main = "Clustering: Complete Linkage")
abline(h = mean(rev(complete$height)[1:3]), col = "red")
com_cut <- ifelse(com_cut == 1, 0, 1)
#table(com_cut, X_tr[,1])


#minimax linkage
library(protoclust)
mini <- protoclust(diss)
mini_cut <- protocut(mini, 2)
mini_cut_cl <- ifelse(mini_cut$cl == 1, 0, 1)
table(mini_cut_cl, X_tr[,1])
mean(mini_cut_cl != X_tr[,1]) #0.2098183

# prototype1 <- X_tr[mini_cut$protos[1],]
# prototype2 <- X_tr[mini_cut$protos[2],]

#summary statistics
save(X_tr, mini, tr_kmean, file = "clusterResult.RData")
X_tr$kmean_cluster <- as.factor(kmean_cluster)
X_tr$mini_cluster <- as.factor(mini_cut_cl)

library(ggplot2)
####Elapsed Time
#minimax: cluster 1 has shorter elapsed time than cluster 2
ggplot(X_tr, aes(x = CRS_ELAPSED_TIME, fill = mini_cluster)) + geom_histogram(binwidth = 3) + 
  labs(x = "Elapsed Time", y = "Number of Flight", title = "Histogram of Elapsed Time")
#kmeans
ggplot(X_tr, aes(x = CRS_ELAPSED_TIME, fill = kmean_cluster)) + geom_histogram(binwidth = 3) + 
  labs(x = "Elapsed Time", y = "Number of Flight", title = "Histogram of Elapsed Time")

####Flight Distance
#Minimax: cluster 1 has shorter airport distance than cluster 2
ggplot(X_tr, aes(x = DISTANCE, fill = mini_cluster)) + geom_histogram(binwidth =80) + 
  labs(x = "Airport Distance", y = "Number of Flight", title = "Histogram of Elapsed Time")
ggplot(X_tr, aes(x = DISTANCE, fill = kmean_cluster)) + geom_histogram(binwidth =80) + 
  labs(x = "Airport Distance", y = "Number of Flight", title = "Histogram of Elapsed Time")


####Temperature
#minimax: PIT temperature looks the same
ggplot(X_tr, aes(x = mini_cluster, y = DEP_Temp.F., color = mini_cluster)) + geom_boxplot() +
  labs(x = "2 Clusters", y = "Pittsburgh Temperature in Fahrenheit", title = "Boxplot of PIT Temperature")
#kmeans
ggplot(X_tr, aes(x = kmean_cluster, y = DEP_Temp.F., color = kmean_cluster)) + geom_boxplot() +
  labs(x = "2 Clusters", y = "Pittsburgh Temperature in Fahrenheit", title = "Boxplot of PIT Temperature")

#Minimax: Temperature of arrival airports in cluster 2 is higher than cluster 1 on average 
ggplot(X_tr, aes(x = mini_cluster, y = ARV_TEMP, color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Airport Temperature", title = "Boxplot of Arrival Temperature")
#kmeans
ggplot(X_tr, aes(x = kmean_cluster, y = ARV_TEMP, color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Airport Temperature", title = "Boxplot of Arrival Temperature")


####Pressure
#PIT pressure no difference
ggplot(X_tr, aes(x = mini_cluster, y = DEP_Pressure.in., color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Pressure", title = "Boxplot of PIT Pressure")
ggplot(X_tr, aes(x = kmean_cluster, y = DEP_Pressure.in., color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Pressure", title = "Boxplot of PIT Pressure")


#Minimax: Pressure of arrival airport in cluster 1 is higher than cluster 2
ggplot(X_tr, aes(x = mini_cluster, y = ARV_Pressure, color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Airport Pressure", title = "Boxplot of Arrival Pressure")
#kmeans: Pressure in cluster 1 tends to be higher than cluster 2
ggplot(X_tr, aes(x = kmean_cluster, y = ARV_Pressure, color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Airport Pressure", title = "Boxplot of Arrival Pressure")



####Visibility (need interpretation)
#PIT
ggplot(X_tr, aes(x = mini_cluster, y = DEP_Visibility.mi., color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Visibility", title = "Boxplot of PIT Visibility")
ggplot(X_tr, aes(x = kmean_cluster, y = DEP_Visibility.mi.,color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Visibility", title = "Boxplot of PIT Visibility")

#Arrival airport
ggplot(X_tr, aes(x = mini_cluster, y = ARV_Visibility, color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Visibility", title = "Boxplot of Arrival Visibility")
ggplot(X_tr, aes(x = kmean_cluster, y = ARV_Visibility, color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Visibility", title = "Boxplot of Arrival Visibility")


####Wind Speed
#PIT: similar
ggplot(X_tr, aes(x = mini_cluster, y = DEP_Wind_Speed.mph., color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Visibility", title = "Boxplot of PIT Wind Speed")
ggplot(X_tr, aes(x = kmean_cluster, y = DEP_Wind_Speed.mph., color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "PIT Visibility", title = "Boxplot of PIT Wind Speed")

#Arrival airport: similar
ggplot(X_tr, aes(x = mini_cluster, y = ARV_WindSpeed, color = mini_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Visibility", title = "Boxplot of Arrival Wind Speed")
ggplot(X_tr, aes(x = kmean_cluster, y = ARV_WindSpeed, color = kmean_cluster)) + 
  geom_boxplot() + labs(x = "2 Clusters", y = "Arrival Visibility", title = "Boxplot of Arrival Wind Speed")


