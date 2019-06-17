library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)

# load data
cust <- read.csv("data.csv")
# rename col names
colnames(cust) <- c("cust_id", "gender", "age", "annual_income_000", "spend_score")

# some eda 
summary(cust$annual_income_000)
summary(cust$spend_score)
hist(cust$annual_income_000)
hist(cust$spend_score)

# scale data
scaled_cust <- as.data.frame(scale(cust[,c(4,5)]))
summary(scaled_cust)

# finding optimal no. of clusters using Elbow method
kk <- 1:10
wss <- c()
for (i in kk){
  cluster_temp <- kmeans(scaled_cust, centers = i, nstart = 25)
  wss <- c(wss, cluster_temp$tot.withinss)
}
plot(wss, type = "b")
# from plot, the optimal number of clusters appears to be at k=5 (where bend is at).

# finding optimal no. of clusters using avg silhouette method
kk <- 2:10
avg_silhou <- c()
for (i in kk){
  cluster_temp <- kmeans(scaled_cust, centers = i, nstart = 25)
  ss <- silhouette(cluster_temp$cluster, dist(cust[,c(4,5)]))
  avg_silhou <- c(avg_silhou, mean(ss[,3]))
}
plot(avg_silhou,type="b")
# From plot above, maximum value of average silhouette occurs at no. of clusters, k = 4.

# finding optimal no. of clusters using gap statistic method
set.seed(123)
gap_stat <- clusGap(scaled_cust, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# print result
print(gap_stat, method = "firstmax")
# plot result
fviz_gap_stat(gap_stat)
# From plot above, maximum gap statistic occurs at no. of clusters, k = 5.

# we will use k-means cluster with k = 5 to segment the data
set.seed(100)
km_clust <- kmeans(scaled_cust, centers = 5, nstart = 25)
#cluster_5$tot.withinss
cust <- cust %>% mutate(km_clusters = km_clust$cluster)
