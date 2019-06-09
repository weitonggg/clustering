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

# finding optimal no. of clusters using Elbow method
kk <- 1:10
wss <- c()
for (i in kk){
  cluster_temp <- kmeans(cust[,c(4,5)], centers = i, nstart = 25)
  wss <- c(wss, cluster_temp$tot.withinss)
}
plot(wss, type = "b")
# from plot, the optimal number of clusters appears to be at k=5 (where bend is at).

# finding optimal no. of clusters using avg silhouette method
kk <- 2:10
avg_silhou <- c()
for (i in kk){
  cluster_temp <- kmeans(cust[,c(4,5)], centers = i, nstart = 25)
  ss <- silhouette(cluster_temp$cluster, dist(cust[,c(4,5)]))
  avg_silhou <- c(avg_silhou, mean(ss[,3]))
}
plot(avg_silhou,type="b")
# From plot above, maximum value of average silhouette occurs at no. of clusters, k = 4.

# finding optimal no. of clusters using gap statistic method
set.seed(123)
gap_stat <- clusGap(cust[,c(4,5)], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# print result
print(gap_stat, method = "firstmax")
# plot result
fviz_gap_stat(gap_stat)
# From plot above, maximum gap statistic occurs at no. of clusters, k = 5.

# we will use k-means cluster with k = 5 to segment the data
set.seed(10)
cluster_5 <- kmeans(cust[,c(4,5)], centers = 5, nstart = 25)
#cluster_5$tot.withinss
cust <- cust %>% mutate(cluster5 = cluster_5$cluster)
ggplot(cust, aes(x = annual_income_000, y = spend_score, colour = factor(cluster5))) +
  geom_point() +
  scale_color_discrete(name = "Clusters") +
  xlab("Annual income of customers (thousands)") +
  ylab("Spending score")
