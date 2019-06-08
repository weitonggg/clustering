###### k-means cluster using elbow and average silhouette method to determine optimal number of clusters

Data downloaded from https://www.kaggle.com/vjchoudhary7/customer-segmentation-tutorial-in-python/activity


###### Load required packages and data
```r
library(ggplot2)
library(dplyr)
library(cluster)

cust <- read.csv("data.csv")
colnames(cust) <- c("cust_id", "gender", "age", "annual_income_000", "spend_score")
````

###### some exploratory data analysis 
````r
summary(cust$annual_income_000)
summary(cust$spend_score)
hist(cust$annual_income_000)
hist(cust$spend_score)
cor(cust$spend_score, cust$annual_income_000) # very weak linear relationship between 2 variables
````
###### Finding the optimal number of clusters using the Elbow method
````r
kk <- 1:10
wss <- c()
for (i in kk){
  cluster_temp <- kmeans(cust[,c(4,5)], centers = i, nstart = 25)
  wss <- c(wss, cluster_temp$tot.withinss)
}
plot(wss, type = "b")
# from plot, bend appears to be at no. of clusters, k = 5
````

###### Finding the optimal number of clusters using the Average Silhouette method
````r
kk <- 2:10
avg_silhou <- c()
for (i in kk){
  cluster_temp <- kmeans(cust[,c(4,5)], centers = i, nstart = 25)
  ss <- silhouette(cluster_temp$cluster, dist(cust[,c(4,5)]))
  avg_silhou <- c(avg_silhou, mean(ss[,3]))
}
plot(avg_silhou, type="b")
# from plot, maximum value of average silhouette occurs at no. of clusters, k = 4
````

###### Finding the optimal number of clustering using Gap statistic
````r
set.seed(123)
gap_stat <- clusGap(cust[,c(4,5)], FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# print result
print(gap_stat, method = "firstmax")
# plot result
fviz_gap_stat(gap_stat)
# from plot, maximum gap statistic occurs at no. of clusters, k = 5
````

````r
set.seed(10)
cluster_5 <- kmeans(cust[,c(4,5)], centers = 5, nstart = 25)
#cluster_5$tot.withinss
cust <- cust %>% mutate(cluster5 = cluster_5$cluster)
ggplot(cust, aes(x = annual_income_000, y = spend_score, colour = factor(cluster5))) +
  geom_point() +
  scale_color_discrete(name = "Clusters") +
  xlab("Annual income of customers (thousands)") +
  ylab("Spending score")


````
