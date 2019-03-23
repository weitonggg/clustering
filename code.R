library(ggplot2)
library(dplyr)

# load data
cust <- read.csv("data.csv")
# rename col names
colnames(cust) <- c("cust_id", "gender", "age", "annual_income_000", "spend_score")

# some eda 
summary(cust$annual_income_000)
summary(cust$spend_score)
hist(cust$annual_income_000)
hist(cust$spend_score)

# finding optimal no. of clusters using elbow method
kk <- 1:10
wss <- c()
for (i in kk){
  cluster_temp <- kmeans(cust[,c(4,5)], centers = i, nstart = 25)
  wss <- c(wss, cluster_temp$tot.withinss)
}
plot(wss, type = "b")
# from plot, bend appears to be at 5

set.seed(10)
cluster_5 <- kmeans(cust[,c(4,5)], centers = 5, nstart = 25)
#cluster_5$tot.withinss
cust <- cust %>% mutate(cluster5 = cluster_5$cluster)
ggplot(cust, aes(x = annual_income_000, y = spend_score, colour = factor(cluster5))) +
  geom_point() +
  scale_color_discrete(name = "Clusters") +
  xlab("Annual income of customers (thousands)") +
  ylab("Spending score")

# cluster 1: low annual income, low spending score,
# cluster 2: average annual income, average score 
# cluster 3: high annual income, low score
# cluster 4: low annual income, high score
# cluster 5: high annual income, high score



