# read in data
cust <- read.csv("data.csv")

# scale numerical variables to standard normal dist
scaled_cust <- as.data.frame(scale(cust[,c(4,5)]))
summary(scaled_cust)

# calculates the euclidean distance between 2 points
dist_mat <- dist(scaled_cust, method = "euclidean")

# hierarchical clustering with average linkage method
hclust_avg <- hclust(dist_mat, method = "average")

# plot dendrogram
plot(hclust_avg)

# cut tree at k = 5
cut_avg <- cutree(hclust_avg, k = 5)
plot(hclust_avg)
rect.hclust(hclust_avg, k = 5)

cust <- mutate(cust, hc_clusters = cut_avg)
