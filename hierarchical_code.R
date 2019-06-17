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

# (average = average distance between 2 clusters)
# complete-linkage: maximum distance between clusters
# single-linkage: minimum distance between clusters (may be used to detect outliers)
# centroid-linkage: finds centroid in clusters and distance between 2 centroids before merging


library(cluster)
cust_pam <- pam(scaled_cust,k=4)
plot(cust_pam)

# choose method of linkage depending on purpose of clustering
# complete & average = more commonly used and produced more balanced trees
# single are used for detecting outliers (unbalanced tree)

# scale data before using clustering algo (normalisation)
colMeans(x)
apply(x, 2 ,sd)
