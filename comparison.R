# compare clusters formed using kmeans and hierarchical
table("km clusters" = cust$km_clusters, "hc clusters" = cust$hc_clusters)
cust %>% 
  group_by(km_clusters) %>% 
  summarise(avg_score = mean(spend_score), avg_income = mean(annual_income_000))
cust %>% 
  group_by(hc_clusters) %>% 
  summarise(avg_score = mean(spend_score), avg_income = mean(annual_income_000))

# plots
# kmeans
ggplot(cust, aes(x = annual_income_000, y = spend_score, colour = factor(km_clusters))) +
  geom_point() +
  scale_color_discrete(name = "Clusters") +
  xlab("Annual income of customers (thousands)") +
  ylab("Spending score")
# hierarchical
ggplot(cust, aes(x = annual_income_000, y = spend_score, colour = factor(hc_clusters))) +
  geom_point() +
  scale_color_discrete(name = "Clusters") +
  xlab("Annual income of customers (thousands)") +
  ylab("Spending score")

