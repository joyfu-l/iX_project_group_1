library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(cluster)
library(lubridate)


#getting rid of unid bc dont want to cluster around unid
# Now scale (normalization)
# establishes normalized df (???) why is she a subset (ik not right terminology but ??)
df_clean_id <- df_clean %>% 
  select(age, peoplelive) %>% 
  scale()


# what does this data look like?
# set seed so normalizable wirth time
set.seed(1234)

# KMEANS
k4 <- kmeans(df_clean, centers = 4, nstart = 25)
# K4 is object with list
# SS = total sum of squares
# nstart how many grounps start with; 25 fairly standard

table(k4$cluster)

# look at characteristics within each cluster

# binding characters back into df
k4_cluster <- as.data.frame(k4$cluster)

df_cluster_k4 <- bind_cols(as.data.frame(df_cluster), k4_cluster) %>% 
  rename(cluster = `k4$cluster`)

ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = age, y = peoplelive, color = as.factor(cluster)))


ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = cft_score, y = opt_score, color = as.factor(cluster)))

df_cluster_k4_sum <- df_cluster_k4 %>% 
  group_by(cluster) %>% 
  summarise(cft_score = mean(cft_score), 
            com_score = mean(com_score), 
            opt_score = mean(opt_score), 
            nobs = n())


# HOW MANY CLUSTERS ARE OPTIMAL?
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- df_clean
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

d <- dist(df_cluster, method = "euclidean")

hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Looks like 3 clusters in dendogram
clusterCut <- cutree(hc1, 3)
table(clusterCut)

df_cluster_h3 <- bind_cols(as.data.frame(df_cluster), as.data.frame(clusterCut))

# could also bind to k-means clusters and look at similiarities

df_cluster_h3_sum <- df_cluster_h3 %>% 
  group_by(clusterCut) %>% 
  summarise(cft_score = mean(cft_score), 
            com_score = mean(com_score), 
            opt_score = mean(opt_score), 
            nobs = n())

# DO THESE CLUSTERS HAVE ANY EXPLANATORY POWER?

h3 <- df_cluster_h3 %>% 
  select(clusterCut) %>% 
  rename(h3 = clusterCut)

k4 <- df_cluster_k4 %>% 
  select(cluster) %>% 
  rename(k4 = cluster)

df_clusters_unid <- bind_cols(df_unid, h3) %>% 
  bind_cols(k4)

# now load the broader df
df <- read.csv("data/raw/teaching_training_data.csv")

df <- left_join(df, df_clusters_unid, by = "unid")


# and we can start running our regression again

reg1 <- lm(working ~ gender + as.factor(k4), data = df)
summary(reg1)
# peole in C1 31% likelt to be in work

reg2 <- lm(working ~ gender + as.factor(h3), data = df)
summary(reg2)

# put both in
reg3 <- lm(working ~ gender + as.factor(k4) + as.factor(h3), data = df)
summary(reg3)

# allow to vary by gender
reg4 <- lm(working ~ gender * as.factor(k4), data = df)
summary(reg4)

# who are the 'cluster 1s' from the kmeans?

# ??
reg5 <- lm(working ~ gender * as.factor(k5), data = df)
summary(reg5)


## x * gender allow to interat; not just diff intercept but diff slope
# times


# trying to find greatest r2

# y = 
# sat