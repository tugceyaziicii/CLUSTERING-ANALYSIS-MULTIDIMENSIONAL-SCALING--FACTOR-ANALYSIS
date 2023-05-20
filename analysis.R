
getwd()
data<-read.csv("fastfood_data.csv", header = TRUE,sep=";")
head(data)
str(data)
#CHERNOFF FACES
data1 <- data
library(aplpack)
data1$item<- NULL
new_names <- paste0("menu", 1:516)
new_names
rownames(data1) <- new_names

rownames(data1) <- c("menu1","menu2","menu3","menu4","menu5","menu6","menu7","menu8","menu9","menu10","menu11","menu12","menu13","menu14","menu15","menu16","menu17","menu18","menu19","menu20")
data1
str(data1)
faces(data1)

#Radar Chart
library(scales)
data_scaled <- round(apply(data1, 2, scales::rescale), 2)
data_scaled <- as.data.frame(data_scaled)
head(data_scaled)
# Variables summary
# Get the minimum and the max of every column  
col_max <- apply(data_scaled, 2, max)
col_min <- apply(data_scaled, 2, min)
# Calculate the average profile 
col_mean <- apply(data_scaled, 2, mean)
# Put together the summary of columns
col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
# Bind variables summary to the data
data_scaled2 <- as.data.frame(rbind(col_summary, data_scaled))
head(data_scaled2)

# Define settings for plotting in a 3x4 grid, with appropriate margins:
library(fmsb)
par(mar = rep(0.8,4))
par(mfrow = c(4,5))
# Produce a radar-chart for each car
for (i in 4:nrow(data_scaled2)) {
  radarchart(
   data_scaled2[c(1:3, i), ],
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = row.names(data_scaled2)[i]
  )
}

 


#CLUSTERING
library(ggplot2)
library(factoextra)
# Compute k-means with k = 4
km.res <- kmeans(data1, 4, nstart = 25)
print(km.res)
# Elbow method for kmeans
fviz_nbclust(data1, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)
# Average silhouette for kmeans
fviz_nbclust(data1, kmeans, method = "silhouette")
# It's possible to compute the mean of each variables by clusters using the original data:
aggregate(data1, by=list(cluster=km.res$cluster), mean)
# Compute k-means with k = 3
km.res <- kmeans(data1, 3, nstart = 2)
print(km.res)
aggregate(data1, by=list(cluster=km.res$cluster), mean)
# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, data1, ellipse.type = "norm")
# Change the color palette and theme
fviz_cluster(km.res,data1,
             palette = "Set2", ggtheme = theme_minimal())
# Show points only
fviz_cluster(km.res,data1, geom = "point")
# Show text only
fviz_cluster(km.res,data1, geom = "text")
# PAM clustering
require(cluster)
pam.res <- pam(data1, 3)
# Visualize pam clustering
fviz_cluster(pam.res, geom = "point", ellipse.type = "norm")
# Hierarchical clustering
# Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(data1, k = 3, hc_method = "complete")
# Visualize dendrogram
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")




#FACTOR ANALYSIS

cor_matrix <- cor(data1)
print(cor_matrix)
data1.fa <- factanal(data1, factors = 2)
print(data1.fa)
data1.fa.none <- factanal(data1, factors = 2, rotation = "none", scores="regression")
data1.fa.none$scores
data1.fa.none
data1.fa.varimax <- factanal(data1, factors = 2, rotation = "varimax")
data1.fa.varimax
data1.fa.promax <- factanal(data1, factors = 2, rotation = "promax")
data1.fa.varimax
par(mfrow = c(1,3))
plot(data1.fa.none$loadings[,1], 
     data1.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")

text(data1.fa.none$loadings[,1]-0.08, 
     data1.fa.none$loadings[,2]+0.08,
     colnames(data1),
     col="red")
abline(h = 0, v = 0)

plot(data1.fa.varimax$loadings[,1], 
     data1.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(data1.fa.varimax$loadings[,1]-0.08, 
     data1.fa.varimax$loadings[,2]+0.08,
     colnames(data1),
     col="blue")
abline(h = 0, v = 0)

plot(data1.fa.promax$loadings[,1], 
     data1.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
text(data1.fa.promax$loadings[,1]-0.08, 
     data1.fa.promax$loadings[,2]+0.08,
     colnames(data1),
     col="orange")
abline(h = 0, v = 0)


#MULTIDIMENSIONAL SCALING (MDS)
# Load required packages

library(magrittr)
library(dplyr)
library(ggpubr)
# Compute MDS
mds <- data1 %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          size = 1,
          repel = TRUE)
# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(data1),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

