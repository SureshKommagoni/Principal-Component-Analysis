# loading wine data

wine <- read.csv("D:\\ExcelR\\Assignments\\PCA\\wine.csv")
View(wine)

help("princomp") # to understand api for princomp

# the first column in wine daata has types of wine
View(wine[-1])

data <- wine[-1]
attach(data)
cor(data)

pcaobj <- princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
# cor = TRUE use correlation matrix for getting PCA scores

summary(pcaobj)
str(pcaobj)
loadings(pcaobj)

plot(pcaobj) # graph showing the important principal components

# comp1 is having high importance

biplot(pcaobj)

pcaobj$scores[,1:3] # top 3 pca scores whcih represents the whole data

# cbind is use to bind the data in column wise

# considering top 3 principal components scores and binding with wine data

wine <- cbind(wine, pcaobj$scores[,1:3])
View(wine)

## Hierarchical Clustering

# preparing data for clustering (considering only pca scores as they represebts the entire data)

clus_data <- wine[,15:17]
View(clus_data)

# normalizing the data

norm_clus <- scale(clus_data) # scale function is used to normalize the data

dist1 <- dist(norm_clus, method = "euclidean") # method to find distance 

# clustering the data using hclust function

fit1 <- hclust(dist1, method = "complete")
plot(fit1) # display dendrogram
plot(fit1, hang = -1)
rect.hclust(fit1, k=7, border = "red")

groups <- cutree(fit1, 7) # cutting dendrogram into 7 clusters

wine_new <- as.matrix(groups) # cluster numbering

View(wine_new)

final1 <- cbind(wine_new, wine) # binding column wise with original data
View(final1)

View(aggregate(final1[,-c(2,16:18)], by = list(wine_new), FUN = mean))

# drwan from the aggregate of the wine data on wine_new

write.csv(final1, file = "wine_Hclustered.csv", row.names = F, col.names = F)
getwd()

## kmeans clustering ####
library(plyr)

fit2 <- kmeans(norm_clus, 7) 
str(fit2)

final2 <- data.frame(fit2$cluster, wine)
final2

# elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(norm_clus)-1)*sum(apply(norm_clus, 2, var)) # Determine the number of clusters by scree-plot

for (i in 1:7) wss[i] = sum(kmeans(norm_clus, centers = i)$withinss)
plot(1:7, wss, type = "b", xlab = "Number of Clusters", ylab = "within groups sum of squares") # look for an "elbow" in the scree plot 
title(sub = "k-Means Clustering Screen-plot")

library(factoextra)
fit3 <- eclust(norm_clus, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit3, geom = "point", frame.type = "norm")

aggregate(wine[,2:17], by = list(fit2$cluster), FUN =mean)

write.csv(final2, file = "wine_kmeans_clustere.csv", row.names = F, col.names = F)
getwd()

table(fit2$cluster)
























