#INTEL CUSTOMER SEGMENTATION

#INSTALL PACKEGES FOR ANALYSIS
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster") # gap statistics 
install.packages("openxlsx")
install.packages("factoextra") #for clustering visualization
install.packages("NbClust") # 30 indexes 
install.packages("clValid") # Dunn's index


#CALLING THE LIBRARIES
library(readxl)
library(tidyverse)
library(cluster)
library(openxlsx)
library(factoextra)
library(NbClust)
library(clValid)

#IMPORT DATA FROM EXCEL
SmartWatch_df <- read_excel("C:/Users/oxana/Desktop/Study/EXETER/MASTERS/Marketing analytics/Report/SmartWatch Data File.xlsx")
View(SmartWatch_df)

#EYEBALL ANALYSIS
summary(SmartWatch_df)

#CHECK FOR MISSING VALUES
sum(is.na(SmartWatch_df)) #Dataframe has no missing values

# STANDARDISE DATA
SmartWatch_dfz <- scale(SmartWatch_df)
View(SmartWatch_dfz)

# CALCULATE EUCLIDEAN DISTANCE
?dist
distance <- dist(SmartWatch_dfz, method = 'euclidean')

# CLUSTER DENDROGRAM 
?hclust
hc.w <- hclust(distance, method = 'ward.D2')   #Same as ward.D, but more stable  in large datasets. 
# Plot the dendrogram to visualise the clustering
plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Hight")


# DETERMINE THE OPTIMAL NUMBER OF CLUSTERS

# Use the elbow method to decide the number of clusters.
x <- c(1:10)
sort_height <- sort(hc.w$height, decreasing = TRUE)    
y <- sort_height[1:10]

# ELBOW PLOT 
plot(x, y, type = "b", main = "Elbow plot", xlab = "Clusters", ylab = "WCV")   
lines(x, y, col = "blue")

#This method involves plotting a graph between the number of clusters 
#against the variance or inertia (within clusters sum of squared distances WCSS value) 
#as a function of the number of clusters.

#It can be clear from the graph - after 3 cluster further increase of the number of clusters 
#almost does not change the WCV value. 
#Therefore, this value can be taken as the optimum k value

plot(hc.w, main = "Cluster Dendrogram", xlab = "Observations", ylab = "Height")
rect.hclust(hc.w, k = 4, border = 2:5)

# Validation of the results, using other methods

# Average Silhouette Method
fviz_nbclust(SmartWatch_dfz, FUN = hcut, method = "silhouette")
#According to the results Average Silhouette Method the optimal number 
#of clusters should be 2

# Gap statistics
gap.stat <- clusGap(SmartWatch_dfz, FUN = hcut, K.max = 10, B = 50)
gap.stat
fviz_gap_stat(gap.stat)


# 30 indexes 

NbClust(SmartWatch_dfz, method = 'ward.D2', index = 'all')$Best.nc

#CHOOSING THE OPTIMAL NUMBER OF INDEXES

clusters_4 <- cutree(hc.w, k = 4)
dunn_index_4 <- dunn(distance = distance, clusters = clusters_2)
print(dunn_index_4)

clusters_1 <- cutree(hc.w, k = 3)
dunn_index_1 <- dunn(distance = distance, clusters = clusters_1)
print(dunn_index_1)

clusters_2 <- cutree(hc.w, k = 2)
dunn_index_2 <- dunn(distance = distance, clusters = clusters_2)
print(dunn_index_2)

clusters_10 <- cutree(hc.w, k = 10)
dunn_index_10 <- dunn(distance = distance, clusters = clusters_2)
print(dunn_index_10)

# Dunn index for 3 clusters is slightly more. Thus, the data will be segmented
# into 3 segments.



# CUT DENDROGRAM INTO 4 CLUSTERS
cluster_final <- cutree(hc.w, k = 4)     
# Display cluster assignments

#CLUSTER PLOT
 
fviz_cluster(list(data = SmartWatch_df, cluster = cluster_final))

df_final <- cbind(SmartWatch_df, cluster_final)
View(df_final)


# CALCULATE SEGMENT SIZES
proportions <- table(df_final$cluster_final) / nrow(df_final)     
percentages <- proportions * 100
# Display segment sizes in percentages
print(percentages)    

# EXPLORE MEAN VALUES OF VARIABLES IN EACH CLUSTER
segments<-
  df_final %>% 
  group_by(cluster_final) %>% 
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# Display the calculated means
segments


write.xlsx(segments, 'segments_1.xlsx')


