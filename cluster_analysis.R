library(dplyr)
library(cluster)
library(factoextra)

data("USArrests") 
df1 <- USArrests   
data("iris") 
df2<-iris[-5]

df1 <- na.omit(df1)
df2 <- na.omit(df2)


df_s1 <- scale(df1)
df_s2<-scale(df2)
mean(df_s1[,1])
var(df_s1[,1])

dist.eucl1 <- get_dist(df_s1, method = "euclidean")
dist.eucl2 <- dist(df_s2, method = "euclidean")

fviz_dist(dist.eucl1)


######K-MEANS#############################
fviz_nbclust(df_s1, kmeans,
             method = "gap_stat")
set.seed(123)
km.res <- kmeans(df_s1, 3, nstart = 25)

# Visualize

fviz_cluster(km.res, data = df_s1, 
             ellipse.type = "norm",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())


#######hierarchical clustering#################


res.hc <- hclust(d = dist.eucl1, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5)
# Compute cophentic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and
# the original distance
cor(dist.eucl1, res.coph)

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)

rownames(df_s1)[grp == 4]

fviz_dend(res.hc,k=4)


library(dendextend)

# Compute 2 hierarchical clusterings
hc1 <- hclust(dist.eucl1, method = "complete")
hc2 <- hclust(dist.eucl1, method = "ward.D2")
hc3 <- hclust(dist.eucl1, method = "average")
# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
dend3 <- as.dendrogram (hc3)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2,dend3)
# Align and plot two dendrograms side by side
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()                       # Draw the two dendrograms

# Compute alignment quality. Lower value = good alignment quality
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  entanglement()  

dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% 
  tanglegram(
    highlight_distinct_edges = FALSE, # Turn-off dashed lines
    common_subtrees_color_lines = FALSE, # Turn-off line colors
    common_subtrees_color_branches = TRUE # Color common branches 
  )

# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")

cor_cophenetic(dend1, dend2)


# Create multiple dendrograms by chaining
dend1 <- df_s1 %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df_s1 %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df_s1 %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- df_s1 %>% dist %>% hclust("centroid") %>% as.dendrogram
dend5 <- df_s1 %>% dist %>% hclust("ward.D2") %>% as.dendrogram
# Compute correlation matrix
dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4, "Ward" = dend5)
cors <- cor.dendlist(dend_list)
# Print correlation matrix
round(cors, 2)

library(corrplot)
corrplot(cors, "pie", "lower")


data <- scale(USArrests)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend)

# 1. Create a customized dendrogram
mycols <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
dend <-  as.dendrogram(hc) %>%
  set("branches_lwd", 1) %>% # Branches line width
  set("branches_k_color", mycols, k = 4) %>% # Color branches by groups
  set("labels_colors", mycols, k = 4) %>%  # Color labels by groups
  set("labels_cex", 0.5) # Change label size

# 2. Create plot 
fviz_dend(dend) 


#heatmap
heatmap(df_s2, scale = "none")

library("pheatmap")
pheatmap(df_s2, cutree_rows = 3)

library("ComplexHeatmap")
Heatmap(df_s2, 
        name = "iris", #title of legend
        column_title = "Variables", row_title = "Samples",
        row_names_gp = gpar(fontsize = 7) # Text size for row names
)

# Heatmap 1
ht1 = Heatmap(df_s2, name = "ht1", km = 3,
              column_names_gp = gpar(fontsize = 9))
# Heatmap 2
ht2 = Heatmap(df_s2, name = "ht2",clustering_method_rows = "ward.D2", 
              col = circlize::colorRamp2(c(-2, 0, 2), c("green", "white", "red")),
              column_names_gp = gpar(fontsize = 9))
# Combine the two heatmaps
ht1 + ht2


# Elbow method
fviz_nbclust(df_s2, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df_s2, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df_s2, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

install.packages(c("factoextra", "fpc", "NbClust"))
library(factoextra)
library(fpc)
library(NbClust)


# Excluding the column "Species" at position 5
df <- iris[, -5]
# Standardize
df <- scale(df)
# K-means clustering
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())
# Hierarchical clustering
hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean", 
                 hc_method = "ward.D2", graph = FALSE)

# Visualize dendrograms
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)
# Statistics for k-means clustering
km_stats <- cluster.stats(dist(df),  km.res$cluster)
# Dun index
km_stats$dunn


###########################################
# Compute cluster stats
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df), 
                             species, km.res$cluster)
# Corrected Rand index
clust_stats$corrected.rand

# Agreement between species and pam clusters
pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
table(iris$Species, pam.res$cluster)
cluster.stats(d = dist(df), 
              species, pam.res$cluster)$vi
# Agreement between species and HC clusters
res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
cluster.stats(d = dist(df), 
              species, res.hc$cluster)$vi
