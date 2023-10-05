library(factoextra)
library(psych)
library(relimp, pos = 4)
library(paran)


data("iris")
str(iris)

pairs.panels(iris[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[iris$Species],
             pch=21)

res.pca <- prcomp(iris[,-5],
             center = TRUE,
             scale. = TRUE)
attributes(res.pca)
print(res.pca)

#DETERMINAÇÃO DO NÚMERO DE COMPONENTES
var<-fviz_eig(res.pca)
var$data

paran(iris[,1:4], iterations = 5000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = FALSE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

pairs.panels(res.pca$x,
             gap=0,
             bg = c("red", "yellow", "blue")[iris$Species],
             pch=21)

#Graficos de Resultados
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca,axes = c(1, 3),
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

groups <- as.factor(iris$Species)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("red",  "blue","green"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)
