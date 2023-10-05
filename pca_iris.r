install.packages(c("FactoMineR", "factoextra", "psych", "relimp", "paran"))

library("FactoMineR")
library("factoextra")
library("psych")
library("relimp", pos=4)
library("paran")

data(iris)
str(iris)

pairs.painel(iris[,-5], gap= 0, bg = c("red", "yellow", "blue")[iris$species], pch=21)

res.pca <- prcomp(iris[,-5], center = TRUE, scale = TRUE)

#results for variables
res.var <- get_pca_var(res.pca)

res.var$coord  #coordenadas
res.var$cos2  # qualidade da representação
res.var$contrib # contribuição para PCs

attributes(res.pca)
print(res.pca)

#determina
var <- fviz_eig(res.pca)
var$data

#screen plot
fviz_eig()

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
             )
