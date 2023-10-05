#CRIANDO AS MATRIZES A E B:
A <- matrix(c(2,6,0,4,1,2,3,4,1),nrow = 3,ncol = 3)
print(A)
B <- matrix(c(2,3,5,6,5,3,8,2,1),nrow = 3,ncol = 3)
print(B)

#DIMENSAO DAS MATRIZES:
dim(A)
dim(B)

#NUMEROS DE LINHAS E COLUNAS:
nrow(A)
ncol(B)

#TRANSPOSTA DA MATRIZ:
t(A)
t(B)

#CRIANDO UMA MATRIZ NULA:
C <- matrix(0,nrow = 3,ncol = 3)
print(C)

#DIAGONAL DE UMA MATRIZ:
diag(A)
diag(B)

diag(c(1,1,1))

#TRACO DA MATRIZ (SOMA DA DIAGONAL):
sum(diag(A))
sum(diag(B))

#ADICAO DE MATRIZES:
A+B

#SUBTRACAO DE MATRIZES:
A-B

#MULTIPLICACAO POR UM ESCALAR:
K <- 3
K*A
K*B

#MULTIPLICACAO ENTRE MATRIZES:
A %*% B

#DETERMINANTE DE UMA MATRIZ:
det(A)
det(B)

#INVERSA DE UMA MATRIZ:
solve(A)
solve(B)

#MATRIZ DE AUTO-VALORES E AUTO-VETORES (DECOMPOSICAO ESPECTRAL):
eigen(A)
eigen(B)

#MEDIA DAS COLUNAS:
colMeans(A)
colMeans(B)

#MEDIA DAS LINHAS:
rowMeans(A)
rowMeans(B)

#SOMA DAS COLUNAS:
colSums(A)
colSums(B)

#SOMA DAS LINHAS:
rowSums(A)
rowSums(B)

#MATRIZ DE COVARIANCIA:
cov(A)
cov(B)

#MATRIZ DE CORRELACAO:
cor(A)
cor(B)

#MATRIZ PADRONIZADA:
Z <- (A-mean(A))/sd(A)
print(Z)


M <- (A-colMeans(A))/sd(A)
