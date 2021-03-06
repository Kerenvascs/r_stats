#########################################################################################################
# ---> Quiz
#
# Utilizanto o dataset mtcars
# 1) Como os modelos de carros podem ser agrupadas
# 1.1) Exemplifique usando agrupamento hierárquicos e k-means
# 1.2) Quantos grupos? Compare os grupos, qual o valor ótimo? Use visualização
# 2) Transforme os dados com base em PCA. É poss�?vel uma redução de dimensionalidade? Quantos PCs?
# 2.1) Represente os resultados da clusterização com visualização
#
#########################################################################################################
install.packages("cluster")
library(cluster)

data(mtcars)
mtcars
head(mtcars)
str(mtcars)
summary(mtcars)
log.mtcars <- log(mtcars[, 1:7])#normaliza��o de dados
class(log.mtcars)


#QUESTAO 1
#1.1
##calcular matriz de dist a partir da matriz de dados, baseado no metodo de distancia euclidiana
dist.mtcars <- dist(log.mtcars[,],method="euclidian")#matriz de distancia
dist.mtcars
class(dist.mtcars)

#matriz
matriz <- as.matrix(dist.mtcars)#matriz de dados
matriz
class(matriz)

#fazer heatmap, sem normaliza��o, com tamanho de matriz definida
heatmap(matriz, scale="none",margin=c(10,10)) # heatmap da matriz de distancia

#Fazer cluster hier�rquico
cluster=hclust(as.dist(matriz),method="single")
plot(cluster)

cluster2=hclust(as.dist(matriz),method="average")
plot(cluster2)

cluster3=hclust(as.dist(matriz),method="complete")
plot(cluster3)


##plot do Cluster2 - Average
rect.hclust(cluster2, 2)


cl.out <- kmeans(log.mtcars, 2, nstart=10)#escolher 10 valores aleatorios e dentro desses dez escolher 2 para serem os centroides
cl.out

#1.2
###calcular o residual
wss <- (nrow(matriz)-1)*sum(apply(matriz, 2,var)) # rodar para cada valor de k #quanto menor for o valor, mais coesos s�o os dados
wss

for (i in 2:7) wss[i] <- sum(kmeans(matriz, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


#Silhouette
plot(silhouette(
  cutree(hclust(dist.mtcars, method="average"),2),
  dist.mtcars))

#####resposta: Ap�s visualizar os resultados da Silhouette, foi poss�vel estabelecer 2 grupos.


# 2) Transforme os dados com base em PCA. É poss�?vel uma redução de dimensionalidade? Quantos PCs?
# 2.1) Represente os resultados da clusterização com visualização
colnames(mtcars)
#fun��o que roda o pca prcomp
log.pca <- prcomp(log.mtcars,
                 center = TRUE,
                 scale. = TRUE)

print(log.pca) # retorna o desvio padrão de cada componente, rotação / scores
plot(log.pca, type = "l") # plot variancia, onde type l � tipo linha
summary(log.pca) # variancia acumulada

# "Clustering" via PCA using Comp1 x Comp2.
biplot(log.pca,pc.biplot=TRUE,cex=0.5,expand=0.8)

#####################################
# >> Melhorando a visualizacao do biplot #outra forma de fazer o pca
#####################################
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
 
g <- ggbiplot(log.pca, obs.scale = 1, var.scale = 1, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


###resposta = � poss�vel reduzir a dimensionalidade para 2 PCS, definidos com base na propor��o de vari�ncia
############# ou seja, PC1 = 75% e PC2 = 16%
