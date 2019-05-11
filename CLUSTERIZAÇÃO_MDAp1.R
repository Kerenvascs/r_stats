################################################################
#
# 
# 
source("http://bioconductor.org/biocLite.R")
biocLite("multtest", dependencies=TRUE)
#
# MDA
############ CLUSTERIZA��O

#########################################################################################################
# Cyclin genes tem um papel importante em leucemia. 
# Use o dataset “golub” para identificar se o perfil de expressão gênica de Cyclin genes são similares. 
# Calcule uma matriz de distancia euclideana para avaliar a similaridade. Use a função “dist”.
#########################################################################################################
##
## Instalar pacote multtest
##
source("https://bioconductor.org/biocLite.R")
biocLite("multtest")

library(multtest); data(golub)
dim(golub)
head(golub)

#criar conjunto com as posi��es dos pacientes que tenham o gene cyclin
index <- grep("Cyclin",golub.gnames[,2]) # indice seleção #procurar cyclin na coluna 2
golub.gnames[index,2]
golub.names

str(golub)
head(golub)
##calcular matriz de dist a partir da matriz de dados, baseado no metodo de distancia euclidiana
dist.cyclin <- dist(golub[index,],method="euclidian")
dist.cyclin

#matriz
diam <- as.matrix(dist.cyclin)
diam

#renomear colunas da matriz de acordo com index
rownames(diam) <- colnames(diam) <- golub.gnames[index,3]# renomeia linhas e cols.
diam[1:5,1:5]

#fazer heatmap, sem normaliza��o, com tamanho de matriz definida
heatmap(diam, scale="none",margin=c(10,10)) # heatmap da matriz de distancia
################# a fun��o padr�o para gerar o heatmap � "hclust", que gera clus hierarquico, no entanto a fun��o heatmap j� cont�m essa fun��o dentro dela.

#########################################################################################################	
# Use a matriz de similaridade acima para encontrar módulos de expressão gênica. 
# Recupere os módulos a partir de uma solução de clustering hierárquico. 
# Execute a função “hclust” com o critério “single-linkage”.
#########################################################################################################
##
## Instalar pacote clValid
##
install.packages("clValid")
install.packages("cluster")
library(cluster)

cluster=hclust(as.dist(diam),method="single")
plot(cluster)
plot(hclust(dist.cyclin,method="single"),labels=golub.gnames[index,2], cex = 0.5)	

rect.hclust(cluster, 2)

plot(silhouette(
  cutree(hclust(dist.cyclin,method="single"),4),
  dist.cyclin))

# DUNN index
library(clValid)
clutree <- cutree(cluster,3)
didx <- dunn(as.dist(diam), clutree)
plot(hclust(dist.cyclin,method="single"),labels=golub.gnames[index,2], cex = 0.5)
rect.hclust(cluster, k=3, border="red") 
#fazer transposi��o (transposta) da matriz fun��o t()
#########################################################################################################	
#	Utilize o dataset “golub” e verifique se a expressão gênica dos genes 
# CCND3 Cyclin D3 e Zyxin são capazes de diferenciar os dois 
# grupos de pacientes (ALL=27 e AML=11).
#########################################################################################################	

data(golub, package="multtest")
clusdata <- data.frame(golub[1042,],golub[2124,])#cortar o dataframe (para definir eu vou primeiramente buscar as linhas onde as minhas variaveis de interesse se encontram)
head(clusdata)
colnames(clusdata)<-c("CCND3 Cyclin D3","Zyxin") ###colocarnomes
gol.fac <- factor(golub.cl,levels=0:1, labels= c("ALL","AML"))##criar uma variavel que chamamos de fator, mas na verdade vamos substituir 0 ou 1 por all ou aml
gol.fac


#gerar plot, onde pch � um requisito do plote, que indica qual legenda vou empregar para cada variavel
plot(clusdata, pch=as.numeric(gol.fac))#atribui simbolo
legend("topright",legend=c("ALL","AML"),pch=1:2)#gera legenda


#plotar dedograma , onde a distancia � euclidiana, o metodo � pela m�dia e labels � o r�tulo.
plot(hclust(dist(clusdata,method="euclidian"),method="average"),labels=gol.fac)

#########################################################################################################		
# Use o método k-means de clustering para explorar o perfil de expressão dos genes 
# CCND3 Cyclin D3 e Zyxin. Utilize “k=2” para identificar se os dois agrupamentos de pacientes 
# (ALL e AML) podem ser divididos a partir do perfil de expressão destes genes.
# to cope with suboptimal solutions use nstart=10
#########################################################################################################	

cl.out <- kmeans(clusdata, 2, nstart=10)#escolher 10 valores aleatorios e dentro desses dez escolher 2 para serem os centroides
cl.out
plot(clusdata, col = cl.out$cluster, pch=as.numeric(gol.fac))
# plot centroids
points(cl.out$centers, col = 1:2, pch = 8, cex=2)

#########################################################################################################
# Verifique se “k=2” é ótimo. Lembre que o método “k-means” busca uma solução (partições) 
# onde a soma residual dos quadrados de cada cluster é minimizada em função dos “k” clusters.
#########################################################################################################
##########AVALIAR  A QUESTAO DO K

###calcular o residual
wss <- (nrow(diam)-1)*sum(apply(diam,2,var)) # rodar para cada valor de k
wss
# try k from 2 to 15, updates wss (within cluster sum of squares)#o k vai variar de 2 a 15 e para cada um vou ter que calcular o valor residual
for (i in 2:8) wss[i] <- sum(kmeans(diam, centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#########################################################################################################
# Utilize o perfil de expressão gênica dos genes MCM3 (dataset golub), 
# que tem um papel importante na replicação de genomas eucarioto, e avalie a força da 
# correlação entre estes genes. Use a função “cor” para calcular a correlação de pearson.
#########################################################################################################
MCM3 <- grep("MCM3",golub.gnames[,2],ignore.case = TRUE)
golub.gnames[MCM3,2]
x <- golub[2289,]; y <- golub[2430,]
plot(x,y) # plot
abline(lsfit(x,y)) # linha regressão
ls.print(lsfit(x,y)) # exibe regressão
ts.plot(cbind(golub[2289,],golub[2430,]), lty=1:2, col=(1:2)) # series plot
legend("topright",legend=c("MCM3-1","MCM3-2"),lty=1:2,col=(1:2))
cor(x,y) 
cor.test(x,y) # intervalo de confiança
