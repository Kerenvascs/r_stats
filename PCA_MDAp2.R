################################################################
#
# 
source("http://bioconductor.org/biocLite.R")
biocLite("multtest", dependencies=TRUE)

# MDA #################################### PCA
################################################################

#########################################################################################################
# ---> PARTE A
# A analise de PCA permite encontrar este n√?vel de depend√™ncia entre as vari√°veis, 
# al√©m de decompor a variabilidade entres componentes. 
# Use o conjunto IRIS dataset
#########################################################################################################

data(iris)
head(iris, 3)
pairs(iris[,-5])

#normalizar dados
log.ir <- log(iris[, 1:4]) ## transformada / log
ir.species <- iris[, 5]

#funÁ„o que roda o pca prcomp
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE)
print(ir.pca) # retorna o desvio padr√£o de cada componente, rota√ß√£o / scores
plot(ir.pca, type = "l") # plot variancia, onde type l È tipo linha
summary(ir.pca) # variancia acumulada

# "Clustering" via PCA using Comp1 x Comp2.
biplot(ir.pca,pc.biplot=TRUE,cex=0.5,expand=0.8)

#####################################
# >> Melhorando a visualizacao do biplot #outra forma de fazer o pca
#####################################
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)

#a funÁ„o groups = quantos grupos que tenho, se j· estiver separado previamente
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

#####################################
# >> Visualizacao dos scores I
#####################################
library(reshape2) #biblioteca que transforma dados
library(ggplot2)
var <- colnames(iris)[-5]
# flatten data
melted <- cbind(var, melt(ir.pca$rotation[,1:2]))
melted#trasformando o formato do dado, usa rotation que È um dos atributos que constam nos dados na pca
# plot
barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=var), stat="identity") +
  facet_wrap(~Var2)
barplot

#####################################
# >> Visualizacao dos scores II
#####################################
###
comp <- data.frame(ir.pca$x[,1:2])
# same as above
# predict(ir.pca)[,1:2]
# Plot top-100
plot(comp, pch=as.numeric(ir.species), col=as.numeric(ir.species))

#####################################
# >> Visualizacao hclust 2-PCS
#####################################
plot(hclust(dist(comp)),labels=iris$Species, col=as.numeric(ir.species))

# Plot dataset sem PCA
plot(hclust(dist(iris[,-5])),labels=iris$Species)

#####################################
## >> Validacao da solucao de clusters 
#####################################

library(clValid)
intern <- clValid(iris[,-5], 2:5, clMethods=c("hierarchical","kmeans","pam"), validation="internal")
summary(intern)

## -- plot -- ##
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#####################################
### >> Validacao da solucao de clusters
### >> usando 2-PCS
#####################################

library(clValid)
intern <- clValid(comp, 2:5, clMethods=c("hierarchical","kmeans","pam"), validation="internal")
summary(intern)

## -- plot -- ##
op <- par(no.readonly=TRUE)
par(mfrow=c(2,2),mar=c(4,4,3,1))
plot(intern, legend=FALSE)
plot(nClusters(intern),measures(intern,"Dunn")[,,1],type="n",axes=F, xlab="",ylab="")
legend("center", clusterMethods(intern), col=1:9, lty=1:9, pch=paste(1:9))
par(op)

#########################################################################################################
# ---> EXTRA
#
# A analise de RDA/PCA permite encontrar este n√?vel de depend√™ncia entre as vari√°veis, 
# al√©m de decompor a variabilidade entres componentes. 
# Use o conjunto IRIS dataset c/ pacote VEGAN
#
#########################################################################################################

require(vegan)#pacote 
pca <- rda(iris[,-5]) # RDA=analise de redundancia, a vantagem em relaÁ„o a pca È que se eu tiver um vetor com parametros de metadado eu posso correlacionar com base nesses parametros.
#o vetor da a condiÁ„o de correlaÁ„o  atraves de um regress„o de cada parametro com relaÁ„o a matriz de abundancia^??EX parametros fisico quimicos
plot(pca, type = 'n', display = 'sites')
cols <- c('red', 'blue', 'green')

points(pca, display='sites', col = cols[iris$Species], pch = 16)
ordihull(pca, groups=iris$Species)
ordispider(pca, groups = iris$Species, label = TRUE)




