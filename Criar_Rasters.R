
library(raster)
library(sp)
library(spatialEco)
library(SDMTools)
library(plotrix)

setwd("C:/Users/SAMSUNG/Desktop/Raster")
# Abre o raster
r2=raster(x ="Raster2_PRODES_Exercicio_UTM.tif" )
r2
plot(r2)
# Transforma esse raster (paisagem) em raster binário
r2=calc(r2, fun=function(x) { x[x != 8] <- 0; return(x) } ) ## tudo que não for 8 vai ficar zero 
r2=calc(r2, fun=function(x) { x[x == 8] <- 1; return(x) } ) ## tudo que for 8 vai ser um
plot(r2)

#seleciona ponto central para criacao de duas paisagens
x=sampleRegular(r2, 3, na.rm = TRUE, sp = TRUE) ##seleciona número(3) de pontos que eu quero de forma regular, o na.rm tira os NA´s se tiver
x@coords ##pra ver os pontos que ele plotou pq o objeto é simplepointdataframe
points(x)

#raios das extensoes a serem analisadas
rads=c(100,500,seq(1000,13000,1000))
rads

#Cria o objeto que guardara os resultados
results=list()

# roda a analise ### função loop
for(i in 1:length(rads)){
  results[[i]]=land.metrics(x=x, y=r2, bw=rads[i], bkgd = NA, metrics = c(2,3,4,6,9,10))
}
names(results)=rads
results

#Criando uma tabela mais amigavel para analise
res=results[[1]][[2]][1,]
res2=results[[1]][[2]][2,]
for(i in 2:15){
  res=rbind(res,results[[i]][[2]][1,])
  res2=rbind(res2,results[[i]][[2]][2,])
}

res=data.frame(res,raio=rads)
res
res2

for(i in 2:7){
plot(rads,res[,i],xlab="Raio",ylab=colnames(res)[i])
lines(rads,res[,i])
points(rads,res2[,i],col=2)
lines(rads,res2[,i],col=2)
}

plot(r2)
points(x,pch=16)
draw.circle(x@coords[2,1],x@coords[2,2],100)


###############################################################################################################################

r3=raster(x="Raster2_PRODES_Exercicio_UTM_120.tif") #2
r4=raster(x="Raster2_PRODES_Exercicio_UTM_240.tif") #4
r5=raster(x="Raster2_PRODES_Exercicio_UTM_480.tif") #8
r6=raster(x="Raster2_PRODES_Exercicio_UTM_960.tif") #16

r3=calc(r3, fun=function(x) { x[x != 8] <- 0; return(x) } )
r3=calc(r3, fun=function(x) { x[x == 8] <- 1; return(x) } )
r4=calc(r4, fun=function(x) { x[x != 8] <- 0; return(x) } )
r4=calc(r4, fun=function(x) { x[x == 8] <- 1; return(x) } )
r5=calc(r5, fun=function(x) { x[x != 8] <- 0; return(x) } )
r5=calc(r5, fun=function(x) { x[x == 8] <- 1; return(x) } )
r6=calc(r6, fun=function(x) { x[x != 8] <- 0; return(x) } )
r6=calc(r6, fun=function(x) { x[x == 8] <- 1; return(x) } )

results2=list()
results3=list()
results4=list()
results5=list()

j=1
for(i in c(3,12)){ ##POSIÇÃO DO RAIO 1000 E 10000)
  results2[[j]]=land.metrics(x=x, y=r3, bw=rads[i], bkgd = NA, metrics = c(2,3,4,6,9,10))
  j=j+1
}
j=1
for(i in c(3,12)){
  results3[[j]]=land.metrics(x=x, y=r4, bw=rads[i], bkgd = NA, metrics = c(2,3,4,6,9,10))
  j=j+1
}
j=1
for(i in c(3,12)){
  results4[[j]]=land.metrics(x=x, y=r5, bw=rads[i], bkgd = NA, metrics = c(2,3,4,6,9,10))
  j=j+1
}
j=1
for(i in c(3,12)){
  results5[[j]]=land.metrics(x=x, y=r6, bw=rads[i], bkgd = NA, metrics = c(2,3,4,6,9,10))
  j=j+1
}

res3=results[[3]][[2]][1,]
res3=rbind(res3,results[[12]][[2]][1,])
res3=rbind(res3,results2[[1]][[2]][1,])
res3=rbind(res3,results2[[2]][[2]][1,])
res3=rbind(res3,results3[[1]][[2]][1,])
res3=rbind(res3,results3[[2]][[2]][1,])
res3=rbind(res3,results4[[1]][[2]][1,])
res3=rbind(res3,results4[[2]][[2]][1,])
res3=rbind(res3,results5[[1]][[2]][1,])
res3=rbind(res3,results5[[2]][[2]][1,])
res3=data.frame(res3,raio=rep(c(1000,10000),5))
res3

res3.2=results[[3]][[2]][2,]
res3.2=rbind(res3.2,results[[12]][[2]][2,])
res3.2=rbind(res3.2,results2[[1]][[2]][2,])
res3.2=rbind(res3.2,results2[[2]][[2]][2,])
res3.2=rbind(res3.2,results3[[1]][[2]][2,])
res3.2=rbind(res3.2,results3[[2]][[2]][2,])
res3.2=rbind(res3.2,results4[[1]][[2]][2,])
res3.2=rbind(res3.2,results4[[2]][[2]][2,])
res3.2=rbind(res3.2,results5[[1]][[2]][2,])
res3.2=rbind(res3.2,results5[[2]][[2]][2,])
res3.2=data.frame(res3.2,raio=rep(c(1000,10000),5))
res3.2

resol=c(60,120,240,480,960)
plot(resol, res3[which(res3$raio==1000),7])
points(resol, res3[which(res3$raio==10000),7],col=2)
plot(resol, res3[which(res3$raio==10000),7],col=2)
