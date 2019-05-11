### Instalar e carregar pacotes
library(raster)
library(rgdal)
library(sp)



#definir  a pasta onde está o arquivo de queimadas
setwd("C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/1 semana/ARQUIVOS/PROGRAMA_QUEIMADAS_INPE")


#Abrir arquivo de queimadas

list.files()

focos2 <- read.csv("Focos.csv", sep = ";", dec = ",", head = T)

focos2
head(focos2)
str(focos2)
summary(focos2)
names(focos2)

#Coordenadas

coordinates(focos2) <- focos2[ ,11:10]
class(focos2)
head(focos2)

# Abrir arquivos do Worldclim

setwd("C:/Users/Keren Vasconcelos/Desktop/Keren/ITV/Análises Espaciais/2 semana/Base/wc10")
list.files()

current.list <- list.files(pattern =".bil$", full.names=TRUE)
current.list
Bioclimate <- stack(current.list)  #faz sanduíche de camadas
names(Bioclimate)

#Criar o extent focos

extent(focos2)

## Criar 100 coordenadas aleatÃ³rias dentro do raster


Longitude <- runif(1000,  -58.21949, -46.24868)
Latitude <- runif(100,  -9.78244, 2.26166)
C <- cbind(Longitude, Latitude)
class(C)
C <- as.data.frame(C)
class(C)


summary(C)


### Extrair os valores de todas as camadas de Worldclim para focos
Varfocos <- extract(Bioclimate, focos2)
Varfocos

#Salvando

write.csv(Varfocos, "Varfocos2.csv", row.names = F)

### Extrair os valores de todas as camadas de Worldclim para aleatorias
Varaleat <- extract(Bioclimate, C)
Varaleat 

#Salvando

write.csv(Varaleat, "Varaleat2.csv", row.names = F)

#Media entre variaveis

apply(Varfocos, 2, mean)
apply(Varaleat, 2, mean)

#Tirar NA

Varaleat <- Varaleat[complete.cases(Varaleat),]
summary(Varaleat)

#Novo apply
apply(Varaleat, 2, mean)


#############################################################
