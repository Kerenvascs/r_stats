###################################################### Experimental Design #####################################

##################################### Power analysis
library(pwr)

#####EFECT SIZE
#Magnitude do efeito = for�a da correla��o, for�a do efeito.

#Baixa magnitude = 0.2
cohen.ES(test="t", size="small")
#M�dia Magnitude = 0.5
cohen.ES(test="t", size="medium")
#Ampla magnitude = 0.8
cohen.ES(test="t", size="large")

### T-test
data(CO2)
head(CO2)
dim(CO2)
unique(CO2$Treatment)
table(CO2$Treatment)

plot(uptake~Treatment, data=CO2)
# mOSTRA QUE O CHILED TEM CONSUMO MAIOR

t.test(uptake~Treatment, data=CO2)

## cohen.d = |mean1 - mean2| / sd
NC <- subset(CO2, Treatment == "nonchilled")
C <- subset(CO2, Treatment=="chilled")
NC
C
cohen.d  <- abs(mean(AU$mpg)) - 

# m�DIA DO GRUPO 1 - M�DIA DO GRUPO 2 E DIVIDE PELO DESVIO PADR�O. uptake � o nome da coluna
cohen.d <- abs(mean(CO2[CO2$Treatment=="nonchilled", "uptake"]) - mean(CO2[CO2$Treatment=="chilled", "uptake"]))/ sd(CO2$uptake)
cohen.d

#TESTE PRA CALCULAR POTENCIA DO T TEST, ONDE D = MAGNITUDE DO EFEITO, TWO SAMPLE PQ DUAS AMOSTRAS ESTAO SENDO COMPARADAS
pwr.t.test(n = 42, d = cohen.d, sig.level = 0.05, power = NULL, type = "two.sample")

#mesma coisa de cima, no entanto agora n�o quero calcular o ppower e sim o tamanho amostral �timo para ter um power de 80%
optimal.sample.t <- pwr.t.test(n = NULL, d = cohen.d, sig.level = 0.05, power=0.80, type = "two.sample")
optimal.sample.t
plot(optimal.sample.t, xlab="sample size per group")

### ANOVA
data(iris)
head(iris)
table(iris$Species)
plot(Sepal.Length~Species, data=iris)

#Variavel de resposta, til, varial preditora
#o p da anova mostra se a hipotese � rejeitada ou n�o, ou seja se tem ou n�o diferencas de septal leght para cada especie
lm <- lm(Sepal.Length~Species, data=iris)
anova(lm)
#Residual � a varia��o para cada especie

## Eta2 <- SumSq(V1) / SumSq(Total)
#testa o efeito de magnitude do teste da anova
#olha a anova
Eta2 <- 63.212 / (63.212 + 38.956)
Eta2
# k = quantas especies, n = quanto, sig = significancia
pwr.anova.test(k = 3, n =50 , f = Eta2, sig.level = 0.05, power = NULL)

#Quantidade de amostras ideal = definir o espa�o amostral
optimal.sample.anova <- pwr.anova.test(k = 3, n = NULL , f = Eta2, sig.level = 0.05, power = 0.8)
optimal.sample.anova
plot(optimal.sample.anova)

##### Correlations
data(airquality)
head(airquality,10)
dim(airquality)

plot(Temp ~ Wind, data=airquality)
abline(lm(Temp ~ Wind, data=airquality), col="red")
cor.test(airquality$Temp, airquality$Wind)
#potencia da correla��o
pwr.r.test(n = 153, r = -0.4579879, sig.level = 0.05, power = NULL, alternative = "two.sided")

#espa�o amostral
optimal.sample.cor <- pwr.r.test(n = NULL, r = -0.4579879, sig.level = 0.05, power = 0.8, alternative = "two.sided")
optimal.sample.cor
plot(optimal.sample.cor)

### Curva de acumulação de espécies
library(vegan)

?BCI
data(BCI)
# cada coluna � uma especie de arvore e cada linha � uma parcela (tem ou nao)
head(BCI)
dim(BCI)

#pega aleatoriamente 2 parcelas de 50 e calcula a riqueza e o desvio padr�o
#specacum � a fun��o para rodar a curva de acumula��o das especies, o ramdom � para rodar aleatoriamente
sp <- specaccum(BCI, method="random")
sp
plot(sp)
#quando a curva se estabiliza mostra que o numero de especies foi suficiente o numero de amostras para identificar a riqueza de especies

boxplot(sp)
#ci.lty = tipo de linha , col=cor
#curva de acumula��o de especies
plot(sp, ci.type="poly", lwd=2, ci.lty=0, col="blue", ci.col="lightblue")


