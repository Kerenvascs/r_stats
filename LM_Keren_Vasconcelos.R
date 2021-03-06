
####################################### Linear Models #########################

####################################### Simple linear regression ##############
install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest)
install.packages("nlme")
library(nlme)
install.packages("MASS")
library(MASS)



##Load data
data(faithful)
head(faithful)
?faithful



##P1: Estimar a duração da próxima erupção se o tempo de espera desde a última erupção foi de 80 minutos. Use coefficients(eruption.lm)
eruption.lm <- lm(eruptions ~ waiting, data=faithful)
summary(eruption.lm)
coefficients(eruption.lm)

fitted(eruption.lm)  #preddicted y values

plot(faithful)

#forma1:
duration = coefficients(eruption.lm)[1] + coefficients(eruption.lm)[2]*80
duration

#forma 2 : fun��o predict
newdata =data.frame(waiting=80)
newdata
predict(eruption.lm, newdata)


##P2: Encontrar o R2 (coeficiente de determinação)
summary(eruption.lm)
####Multiple R-squared:  0.8115, ou seja 81%

##P3: Determinar se tem uma relação significativa entre a duração das erupções e o tempo de espera, e graficar essa relação
summary(eruption.lm)
plot(eruptions ~ waiting, data=faithful)


abline(a = -1.874016, b = 0.075628, col="red")

##P4: Estandarize a variável preditora (waiting) usando scale (var, scale=TRUE, center=TRUE) e faça histogramas com a variável original e a transformada
hist(faithful$waiting)
hist(scale(faithful$waiting, scale =T, center=T))


##P5: Determinar se a relação entre a duração das erupções e o tempo de espera, muda quando utilizamos a variável waiting estandarizada
eruption.lm2 = lm(eruptions ~ scale(waiting, scale=T, center = T), data=faithful)
summary(eruption.lm2)
summary(eruption.lm)


##P6: Avaliar os residuos do modelo, graficando residuals vs fitted values, e checando a normalidade dos residuos

plot(eruption.lm)
eruption.lm$residuals

res <- resid(eruption.lm)
res
hist(res)

faithful$residuals <- resid(eruption.lm)
head(eruption.lm)


##P7: Checar a independência dos residuos, usando acf (auto correlation function)

acf(res) # a primeira barra sempre vai ser 1 pq � a correla��o dela com ela mesma
# a segunda barra mostra que faz sentido o tempo de espera entre uma erup��o e outra, ou seja h� um grau de dependencia
#dentro do tracejado azul s�o independentes, fora do tracejado s�o dependentes
# o grafico mostra que o tempo de espera entre uma erup��o e a seguinte esta correlacionado negativamente, ou seja quanto mais esperei por 1, a 2 sera menor.


################################## Multiple Regression ########################

data(airquality) ## LM - Multiple regression
head(airquality)
?airquality

##P1: Avaliar correlações entre as variáveis preditoras (multicolinearidade), numérica e gráficamente
cor(airquality[ , 1:3])
### remover nas
airquality2 <- airquality[complete.cases(airquality), ]
airquality2

cor(airquality2[ , 1:3]) #wind*ozone

pairs(airquality2[ , 1:3])
#avaliar grafico e tabelinha de correla��o
## Correla��o = -0,6, alta, logo, as variaveis Ozone e Wind N�o podem ser utilizadas juntas.

pairs(airquality2[, 1:3])
pairs(airquality2[, 1:3], panel=panel.smooth) #melhora a visualiza��o 


###P2: Construir um modelo de regressão múltipla com as variáveis preditoras não correlacionadas, e determinar quais delas estão relacionadas significativamente com a temperatura, e como

airquality.lm <- lm(Temp ~ Ozone + Solar.R, data=airquality2)
summary(airquality.lm)
####interpreta��o solar radiation nao influencia a temperatura e ozone influencia  a temperatura positivamente
#ozone influencia significativamente a temp independente do efeito da radia��o solar.

summary(airquality.lm, correlation = T)#posso avaliar a correla��o entre as duas variaveis acima
###o valor negativo significa que que nao tem correla��o forte entre ozonio e solar, portanto podem ser usadas


###P3: Estimar a temperatura se Ozone = 200 e Solar.R=300
#forma 2 : fun��o predict
newdata = data.frame(Ozone = 200, Solar.R=300) #novo data frame para valores de ozone =200 e solar=300
newdata
predict(airquality.lm, newdata)


#P4: Avaliar os residuos do modelo, graficando residuals vs fitted values, e checando a normalidade dos residuos
plot(airquality.lm)
#1 parece ter um vies, um padrao
#3 tem homogeneidade de  variancia
#4 tem obs muito influentes


#P5: Determinar qual variavel preditora está causando o padrão nos residuos
res <- resid(airquality.lm)
plot(airquality2$Ozone, res); abline(0,0, col="red")
plot(airquality2$Solar.R, res) ; abline(0,0,col="red")

plot(Temp ~ Ozone, data=airquality2)
scatter.smooth(log(airquality2$Ozone), airquality2$Temp)

###ozone e a variavel que esta influenciando o padrao de residuos


##P6: Avaliar numérica e gráficamente se uma transformação logarítmica (log) melhora o r2 do modelo e elimina o padrão nos residuos
plot(Temp ~ log(Ozone), data=airquality2)
scatter.smooth(log(airquality2$Ozone), airquality2$Temp)

lm <- lm(Temp ~ Ozone, data=airquality2)
summary(lm)


plot(Temp~ Ozone, data=airquality2, main= "r2=0.488")


R2 <- summary(lm(Temp ~ log(Ozone), data=airquality2))$adj.r.squared
plot(Temp~ log(Ozone), data=airquality2, main=paste("r.squared=" , R2))

airquality.lm2 <- lm(Temp ~ log(Ozone) + Solar.R, data=airquality2)

plot(airquality.lm2)
#uando ploto temp e ozone a rela��o parece uma curva e nao uma reta, entao se fa�o lm disso nao vai ser legal
 #se eu tiro log a rela��o fica muito mais linear, entao quando fizer o grafico usando o log os residuos vao ficar mais homogeneos


##P7: Checar a independência dos residuos, usando a função acf (auto correlation function)
acf(res)


################################## ANOVA ####################################
data(chickwts) 
head(chickwts)
?chickwts

##P1: Fazer um boxplot que mostre a variação do peso (weight) em função do alimento (feed) recebido
boxplot(weight ~ feed, data=chickwts)

#se for duas aplica t test
#mais de duas aplica anova


##P2: Construir um modelo linear com o peso (weight) como variavel de resposta e o alimento (feed) como preditor categórico

chickwts.lm <- lm(weight ~ feed, data=chickwts)
chickwts.lm

##P3: Testar o efeito do alimento (feed) no peso (weight), utilizando um ANOVA

anova(chickwts.lm)
summary(chickwts.lm)


#feed influencia 
##Pr probabilidade da hip nula e pequena ou seja tem diferen�as entre alimentos, o alimento que da pra galinha influencia o peso dela


##P4: Comparar o efeito de cada tipo de alimento (feed) sobre o peso (weight), utilizando “sunflower” como alimento de referência (base level)
summary(airquality.lm)
levels(chickwts$feed)
levels(chickwts$feed) <-c("sunflower", "casein", "horsebean", "linseed", "meatmeal", "soybean")
levels(chickwts$feed)
###a primeira categoria vai ser considerada como referencia
##se quero mudar categoria so mudar a ordem

chickwts.lm2 <- lm(weight ~ feed, data=chickwts)
summary(chickwts.lm2)
anova(chickwts.lm2)



##P5: Avaliar os residuos do modelo, graficando residuals vs fitted values, e checando a normalidade dos residuos
plot(chickwts.lm2)
#1 linearidade ta boa
#2  ta bom
#3 homog de var ta bom
#4 obs influente nao tem
#modelo ta otimo
#independencia res
acf(resid(chickwts.lm2))


########################################### ANCOVA / Complex LM ###############################3
data(mtcars)
head(mtcars)
?mtcars

##P1: Transformar as variaveis cyl, vs, am e gear em fatores
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
str(mtcars)

##P2: Construir um modelo linear com o rendimento (mpg) como variavel de resposta e cyl e hp como variaveis preditoras
mtcars.lm <- lm(mpg ~ cyl + hp, data = mtcars)


##P3: Testar o efeito de cyl e hp no rendimento (mpg), utilizando anova e summary
anova(mtcars.lm)
summary(mtcars.lm)
levels(mtcars$cyl)
#neste caso o estimate do 6 � menor do que p de 4, ou seja ele faz menos milhas por galao que 4 . ou seja consumo � menor.


##P4: Invertir a ordem de cyl e hp no modelo, e testar o efeito de cyl e hp no mpg, utilizando anova e summary. O que muda?
##quando roda anova importa a ordem que colocamos as variaveis preditoras

mtcars.lm2 <- lm(mpg ~ hp + cyl, data = mtcars)
anova(mtcars.lm)
anova(mtcars.lm2)


##P5: Construir um modelo linear com o rendimento (mpg) como variavel de resposta e todas as variaveis preditoras possíveis

colnames(mtcars)

mtcars.lm3 <- lm(mpg ~ cyl + disp  + hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars)

##P6: Avaliar a multicolinearidade entre as variaveis continuas. Utilize a função vif do pacote “car” e elimine as variaveis com vif > 3
library(car)
 vif(tcars.lm3)#disp #variance inflation fcator
 summary(mtcars.lm3, correlation=T)

 mtcars.lm4 <- lm(mpg ~ cyl + hp + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
 vif(mtcars.lm4)
 
 mtcars.lm5 <- lm(mpg ~ cyl + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
 vif(mtcars.lm5)
 summary(mtcars.lm5, correlation=TRUE)
 
 
  
  #### maior numero de modelos com variaveis nao correlacionadas?
##P7: Construir um modelo linear com o rendimento (mpg) como variavel de resposta e todas as variaveis preditoras não correlacionadas
mtcars.lm5 <- lm(mpg ~ cyl + drat + wt + qsec + vs + am + gear + carb, data=mtcars)
mtcars.lm5

 
 
##P8: Determinar se seu modelo está sobre-ajustado: N / K ≥ 10, (N = número de observações; K = número de variaveis)
str(mtcars)
N = 32
K = 3
N/K  ##OK
##o modelo nao e bom, pois o min seria 10
#overfitting


##P9: Construir um modelo não sobre-ajustado com o rendimento (mpg) como variavel de resposta e variaveis preditoras não correlacionadas
mtcars.lm6 <- lm(mpg ~ cyl + drat + wt, data=mtcars)

summary(mtcars.lm6)
##quanto mais pesado o carro pior o rendidmento
##quanto maior o drat menor o rendimento
#cilindros
#r quadrado e bom
N=32 
K= 3  
N/K

##P10: Determinar quais variaveis estão relacionadas significativamente com o rendimento (mpg), e como

summary(mtcars.lm6)
anova(mtcars.lm6)


##P11: Avaliar os residuos do modelo, graficando residuals vs fitted values, e checando a normalidade dos residuos
plot(mtcars.lm6)

#m padrao
#tem homeg
#nao tem obs influencia
#modelo ta otimo

acf(resid(mtcars.lm6))
