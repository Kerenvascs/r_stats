

######################################################## Generalized Linear Models (GLM) ###################################################

#######################################Gaussian GLM
data(airquality)
head(airquality)
?airquality

##P1: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúgaussian‚Äù dos errors, usando Temp como a variavel de resposta e Wind como variavel preditora
M1 <- glm(Temp ~ Wind, data=airquality, family="gaussian")


##P2: Comparar esse modelo com um modelo linear simples (lm), com Temp como a variavel de resposta e Wind como variavel preditora

M1 <- glm(Temp ~ Wind, data=airquality, family="gaussian")
M2 <- lm(Temp ~ Wind, data=airquality)

anova(airquality.glm, airquality.lm)###n„o posso fazer isso, so pode comoprar lm com lm e glm com glm

#####neste caso a soluÁ„o È:

summary(M1)
summary(M2)
##P3: Avaliar os residuos do glm, graficando residuals vs fitted values, e checando a normalidade dos residuos
plot(M1)


##P4: Utilizar AIC ou BIC para seleccionar os melhores modelos, utilizando Temp como a variavel de resposta e Wind, Ozone e Solar.R como variaveis preditoras. 
## N√£o construir modelos com preditores correlacionados (colineares). Interprete o warning Warning message, e solucione ele

colnames(airquality)
summary(airquality)

airquality2 <- airquality[complete.cases(airquality), ]

T1 <- glm(Temp ~ Wind + Ozone + Solar.R, data=airquality2, family ="gaussian")
summary(T1, correlation=T)

T2 <- glm(Temp ~  Ozone + Solar.R, data=airquality2, family ="gaussian")
T3 <- glm(Temp ~ Solar.R + Wind, data=airquality2, family ="gaussian")
T4 <- glm(Temp ~ Wind, data=airquality2, family ="gaussian")
T5 <- glm(Temp ~ Solar.R, data=airquality2, family ="gaussian")
T6 <- glm(Temp ~ Ozone, data=airquality2, family ="gaussian")

library(MuMIn)

model.sel(T2, T3, T4, T5, T6, rank="AIC")

###InterpretaÁ„o primeiro olhar pro delta e os modelos com delta menor que 2 s„o igualmente bons. Logo Os melhores modelos s„o T6 e T2

##P5: Apresentar os melhores modelos (‚àÜAIC ‚â§ 2) em uma tabela
tab <- model.sel(T2, T3, T4, T5, T6, rank="AIC")

tab

tab[tab$delta<=2]

#######################################Poisson GLM

data(Orange) 
head(Orange)
?Orange
summary(Orange)

##P1: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúpoisson‚Äù dos errors, usando age como a variavel de resposta e circumference como variavel preditora
ora1 <- glm(age ~ circumference, data=Orange, family="poisson")
summary(ora1)

### resp = quanto mais velha a arvore, maior a circunferencia do tronco

##P2: Especificar a link function do modelo. Use: poisson(link= ‚Äúlog‚Äù)

###POisson original
ora2 <- glm(age ~ circumference, data=Orange, family="poisson" (link = "log"))

plot(age ~ log(circumference), data=Orange)


##P3: Avaliar overdisperssion: Residual deviance / df > 2
#### Residual deviance/ nResidual
summary(ora2)  
3062/33
3062/33 >2
####pode dar nomes, criar objetos e dividir depois

##P4: Avaliar os residuos do glm, graficando residuals vs fitted values, e checando a normalidade dos residuos

plot(ora2)
plot
abline


##P5: Graficar a rela√ß√£o entre age e circumference, adicionando a abline do glm
plot(age ~ circumference, data=Orange)
#o glm ta usando a link function pra normalizar os dados 
plot(log(age) ~ circumference, data=Orange)
?abline
abline###colocar coeficientes

##P6: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúquasipoisson‚Äù dos errors, usando age como a variavel de resposta e circumference como variavel preditora
 
ora3 <- glm(age ~ circumference, data=Orange, family="quasipoisson" (link = "log"))


##P7: Determinar o Dispersion parameter desse modelo
summary(ora3)
###### OLHAR PRA to be 86.57086 = SOBREDISPERS√O, ELE J ESTA CONSIDERANDO
####RESPOSTA = 86.57086


##P8: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúnegative binomial‚Äù dos errors, usando age como a variavel de resposta e circumference como variavel preditora. 
## Use a fun√ß√£o glm.nb do pacote ‚ÄúMASS‚Äù.
library(MASS)
ora4 <- glm.nb(age ~ circumference, data=Orange)
ora4
summary(ora4)
## essa familia lgm nb ja considera a dispers„o, entao sempre que eu tenha dados de counts, rodar 
##1 glm poisson normal, verificar se tem dispersao.
##2 ou gerar um quasipoison
## ou gerar um negative binomial

##P9: Comparar os resultados dos diferentes glm (poisson, quasipoisson e negative binomial) e seleccionar o melhor modelo.
## Daria para utilizar AIC em esse caso?

summary(ora2)###GLM POISSON
summary(ora3)###GLM QUASIPOISSON - NAO E BASEADO EM MAXIMA VEROSSIMILHAN«A - lIKEKELIHOOD
summary(ora4)###GLM NB

AIC(ora2, ora3, ora4)
##### o ora3 d· resposta NA, pois nao tem likelyhood, vafiavel de verossimilhanÁa
#####ou seja nao posso comparar modelos que tem baseses diferentes


################################ Binomial GLM - Logistic regression #########################
data(mtcars)
?mtcars

##P1: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúbinomial‚Äù dos errors, usando transmiss√£o (am) como a variavel de resposta e pot√™ncia (hp) e peso (wt) como variaveis preditoras
mtcars.glm <- glm(am ~ hp + wt, data=mtcars, family="binomial"(link = "logit"))
mtcars.glm
summary(mtcars.glm)
unique(mtcars$am)
#####escolheu a pq È categorica
mtcars$am <- factor(mtcars$am)


##P2: Estimar a probabilidade de que um carro seja construido com transmiss√£o manual se ele tem uma pot√™ncia de 120hp e pesa 2800 lbs

#forma 2 : funÁ„o predict
newdata =data.frame(hp=120, wt=2.800)
newdata
predict(mtcars.glm, newdata, type="response")
##pot√™ncia de 120hp e pesa 2800 lbs
###sem response fica a probabilidade do modelo geral
## resposta 64 porcento de probabilidade do carro ser manual
##### vamos nos basear pela proximidade do valor 1. ou seja  se predict= 0.64 h· 64% do carro ser manual, pq esta mais proximo de 1.
####considerando manual = 1, automatico = 0


##P3: Determinar se tem uma rela√ß√£o significativa entre alguma das variaveis preditoras (hp e wt) e a variavel de resposta (am), e interpretar essa rela√ß√£o. 

summary(mtcars.glm)
### interpretaÁ„o: HP a medida que aumento hp , aumenta a probabilidade do carro ser manual
#######wt

plot(wt ~ am, data=mtcars)

##P4: Avaliar overdisperssion: Residual deviance / df > 2
summary(mtcars.glm)
10.059/29
10.059/29>2
#### n„o h· dispers„o

##P5: Avaliar os residuos do modelo, graficando residuals vs fitted values, e checando a normalidade dos residuos. 
plot(mtcars.glm)
plot
abline

##P6: Utilizar um LRT para avaliar se as variaveis preditoras (hp e wt) aumentam a likelihood (diminuem o AIC) do modelo, quando comparado com um modelo sem elas.

mtcars.glm <- glm(am ~ hp + wt, data=mtcars, family="binomial"(link = "logit"))
drop1(mtcars.glm, test="Chisq")
#omelhor modelo È p e wt, se tirar fica ruim
##### o melhor AIC È sem tirar nenhuma variavel, NONE = 16.059


################################################### Binomial GLM - Proportion data 
data(menarche)
head(menarche)
?menarche
colnames(menarche)

##P1: Construir um modelo linear generalizado (glm) com uma distribui√ß√£o ‚Äúbinomial‚Äù dos errors, usando a propor√ß√£o de ninhas que atingiram a menarche como a variavel de resposta e idade (Age) como variaveil preditora. 
## Utilize cbind(Menarche, Total-Menarche)
summary(menarche)

Men <- glm(cbind(Menarche, Total - Menarche) ~ Age, data=menarche, family = binomial(link = "logit"))

#toda funÁ„o cbind È y, variavel de resposta


##P2: Estimar a probabilidade de que uma ninha atinga a menarche aos 15 anos

newdata =data.frame(Age=15)
newdata
predict(Men, newdata, type="response")
##### a probabilidade de 15 anos È 96%


##P3: Determinar se tem uma rela√ß√£o significativa entre a idade das ninhas e a propor√ß√£o delas que atingiu a menarche.
summary(Men)
###### InterpretaÁ„o: Estimate = 1.63197 Quando a idade aumenta, aumenta a proporÁ„o de meninas ter a menarche


##P4: Determinar se a inclus√£o da variavel Age aumenta a likelihood do modelo (diminue o AIC).
summary(Men)


##P5: Avaliar overdisperssion: Residual deviance / df > 2

26.703/23 > 2

##n„o h· dispers„o

##P6: Plotar a rela√ß√£o entre idade e a propor√ß√£o de ninhas que atingiu a menarche 

plot(log(menarche$Age), (menarche$Menarche/menarche$Total*100), xlab="Age", ylab="Proportion of Girls  who have reached menarche")
lines(log(menarche$Age), (menarche$Menarche/menarche$Total*100), color="red")



##P7: Utilizar AIC para avaliar se uma transforma√ß√£o logar√≠tmica (log) de Age melhora ou piora ou modelo


Men2 <- glm(cbind(Menarche, Total - Menarche) ~ log(Age), data=menarche, family = binomial(link = "logit"))
AIC(Men,Men2)
