library(MuMIn)
data(mtcars)
mtcars


###########Likelihood vs. AIC
Likelihood <- seq(0.01, 1, by=0.001)
k <- 1
AIC <- -2*log(Likelihood) + 2*k

plot(Likelihood, AIC)

#########AIC vs. K
K <- c(1:991)


for(i in K){
  AIC_k <- -2*log(Likelihood) + (2*K)
}

plot(K, AIC_k, xlab="AIC", ylab="K")

################################################### Model Selection ##################################
data(mtcars)
head(mtcars)
?mtcars

##P1: Transformar as variaveis cyl, vs, am e gear em fatores
mtcars$cyl <- factor(mtcars$cyl)  ##elas eram numeros e agora quero que sejam fatores, ou seja leia elas como categoricas
mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
mtcars$gear <- factor(mtcars$gear)
str(mtcars)

##P2: Construir um modelo linear com o rendimento (mpg) como variavel de resposta e todas as variaveis preditoras continuas nÃ£o correlacionadas (problema 7 do exercicio anterior)

colnames(mtcars)

mtcars.lm <- lm(mpg ~ cyl + disp  + hp + drat + wt + qsec + vs + am + gear + carb, data=mtcars)
mtcars.lm

library(car)
vif(mtcars.lm)    #disp #variance inflation fcator, tem que ser abaixo de 3, o que indica que nao tem multicoliniearidade
summary(mtcars.lm, correlation=T)
mtcars.lm

mtcars.lm2 <- lm(mpg ~ cyl + hp + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
vif(mtcars.lm2)


mtcars.lm3 <- lm(mpg ~ cyl + drat + wt + qsec + vs + am + gear + carb, data = mtcars)
vif(mtcars.lm3) #####todos abaixo de 3, ok

##P3: Construir 3 modelos nÃ£o sobre-ajustados com o rendimento (mpg) como variavel de resposta e diferentes combinaÃ§Ãµes de variaveis preditoras nÃ£o correlacionadas

str(mtcars)
N = 32
K = 3
N/K  

mtcars.lm3 <- lm(mpg ~ cyl + drat + wt, data=mtcars)

mtcars.lm4 <- lm(mpg ~ drat + wt + qsec, data=mtcars)

mtcars.lm5 <- lm(mpg ~ vs + am + gear, data=mtcars)

##P4: Comparar os 3 modelos usando as funÃ§Ãµes AIC e BIC
AIC(mtcars.lm3, mtcars.lm4, mtcars.lm5)
######### modelo 4 tem menor valor de aic, é o melhor
BIC(mtcars.lm3, mtcars.lm4, mtcars.lm5)
######## modelo 4 é melhor, pois tem menor BIC, portanto explicará melhor o conjunto de dados. 
###############é  a melhor combinação d evariaveis que explica o rendimento do carro.


##P5: Comparar os 3 modelos usando a funÃ§Ã£o model.sel do pacote â€œMuMInâ€
library(MuMIn)

model.sel(mtcars.lm3, mtcars.lm4, mtcars.lm5)
##########lohlik e verosi transformada com log, delta coloca primeiro o modelo com menor AIC, ordena modelos pelo n de AIC.
######### é a diferença entre AIC, 0 pq é o melhor modelo contra ele mesmo, diferença entre o 2 e o melhor, o terceiro e o melhor, o 4 e o melhor.
######### weight é a probabilidade  do modelo ser melhor
######## criterio global  delta menor ou igual a 2 indica a qualidade do modelo, nesse caso os tres sao bons.

##P6: Criar 2 modelos aninhados e comparar eles usando anova(modelo1, modelo2)
#### modelos alinhasdos sao modelos similares, mas um deles tem uma variavel a mais.

mtcars.lm6 <- lm(mpg ~ drat + wt + qsec, data=mtcars)
mtcars.lm7 <- lm(mpg ~ drat + wt, data=mtcars)

anova(mtcars.lm6)
anova(mtcars.lm7)
anova(mtcars.lm6, mtcars.lm7)

### neste caso probabilidade muito baixo indica que os modelos são diferentes
####osegundo modelo e significativamente pior que o modelo 1
# a variavel qsec e importante, se tiro o AIC aumenta, se deixo o AIC e menor
# tirar essa variavel faz co que o modelo seja pior, que aumente o AIC
AIC(mtcars.lm6, mtcars.lm7)
### corrobora com a hipotese de que qsec é importante. a diferença de AIC é grande.

##P7: Seleccionar o melhor dos 3 modelos e avaliar se cada uma das variaveis preditoras diminuem significativamente o AIC. Use drop1(modelo, test=â€Chisqâ€)

mtcars.lm4 <- lm(mpg ~ drat + wt + qsec, data=mtcars)

drop1(mtcars.lm4, test="Chisq") # teste de qui quadrado, compara o modelo completo com modelos que exlcuem uma variavels por vez
#faz um modelo sem cada uma dessas variaveis. sem drat, sem wt, sem qsec. a resposta mostra que so wt e qsec sao mais importantes,
###logo posso tirar drat
## nao pode ser usado no inicio pq nao podemos fazer modelos com mais de tres variaveis

##P8: Eliminar o(s) preditor(es) nÃ£o significativos no LRT, e criar um modelo (o modelo final) que sÃ³ possua preditores que diminuam significativamente o AIC

mtcars.lm8 <- lm(mpg ~ wt + qsec, data=mtcars)
drop1(mtcars.lm8, test="Chisq")

##P9: Utilizar um protocolo de seleÃ§Ã£o de modelos baseado em AIC para determinar o melhor modelo de rendimento (mpg). 
## Incluir um modelo nulo (lm(mpg ~ 1, data=mtcars)

colnames(mtcars)

m0= lm(mpg ~ 1, data=mtcars)

m1= lm(mpg ~ cyl, data=mtcars)

m2= lm(mpg ~ disp, data=mtcars)

m3= lm(mpg ~ hp, data=mtcars)

m4= lm(mpg ~ drat, data=mtcars)

m5= lm(mpg ~ wt, data=mtcars)

m6= lm(mpg ~ qsec, data=mtcars)

m7= lm(mpg ~ vs, data=mtcars)

m8= lm(mpg ~ am, data=mtcars)

m9= lm(mpg ~ gear, data=mtcars)

m10= lm(mpg ~ carb, data=mtcars)

model.sel(m0, m1, m2, m3 ,m4, m5, m6, m7,m8, m9, m10,rank="AICc" )
#### m5 é igual, sempre sera o mesmo modelo.
#### olhar, delta, menor, melhor.
#### AICc menor é melhor
#### weight maior, como se fosse 83%

#########OUTRA FORMA DE FAZER

options(na.action = na.fail)
mtcars.full <- lm(mpg ~ cyl+drat+wt+qsec+vs+am+gear, data=mtcars)
Allmodels <- dredge(mtcars.full, rank= "AIC", m.lim=c(0,3)) # contendo de 0 a 3 variaveis)
dim(Allmodels)

Allmodels[Allmodels$delta<=10, ]
#melhor modelo foi 82, com am , qsec e wt

##P10: Avaliar o efeito das variaveis preditoras desse modelo final
mtcars.best <- lm(mpg ~ am + qsec + wt, data=mtcars)
anova(mtcars.best)
summary(mtcars.best) # para avaliar sempre usar summary, pq gera os estimates
#####################o resultado mostra que as variaveis que influenciam o rendimento são qsec e wt
#################### o 1 que aparece ao lado do am representa a categoria de referencia, 1/0 automatico/manual
##################### o rendimento é maior no am1, ou seja nos carros manuais é significativamente maior que nos automaticos
##################### quando o qsec é maior o rendimento é maior, maior o arranque do motor 
#################### estimate é b1, b0 é intercept
#################### se b1 é positivo ele é diretamente proporcional
################### no caso do wt, o numero do estimate siginifica que a medida que o peso do carro aumenta o rendimento diminui.


##P11: Criar uma tabela com os resultados da seleÃ§Ã£o de modelos e outra com os efeitos dos preditores do modelo final. Interpretar as duas tabelas. 

mtcars.full <- lm(mpg ~ cyl+drat+wt+qsec+vs+am+gear, data=mtcars)

Allmodels <- dredge(mtcars.full, rank= "AIC", m.lim=c(0,3)) # contendo de 0 a 3 variaveis)
dim(Allmodels)

Allmodels[Allmodels$delta<=2, ] #SELECIONA O MELHOR MODELO, mostra tabel do melhor modelo
summary(mtcars.best) #mostra as variaveis, avalias as estrelinhas

################OU


Table_Allmodels <- Allmodels[Allmodels$delta<=3, ]#tres pra aparecer mais modelos e ficar mais bonito
write.csv(as.data.frame(Table_Allmodels), file="Table_Allmodels.csv") #####salvar tabela dos modelos como excel


Table_bestmodel <- summary(mtcars.best)

write.csv(as.data.frame(Table_bestmodel$coefficients), file="Table_bestmodel.csv")




