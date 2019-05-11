
############################################ Linear Mixed Models #########################################3

########################################### LMM - Random Intercepts vs Random Slopes
data(airquality) 
head(airquality)
?airquality

##P1: Construir um modelo linear misto (LMM) tipo random intercept, com Temp como a variavel de resposta, Ozone como preditor fixo e Month como fator rand√¥mico. 
## Use lmer do pacote ‚Äúlme4‚Äù
library(lme4)
library(lmerTest)

airquality2 <- airquality[complete.cases(airquality), ]

airquality.lmer <- lmer(Temp ~ Ozone + (1|Month), data=airquality2)
summary(airquality.lmer)
##P2: Construir um modelo linear misto (LMM) tipo random intercept and slope com Temp como a variavel de resposta, 
## Ozone como preditor fixo e Ozone aninhado em Month como fator rand√¥mico (assumir que a rela√ß√£o entre Temp e Ozone √© vari√°vel entre os meses). 
## Use lmer do pacote ‚Äúlme4‚Äù

airquality.lmer2 <- lmer(Temp ~ Ozone + (1 + Ozone|Month), data=airquality2)
summary(airquality.lmer2)


##P3: Compare os resultados dos dois modelos

summary(airquality.lmer)
summary(airquality.lmer2)


##P4: Utilizar AIC para comparar um modelo linear simples (sem variaveis rand√¥micas), com um modelo misto tipo random intercept, 
## que possua uma √∫nica variavel rand√¥mica (Month). Use gls e lme do pacote ‚Äúnlme‚Äù
library(nlme)

airquality.lme <- lme(Temp ~ Ozone, random = ~ 1|Month, data=airquality2, method="REML")
summary(airquality.lme)

airquality.lm<- gls(Temp ~ Ozone, data=airquality2, method="REML") #default

AIC(airquality.lme, airquality.lm) #comparar aic
anova(airquality.lme, airquality.lm) #comparar aic
plot(airquality.lme)
####incluir o fator randomico diminui muito o valor do meu modelo, entao eu preciso utilizar o modelo misto.
###dois modelos um com fator randomico e outro sem
####
##P5: Comparar os estimates de modelos mistos id√©nticos construidos utilizando as fun√ß√µes lmer e lme

summary(airquality.lme) #lme
summary(airquality.lmer) #lmer

################################################## LMM - Model Selection with REML/ML

politeness <-  read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
head(politeness)
#write.csv(politeness, file="politeness.csv")

##P1: Determinar se tem c√©lulas em branco (NAs) e criar um novo dataset sem elas
summary(politeness)
poli <- politeness[complete.cases(politeness), ]
summary(poli)

##P2: Fazer um boxplot que mostre a varia√ß√£o de frequency em fun√ß√£o de attitude e gender
colnames(politeness)

boxplot(frequency ~ attitude + gender, data=poli)
####interpretaÁ„o

##P3:  Construir um modelo misto, com frequency como variavel de resposta, attitude e gender como fatores fixos e subject como fator rand√¥mico.                                    
## Utilize lmer do pacote ‚Äúlme4‚Äù
summary(politeness)

#forma1:pacote lme4
t.lmer <- lmer(frequency ~ attitude + gender + (1|subject), data=poli)
summary(t.lmer)

#forma1:pacote nlme
t.lme <- lme(frequency ~ attitude + gender, random = ~1|subject, data=poli, method = "REML")#se nao escrever method ele ja puxa o default
summary(t.lme)

#s„o equivalentes, sÛ utilizo a segunda quando quero comparar dois modelos randomicos

##P4: Determinar a variabilidade entre indiv√≠duos (subject), avaliar o efeito das variaveis preditoras (fixas) sobre frequency, 
## e determinar os diferentes intercepts para cada subject
summary(t.lmer)
###olhar para o item Std.Dev. Variabilidade = 24.57, o efeito que ela causa no modelo È de 24.57, e pode ser relevante
#####para saber se ta influenciando È sÛ ir e avaliar a do outro modelo
###para avaliar as variaveis preditoras fixas  olhar para estimates. como È fator ele considera um so gupo como referencia, neste caso ele considera o primeiro que aparecer no campo levels.
levels(poli$attitude) 
levels(poli$gender)
###a frequencia dos homens e menor do que a frequencia das mulheres, pq ta negativo o numero do estimate
#primeiro comparou com formal e informal
#depois comparou homem e mulher
####Attitude È as pessoas do sexo masculino tem uma agressividade menor que o feminino?
#### nesse caso a referencia È mulher


###INTERCEPTS
coef(t.lmer)
#os valores diferentes de dados em relaÁ„o a reta, para cada pessoa.

##P5: Utilizar AIC e BIC para determinar a melhor estrutura rand√¥mica do modelo, considerando subject e scenario como variaveis rand√¥micas. 
## Compare modelos mistos tipo random intercept e random intercept + slope.
## Utilize restricted maximum likelihood (REML=TRUE)
library(MuMIn)

t.lmer1 <- lmer(frequency ~ attitude + gender +(1|subject), data=poli, REML=T)

t.lmer2 <- lmer(frequency ~ attitude + gender +(1|subject) +(1|scenario) , data=poli, REML=T) ### essa estrutura È a meslhor a partir da interpretaÁ„o de resultados AIC

t.lmer3 <- lmer(frequency ~ attitude + gender +(1 + attitude|subject) +(1|scenario) , data=poli, REML=T)#modelo de ramdom diferente

AIC(t.lmer1, t.lmer2, t.lmer3)


##P6: Utilizar um protocolo de sele√ß√£o de modelos (AIC/BIC ou LRT) para determinar a melhor estrutura fixa do modelo, utilizando a melhor estrutura rand√¥mica determinada anteriormente. 
## Utilize maximum likelihood (REML=FALSE)

t.lmer2 <- lmer(frequency ~ attitude + gender +(1|subject) +(1|scenario) , data=poli, REML=F) 


p1 <- lmer(frequency ~ attitude + gender +(1|subject), data=poli, REML=F)

p2 <- lmer(frequency ~ gender +(1|subject) +(1|scenario) , data=poli, REML=F) ### essa estrutura È a meslhor a partir da interpretaÁ„o de resultados AIC

p3 <- lmer(frequency ~ attitude +(1 + attitude|subject) +(1|scenario) , data=poli, REML=F)#modelo de ramdom diferente

p4 <- lmer(frequency ~ 1 +(1 + attitude|subject) +(1|scenario) , data=poli, REML=F)

model.sel(p1, p2, p3, p4)
#1 È melhor

##P7: Comparar os estimates do melhor modelo construido utilizando REML e ML.

p1a <- lmer(frequency ~ attitude + gender +(1|subject), data=poli, REML=T)
summary(p1a)
summary(p1)
###ajeitar, nao ta aparecendo  valor de p
###resposta= nao muda o resultado, o que vai mudar È

##P8: Construir um modelo misto com distribui√ß√£o binomial (logistic regression), utilizando gender como variavel de resposta, frequency como fator fixo e subject como fator rand√¥mico. 
## Utilize a fun√ß√£o glmer (pacote ‚Äúlme4‚Äù)
##########GLM MISTO FUN«√O = GLMER

G1 <- glmer(gender ~ scale(frequency, scale=T) + (1|subject), family="binomial", data=poli)
####Muitos modelos glm mistos nao roda bem com valor de variavel grande, 
####entao o warning pede um scale, pra tranformar o valor da variavel

plot(frequency ~ gender, data=poli)
hist(poli$frequency)
### logo o scale vai trasformar esses valores


##P9: Avaliar a rela√ß√£o entre frequency e gender, num√©rica e gr√°ficamente. 
summary(G1)
## nos estimates, scale È negativo, quanto mais agressivo È, significa que È mulher.
plot(G1)

plot(frequency ~ gender, data=poli)
### mulheres parecem ter o comportamento mais agressivo que homens

####### Questions? See: http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#reml-for-glmms