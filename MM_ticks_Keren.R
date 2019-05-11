library(lme4)
data(grouseticks)
head(grouseticks)
?grouseticks

plot (TICKS ~ LOCATION, data= grouseticks)
plot (TICKS ~ YEAR, data= grouseticks)
plot (TICKS ~ HEIGHT, data= grouseticks)
plot (TICKS ~ BROOD, data= grouseticks)
plot (TICKS ~ INDEX, data= grouseticks)

table(grouseticks$LOCATION)
table(grouseticks$YEAR)
table(grouseticks$TICKS)
table(grouseticks$BROOD)


##sera q e interessante avaliar se lugares diferentes de coleta tem diferentes num de carrapato?
##siiiim, entra como variavel fixa
##broods = var ramdomica

## Que fatores determinam a infestaÃ§Ã£o dos filhotes com carrapatos?

library(lme4)
library(lmerTest)
summary(grouseticks)
colnames(grouseticks)

unique(grouseticks$LOCATION)

mode(grouseticks$LOCATION)
class(grouseticks$LOCATION)
levels(grouseticks$LOCATION)

str(grouseticks)

###variaveis randomicas: que possivelmente nao interferem na analise =  INDEX, BROOD, LOCATION,
###variaveis fixas:  HEIGHT, YEAR
### não dá pra comparar com base em location, pois existem locais com apenas 1 observação



G1 <- glmer(TICKS ~ scale(HEIGHT, scale=T) + (1|BROOD) + (1|INDEX) + (1|LOCATION), family="poisson", data=grouseticks)

G2 <- glmer(TICKS ~ YEAR + (1|BROOD) + (1|INDEX) + (1|LOCATION), family="poisson", data=grouseticks)

G3 <- glmer(TICKS ~ scale(HEIGHT, scale=T) + YEAR +  (1|BROOD) + (1|INDEX) + (1|LOCATION), family="poisson", data=grouseticks)

G4 <- glmer(TICKS ~ 1 +  (1|BROOD) + (1|INDEX) + (1|LOCATION), family="poisson", data=grouseticks) #Modelo nulo


######Para Verificar overdisperssion

overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(G1)#ok
overdisp_fun(G2)#ok
overdisp_fun(G3)#ok
overdisp_fun(G4)#ok


#####p muito pequeno rejeita a hipoteses nula, tem overdispersão
#####p prox a 1 esta ok
#### se p  estivesse pequeno, eu poderia resolver utilizado o modelo bonimial glm.bn ou adcionar um fator randomico individual(ID) ou seja, INDEX.

AIC(G1, G2, G3, G4)
#Model sel: quais variaveis determinam
model.sel(G1, G2, G3, G4)
#RESPOSTA = Modelo G3 é melhor, pois apresenta o menor valor de delta e menor valor de AIC,
########### Assim, as variáveis que determinam a infestaÃ§Ã£o dos filhotes com carrapatos são: Altura e Ano (fixas).

#Como as variaveis influenciam = Summary

summary(G3)
levels()
#elevação negatuvamente, quanto mais alto, menos carrapastos.

#resíduos

plot(G3)
#a concentração de pontos a esquerda, é da natureza dos dados. A dispersão concentrada só no início, significa que o modelo é ruim no começo e bom no final.

RES <-  resid(G3)
plot(grouseticks$HEIGHT, RES)
plot(grouseticks$YEAR, RES)
plot


#Final plots

plot(TICKS ~ HEIGHT, data=grouseticks)
plot(TICKS ~ YEAR, data=grouseticks)

library(effects)

grouseticks$HEIGHT.Scale <- scale(grouseticks$HEIGHT, scale=T, center=T)
Mplot <- glmer(TICKS ~  HEIGHT.Scale + YEAR +  (1|BROOD) + (1|INDEX) + (1|LOCATION), family="poisson", data=grouseticks)

AE <- allEffects(Mplot)

plot(AE)
