####################QUIZ2##################
##############Keren Vasconcelos###########


###############
######QUIZ#####
###############
library(corrgram)
library(dplyr)
library(devtools)
library(EDAWR)


## (Quiz 1)
## Using mtcars, select the cars that have a weight of more than 3 tonnes, and calculate the mean number of horse power.

?mtcars
dim(mtcars)
View(mtcars)
?tb
head(mtcars)


mtcars %>%
  filter(wt>= 3)%>%
  summarise(mean = mean(hp))

##RESULTADO: mean=1 177.35      


## (Quiz 2)
## Calculate the sums of squares for hp, and disp columns in the mtcars dataset, 
## where the sums of squares are defined as the sum of  (value - mean(value))^2. 

head(mtcars)

mtcars %>%
  mutate(sums_sq= (hp - mean(hp))^2) %>%
  mutate(sums_dis_sq= (disp - mean(disp))^2)      


## (Quiz 3)
## Take the iris dataset, then group it by Species, then summarize it by calculating the mean of the Sepal.Length, 
## then use ggplot2 to make a simple bar plot.
head(iris)
library(ggplot2)

iris %>% group_by(Species)%>%
  summarise(mean = mean(Sepal.Length))%>% 
              ggplot(aes(x=mean, fill=Species)) + stat_bin(geom="bar",bins=4) + theme_bw()

## (Quiz 4)
## Using tb, calculate the sum of turbeculosis cases in Brazil grouped by sex, among all three categories

head(tb)
str(tb)
summary(tb)

tb %>%
  filter(country=="Brazil") %>%
  group_by(sex) %>%
  summarise(sumchild = sum(child, na.rm =T), sumadult = sum(adult, na.rm = T), sumelderly = sum(elderly, na.rm = T))


## (Quiz 5)
## First install package:
## install.packages("hflights")
library(hflights)
head(hflights)
summary(hflights)
str(hflights)
colnames(hflights)
?hflights




## Given the flights dataset from the hflights package:
##  5.1 How many flights there are per month.
hflights %>%
  group_by(Month) %>%
  summarise(flightspermonth = n())


##  5.2 Which destination has the largest amount of incoming flights.
hflights %>%
  group_by(Dest) %>%
  summarise(destination = n()) %>%
  arrange(desc(destination))
#######################################################Resultado = A cidade que mais recebe vôos é Dallas


##  5.3 Which carrier has the most air time.

hflights%>%
  group_by(UniqueCarrier)%>%
  summarise(company = sum(ArrTime, na.rm = T)) %>%
  arrange(desc(company))

########################################Resultado = A companhia que passou mais tempo no ar foi a CO.

