library(caTools)
library(e1071)
library(caret)
library(tidyverse)

# database
base = read.csv('Suicide Info.csv.')
summary(base)

# data treatment
unique(base$sex)
base$sex = factor(base$sex, levels = c("male", "female"), labels = c(0,1))

unique(base$ï..country)
base$ï..country = factor(base$ï..country, levels = unique(base$ï..country), labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101))

unique(base$age)
base$age = factor(base$age, levels = unique(base$age), labels = c(1,2,3,4,5,6))

unique(base$generation)
base$generation = factor(base$generation, levels = unique(base$generation), labels = c(1,2,3,4,5,6))
base[is.na(base$generation)]

unique(base$gdp_for_year....)
base$gdp_for_year.... = NULL

base$country.year = NULL

mean(base$HDI.for.year[base$HDI.for.year > 0], na.rm = TRUE)

base$HDI.for.year = ifelse(base$HDI.for.year < 0,0.7766011, base$HDI.for.year)

base$HDI.for.year = ifelse(is.na(base$HDI.for.year), mean(base$HDI.for.year, na.rm = TRUE), base$HDI.for.year)
base$HDI.for.year = NULL
base$gdp_per_capita....=NULL

base[,2] = scale(base[,2])
base[,5] = scale(base[,5])
base[,6] = scale(base[,6])
base[,7] = scale(base[,7])

#/////////////////////////////naivebayes//////////////////////////////////

set.seed(1)
divisao = sample.split(base$generation, SplitRatio = 0.75)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = naiveBayes(x = base_treinamento[-8], y = base_treinamento$generation)
print(classificador)

previsoes = predict(classificador, newdata = base_teste[-8])
matriz_confusao = table(base_teste[, 8], previsoes)
print(matriz_confusao)

confusionMatrix(matriz_confusao)


                  