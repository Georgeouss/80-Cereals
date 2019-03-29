# Asen Penchev, fn: 71810, IS, group 1
# Georgi Burgazliev, fn: 71781, IS, group 1
# Simeon Yachev, fn: 71824, IS, group 1

#loads our dataset
cereal.dataset = read.csv("D:/SU/Statistika/Rshit/R_homework1/cereal.csv", header = TRUE)

#run dataset
cereal.dataset

attach(cereal.dataset)

#this function gets the type of our dataset
class(cereal.dataset)

#this function extract sample of 50 integers out of the hole dataset
cereal.sample.int = sample(1:62, 50, replace = FALSE)

#random sample of 50 rows of the original data frame "cereal.dataset"
cereal.dataset.sample = cereal.dataset[cereal.sample.int,]

detach(cereal.dataset)
attach(cereal.dataset.sample)

#lets work with cereals that have fiber
install.packages("dplyr")
library(dplyr)    
cereal.dataset.with.fiber = filter(cereal.dataset.sample, fiber != 0.0)
detach(cereal.dataset.sample)
attach(cereal.dataset.with.fiber)

#This function shows the first 5 results from the dataset
head(cereal.dataset.with.fiber, n = 5)

#Categorical data (quality)

#name, mfr, type

#Numerical data (quantity)       continuous  discrete

#calories - discrete
#protein - discrete
#fat - discrete
#sodium - discrete
#fiber - discrete
#carbo - discrete (ама нз що има няква закуска дето е с -1 carbo)
#sugars - discrete (ама нз що има няква закуска дето е с -1 sugars)
#potass - discrete
#vitamins - discrete
#shelf - discrete
#weight - discrete
#cups - discrete
#rating - continuous

#descriptive statistics
summary(cereal.dataset.with.fiber)

#row with the cereal that has the highest amount of protein 
cereal.max.protein = cereal.dataset.with.fiber[protein == max(protein),]

#row with the cereal that has the lowest amount of protein 
cereal.min.protein = cereal.dataset.with.fiber[protein == min(protein),]

