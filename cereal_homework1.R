# Asen Penchev, fn: 71810, IS, group 1
# Georgi Burgazliev, fn: 71781, IS, group 1
# Simeon Yachev, fn: 71824, IS, group 1

library(UsingR)

#maker sure the file is in the working directory
getwd()

#loads our dataset
cereal.dataset = read.csv("C:/Users/Mi/Desktop/cereal.csv", header = TRUE)

#run dataset
cereal.dataset

attach(cereal.dataset)

#this function gets the type of our dataset
class(cereal.dataset)

#get the same sequence
set.seed(71781)

#this function extract sample of 50 integers out of the whole dataset
cereal.sample.int = sample(1: nrow(cereal.dataset), 50, replace = FALSE)

#random sample of 50 rows of the original data frame "cereal.dataset"
cereal.dataset.sample = cereal.dataset[cereal.sample.int,]

detach(cereal.dataset)
attach(cereal.dataset.sample)

#remove all invalid rows
cereal.dataset.sample = cereal.dataset.sample[complete.cases(cereal.dataset.sample), ]
cereal.dataset.sample = cereal.dataset.sample[fat>=0 & calories >=0 & sodium >=0 & carbo>=0
                                              & fiber>=0 & sugars >=0 & potass>=0 ,]

#Categorical data (quality) - name, mfr, type , shelf
#Numerical data (quantity)       
#calories - discrete
#protein - discrete
#fat - discrete
#sodium - discrete
#fiber - discrete
#carbo - discrete 
#sugars - discrete 
#potass - discrete
#vitamins - discrete
#weight - discrete
#cups - discrete
#rating - continuous

#descriptive statistics
summary(cereal.dataset.sample)
#names are unique so there are no statistical characteristics

#row/s with the cereal/s with the highest amount of protein 
cereal.max.protein = cereal.dataset.sample[protein == max(protein),]

#row/s with the cereal/s with the lowest amount of protein 
cereal.min.protein = cereal.dataset.sample[protein == min(protein),]

#the number of cereals with no suger and low amount of calories 
nrow(cereal.dataset.sample[calories <= 100 & sugars==0 ,])

#Calories per serving and per cup are different
calories.cup = round(as.numeric(calories) / as.numeric(cups))
cat("Range of calories per weight:", min(calories, na.rm = TRUE), "-", 
    max(calories, na.rm = TRUE), "\n")
cat("Range of calories per cup:", min(calories.cup, na.rm = TRUE), "-", 
    max(calories.cup, na.rm = TRUE), "\n")

#gets the names of the manifacturers
factor(cereal.dataset$mfr)

#visual representation of the mfr
barplot(table(mfr),
        col = c("red", "blue","green" ,"yellow" ,"pink" ,"purple","orange"),
        main = "Manifacturers",
        ylab = "Quantity")

#all manifacturers produce cereals of type C except A
barplot(table(type ,mfr),col = c("navy","purple"),main = "Productions of mfrs",legend.text = T)

#it shows that most of the cereals are rated between 35-40 % and less after 60 %
#the probability of the cereal to be 30-40  rated is high and above 60 - less
hist(rating,probability = TRUE )
lines(density(rating),col="red")
rug(jitter(rating))

#the median of the chart is 100 cal , as A has only one value its avg cal in a cereal = 100
#G avg = 110 and there are some outliers under(100) , above(130,140) 
# analogically for the others it shows the minimum , first quartile , median , IQR , max 
boxplot(calories~mfr)

#all plots
plot(cereal.dataset.sample)

#low sugar cereals are highly rated
plot(sugars,rating)
#the cereals with the greatest rating (light blue) have the lowest sugar and calorie content!!
library(ggplot2)
ggplot(data=cereal.dataset.sample,aes(x=sugars,y=calories,col=rating))+
  geom_jitter(data=cereal.dataset.sample, aes(sugars,calories,col=rating))+
  labs(x="Sugar",y="Calories")+
  geom_smooth(method="lm",se=FALSE,col='black')+
  theme_bw()


#regression line 2way
simple.lm(sugars,rating)
abline(lm(rating~sugars))
abline(rlm(rating~sugars),lty=4)
#the corelation
cor(sugars,rating)
#find index of the closest (x, y) coordinates to the mouse click
identify(sugars,rating,n=2)


#numeric to categorical for calories
calories.categories = cut(calories,breaks = c(min(calories)-1,quantile(calories,.25),
                                  quantile(calories,.75),max(calories)))
#the probability of getting low , medium and high in cal cereal
prop.table(table(calories.categories))
medium.cal.prob = prop.table(table(calories.categories))[2]
medium.cal.prob
prop.test(20,length(calories),medium.cal.prob)

levels(calories.categories) = c("low cal","medium cal","high cal");
barplot(table(calories.categories),col = c("red","green","blue"))

#you can't buy low in carbo and high in cal cereal , there is no straight dependency
carbo.categories = cut(carbo,breaks = c(min(carbo)-1,quantile(carbo,.25),quantile(carbo,.75),max(carbo)))

levels(carbo.categories) = c("low carbo","medium carbo","high carbo");
barplot(table(calories.categories,carbo.categories),col = c("red","green","blue"),beside = TRUE,legend.text = T)

#plot both a histogram and a boxplot to show the relationship between the two graphs
simple.hist.and.boxplot(sodium)
boxplot(scale(sugars),scale(carbo))

# compares densities by creating violin plots. These are similar to boxplots, 
# only instead of a box, the density is drawn with it’s mirror image.
simple.violinplot(scale(protein),scale(fat))


#binomial distribution , the probabilty to select half of the low in calories cereals
dbinom(25,50,0.42)

#normal distribution , the percentage of cereals that contains more than 100cal)
pnorm(100, mean(calories), sd(calories), lower.tail=FALSE) 

library(fitdistrplus)
library(logspline)
install.packages("logspline")
install.packages("fitdistrplus")

#use the functiondescdist to gain some ideas about possible candidate distributions.
descdist(potass, discrete = TRUE)
#comparisson
fit.nbinom = fitdist(potass, "nbinom")
fit.pois = fitdist(potass, "pois")
plot(fit.nbinom)
plot(fit.pois)
#potass is more likely to be negative binom
hist(potass,probability = TRUE)
lines(density(potass),col="red")
rug(jitter(potass))

#normal distribution
descdist(calories, discrete = TRUE)
hist(calories,probability = TRUE)
lines(density(calories),col="red")
rug(jitter(calories))

#Negative binomial
descdist(fat, discrete = TRUE)
fat.fit.nbinom = fitdist(fat, "nbinom")
plot(fat.fit.nbinom)
hist(fat,probability = TRUE)
lines(density(fat),col="red")
rug(jitter(fat))

#(?)
descdist(protein, discrete = TRUE)
shapiro.test(protein)
protein.fit.pois = fitdist(protein, "pois")
plot(protein.fit.pois)
#(?)
shapiro.test(fiber)
descdist(fiber, discrete = TRUE)
plot(fiber.fit.nbinom)
hist(fiber,probability = TRUE)
lines(density(fiber),col="red")
rug(jitter(fiber))
#norm distribution(?)
descdist(sodium, discrete = TRUE)
sodium.fit.norm = fitdist(sodium, "norm")
plot(sodium.fit.norm)
hist(sodium,probability = TRUE)
lines(density(sodium),col="red")
rug(jitter(sodium))
#normal ditribution
descdist(carbo, discrete = TRUE)
carbo.fit.norm = fitdist(carbo, "norm")
plot(carbo.fit.norm)
hist(carbo,probability = TRUE)
lines(density(carbo),col="red")
rug(jitter(carbo))
#norm(?)
descdist(sugars, discrete = TRUE)
sugars.fit.norm = fitdist(sugars, "norm")
plot(sugars.fit.norm)
hist(sugars,probability = TRUE)
shapiro.test(sugars)
lines(density(sugars),col="red")
rug(jitter(sugars))
#poisson
descdist(vitamins, discrete = TRUE)
vitamins.fit.pois = fitdist(vitamins, "pois")
plot(vitamins.fit.pois)
hist(vitamins,probability = TRUE)
lines(density(vitamins),col="red")
rug(jitter(vitamins))
#normal distribution(?)
descdist(weight, discrete = TRUE)
hist(weight,probability = TRUE)
lines(density(weight),col="red")
rug(jitter(weight))
#norm(?)
descdist(cups, discrete = TRUE)
hist(cups,probability = TRUE)
lines(density(cups),col="red")
rug(jitter(cups))
#rating weibull(?)
descdist(rating, discrete = FALSE)
rating.fit.weibull = fitdist(carbo, "weibull")
plot(rating.fit.weibull)

#find approximate q1 of calories
qnorm(p=.25,mean(calories),sd(calories),lower.tail = TRUE)

#probability density function
dens = dnorm(calories,mean(calories),sd(calories))
plot(calories,dens)
abline(v=mean(calories))

#95% confidence interval for carbo
carbo.avg = mean(carbo)
error = qnorm(.975) * sd(carbo) / sqrt(length(carbo))
cat("confidence interval for carbo:", carbo.avg - error, "-", 
    carbo.avg + error, "\n")

#The 95% confidence interval of the mean carbo for sugar = 10
predict(lm(carbo~sugars),data.frame(sugars=10),interval = "confidence")

#mean an confidence interval for fat 
plot(fat,sodium)
fat.values = unique(fat)
n_groups = length(fat.values)

sample_mean = rep(NA,n_groups)
cis = matrix(nrow = n_groups,ncol = 2)
for (i in 1:n_groups){
  #extract relevant data
  rows = which(fat==fat.values[i])
  observations = sodium[rows]
  #store sample mean
  sample_mean[i] = mean(observations)
  #construct ci
  stdev = sd(observations)
  n = length(observations)
  se_mean = stdev / sqrt(n)
  #store ci (confidence interval)
  cis[i,1] = sample_mean[i] - 2*se_mean
  cis[i,2] = sample_mean[i] + 2*se_mean
}

#means of each group
points(x=fat.values,y=sample_mean,col="red" , cex= 2)
#add ci
segments(x0=fat.values,x1=fat.values, y0= cis[,1],y1 =cis[,2],
         col = "red" , lwd = 2)

#The p-value reports how likely we are to see this data

#the manufacturers claims that the mean carbos of a cereal are more than 15
#In a sample of 50 cereals, it was found that they have 14.51 hours on average.
#Assume the population standard deviation is 3.9. At .05 significance level, 
#can we reject the claim by the manufacturers?
carbo.mean = mean(carbo) 
carbo.sd = sd(carbo)
#The null hypothesis is that ?? ??? 30. We begin with computing the test statistic.
mu0 = 15 #hypothesis value
carbo.length = length(carbo)
z = (carbo.mean - mu0)/(carbo.sd/sqrt(carbo.length)) 
#We apply the pnorm function to compute the lower tail p-value of the test statistic.
#As it turns out to be higher than the .05 significance level,
#we do not reject the null hypothesis that ?? ??? 30.
pnorm(z) > 0.05

#Manifacturars claims that there are at most 100 calories per serving in a cereal. 
#In a sample of 50 cereals, it is found that the mean amount of calories in a cereal
#is 107.2 cals. Assume that the population standard deviation is 21.76 grams. 
#At .05 significance level, can we reject the claim by the mfrs?
cal.mean = mean(calories)
cal.sd = sd(calories)
#The null hypothesis is that ?? ??? 100
cal.mu0 = 100 ; # hypothesized value
cal.length = length(calories)
cal.z = (cal.mean - cal.mu0)/(cal.sd/sqrt(cal.length))
#we apply the pnorm function to compute the upper tail p-value of the test statistic.
#As it turns out to be less than the .05 significance level, 
#we reject the null hypothesis that ?? ??? 100.
pnorm(cal.z, lower.tail=FALSE) < 0.05

#21 out of 50 cereals are low in calories. At 0.5 significance level, 
#can we reject the null hypothesis that the proportion of low in calories cereals
#in the population is above 50% ?
#-----------------------------------
#The null hypothesis is that p ??? 0.5
#p-value of the test statistic. As it turns out to be greater than the .05 
#significance level, we do not reject the null hypothesis that p ??? 0.5
prop.test(21,cal.length, p=.5, alt="less", correct=FALSE)

#p-value  is greater than the .05 significance level, we do not reject 
#the null hypothesis that the calories are independent of the carbos
tbl = table(calories.categories,carbo.categories)
chisq.test(tbl)

#p-value  is greater than the .05 significance level, we reject 
#the null hypothesis that the type is independent of the mfr
chi = chisq.test(table(type,mfr))
chi$p.value < 0.05
