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

#Categorical data (quality) - name, mfr, type
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
#shelf - discrete
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
#regression line 2way
simple.lm(sugars,rating)
abline(lm(rating~sugars))
abline(rlm(rating~sugars),lty=4)
#the corelation
cor(sugars,rating)
#find index of the closest (x, y) coordinates to the mouse click
identify(sugars,rating,n=2)


#numeric to categorical for calories
calories.categories = cut(calories,breaks = c(min(calories),quantile(calories,.25),
                                  quantile(calories,.75),max(calories)))
levels(calories.categories) = c("low cal","medium cal","high cal");
barplot(table(calories.categories),col = c("red","green","blue"))

#you can't buy hight in carbo and high in cal cereal , there is no straight dependency
carbo.categories = cut(carbo,breaks = c(min(carbo),quantile(carbo,.25),quantile(carbo,.75),max(carbo)))

levels(carbo.categories) = c("low carbo","medium carbo","high carbo");
barplot(table(calories.categories,carbo.categories),col = c("red","green","blue"),beside = TRUE,legend.text = T)

#plot both a histogram and a boxplot to show the relationship between the two graphs
simple.hist.and.boxplot(sodium)

   
boxplot(scale(sugars),scale(carbo))
simple.violinplot(scale(protein),scale(fat))

