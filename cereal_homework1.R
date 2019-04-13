# Asen Penchev, fn: 71810, IS, group 1
# Georgi Burgazliev, fn: 71781, IS, group 1
# Simeon Yachev, fn: 71824, IS, group 1

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

#this function extract sample of 50 integers out of the hole dataset
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
barplot(table(type ,mfr),col = c("navy","purple"),main = "Productions of mfrs")

#it shows that most of the cereals are rated between 35-40 % and less after 60 %
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
abline(lm(sugars~rating))
#the corelation
cor(sugars,rating)

