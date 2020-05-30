library(foreign)
library(ggplot)
library(dplyr)
#loading the dataset
data<-read.dta("C:/Users/tirufamily/Desktop/Linear model/MentalHealth.dta")
data

#(a.)  scatter plot matrix of variables of the dataset
pairs(~mentalImpair+lifeEvents+ses, data)

#(b.)   Linear regression and testing the significance using t test
slr1 <- lm(mentalImpair ~ lifeEvents, data)
slr1
summary(slr1)

t_val<-2.471^2
t_val


#(c.)
slr2 <- lm(mentalImpair ~ 1, data)
rss <- function(model) sum(residuals(model)^2)
p <- rss(slr1)/rss(slr2)   #pearsons correlation coefficient
p
c(p, sqrt(p))
rss

#(d.)
data <- mutate(data, lifeCsq = (lifeEvents - mean(lifeEvents))^2) #adding quadratic term
data
slr3 <- lm(mentalImpair ~ lifeEvents + lifeCsq, data)
slr3
summary(slr3)


#(e.)
slr4 <- lm(mentalImpair ~ ses, data)
cor(data$mentalImpair, data$ses)   #correlation coefficient
std <- function(x) (x - mean(x))/sqrt(var(x))   #function to standardize the variable
#std
slr5 <- lm(std(mentalImpair) ~ std(ses), data)
summary(slr5)
