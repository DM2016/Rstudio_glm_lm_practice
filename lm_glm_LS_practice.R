###Practice with Generalized, linear and generalized least square models
##lm: linear regression, normal errors, constant variance
##glm: generalized linear models, non-normal errors, non-constant variance
##gls: generalized least squared model, correlated errors, spatial/temporal

#clear environment
rm(list = ls())

#tests airquality dataset for several characteristics
airquality
dim(airquality)
names(airquality)
mode(airquality)
str(airquality)
summary(airquality)

#some basic plots (2 versions of the same)
plot(Ozone~Wind, airquality)
plot(airquality$Wind, airquality$Ozone)

#Linear Model: y = a + bx + & (& is standin for epsilon)
model1 = lm(Ozone~Wind, airquality)
plot(model1)
coef(model1)

Ozone1=coef(model1)[1] + coef(model1)[2]*19
Ozone2=coef(model1)[1] + coef(model1)[2]*20

Ozone1
Ozone2

#glm v1: log(y) = a + bx + & (& is standin for epsilon)
#glm v2: y = e^(a+bx) + & (& is standin for epsilon)
#uses exponential functions

model2 = glm(Ozone~Wind, airquality, family=poisson)
coef(model2)

Ozone3.glm=exp(coef(model2)[1] + coef(model2)[2]*19)
Ozone4.glm=exp(coef(model2)[1] + coef(model2)[2]*20)

Ozone3.glm
Ozone4.glm

plot(Ozone~Wind, airquality)

Ozone4.glm/Ozone3.glm

#slope coefficient of model2 using second coefficient (-0.14)
exp(coef(model2)[2]) #exp(-0.1488753)
#answer: 0.8616765 

#if slope in glm fit by poisson errors is exponentialized to (-0.1488753)
#it gives proportional change in Ozone conc. if we change wind speed by 1 unit
#Ozone Concentration will go down by a factor or 0.1488753

#In a glm, an individual slop gives an estimate of the multiplicative change
#in the response variable for a one unit change in the corresponding exploratory variable
#slope b = -0.14
#for a 1 unit change in Wind speed,
#Ozone concentration decreases e^-0.14 fold

####generalized least squares models
###Can be used to account for temporal autocorrelation

library(nlme)
#model3 = gls(Ozone~Wind, airquality) #returns an error, need to filter out NA values
summary(airquality$Ozone)
model3 = gls(Ozone~Wind, airquality, na.action = na.exclude)

airquality$Date=
as.Date(paste(1973, airquality$Month, airquality$Day, sep="-"))

library(lattice)
xyplot(Ozone~Date, airquality)

#model 4 incorporates Ozone~Wind*Date, temporal auto correlation
model4 = gls(Ozone~Wind*Date, airquality, na.action = na.exclude) #This gives an error, b/c there are missing values
plot(ACF(model4))

air2=subset(airquality, complete.cases(Ozone))
#model 5 will incorporate the new air2 dataset, with no missing values using complete cases function

model5 = gls(Ozone~Wind*Date, air2) #this will run without errors
plot(ACF(model5, form=~Date), alpha=0.05)

##to account for temporal pattern shown in plot from model 5 (outlier data, or something correlating with itself)
model6=update(model5, correlation=corAR1())

##multimodel interference library
library(MuMIn)
AICc(model5, model6)
summary(model6)

#

