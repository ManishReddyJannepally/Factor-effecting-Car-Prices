rm(list = ls()) # clearing the environment
getwd() # current working directory
options(warn = -1)
setwd("F:/IIIT - B/Module 3/Linear Regression/Assignment") # setting working directory
list.files()

if(!file.exists("CarPrice_Assignment.csv")){
        car_data_url = "https://cdn.upgrad.com/UpGrad/temp/a9f2334f-9eb2-4160-8486-701584204e08/CarPrice_Assignment.csv"
        download.file(car_data_url,"CarPrice_Assignment.csv", mode = 'wb')
}
# libraries
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(car)
library(MASS)

# loading the data
cars_data = fread("CarPrice_Assignment.csv")
head(cars_data)
str(cars_data)
dim(cars_data)

nrow(unique(cars_data)) # checking for duplicate rows
which(duplicated(cars_data))

# checking for the missing values
colSums(is.na(cars_data)) # no missing values

# lets find the outliers
quantile(cars_data$wheelbase, seq(0,1,0.01))

quantile(cars_data$carlength, seq(0,1,0.01))

quantile(cars_data$carheight, seq(0,1,0.01))

quantile(cars_data$curbweight, seq(0,1,0.01))

quantile(cars_data$enginesize, seq(0,1,0.01))

quantile(cars_data$boreratio, seq(0,1,0.01))

quantile(cars_data$stroke, seq(0,1,0.01))

quantile(cars_data$compressionratio, seq(0,1,0.01))
# there is a sudden increase of values from 90th %tile. But diesel engines from
# bigger brands have compression ratio above 20. I checked the data. All the values 
# are from diesel engines. So, I am not handling them

quantile(cars_data$horsepower, seq(0,1,0.01))
# the value 288 (100th %tile) is because the porsche car has 8 cylinders for that 
# observation. So, its not an outlier

quantile(cars_data$peakrpm, seq(0,1,0.01))

quantile(cars_data$citympg, seq(0,1,0.01))

quantile(cars_data$highwaympg, seq(0,1,0.01))

# As per my business understanding, the high values in some variables are due to high 
# number of sylinders, enginesize etc. Bigger brand's premium cars have such values. So,
# I choose not to handle outliers


# seperating the brand name of the car from the CarName variable
car_price = cars_data %>% separate(CarName, "car_brand", sep = " ")


# there are factor variables according to data dictionary. checking the format 
# of variables
str(car_price)

# changine the neccessary columns to factors (according to data dictionary)
car_price = car_price %>% mutate_if(is.character,as.factor)
str(car_price)

car_price$symboling = as.factor(car_price$symboling)

# checking for the data correctness in factor variables
lapply(car_price[sapply(car_price, is.factor)], unique )

# there are some issues to be addressed in car_brand variable.

# correcting brand names in car_brand variable
car_price$car_brand = tolower(car_price$car_brand)
car_price$car_brand = gsub("vokswagen","volkswagen", car_price$car_brand)
car_price$car_brand = gsub("vw","volkswagen", car_price$car_brand)
car_price$car_brand = gsub("toyouta","toyota", car_price$car_brand)
car_price$car_brand = gsub("porcshce","porsche", car_price$car_brand)
car_price$car_brand = gsub("maxda","mazda", car_price$car_brand)



# after the corrections we need to change the corrected variables as factors again
car_price = car_price %>% mutate_if(is.character,as.factor)
str(car_price)
lapply(car_price[sapply(car_price, is.factor)], unique )

# DUMMY variable creation

# replacing 2 level categorical variables with 1 and 0 as categories

levels(car_price$fueltype) = c(1,0) # 1 - diesel, 0 - gas
car_price$fueltype = as.numeric(levels(car_price$fueltype))[car_price$fueltype]

levels(car_price$aspiration) = c(1,0) # 1 - std, 0 - turbo
car_price$aspiration = as.numeric(levels(car_price$aspiration))[car_price$aspiration]

levels(car_price$doornumber) = c(1,0) # 1 - four, 0 - two
car_price$doornumber = as.numeric(levels(car_price$doornumber))[car_price$doornumber]

levels(car_price$enginelocation) = c(1,0) # 1 - front, 0 - rear
car_price$enginelocation = as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]

# now creating dummy variables for variables with more than 3 levels

# symboling into dummies

dummy_symboling = data.frame(model.matrix(~ symboling, data = car_price))
dummy_symboling = dummy_symboling[-1]

# car_brand into dummies

dummy_car_brand = data.frame(model.matrix(~ car_brand, data = car_price))
dummy_car_brand = dummy_car_brand[-1]

# carbody into dummies

dummy_carbody = data.frame(model.matrix(~carbody, data = car_price))
dummy_carbody = dummy_carbody[-1]

# drivewheel into dummies

dummy_drivewheel = data.frame(model.matrix(~drivewheel, data = car_price))
dummy_drivewheel = dummy_drivewheel[-1]

# engine type into dummies

dummy_enginetype = data.frame(model.matrix(~enginetype, data = car_price))
dummy_enginetype = dummy_enginetype[-1]

# cylinder number into dummies

dummy_cylindernumber = data.frame(model.matrix(~ cylindernumber, data = car_price))
dummy_cylindernumber = dummy_cylindernumber[-1]

# fuel system into dummies

dummy_fuelsystem = data.frame(model.matrix(~ fuelsystem, data = car_price))
dummy_fuelsystem = dummy_fuelsystem[-1]

# removing the actual variables and binding the dummy variables created

names(car_price[c(2,3,7,8,15,16,18)])

car_price = car_price[-c(2,3,7,8,15,16,18)]
car_price2 = cbind(car_price,dummy_symboling, dummy_car_brand, dummy_carbody, dummy_cylindernumber, dummy_drivewheel,
      dummy_enginetype, dummy_fuelsystem)

# the variable Car_ID is a unique id of each observation. So, It does'nt make sence to include it

car_price2 = car_price2[-1]

rm(list=setdiff(ls(), c("car_price","car_price2")))


# Train and Test datasets
set.seed(550)
train_indices = sample(1:nrow(car_price2), 0.7*nrow(car_price2))
train = car_price2[train_indices,]
test = car_price2[-train_indices,]

# model building

# I am going for stepwise selection to build a ribust model

model_1 = lm(price ~., data = train)
summary(model_1) # adjusted R squared is 96.42% but many insigficant variables


# StepAIC method

step_model = stepAIC(model_1, direction = "both")
step_model

# model_2 using the output given by stepAIC

model_2 = lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumbersix + cylindernumberthree + cylindernumbertwelve + 
                     cylindernumbertwo + drivewheelrwd + enginetypeohc + fuelsystem2bbl + 
                     fuelsystemmpfi, data = train)

summary(model_2) # adjusted R squared is 96.82%. A slight increase

# let's check the Variance Inflation Factor (VIF) for model_2
vif(model_2)

# cylinder numbers 4,5,6, engine size and car height, curb weight are having high VIF values.
# but only cylinder number 6 is insignificant according to summary of model_2.


# let's build model_3 removing cylindernumbersix variable.

model_3 = lm(formula = price ~ fueltype + aspiration + enginelocation + 
                     wheelbase + carlength + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + 
                     cylindernumbertwo + drivewheelrwd + enginetypeohc + fuelsystem2bbl + 
                     fuelsystemmpfi, data = train)

summary(model_3) # adjusted R squared is 96.74%.

# VIF of model_3

vif(model_3)

# fueltype variable is having high VIF and it is insignificant. remaining variables
# with high VIF are significant according to the model_3 summary

# removing fueltype variable for model_4

model_4 = lm(formula = price ~ aspiration + enginelocation + 
                               wheelbase + carlength + carwidth + carheight + curbweight + 
                               enginesize + boreratio + stroke + compressionratio + peakrpm + 
                               car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                               car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                               car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                               carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                               cylindernumberthree + cylindernumbertwelve + 
                               cylindernumbertwo + drivewheelrwd + enginetypeohc + fuelsystem2bbl + 
                               fuelsystemmpfi, data = train)
summary(model_4) # adjusted R squared 96.2%

# VIF of model_4

vif(model_4) 

# carlength variable is having high VIF and insignificant in model_4 summary

# model_5 without carlength variable

model_5 = lm(formula = price ~ aspiration + enginelocation + 
                     wheelbase + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + 
                     cylindernumbertwo + drivewheelrwd + enginetypeohc + fuelsystem2bbl + 
                     fuelsystemmpfi, data = train)
summary(model_5) # adjusted R squared 96.13%

vif(model_5) # VIF of model_5

# removing fuelsystemfi as it is insignificant and has high VIF

model_6 = lm(formula = price ~ aspiration + enginelocation + 
                     wheelbase + carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                     drivewheelrwd + enginetypeohc + fuelsystem2bbl, data = train)
summary(model_6) # adjusted R squared 96.13%
vif(model_6) # VIF for model_6

# wheelbase is having high VIF and insignificant according model_6 summary

# removing wheelbase for model_7

model_7 = lm(formula = price ~ aspiration + enginelocation + 
                     carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                     drivewheelrwd + enginetypeohc + fuelsystem2bbl, data = train)
summary(model_7) # adjusted R squared 96.03%
vif(model_7) # VIF for model_7

# drivewheelrwd is insignificant and with high VIF. So, removing

model_8 = lm(formula = price ~ aspiration + enginelocation + 
                     carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + compressionratio + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                     enginetypeohc + fuelsystem2bbl, data = train)
summary(model_8) # adjusted R squared 96.03%
vif(model_8) # VIF for model_8

# Compressionratio is insignificant and has high VIF. So, removing it

model_9 = lm(formula = price ~ aspiration + enginelocation + 
                     carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + carbodysedan + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                     enginetypeohc + fuelsystem2bbl, data = train)
summary(model_9) # adjusted R squared 96.03%
vif(model_9) # VIF for model_9

# carbodysedan is insignificant and has high VIF

model_10 = lm(formula = price ~ aspiration + enginelocation + 
                     carwidth + carheight + curbweight + 
                     enginesize + boreratio + stroke + peakrpm + 
                     car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                     car_brandisuzu + car_brandmitsubishi + car_brandpeugeot + 
                     car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                     carbodyhatchback + cylindernumberfive + cylindernumberfour + 
                     cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                     enginetypeohc + fuelsystem2bbl, data = train)
summary(model_10) # adjusted R squared 96.06%
vif(model_10) # VIF for model_10

# car_brandisuzu is having high VIF and insignificant. removing it

model_11 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandaudi + car_brandbmw + car_brandbuick + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                      carbodyhatchback + cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_11) # adjusted R squared 96.09%
vif(model_11) # VIF for model_11

# car_brandbuick is having high VIF and insignificant. removing it

model_12 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandaudi + car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                      carbodyhatchback + cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_12) # adjusted R squared 96.05%
vif(model_12) # VIF for model_12

# car_brandauto is having high VIF and insignificant. removing it

model_13 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + carbodyhardtop + 
                      carbodyhatchback + cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_13) # adjusted R squared 96.07%
vif(model_13) # VIF for model_13

# carbodyhardtop is insignificant as per model_13. Lets remove it and see how it effects

model_14 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      carbodyhatchback + cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_14) # adjusted R squared 96.00%. No significant decrease
vif(model_14) # VIF for model_14

# carbodyhatchback is insignificant now. Lets remove that and check the model

model_15 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + curbweight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_15) # adjusted R squared 95.95%. No significant decrease
vif(model_15) # VIF for model_15

# enginesize and curbweight is having high VIF. let's find the correlation of eniginesize with curbweight
cor(car_price2$enginesize, car_price2$curbweight)

# both the enginesize and curbweight are highly correlated. 
# Let's remove the varibale with high p-values between them

# curbweight is having high p-values than enginesize.

model_16 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth + carheight + 
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_16) # adjusted R squared 95.06%.
vif(model_16) # VIF for model_16

# carheight is insigficant now and with high VIF. removing carheight

model_17 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc + fuelsystem2bbl, data = train)
summary(model_17) # adjusted R squared 95.07%. No significant decrease
vif(model_17) # VIF for model_17

# removing fuelsystem2bbl as per high VIF and less significant value (high p-values)

model_18 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + car_branddodge + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_18) # adjusted R squared 94.94%. No significant decrease
vif(model_18) # VIF for model_18

# removing car_branddodge as it is less significant

model_19 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandplymouth + car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_19) # adjusted R squared 94.88%. No significant decrease
vif(model_19) # VIF for model_19

# removing car_brandplymouth as it is less significant

model_20 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke + peakrpm + 
                      car_brandbmw + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_20) # adjusted R squared 94.8%. No significant decrease
vif(model_20) # VIF for model_20

# removing peakrpm as it is insignificant

model_21 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + 
                      car_brandmitsubishi + car_brandpeugeot + 
                      car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_21) # adjusted R squared 94.74%. No significant decrease
vif(model_21) # VIF for model_21

# removing car_brandmitsubishi as it is less significant
model_22 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + 
                      car_brandpeugeot + 
                      car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_22) # adjusted R squared 94.6%. No significant decrease
vif(model_22) # VIF for model_22

# All the variables became significant now. lets find some more correlations.

# enginesize and carwidth are with high VIFs. Lets find the correlation between them
cor(car_price2$enginesize, car_price2$carwidth)

# enginesize and carwidth are highly correlated. let's remove the variable with 
# less significance between them. carwidth is less significant

model_23 = lm(formula = price ~ aspiration + enginelocation +
                      enginesize + boreratio + stroke +
                      car_brandbmw + 
                      car_brandpeugeot + 
                      car_brandsaab + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_23) # adjusted R squared 93.07%. 1.5% decreased.

# so, removing carwidth is not good. Let's try other options

# removing car_brandsaab as it is less significant
model_24 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + 
                      car_brandpeugeot + 
                      car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_24) # adjusted R squared 94.3%. No significant decrease
vif(model_24) # VIF for model_24

# removing car_brandpeugeot as it is less significant
model_25 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw +  
                      car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + cylindernumbertwo + 
                      enginetypeohc, data = train)
summary(model_25) # adjusted R squared 93.7%. 0.6% decreased
vif(model_25) # VIF for model_25

# removing enginetypeohc as it is insignificant
model_26 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + 
                      cylindernumbertwo, data = train)
summary(model_26) # adjusted R squared 93.58%. 0.6% decreased
vif(model_26) # VIF for model_26

# All the variable are most significant by model_26. Let's check on test data

# Predict the car prices in the testing dataset
Predict_1 <- predict(model_26,test[-18])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared for model_26
rsquared # there is difference of almost 10% between r squared of train and test.

# Let's remodel the model_26 further.

# enginesize and cylindernumberfour are having high VIF but are significant. 
# These 2 are negatively correlated(-0.6314309)
# I am trying to remove on of these and check whether we can make a robust model

# removing enginesize

model_27 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      boreratio + stroke +
                      car_brandbmw + car_brandvolvo + 
                      cylindernumberfive + cylindernumberfour + 
                      cylindernumberthree + cylindernumbertwelve + 
                      cylindernumbertwo, data = train)
summary(model_27)

# adjusted r squared has fallen more than 10%. So, we can not remove enginesize.

# try removing cylindernumberfour and let's see how model worls

model_28 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + car_brandvolvo + 
                      cylindernumberfive +
                      cylindernumberthree + cylindernumbertwelve + 
                      cylindernumbertwo, data = train)
summary(model_28) # Adjusted R squared 92.27%

# model_28 has impacted adjusted R squared by 1% and made cylindernumberthree and five insignificant

# lets remove insignificant variables from model_28

model_29 = lm(formula = price ~ aspiration + enginelocation + 
                     carwidth +
                     enginesize + boreratio + stroke +
                     car_brandbmw + car_brandvolvo + cylindernumbertwelve + 
                     cylindernumbertwo, data = train)
summary(model_29) # Adjusted R squared 91.85%
vif(model_29)

# removing car_brandvolvo as it is insignificant

model_30 = lm(formula = price ~ aspiration + enginelocation + 
                      carwidth +
                      enginesize + boreratio + stroke +
                      car_brandbmw + cylindernumbertwelve + 
                      cylindernumbertwo, data = train)
summary(model_30) # Adjusted R squared 91.53%
# Now, lets check this model on the test data

# Predict the car prices in the testing dataset
Predict_2 <- predict(model_30,test[-18])
test$test_price <- Predict_2

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared for model_26
rsquared # 87.07% with test data

# so, my final model is model_30 with R squared values of train dataset is 92% 
# R squared values of test dataset is 87.07%.
