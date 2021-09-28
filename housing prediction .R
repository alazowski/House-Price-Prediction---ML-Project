library(skimr)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)


#load data
house <- read.csv("downloads/kc_house_data.csv")

#stats summary of the data
summary(house)
str(house)
skim(house) #skimr package

#we can make use of the latitude and longtitude to visualize location of the data
Price_max <- max(house['price'])
Price_min <- min(house['price'])

#use tableau for geographical visualization

#see the relationship between house price and number of bedrooms
ggplot(house, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#change year of renovation to boolean. (0 for no renovation, 1 for renovation)
house$yr_renovated  [house$yr_renovated  > 0] <- 1
house$yr_renovated  [house$yr_renovated  == 0] <- 0

mean_price <- house %>% filter(price >= 540000 & price <= 541000)
ggplot(mean_price, aes(x= sqft_living, y= price))+ geom_boxplot()

#removing the extreme outliers
room_outlier <-
  house %>% filter(bedrooms >= 30) #filter the entire row
house2 <- house[!(house$bedrooms > 30), ]

summary(house2)

ggplot(house2, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#relationship with grade and condition
ggplot(house2, aes(x = condition, y = grade)) + geom_point()
#condition and grade is to determined the quality of the house (pick one)

drops <- c('grade', 'zipcode')
house3 <- house2[,!(names(house2) %in% drops)]

#relationship with price and conditions
ggplot(house3, aes(x = condition, y = price)) + geom_point(alpha = 0.5)
#why does the highest condition house has lower range? 
#could there be other factor that influence the price of the house?


#relationship with price and square foot space
ggplot(house3, aes(x = sqft_living, y = price)) + geom_point(alpha = 0.2)

#plot sqft_lot and price
ggplot(house3, aes(x = sqft_lot, y = price)) + geom_point(alpha = 0.2)

# create a new column for the age of house: current year- year built

# check if renovation will have any effects on the pricing
ggplot(house3, aes(x = yr_renovated, y = price)) + geom_point(alpha = 0.5) + scale_x_discrete(limits =
                                                                                                c(0, 1))
#There is a weak correlation between pricing and year of renovation

#option 2:
if (house$yr_renovated == 0) {
  ggplot(house2, aes(x = price)) + geom_histogram()
}


if (house$yr_renovated == 1) {
  ggplot(house2, aes(x = price)) + geom_histogram()
}


#split the data set to train and test:
set.seed(1234)
rnorm(1)
test_set <- sample(nrow(house), nrow(house) * 0.8)
house.test <- house[test_set, ]
house.train <- house[-test_set, ]

#simple linear regression
f1 <-
  as.formula(
    price ~
      bedrooms +
      bathrooms +
      sqft_living +
      sqft_lot +
      floors +
      waterfront +
      view +
      condition +
      grade +
      sqft_above +
      sqft_basement +
      yr_built +
      yr_renovated +
      lat +
      long
  )

y.train <- house.train$price
y.test <- house.test$price

fit.lm1 <- lm(f1, house.train)
yhat.train.lm1 <- predict(fit.lm1)
mse.train.lm1 <- mean((y.train - yhat.train.lm1) ^ 2)

#2nd simple linear regression

f2 <-
  as.formula(
    price ~
      bedrooms +
      bathrooms +
      sqft_living +
      sqft_lot +
      floors +
      waterfront +
      view +
      sqft_above +
      sqft_basement +
      yr_built +
      yr_renovated
  )

fit.lm2 <- lm(f2, house.train)
yhat.train.lm2 <- predict(fit.lm2)
mse.train.lm2 <- mean((y.train - yhat.train.lm2) ^ 2)

#2nd simple linear regression

f3 <-
  as.formula(price ~
               bedrooms +
               bathrooms +
               sqft_living +
               sqft_lot +
               waterfront +
               view)

fit.lm3 <- lm(f3, house.train)
yhat.train.lm3 <- predict(fit.lm3)
mse.train.lm3 <- mean((y.train - yhat.train.lm3) ^ 2)
