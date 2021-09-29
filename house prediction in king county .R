library(readr)
library(ggplot2)
library(ggcorrplot)
library(mlbench)
library(Amelia)
library(caret)
library(plotly)
library(caTools)
library(reshape2)
library(dplyr)
library(tidyverse)
library(data.table)
library(skimr)
library(psych)
library(corrplot)

#load data
house <- read.csv("/Users/geechhorhuot/Downloads/kc_house_data.csv")

#examine the dataset
str(house) #21613 obs. of  21 variables
summary(house) # Mean price: 540088
skim(house) # no mmissing values

# find missing values 
missmap(house, col=c('yellow','black'), y.at= 1, y.labels= ' ', legend= TRUE)

#change year of renovation to boolean. (0 for no renovation, 1 for renovation)
house$yr_renovated  [house$yr_renovated  > 0] <- 1
house$yr_renovated  [house$yr_renovated  == 0] <- 0

##Exploratory Analysis: 
#see the relationship between house price and number of bedrooms
ggplot(house, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple') #one outlier in the plot 

#removing the extreme outliers
room_outlier <-
  house %>% filter(bedrooms >= 30) #filter the entire row
house2 <- house[!(house$bedrooms > 30), ]

ggplot(house2, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#condition and grade is to determined the quality of the house (pick one)
drops <- c('grade', 'zipcode')
house3 <- house2[,!(names(house2) %in% drops)]

summary(house3)

#The average price of the house: 
Avg_house_price <- mean(house3$price)
Avg_house_price

#relationship with price and square foot space
ggplot(house3, aes(x = sqft_living, y = price)) + geom_point(alpha = 0.2)

#plot sqft_lot and price
ggplot(house3, aes(x = sqft_lot, y = price)) + geom_point(alpha = 0.2)

#relationship with price and conditions
ggplot(house3, aes(x = condition)) + geom_bar(alpha = 0.5)
#why does the highest condition house has lower range? 
#could there be other factor that influence the price of the house?

##manipulating data table to see correlation
data1 <- house3[, 3:17]
pairs(data1)
pairs(
  ~  price + bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + sqft_above + yr_renovated,
  data = data1)
plot(data1)


#visualizing density of variables 
house3 %>% ggplot(aes(price))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living15))+stat_density()+theme_bw()
