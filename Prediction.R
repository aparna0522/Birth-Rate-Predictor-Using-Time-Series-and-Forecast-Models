######################################################################################
# MINI PROJECT 2020 (3rd Year: Usha Mittal Institute Of Technology)
# Team Members : Janhavi Choudhari, Aparna Naik, Vedanti Pawar
# Project Name : Sentiment Analysis Using Text Mining On the Health Index Of India
# Project Aim  : Comparison of life expectancies of India and the USA
# Date : 12/03/2020     
######################################################################################

library(forecast)
### This dataset is the life expectancy from birth in years of India
data <- read.csv("Prediction Dataset.csv")

### Here ts is called the timeseries function and data$Life.Expectancy.from.Birth represents 
### the variable on which we wish to do the prediction. Frequency represents how the data is.
### Data is yearly data so the frequency is 1. Start represents the starting year and month.
tsdata <- ts(data$Life.Expectancy.from.Birth,frequency = 1,start = c(1950,1))

### This is the plot of the graph which shows the life expectancy of India till 2019
plot(tsdata)

## auto.arima function is used to make the graph stationary. By stationary it means that the 
## mean must be 0 and the variance must be constant.
autoarimal<-auto.arima(tsdata)

## Forecast is a function which has parameters autoarimal which contains the data of the mean 
## and the variance of the time series data. Here h=20 means that we want to predict/forecast 
## the values of the life expectancy for the next 20 years.
forecast1<-forecast(autoarimal,h=20 )
forecast1

## Here we plot the actual graph, final output which represents the output for the next 20 years
plot(forecast1)

## Residuals are found to check the mean and variance
plot(forecast1$residuals)

## This function brings it to the normal form
qqnorm(forecast1$residuals)

## acf and pcf is the autocorrelation and the partial correlation functions.
## If the values of all the points lie in the region specified by the blue lines then it is 
## considered as stationary and is desirable
acf(forecast1$residuals)
pacf(forecast1$residuals)

## This gives the summary and represents the errors in the calculations performed.
summary(autoarimal)

## Plot of the final forecast after making the graph stationary.
plot(forecast1)

## This Prediction Dataset is the dataset of life expectancy from birth of the USA.
data1 <- read.csv("Prediction Dataset USA.csv")
tsdata1 <- ts(data1$Life.Expectancy.from.Birth..Years.,frequency = 1,start = c(1950,1))
plot(tsdata1)
autoarimal1<-auto.arima(tsdata1)
forecast2<-forecast(autoarimal1,h=20 )
forecast2
plot(forecast2)
plot(forecast2$residuals)
qqnorm(forecast2$residuals)
acf(forecast2$residuals)
pacf(forecast2$residuals)
accuracy(autoarimal1)
plot(forecast2)
