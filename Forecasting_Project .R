# Get environment ready by clearing out variables in workspace
rm(list = ls())

# Load libraries
library(fpp2)
library(quantmod)
library(urca)
library(lmtest)
library(vars)

# Load time series data
sp <- read.csv("SP_2000_2021.csv")
View(sp)
sp$Adj_Close_scaled <- sp$Adj_Close / 100  # scale for S&P Index 
sp.ts <- ts(sp$Adj_Close_scaled, start = c(2000,1), frequency = 12)

unrate <- read.csv("UNRATE_2000_2021.csv")
View(unrate)
unrate.ts <- ts(unrate$UNRATE, start = c(2000,1), frequency = 12)


# Plot for data visualization
plot(sp.ts, xlab = "Date", ylab = "", main = "S&P 500 Index (in 100s) vs. US Unemployment Rate", 
     ylim = c(0,50),  bty = "l", lwd = 2)
lines(unrate.ts, col = "blue", lwd = 2)
legend(x="top", legend=c("S&P Index", "Unemployment Rate"), col=c("black", "blue"), 
       lty = 1, lwd = 2, cex=0.8)
grid()

ggseasonplot(unrate.ts,polar=TRUE)
# Check for stationarity using Augmented Dickey-Fuller (ADF) test

## ADF for S&P INDEX
sp.adf <- ur.df(sp.ts, type = "trend", selectlags = "AIC")
summary(sp.adf)
### Estimated coefficient on 'z.lag.1' is not statistically different than zero
### Ho CANNOT BE REJECTED - S&P INDEX ARE NONSTATIONARY

## Take 1st differencing to remove trend
sp.diff <- diff(sp.ts, lag = 1)
sp.diff.adf <- ur.df(sp.diff, type = "trend", selectlags = "AIC")
summary(sp.diff.adf)
### This time Ho can be rejected - DIFFERENCED S&P INDEX ARE STATIONARY
plot(sp.diff.adf)
plot(sp.diff)

## ADF for Unemployment Rate

## ADF for Unemployment Rate
unrate.adf <- ur.df(unrate.ts, type = "none", selectlags = "AIC")
summary(unrate.adf)
### Estimated coefficient on 'z.lag.1' is not statistically different than zero
### Ho CANNOT BE REJECTED - Unemployment rate is NONSTATIONARY

## Take 1st differencing to remove trend
unrate.diff <- diff(unrate.ts, lag = 1)
unrate.diff.adf <- ur.df(unrate.diff, type = "none", selectlags = "AIC")
summary(unrate.diff.adf)

#cointegration

reg <- lm(unrate.ts ~ sp.ts)  #  SH = a + (b x SZ) + e
summary(reg)

#  Test residuals of estimated long-term cointegration relationship
res <- reg$residuals

df.res <- ur.df(res, type = "drift", selectlags = "AIC")
summary(df.res) 
?grangertest
#  Check whether S&P granger causes Unemployment
grangertest(unrate.ts ~ sp.ts, order = 1)
#  Ho: ChangeS&P does not granger cause unemplotment rate
#  -  Reject Ho since p-value is less than 0.05 critical value

grangertest(sp.ts ~ unrate.ts  , order = 1)
# Two variables are endogenous. Use VAR model

#  Select optimal number of lags
#lagselect <- VARselect(data, lag.max = 10, type = "const")    
#lagselect$selection  
#  Fit VAR model
#model.1 <- VAR(data, p = 2, type = "none", season = NULL, exogen = NULL)   
#summary(model.1)   
#  Use VAR model for forecasting
#forecast <- predict(model.1, n.ahead = 2, ci = 0.95)
#fanchart(forecast)

#Policy Simulation
#grangerGDP <- causality(model.1,cause="ChangeGDP")
#grangerGDP

###Basic forecast model
nValid <- 36
nTrain <- length(unrate.ts) - nValid
train.ts <- window(unrate.ts,start=c(2000,1),end=c(2000,nTrain),frequency=12)
valid.ts <- window(unrate.ts,start=c(2000,nTrain+1),frequency=12)

#basic forecasting model
mod1 <- meanf(train.ts,h=nValid)
mod2 <- rwf(train.ts,h=nValid)
#mod3 <- snaive(train.ts,h=nValid)
mod4 <- rwf(train.ts,h=nValid,drift=TRUE)
autoplot(unrate.ts)+
  autolayer(mod1,series="Mean",PI=FALSE)+
  autolayer(mod2,series = "naive",PI=FALSE)+
  autolayer(mod4,series = "drift",PI=FALSE)+
  guides(colour=guide_legend(title="Forecast"))
grid()

accuracy(mod4,valid.ts)
#accuracy(mod3,valid.ts)#seasonal naive best
accuracy(mod2,valid.ts)
accuracy(mod1,valid.ts)



library(fpp2)
library(quantmod)           #  Used to import time series data
library(urca) 
##Arima
#  - Fit model with external data: S&P

###Basic forecast model
arima <- auto.arima(train.ts)
arima.ext <- auto.arima(train.ts, xreg = sp.diff[c(1:224)])
summary(arima)
summary(arima.ext)

# USE FORECAST FUNTION TO GENERATE FORECAST BASED ON ARIMA MODEL RESULTS
arima.pred <- forecast(arima, h = nValid, level = 0)
arima.ext.pred <- forecast(arima.ext, h = nValid, level = 0,xreg = sp.diff[c(225:261)])
#   Plot results

plot(arima.ext.pred, xlab = "Time", ylab = "Unemployment", bty = "l",
     main = "Unemployment forcast with ARIMA")
lines(arima.ext.pred$fitted,lwd=2,col="purple")
lines(arima.pred$fitted,lwd=2, col = "blue")
lines(arima.pred$mean,col="red")
lines(unrate.diff)
