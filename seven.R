library(forecast)
setwd("/Users/Chris Iyer/Documents/Assignment5Predictive/")
air <- read.csv("AirTravel.csv", header = T, stringsAsFactors = F)
dim(air)
air.ts <- ts(air$Air, start = c(1990, 1), end = c(2000, 12), frequency = 12)
time <- time(air.ts)
airValidLength <- 12
airTrainLength <- length(air.ts) - airValidLength
#aT <- window(air.ts, end = c(1990, airTrainLength))
#aV <- window(air.ts, start = c(1990, airTrainLength + 1))
airTrainWindow <- window(air.ts, start = time[1], end = time[airTrainLength])
airValidWindow <- window(air.ts, start = time[airTrainLength + 1], end = time[airTrainLength + airValidLength])
#Acf to view trend seasonality. Shows strong AC both components.
Acf(air.ts)
#AC on differenced = 1. Shows strong seasonal remaining
Diff1 <- diff(air.ts, lag = 1)
Acf(Diff1)
#Differenced = 12, Acf shows significant trend
Diff2 <- diff(air.ts, lag = 12)
Acf(Diff2)
#Double differenced
Diff3 <- diff(diff(air.ts, lag = 1), lag = 12)
Acf(Diff3)
#arima on each
NoDiff <- Arima(air.ts, order = c(1,0,0))
NoDiff
# Calculate the two-tailed p-value from a t-distribution p = 0.0007696904 
2*pt(-abs((1 - NoDiff$coef ["ar1"]) / 0.0478), df=length(air.ts)-1)
Diff1Arima <- Arima(Diff1, order = c(1,0,0))
Diff1Arima
#lag = 1 differenced. p = 1.343229e-29. p<alpha
2*pt(-abs((1 - Diff1Arima$coef["ar1"]) / 0.0844), df=length(air.ts)-1)
Diff2Arima <- Arima(Diff2, order = c(1,0,0))
Diff2Arima
#lag-12 differenced. p = 9.623052e-07. p < alpha
2*pt(-abs((1 - Diff2Arima$coef ["ar1"]) / 0.0701), df=length(air.ts)-1)
#Same on double differenced. p = 0.9989818. p > alpha. Accept null. No more systematic pattern. 
Diff3Arima <- Arima(Diff3, order = c(1,0,0))
Diff3Arima
2*pt(-abs((1 - Diff3Arima$coef ["ar1"]) / .0916), df=length(air.ts)-1)
#fit a model Linear Trend and Season
airLinearSeason <- tslm(airTrainWindow ~ trend + season)
summary(airLinearSeason)
#look at Acf Plot of the residuals
airResidualsAcf <- Acf(airLinearSeason$residuals)
#look at the values that are plotted
airResidualsAcf
#run an AR(1) model on the residuals to 
ARModel <- Arima(airLinearSeason$residuals, order = c(2,0,0))
ARModel
#calculate the t-statistic for the ar1 coefficient = 6.729658
ARModel$coef["ar1"]/0.0936
#t statistic for ar2 = -0.3356196 . Not statistically significant
ARModel$coef["ar2"] / 0.0976
# ar2 is not signifi. cantIf >2 or <-2 it's statistically significant.
#estimate p value based on a t distribution p = 7.592625e-19
2*pt(-abs((ARModel$coef["ar2"])/0.1102), df = (length(airTrainWindow) - 1))
#estimate p value based on a normal distribution p = 7.682249e-05
2*pnorm(-abs((ARModel$coef["ar1"])/0.1090))
#p is smaller than alpha, 8.220113 < 1.615918e-07, so this is not a random walk. 
#have we accounted for the autocorrelation in the series?
Acf(ARModel$residuals)
#yes. 
#Use the original fitted regression model and the autocorrelation of the errors to forecast for the validation. 
linearForecast <- forecast(airLinearSeason, h = airValidLength)
linearForecast
#generate a forecast of the error terms. 
airErrorForecast <- forecast(ARModel, h = airValidLength)
airErrorForecast
adjustedAirForecast <- linearForecast$mean + airErrorForecast$mean
adjustedAirForecast
plot(airValidWindow, bty = "l", main = "Validation Period", xaxt = "n", xlab = "Year 2000", yaxt = "n", ylab = "ARPM (millions)", ylim = c(47000000, 68000000))
axis(1, at = seq(2000, 2000 + 11/12, 1/12), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
axis(2, at = seq(45000000, 70000000, 2000000), labels = format(seq(45, 70, 2)), las = 2)
lines(adjustedAirForecast, lwd = 2, col = "orange")
lines(linearForecast$mean, col = "green", lwd = 2, lty = 2)
legend(2000, 69000000, c("Actuals", "Adjusted MAPE = 3.82", "Linear MAPE = 3.80"), lwd = c(1,2, 2), lty = c(1,1, 2), col = c("black", "orange", "green"), bty = "n")
accuracy(linearForecast, airValidWindow)
accuracy(adjustedAirForecast, airValidWindow)





