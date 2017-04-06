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
#fit a model Linear Trend and Season
airLinearSeason <- tslm(airTrainWindow ~ trend + season)
summary(airLinearSeason)
#look at Acf Plot of the residuals
airResidualsAcf <- Acf(airLinearSeason$residuals)
#look at the values that are plotted
airResidualsAcf
#run an AR(1) model on the residuals to 
ARModel <- Arima(airLinearSeason$residuals, order = c(1,0,0))
ARModel
#calculate the t-statistic for the ar1 coefficient
ARModel$coef["ar1"]/0.0743
#t-statistic = 8.220113. If >2 or <-2 it's statistically significant.
#estimate p value based on a t distribution p = 7.076335e-07
2*pt(-abs((1 - ARModel$coef["ar1"])/0.0743), df = (length(airTrainWindow) - 1))
#estimate p value based on a normal distribution p = 1.615918e-07
2*pnorm(-abs((1 - ARModel$coef["ar1"])/0.0743))
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





