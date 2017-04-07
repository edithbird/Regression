library(forecast)
library(dplyr)
library(Hmisc)
setwd("/Users/Chris Iyer/Documents/Assignment5Predictive/")
canadian <- read.csv("CanadianWorkHours.csv", header = TRUE, stringsAsFactors = FALSE)
names(canadian)[2] <- "Hours"
#convert to a time series
canadian.ts <- ts(canadian$Hours, start=c(1966, 1), frequency=1)
plot(canadian.ts, main = "Canadian")
validLength <- 5
trainLength <- length(canadian.ts) - validLength
windowTrain <- window(canadian.ts, start = c(1966,1), end = c(1966, trainLength))
windowValid <- window(canadian.ts, start = c(1966, trainLength + 1), end =  c(1966, trainLength + validLength))
yrange <- range(canadian.ts)
#plot ts and linear trend
plot(c(1966, 2000), yrange, type="n", xlab="Year",  ylab = "Average Weekly Hours Worked",  main = "Average Weekly Hours Worked in Canadian Manufacturing", bty="l", xaxt="n", yaxt="n")
# Add the x-axis
axis(1, at=seq(1966,2000,1), labels=format(seq(1966,2000,1)))
# Add the y-axis
axis(2, at=seq(30,40, 0.5), labels=format(seq(30,40, 0.5)), las=2)
# Add the time series canadian
lines(canadian.ts, bty="l")
lines(linearTrain$fitted.values, col = "red")
# Add a legend
legend(1981,37.5, c("Actual Data", "Linear Trend Model"), lty=c(1,1), col=c("black", "red"), bty="n")
#strong autocorrelaion at 1, indicating trend. Stickiness. 
Acf(canadian.ts, lag.max = 1)
#difference and correct for trend
Acf(diff(canadian.ts, lag = 1), lag.max = 12)
canadaAcf <- Acf(canadian.ts)
canadaAcf
plot(diff(canadian.ts, lag=1))
#is this a random walk?
canadaFit <- arima(canadian.ts, order = c(1,0,0))
canadaFit
#p value using a t distribution ar1 = 0.189019 
2*pt(-abs((1 - canadaFit$coef["ar1"]) / 0.0462), df=length(canadian.ts)-1)
#p using a nirmal distribution ar1 = 0.1801306
2*pnorm(-abs((1 - canadaFit$coef["ar1"]) / 0.0462))
#If alpha = 0.05, p > alpha, accept the null. This is a random walk. 
walMart <- read.csv("WalMartStock1.csv", header = TRUE, stringsAsFactors = FALSE)
walMart.ts <- ts(walMart$Close, start = c(2001, 1), frequency = 248)
yrange <- range(walMart.ts)
plot(c(2001, 2002), yrange, type = "n", xlab = "Year", ylab = "Daily Closing Price", main = "WalMart", bty = "l", xaxt="n", yaxt="n")
lines(walMart.ts, bty="n", col = "black")
#lines(walMart2.ts, bty = "n", col ="blue")

# Add the x-axis broken out by month
axis(1, at=seq(2001,2001+11/12,1/12), labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"))

# Add the y-axis
axis(2, at=seq(40,65, 2), labels=format(seq(40,65, 2)), las=2)
legend(2001,60, c("Actual"), lty=c(1), col=c("black"), bty="n")
walMartAcf <- Acf(walMart.ts, lag.max = 1)
walMartAcf
Acf(diff(walMart.ts, lag = 1))
plot(diff(walMart.ts, lag = 1))
walMartFit <- arima(walMart.ts, order = c(1,0,0))
walMartFit
# Calculate the two-tailed p-value from a t-distribution, p = 0.01896261
2*pt(-abs((1 - walMartFit$coef["ar1"]) / 0.0187), df=length(walMart.ts)-1)
# Now calculate it using the normal distribution, p = 0.01818593
2*pnorm(-abs((1 - walMartFit$coef["ar1"]) / 0.0187))
#souvenir sales
souvenirSales<- read.csv("SouvenirSales.csv")
souvenir.ts <- ts(souvenirSales$Sales, start = c(1995,1), frequency = 12)
length(souvenir.ts)
plot(souvenir.ts, type = "n", xaxt = "n", yaxt = "n", xlab = "Year", ylab = "Sales (thousands)", main = "Souvenir Sales")
lines(souvenir.ts, col = "grey48", lwd = 2, lty = 1, pch = 18, type = "o")
# Add the x-axis
axis(1, at=seq(1995,2002,1), labels=format(seq(1995,2002,1)))
axis(2, at = seq(0, 110000, 20000), labels = format(seq(0, 110, 20)), las = 2)
souvenirFit <- Arima(souvenir.ts, order = c(1,0,0))
souvenirFit
#fit t distribution p = 0.003975943
2*pt(-abs((1 - souvenirFit$coef["ar1"])/ 0.1178), df = length(souvenir.ts) - 1)
#normal distribution, p = 0.003048286
2 * pnorm(-abs((1 - souvenirFit$coef["ar1"]) / 0.1178))
#Shipments of Household Appliances
appliance <- read.csv("ApplianceShipments1.csv", header = TRUE, stringsAsFactors = FALSE)
head(appliance)
#this is the improperly ordered set
appliance <- appliance %>% select(Quarter, Shipments)
#split Quarter column
col1Split <- strsplit(appliance$Quarter, "-")
#Make quarter column into proper columns in a dataframe
tempDF <- data.frame(matrix(unlist(col1Split), nrow = length(col1Split), byrow = TRUE), appliance$Shipments)
#reorder rows by year and then by quarter
tempDF <- tempDF[order(tempDF$X2, tempDF$X1),]
#merge quarter and year back together
m <- paste0(tempDF$X1, "-", tempDF$X2)
#put a new dataframe together
newDF <- data.frame(m, tempDF$appliance.Shipments)
#rename columns
names(newDF)[1] <- "Quarter"
names(newDF)[2] <- "Shipments"
#make this into a timeseries
applianceShipments.ts <- ts(newDF$Shipments, start = c(1985, 1), end = c(1989,4), frequency = 4)
plot(applianceShipments.ts, type = "l", ylab =  "Appliance Shipments", main = "Correct Appliance Shipments")
yrange <- range(applianceShipments.ts)
# Set up the plot
plot(c(1985, 1989), yrange, type="n", xlab="Year",  ylab="Appliance Sales", bty="l", xaxt="n", yaxt="n",  main = "Appliance Store Sales")
# Add the time series canadian training set
lines(applianceShipments.ts, bty="l", type = "o")
axis(1, at=seq(1985,1989,1), labels=format(seq(1985,1989,1)), minor.tick(nx = 4))
# Add the y-axis
axis(2, at=seq(3000, 5000, 200), labels=format(seq(3000, 5000, 200)), las=2)
legend(1986, 4200, c("Appliance Store Sales"), col = c("black"), lwd = c(1,1),  bty = "n", lty = c(1,1))
applianceAcf <- Acf(applianceShipments.ts)
applianceAcf
#Acf with detrend dossnt make it better.
Acf(diff(applianceShipments.ts, lag=1), lag.max=12, main="ACF Plot for Differenced Series")
applianceFit <- Arima(applianceShipments.ts, order = c(1,0,0))
applianceFit
# Calculate the two-tailed p-value from a t-distribution p = 0.004612313 
2*pt(-abs((1 - applianceFit$coef["ar1"]) / 0.2230), df=length(applianceShipments.ts)-1)
# Now calculate it using the normal distribution p = 0.001328536
2*pnorm(-abs((1 - applianceFit$coef["ar1"]) / 0.2230))
