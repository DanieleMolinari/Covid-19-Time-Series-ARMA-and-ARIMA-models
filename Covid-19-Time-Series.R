data <- read.csv("covid19_subset.csv")
str(data)
summary(data)
#I transform dataRep as.Date
data$dateRep <- as.Date(data$dateRep)
#I tranasform deaths as a time series
data$deaths <- ts(data$deaths)


library(tidyverse)

#I divide data io order to have for each country a data set
France <- data %>%
  filter(countriesAndTerritories=="France")
Japan <-data %>%
  filter(countriesAndTerritories=="Japan")
Philip <- data %>%
  filter(countriesAndTerritories=="Philippines")
#I now create training and test data set and I check which row
#is the 15th of May
which(France$dateRep<="2020-05-15")
which(Japan$dateRep<="2020-05-15")
which(Philip$dateRep<="2020-05-15")
trainFrance <- France[(1:69),]
trainJapan <- Japan[(1:66),]
trainPhilip <- Philip[(1:61),]
testFrance <- France[(70:85),]
testJapan <- Japan[(67:82),]
testPhilip <- Philip[(62:77),]


library(ggplot2) 
#Some exploratory analysis plots
plotFrance <- ggplot(trainFrance, aes(dateRep, deaths)) +
  ggtitle("France") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")
plotJapan <- ggplot(trainJapan, aes(dateRep, deaths)) +
  ggtitle("Japan") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")
plotPhilip <- ggplot(trainPhilip, aes(dateRep, deaths)) +
  ggtitle("Philippines") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")

library(gridExtra)

grid.arrange(plotFrance, plotJapan, plotPhilip, ncol = 1)

#from the plots above a log transformation is needed. I do it for bot
#trainig and test data set
trainFrance$deaths <- log(trainFrance$deaths+1)
trainJapan$deaths <- log(trainJapan$deaths+1)
trainPhilip$deaths <- log(trainPhilip$deaths+1)

testFrance$deaths <- log(testFrance$deaths+1)
testJapan$deaths <- log(testJapan$deaths+1)
testPhilip$deaths <- log(testPhilip$deaths+1)

plotFrance <- ggplot(trainFrance, aes(dateRep, deaths)) +
  ggtitle("France") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")
plotJapan <- ggplot(trainJapan, aes(dateRep, deaths)) +
  ggtitle("Japan") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")
plotPhilip <- ggplot(trainPhilip, aes(dateRep, deaths)) +
  ggtitle("Philippines") +
  geom_line() +
  xlab("Time") + ylab("Number of deaths")
grid.arrange(plotFrance, plotJapan, plotPhilip, ncol = 1)


#I create a linear model for France
France_linear <- lm(trainFrance$deaths ~ trainFrance$dateRep)$fitted.values
summary(lm(trainFrance$deaths ~ trainFrance$dateRep))
ggplot(trainFrance, aes(dateRep, deaths)) +
  ggtitle("France") +
  geom_line() +
  geom_line(aes(y=France_linear), color = "#EA30DC", size=1) +
  xlab("Time") + ylab("Number of deaths")
summary(lm(trainFrance$deaths ~ trainFrance$dateRep))
#here linear does not seem to be realistic based on the summary of the model

#I check non-linear model using spline for France
library(splines)
#number of knots q = 3 has been chosen as it is the smallest value that adequately remove trend
#based on the correlogram of residual series.
France_trend <- ns(trainFrance$deaths, 1)
France_spline <- lm(trainFrance$deaths ~ France_trend)
summary(France_spline)

plot_trend_France <- ggplot(trainFrance, aes(dateRep, deaths)) +
  ggtitle("France") +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
plot_trend_France
#I check if the correlogram confirm only trend and no seasonality
acf(trainFrance$deaths, main = "Correlogram France")
#I calculate the residual series
trainFrance$residual <- trainFrance$deaths - France_spline$fitted.values

plotFranceRes <- ggplot(trainFrance, aes(dateRep, residual)) +
  ggtitle("France") +
   geom_line(color = "#4575b4", size=1) +
  xlab("Time") + ylab("Number of deaths")
plotFranceRes
#I check for autocorrelation of the residuals
France_acf <- acf(trainFrance$residual, plot = FALSE)
France_pacf <- pacf(trainFrance$residual, plot = FALSE)
par(mfrow = 1:2)
plot(France_acf, main = "France Residuals")
plot(France_pacf, main = "France Residuals")
par(mfrow = c(1,1))


#forecasting for France
library(forecast)

#I use function holt because my data don't have seasonality and the series
#is not stationary
#I first check the best value for alpha
alpha <- seq(0.01, 0.99, by = 0.01)
rmspeFR <- NA
for(i in seq_along(alpha)) {
  fit <-  holt(ts(trainFrance$deaths), alpha = alpha[i], model = "AAN", h = 69)
  forca <- forecast(fit)
  rmspeFR[i] <- sqrt(sum((trainFrance$deaths-forca$mean)^2)/15)
}
best_alphaFR <- tibble(alpha, rmspeFR)
alphaFR.min <- filter(best_alphaFR, rmspeFR == min(rmspeFR))
ggplot(best_alphaFR, aes(alpha, rmspeFR)) +
  geom_line() +
  geom_point(data = alphaFR.min, aes(alpha, rmspeFR), size = 4, color =  "#EA30DC")
alphaFR.min$alpha
#best alpha = 0.1

#I now make forecasting using Exponential Smoothing with function holt
#and Auto ARIMA
par(mfrow = c(1,1))
pred_exp_FR <- holt(ts(trainFrance$deaths), alpha = 0.1, model = "AAN", h = 16)
forca_exp_FR <- forecast(pred_exp_FR, 16)
plot(forca_exp_FR)
rmspe_exp_FR <- sqrt(sum((testFrance$deaths - forca_exp_FR$mean)^2)/15)
rmspe_exp_FR


auto_arima_FR <- auto.arima(trainFrance$deaths)
forca_arima_FR <- forecast(auto_arima_FR, 16)
plot(forca_arima_FR)
rmspe_arima_FR <- sqrt(sum((testFrance$deaths - forca_arima_FR$mean)^2)/15)
rmspe_arima_FR


#Now I analyse the time series of Japan starting witha a linear model
trainJapan$fv_liear <- lm(trainJapan$deaths ~ trainJapan$dateRep)$fitted.values
ggplot(trainJapan, aes(dateRep, deaths)) +
  ggtitle("Japan") +
  geom_line() +
  geom_line(aes(y=fv_liear), color = "#EA30DC", size=1) +
  xlab("Time") + ylab("Number of deaths")
summary(lm(trainJapan$deaths ~ trainJapan$dateRep))
#here linear seems more realistic than France even though the trend is quite steep probably spline 
#could do a better job.
#I use spline and the compare the two models
#number of knots q = 3 has been chosen because is the best fitting for the data

Japan_trend <- ns(trainJapan$deaths, 3)
trainJapan$fv_spline <- lm(trainJapan$deaths ~ Japan_trend)$fitted.values
summary(lm(trainJapan$deaths ~ Japan_trend))

plotJapanTrans <- ggplot(trainJapan, aes(dateRep, deaths)) +
  ggtitle("Japan Spline") +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
plotJapanTrans
#The non-linear model seems to explain better the data
#I now calculate the residual series
trainJapan$spline_res <- trainJapan$deaths - trainJapan$fv_spline

plot_Japan_Spline_Res <- ggplot(trainJapan, aes(dateRep, spline_res)) +
  ggtitle("Japan Spline Residuals") +
  geom_line(color = "#4575b4", size=1) +
  xlab("Time") + ylab("Number of deaths")
plot_Japan_Spline_Res


#I check aoutocorrelation of the residuals
par(mfrow = 1:2)
acf(trainJapan$spline_res)
pacf(trainJapan$spline_res)
par(mfrow = c(1,1))




#I can start with forecasating
#As for France I choose the best value of alpha to use in the holt function
rmspeJ <- NA
for(i in seq_along(alpha)) {
  fit <-  holt(ts(trainJapan$deaths), alpha = alpha[i], model = "AAN", h = 66)
  forca <- forecast(fit)
  rmspeJ[i] <- sqrt(sum((trainJapan$deaths-forca$mean)^2)/15)
}
best_alphaJ <- tibble(alpha, rmspeJ)
alphaJ.min <- filter(best_alphaJ, rmspeJ == min(rmspeJ))
ggplot(best_alphaJ, aes(alpha, rmspeJ)) +
  geom_line() +
  geom_point(data = alphaJ.min, aes(alpha, rmspeJ), size = 4, color =  "#EA30DC")
alphaJ.min$alpha

#Exponential Smooth 
pred_exp_J <- holt(ts(trainJapan$deaths), alpha = 0.04, model = "AAN", h = 16)
forca_exp_J <- forecast(pred_exp_J, 16)
plot(forca_exp_J)
rmspe_exp_J <- sqrt(sum((testJapan$deaths - forca_exp_J$mean)^2)/15)
rmspe_exp_J

#Auto ARIMA
auto_arima_J <- auto.arima(trainJapan$deaths)
forca_arima_J <- forecast(auto_arima_J, 16)
plot(forca_arima_J)
rmspe_arima_J <- sqrt(sum((testJapan$deaths - forca_arima_J$mean)^2)/15)
rmspe_arima_J



#I noe analyse Philippines starting with the linear model
trainPhilip$fv_liear <- lm(trainPhilip$deaths ~ trainPhilip$dateRep)$fitted.values
ggplot(trainPhilip, aes(dateRep, deaths)) +
  ggtitle("Philippines") +
  geom_line() +
  geom_line(aes(y=fv_liear), color = "#EA30DC", size=1) +
  xlab("Time") + ylab("Number of deaths")
summary(lm(trainPhilip$deaths ~ trainPhilip$dateRep))


#I use now splines 
#number of knots q = 1 has been chosen as it is the smallest value that adequately remove trend
#based on the correlogram of residual series.
Philip_trend <- ns(trainPhilip$deaths,1)
trainPhilip$fv_spline <- lm(trainPhilip$deaths ~ Philip_trend)$fitted.values
summary(lm(trainPhilip$deaths ~ Philip_trend))

plotPhilipTrans <- ggplot(trainPhilip, aes(dateRep, deaths)) +
  ggtitle("Philippines Spline") +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
plotPhilipTrans
#I calculate the residual series
trainPhilip$spline_res <- trainPhilip$deaths - trainPhilip$fv_spline

plot_Philp_Spline_Res <- ggplot(trainPhilip, aes(dateRep, spline_res)) +
  ggtitle("Philippines Spline Residuals") +
  geom_line(color = "#4575b4", size=1) +
  xlab("Time") + ylab("Number of deaths")
plot_Philp_Spline_Res

#I now check autocorrelation
acf(trainPhilip$spline_res)
pacf(trainPhilip$spline_res)



#Prediction Philippines
rmspePH <- NA
for(i in seq_along(alpha)) {
  fit <-  holt(ts(trainPhilip$deaths), alpha = alpha[i], model = "AAN", h = 61)
  forca <- forecast(fit)
  rmspePH[i] <- sqrt(sum((trainPhilip$deaths-forca$mean)^2)/15)
}
best_alphaPH <- tibble(alpha, rmspePH)
alphaPH.min <- filter(best_alphaPH, rmspePH == min(rmspePH))
ggplot(best_alphaPH, aes(alpha, rmspePH)) +
  geom_line() +
  geom_point(data = alphaPH.min, aes(alpha, rmspePH), size = 4, color =  "#EA30DC")
alphaPH.min$alpha


pred_exp_PH <- holt(ts(trainPhilip$deaths), alpha = 0.04, model = "AAN", h = 16)
forca_exp_PH <- forecast(pred_exp_PH, 16)
plot(forca_exp_PH)
rmspe_exp_PH <- sqrt(sum((testPhilip$deaths - forca_exp_PH$mean)^2)/15)
rmspe_exp_PH


auto_arima_PH <- auto.arima(trainPhilip$deaths)
forca_arima_PH <- forecast(auto_arima_PH, 16)
plot(forca_arima_PH)
rmspe_arima_PH <- sqrt(sum((testPhilip$deaths - forca_arima_PH$mean)^2)/15)
rmspe_arima_PH


#I now create plots fo the three countries using all data and spline to draw trends
#and a red line to divide data in training and test data set and thus 
#compare them with the forecasting plots
Gen_France_trend <- ns(France$deaths, 1)
Gen_France_spline <- lm(France$deaths ~ Gen_France_trend)
Gen_plotFrance <- ggplot(France, aes(dateRep, log(deaths))) +
  ggtitle("France") +
  geom_line() +
  geom_vline(xintercept=as.numeric(France$dateRep[69]), colour = "red") +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
Gen_plotFrance

Gen_Japan_trend <- ns(trainJapan$deaths, 3)
Gen_Japan_spline <- lm(trainJapan$deaths ~ Gen_Japan_trend)
Gen_plotJapan <- ggplot(Japan, aes(dateRep, log(deaths))) +
  ggtitle("Japan") +
  geom_line() +
  geom_vline(xintercept=as.numeric(Japan$dateRep[69]), colour = "red") +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
Gen_plotJapan

Gen_Philip_trend <- ns(Philip$deaths, 1)
Gen_Philip_spline <- lm(Philip$deaths ~ Gen_Philip_trend)
Gen_plotPhilip <- ggplot(Philip, aes(dateRep, log(deaths))) +
  ggtitle("Philippines") +
  geom_line() +
  geom_vline(xintercept=as.numeric(Philip$dateRep[69]), colour = "red") +
  geom_smooth(method = "lm", formula = y ~ ns(x, 3), color = "#EA30DC", size=1, se = FALSE) +
  xlab("Time") + ylab("Number of deaths")
Gen_plotPhilip

