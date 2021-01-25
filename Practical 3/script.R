library(ggplot2)
library(dplyr)

accuracy <- function(obs, fcast){
  
  A <-sum(obs & fcast)
  B <-sum(!obs & fcast)
  C <-sum(obs & !fcast)
  D <-sum(!obs & !fcast)
  return((A+D)/(A+B+C+D))
}

# Data ingestion
data24 <- read.csv('data/temps2012_24hr.csv')
data72 <- read.csv('data/temps2012_72hr.csv')
good_days <-!(is.na(data24$obs)   |   is.na(data24$fcst)   | is.na(data72$fcst))
data24<-data24[good_days,]
data72<-data72[good_days,]


# Plotting time series
plot(data24$obs, type='l', main='MetOffice temperature forecast',
     xlab='Day', ylab='Temperature (deg Celsius)')
lines(data24$fcst, col='blue')
lines(data72$fcst, col='red')
legend(x='topright', legend=c('Obs', 'Pred 24h', 'Pred 72h'),
       col=c('black', 'blue', 'red'), lty=c(1,1,1))

# Checking Bias
bias24 <- data24$fcst - data24$obs
bias72 <- data72$fcst - data24$obs
sum(bias24)
sum(bias72)


# Checking sharpness
sd(data24$fcst)
sd(data72$fcst)
sd(data24$obs)

hist(data24$fcst)
hist(data24$obs)

# Checking accuracy
cor(data24$obs, data24$fcst)
cor(data24$obs, data72$fcst)
rmse24 <- sqrt(mean((data24$obs-data24$fcst)^2))
rmse72 <- sqrt(mean((data24$obs-data72$fcst)^2))

# Contingency table
thres = 19.5
obs_warm = (data24$obs > thres)
f24_warm = (data24$fcst > thres)
f72_warm = (data72$fcst > thres)

acc24 = accuracy(obs_warm, f24_warm)
acc72 = accuracy(obs_warm, f72_warm)
