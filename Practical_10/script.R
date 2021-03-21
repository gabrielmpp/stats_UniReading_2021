# ---- Downloading HadCRUT4 annual data ---- #

hadcrut_url <- 'https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt'
destfile <- 'data/hadcrut.txt'
download.file(hadcrut_url, destfile)

# ---- Reading and selecting relevant columns ---- #
# The first column is years and the second the median temperature

df <- read.table(destfile)
df <- df[, c(1, 2)]
colnames(df) <- c("Year", "Temp")
df <- df[df$Year >= 1972, ]

plot(df, type='l')
lines(c(2000,2014), c(0.45, 0.45), col='red')
# Was there a pause in global warming?

# ---- Simple test: comparing slopes of 1972-2000 to 2000-2014 ---- #
df1 <- df[df$Year < 2000 & df$Year >= 1972, ]
df2 <- df[df$Year <= 2014 & df$Year >= 2000, ]
model1 <- lm(df1$Temp ~ df1$Year)
model2 <- lm(df2$Temp ~ model1$coefficients[1] + df2$Year)
predict1 <- predict(model1, df1)
predict2 <- predict(model2, df2)
plot(df, type='l')
lines(df1$Year, predict1, col='red')
lines(df2$Year, predict2, col='blue')

summary(model1)
summary(model2)
# Slope 1 is greater than slope 2. Also, it is not zero at a > 99% confidence level
# Slope 2 is not significantly different than zero at a 95% confidence level

# However, we cannot simply compare the two slopes without having the intercept fixed. Let's run again the linear model for 2000-2014
intercept1 = model1$coefficients[1]
shifted_df2 = df2$Temp - intercept1  # Remove the intercept based on the first period
model3 <- lm(shifted_df2 ~ 0 + df2$Year)  # having zero here tells R to only fit a slope
summary(model1)
summary(model3)
# Now the slopes are much closer! 

# ---- Histogram based on a Monte Carlo simulation ---- #
# Here we will use the Monte Carlo method to generate artificial time series within the
# 1972-2000 variability in order to see how unlikely the 2000-2014 trend was
num_sims <- 1000
sdev_model1 <- sd(model1$residuals)
a =  model1$coefficients[1]
for (sim in 1:num_sims){
  # Here we generate random noise that represents the interannual variability between 1972-2000
  random_noise <- rnorm(nrow(df1), mean=0, sd=sdev_model1)
  # And add that to the observed linear trend
  perturbed_data <- predict1 + random_noise
  perturbed_data = perturbed_data -a
  simulated_slopes[sim] <- lm(perturbed_data ~ 0+df1$Year)$coefficients[1]
}

hist(simulated_slopes)
# We can see that the 2000-2014 falls well within the expected trend!


