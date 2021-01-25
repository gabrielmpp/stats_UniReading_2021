ds <- read.csv('data/ClimObs_1918to2020.csv', header = TRUE, sep = ',', 
                )
mean(ds$RR)
median(ds$RR)
sd(ds$RR)
range(ds$RR)
summary(ds)
skew_r <- mean(((ds$RR - mean(ds$RR) / sd(ds$RR)))^3)
skew_T <- mean(((ds$Tbar - mean(ds$Tbar) / sd(ds$Tbar)))^3)
skew_r < skew_T #  rainfall skewness is more negative
hist(ds$RR)
plot(ds$Season_yr, ds$Tbar)
