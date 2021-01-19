# Plot 1
x <- seq(0, 2*pi, 0.1)
y = sin(x)
plot(x, y, main='sine function', xlab='x', ylab='y(x)', type='l', lwd=2)
grid()

# Plot 2
y2 <- sin(x) ^ 2
lines(x, y2, lwd=2, type='l', col='blue')
legend('bottomleft', legend = c('Line 1', 'Line 2'), col=c('red', 'blue'), lty=1)
dev.copy(png, 'Practical\ 1/plots/myplot.png')
dev.off()
library(ggplot2)


