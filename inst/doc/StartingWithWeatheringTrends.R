## ------------------------------------------------------------------------
library(WeatheringTrends)
data(welldata)
fit1 <- FitElementRatio("Sr", "Zr", "depth.top", welldata)
fit1$par
fit1$output
plot(fit1)

## ------------------------------------------------------------------------
fit1 <- profile(fit1, "d")
fit1$confint
plot(fit1)

## ------------------------------------------------------------------------
fit2 <- FitElementRatio("Zr", "Sr", "depth.top", welldata)
fit2$par
fit2$output
plot(fit2)

## ---- fig.show='hold', fig.width=8, fig.height=8-------------------------
fits <- FitElementRatios(c("Sr", "Pb"), c("Zr", "V"), "depth.top", welldata)
par(mfrow=c(2,2))
for(i in 1:2) for(j in 1:2) plot(fits[[i]][[j]])

