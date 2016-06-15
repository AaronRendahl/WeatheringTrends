## ------------------------------------------------------------------------
library(WeatheringTrends)
data(welldata)
fit1 <- FitElementRatio("Sr", "Zr", "depth.top", welldata)
fit1$par
fit1$output
fit1$confint
plot(fit1)

## ------------------------------------------------------------------------
fit2 <- FitElementRatio("Zr", "Sr", "depth.top", welldata)
fit2$par
fit2$output
plot(fit2)

## ---- fig.show='hold', fig.width=8, fig.height=10------------------------
fits <- FitElementRatios(c("Sr", "Pb", "CaO"), c("Zr", "V"), "depth.top", welldata, profile=FALSE)
nm <- length(fits)
ni <- length(fits[[1]])
par(mfrow=c(nm, ni), mar=c(2.5, 2.5, 2, 0))
for(i in 1:nm) for(j in 1:ni) plot(fits[[i]][[j]])

