## ------------------------------------------------------------------------
library(WeatheringTrends)
data(welldata)
(fit1 <- FitElementRatio("Sr", "Zr", "depth.top", welldata))
plot(fit1)

## ------------------------------------------------------------------------
coef(fit1)
coef(fit1, type="par")
coef(fit1, type="par.long")

## ------------------------------------------------------------------------
(fit2 <- FitElementRatio("Zr", "Sr", "depth.top", welldata))
plot(fit2)

## ---- fig.show='hold', fig.width=8, fig.height=10------------------------
fits <- FitElementRatios(c("Sr", "Pb", "CaO"), c("Zr", "V"), "depth.top", welldata, profile=FALSE, verbose=FALSE)
plot(fits)

## ------------------------------------------------------------------------
coef(fits)
coef(fits, type="par")
coef(fits, type="par.long")

## ---- fig.width=8, fig.height=4------------------------------------------
(fita <- FitElementRatio("Sr", "Zr", "depth.top", welldata, profile=FALSE))
par(mfrow=c(1,2))
plot(fita)
plot(fita, log=FALSE)

## ---- fig.width=8, fig.height=4------------------------------------------
(fitb <- FitElementRatio("Sr", "Zr", "depth.top", welldata, loglinear=TRUE, profile=FALSE))
par(mfrow=c(1,2))
plot(fitb)
plot(fitb, log=FALSE)

## ---- fig.width=8, fig.height=4------------------------------------------
tau1 <- FitTau("Sr", "Zr", "depth.top", welldata, cutoff=7.5)
plot(tau1)

## ---- fig.width=6, fig.height=6------------------------------------------
taus <- FitTaus(c("Sr", "Pb"), c("Zr","V"), "depth.top", welldata, cutoff=c(7.5,12))
plot(taus)

