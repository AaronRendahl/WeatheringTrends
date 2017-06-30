## ---- include=FALSE---------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(prompt=TRUE, comment=NA)
options(width=120)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#       install.packages("devtools")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#       devtools::install_github("fisherba/WeatheringTrends")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#       install.packages("ggplot2")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  file.copy(system.file(package="WeatheringTrends", "extscripts", "WeatheringTrendsBeginner.R"), ".")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  help(package="WeatheringTrends")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  vignette("WeatheringTrendsBeginnerR", "WeatheringTrends")

## ---------------------------------------------------------------------------------------------------------------------
library(WeatheringTrends) # load package once per R session

## ---------------------------------------------------------------------------------------------------------------------
library(ggplot2) # load package once per R session

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  getwd()

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  file.copy(system.file(package="WeatheringTrends", "extdata", "WTexampleLP.csv"), ".")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  WTexampleLP <- read.csv("WTexampleLP.csv")

## ---- echo=FALSE------------------------------------------------------------------------------------------------------
## for vignette, read file directly instead
WTexampleLP <- read.csv(system.file(package="WeatheringTrends", "extdata", "WTexampleLP.csv"))

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  View(WTexampleLP)

## ---------------------------------------------------------------------------------------------------------------------
well1 <- subset(WTexampleLP, Site=="LP_Well1")
well2 <- subset(WTexampleLP, Site=="LP_Well2")

## ---- fig.width=4, fig.height=3---------------------------------------------------------------------------------------
ggplot(well1) + scale_y_reverse() +
  geom_point(aes(x=CaO, y=voBottom/100, colour=Site))

## ---------------------------------------------------------------------------------------------------------------------
mm <- c("Na2O", "MgO", "CaO", "Fe2O3")

## ---------------------------------------------------------------------------------------------------------------------
ii <- c("Zr_ppm", "Nb_ppm")

## ---- fitmodels-------------------------------------------------------------------------------------------------------
well1rock <- FitElementRatios(mm, ii, "voBottom", well1)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  mm_full <- c("Na2O", "MgO", "Al2O3", "SiO2", "K2O",
#          "CaO", "MnO", "Fe2O3", "P2O5",
#          "Ba_ppm", "Ce_ppm", "Co_ppm", "Cr_ppm",
#          "Cu_ppm", "La_ppm", "Nd_ppm", "Ni_ppm",
#          "Pb_ppm", "Rb_ppm", "Sc_ppm", "Sr_ppm",
#          "Th_ppm", "V_ppm", "Y_ppm", "Zn_ppm")
#  ii_full <- c("Zr_ppm", "TiO2", "Hf_ppm", "Nb_ppm")
#  well1rock_full <- FitElementRatios(mm_full, ii_full, "voBottom", well1)

## ---------------------------------------------------------------------------------------------------------------------
plot(well1rock, scales="sliced")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  plot(well1rock, scales="sliced") # applies the same horizontal scale range to all plots
#  plot(well1rock, scales="free") # makes scales fit each individual plot

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  getwd()

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  pdf("well1rock_todaysdate.pdf", height=8.5, width=4)
#  plot(well1rock, scales="sliced")
#  dev.off()

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  pdf("well1rock_full_todaysdate.pdf", height=60, width=8)
#  plot(well1rock_full, scales="sliced")
#  dev.off()

## ---------------------------------------------------------------------------------------------------------------------
coef(well1rock, type="output")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  write.csv(coef(well1rock, type="output"), file="well1rockoutput_todaysdate.csv", row.names=FALSE)

## ---------------------------------------------------------------------------------------------------------------------
coef(well1rock, type="par.long")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  write.csv(coef(well1rock,type="par.long"), file="well1rockCI_todaysdate.csv", row.names=FALSE)

## ---------------------------------------------------------------------------------------------------------------------
coef(well1rock, type="par")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  write.csv(coef(well1rock,type="par"), file="well1rockPAR_todaysdate.csv", row.names=FALSE)

## ---------------------------------------------------------------------------------------------------------------------
taus1 <- FitTaus(c("Na2O", "MgO", "CaO", "Fe2O3"),
                 c("Zr_ppm", "Nb_ppm"),
                 "voBottom", well1, cutoff=1230)

## ---------------------------------------------------------------------------------------------------------------------
taus1 <- FitTaus(mm, ii, "voBottom", well1, cutoff=1230)

## ---------------------------------------------------------------------------------------------------------------------
plot(taus1, scales="free")

## ---- eval=FALSE------------------------------------------------------------------------------------------------------
#  pdf("taus1_todaysdate.pdf", height=8.5, width=4)
#  plot(taus1, scales="free")
#  dev.off()

## ---- fig.width=3, fig.height=3---------------------------------------------------------------------------------------
plot(TiO2 ~ Nb_ppm, data=well1)
abline(lm(TiO2 ~ Nb_ppm, data=well1))

## ---------------------------------------------------------------------------------------------------------------------
cor.test(~ TiO2 + Nb_ppm, data=well1)

## ---------------------------------------------------------------------------------------------------------------------
sum(complete.cases(well1[c("TiO2", "Nb_ppm")]))

## ---- fig.width=3, fig.height=3---------------------------------------------------------------------------------------
plot(Zr_ppm ~ Hf_ppm, data=well1)
abline(lm(Zr_ppm ~ Hf_ppm, data=well1))
cor.test(~ Zr_ppm + Hf_ppm, data=well1)
sum(complete.cases(well1[,c("Zr_ppm", "Hf_ppm")]))

## ---------------------------------------------------------------------------------------------------------------------
well1immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well1)
well1immobile$s.overall # display standard deviation for the element Nb, you may skip this step
mean(well1$Nb_ppm) # mean of element Nb, you may skip this step
# coefficient of variation of element Nb is a computation using two codes above
well1immobile$s.overall/mean(well1$Nb_ppm)*100

## ---------------------------------------------------------------------------------------------------------------------
well2immobile <- FitElementRatio("Nb_ppm", "one", "voBottom", well2)
well2immobile$s.overall
mean(well2$Nb_ppm)
well2immobile$s.overall/mean(well2$Nb_ppm)*100

