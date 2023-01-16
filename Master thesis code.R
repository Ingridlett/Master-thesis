library(tidyverse)
library(janitor)
library(dplyr)
library(readxl)
library(tseries)# for ADF unit root tests
library(dynlm)
library(knitr)
library(broom)#for `glance(`) and `tidy()`
library(flextable)
library(mosaic)
library(car)#for `hccm()` robust standard errors
library(sandwich)
library(knitr) #for kable()
library(forecast) 
library(lmtest) #for `coeftest()` and `bptest()`.
library(rockchalk) 
library(urca)
library(ggplot2)


# View(handelstall)
# str(handelstall)
# head(handelstall)
# names(handelstall)
# summary(handelstall)

# View(konsum)
# str(konsum)
# head(konsum)
# names(konsum)
# summary(konsum)

summary(handelstall)

klippfisk <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "klippfisk hel", date > "2010-12-01")
saltet <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "saltet hel, konvensjonell", date > "2010-12-01")
fryst <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "fryst hel", date > "2010-12-01")

### Klippfisk Time Series display ###
### Looks to be forming peaks around year end each year (in line with tradition of bacalao in december),
### however it is a little hard to tell, should try to display with every year and month displayed
### portraying a common trend with a nonconstant mean?? The series are probably nonstationary.
klippfisk <- ts(klippfisk, start = c(2010,12), end = c(2022, 11), frequency = 12)
ts.plot(klippfisk[,"mengde"],klippfisk[,"Verdi"], type="l", lty=c(1,2), col=c(1,2), main="KLIPPFISK")
legend("topleft", border=NULL, legend=c("mengde","Verdi"), lty=c(1,2), col=c(1,2))

### Salted Fish Time Series display ###
### Clear peaks showing seasonality around year end each year
saltet <- ts(saltet, start = c(2010,12), end = c(2022, 11), frequency = 12)
ts.plot(saltet[,"mengde"],saltet[,"Verdi"], type="l", lty=c(1,2), col=c(1,2), main="SALTET")
legend("topleft", border=NULL, legend=c("mengde","Verdi"), lty=c(1,2), col=c(1,2))

### Frozen Fish Time Series display ###
### There are definite peaks but showing more variation than the other two time series. Very high peak
### late 2021, perhaps around Christmas, compared to earlier years - What could it be due to? Perhaps
### something to do with the pandemic (COVID-19) this would be interesting to delve into in terms of 
### structural changes and our research question in that part of the paper.
fryst <- ts(fryst, start = c(2010,12), end = c(2022, 11), frequency = 12)
ts.plot(fryst[,"mengde"],fryst[,"Verdi"], type="l", lty=c(1,2), col=c(1,2), main="FRYST")
legend("topleft", border=NULL, legend=c("mengde","Verdi"), lty=c(1,2), col=c(1,2))


??Plot

###  comment: We interpret that "mengde" is amount of fish sold per tonnes and that value (NOK/EUR) is per thousands
### example: Cod (whole) sold 2010-01-01 was amount 177.01 to a NOK value of 4000.00 meaning 
### we sold in January 2010 177 tonnes of cod to a value of MNOK 4,000,000 (4000*1000)

library(tseries)

# Test for stationarity,
# H0: Unit-root, i.e., nonstationary.
adf.test(klippfisk[,"mengde"])
adf.test(klippfisk[,"Verdi"])

adf.test(saltet[,"mengde"])
adf.test(saltet[,"Verdi"])

adf.test(fryst[,"mengde"])
adf.test(fryst[,"Verdi"])

# ADF in first differences
adf.test(diff(klippfisk[,"mengde"]))
adf.test(diff(klippfisk[,"Verdi"]))

adf.test(diff(saltet[,"mengde"]))
adf.test(diff(saltet[,"Verdi"]))

adf.test(diff(fryst[,"mengde"]))
adf.test(diff(fryst[,"Verdi"]))

### The stationarity tests indicate that all three series are ...

# check for cointegration
# estimate the long-run relationship
library(dynlm)
fit1 <- dynlm(mengde~Verdi-1, data = klippfisk)
summary(fit1)

fit2 <- dynlm(mengde~Verdi-1, data = saltet)
summary(fit2)

fit3 <- dynlm(mengde~Verdi-1, data = fryst)
summary(fit3)

### the intercept term is omitted/not omitted bacause...
### has or has no economic meaning

# residuals
ehat1_klipp <- resid(fit1)
plot(ehat1_klipp)

ehat2_salt <- resid(fit2)
plot(ehat2_salt)

ehat3_fryst <- resid(fit3)
plot(ehat3_fryst)


library("ggplot2")


