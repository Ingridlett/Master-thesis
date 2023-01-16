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

write.table(handelstall, "C:/Users/lettr/OneDrive/Desktop/handelstall.xlxs", sep=",",row.names = FALSE)

handelstall.filtrert <- read_excel("C:/lettr/OneDrive/Desktop/handelstall.v1.xlxs", 
                            sheet = "Sheet 1")
summary(handelstall)

Pris <- mutate(handelstall=verdi_eur*mengde)

x1 <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "klippfisk hel", date > "2010-12-01")
x2 <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "saltet hel, konvensjonell", date > "2010-12-01")
x3 <- filter(handelstall, RAPPORT_LAND == "NO", FLYT == "E", Isocode=="PT", PRODUKTHOVEDNO == "fryst hel", date > "2010-12-01")

merge <- rbind(x1,x2,x3)
merge <- merge %>% mutate(Pris = verdi_eur/mengde)
merge$Pris
summary(merge)
names(merge)

??Plot

###  comment: We interpret that "mengde" is amount of fish sold per tonnes and that value (NOK/EUR) is per thousands
### example: Cod (whole) sold 2010-01-01 was amount 177.01 to a NOK value of 4000.00 meaning 
### we sold in January 2010 177 tonnes of cod to a value of MNOK 4,000,000 (4000*1000)

library("ggplot2")

x.ts <- ts(merge$Pris, frequency = 12, start = c("2011"), end = ("2022"))
plot(x.ts)

x1 <- x1 %>% mutate(Pris=verdi_eur/mengde)
x1ts <- x1$Pris, frequency()
                    
ts(merge$Pris, frequency = 12, start = c("2011"), end = ("2022"))
plot(x.ts)

y11 <- merge$Pris
y12 <- merge$Pris

plot(x.ts,y11,y12)
