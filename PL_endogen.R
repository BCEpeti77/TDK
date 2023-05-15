library(tsDyn)
library(vars)
library(urca)
library(forecast)
library(lattice)
library(tseries)
library(readxl)
library(lubridate)
library(zoo)
library(tidyverse)
library(stringr)
library(frequencyConnectedness)
setwd("C:/Users/Gép/Desktop/TDK/Adatok/Rendes adatok/Végleges")

PL_endogen <- as.data.frame(read.csv("Lengyelország_endogen.csv", sep = ","))
PL_endogen <- ts(PL_endogen[,2:4],frequency = 4, start = 2006)
plot.ts (PL_endogen)

#stacionaritás vizsgálata
lapply(PL_endogen, function(x){kpss.test(x)}) # Egységgyök
lapply(PL_endogen, function(x){adf.test(x)}) # Egységgyök
lapply(PL_endogen, function(x){pp.test(x)}) # Egységgyök

#differenciálás
PLendogendiff <- diff(PL_endogen, lag=1, differences =1)
lapply(PLendogendiff,function(x){kpss.test(x)})
lapply(PLendogendiff,function(x){adf.test(x)})
lapply(PLendogendiff, function(x){pp.test(x)})

plot.ts(PLendogendiff)

#2. Késleltetés meghatátozása a modellből
lagselectionPLendogen <- VARselect(PLendogendiff, lag.max = 8, type = "const")
lagselectionPLendogen$selection
#AIC=4, HQ=1, SC=1, FPE=4
#AIC-t választom -> 4-es lag #nem kell levonni
#1-es laggal is megpróbálom

#3.johansen teszt
jotestPLendogen <- ca.jo(PL_endogen, ecdet = "const", type = "eigen")
jotestPLendogen2 <- ca.jo(PL_endogen, ecdet = "const", type = "trace")
summary(jotestPLendogen)
summary(jotestPLendogen2)
#Eigen és trace szerint 1 kointegráció van (r=0)


PL_exogen <- as.data.frame(read.csv("Lengyelország_exogen.csv", sep = ","))
PL_exogen <- ts(PL_exogen[,2:5],frequency = 4)
exog_varsPL <- PL_exogen[, c("Unemployement", "Total.reserve", "GDP", "GOV.DEBT")]


#VECM modell lag=4
PLendogen_1 <- VECM(PL_endogen, lag = 4, include = "const", estim = "ML", exogen = exog_varsPL)
summary(PLendogen_1)
#ER negatív hatás, CPI negatív


#exogéneket fel kell venni

vecmresid <- as.data.frame(resid(PLendogen_1))
lapply(vecmresid, function(i)Box.test(i,lag=15,type = "Ljung-Box"))
#mindegyiknél elfogadjuk a Ho=fehérzaj-t




#VECM modell lag=4 tesztek

#VECM -> VAR
PLendogen_1VAR <- vec2var(jotestPLendogen2)

#Serial correlation
SerialPLendogen1 <- serial.test(PLendogen_1VAR, lags.pt = 3, type= "PT.asymptotic")
SerialPLendogen1 #p=0,1655 H0=nincs autokorreláció

#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchPLendogen1 <- arch.test(PLendogen_1VAR, lags.multi = 15, multivariate.only = TRUE)
ArchPLendogen1 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormPLendogen1 <- normality.test(PLendogen_1VAR, multivariate.only = TRUE)
NormPLendogen1 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF

ERirfPLendogen <- irf(PLendogen_1VAR, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERirfPLendogen, ylab= "ER", main= "TB sokk hatása az ER-re")


CPIirfPLendogen <- irf(PLendogen_1VAR, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIirfPLendogen, ylab= "CPI", main= "TB sokk hatása az CPI-re")


TBirfPLendogen <- irf(PLendogen_1VAR, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBirfPLendogen, ylab= "TB", main= "ER sokk hatása az TB-re")


ERCPIirfPLendogen <- irf(PLendogen_1VAR, impulse = "ER", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERCPIirfPLendogen, ylab= "CPI", main= "ER sokk hatása a CPI-ra")


CPIERirfPLendogen <- irf(PLendogen_1VAR, impulse = "CPI", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIERirfPLendogen, ylab= "ER", main= "CPI sokk hatása az ER-re")


CPITBirfPLendogen <- irf(PLendogen_1VAR, impulse = "CPI", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPITBirfPLendogen, ylab= "TB", main= "CPI sokk hatása a TB-re")

#Variancia dekompozíció
FEVDPLendogen2 <- genFEVD(PLendogen_1VAR, n.ahead = 10)
FEVDPLendogen2
#első a TB önmagából 81,44-et magyaráz er ből 4,27-et, cpi ból 14,28-at




#VECM model lag=1

PLendogen_5 <- VECM(PL_endogen, lag = 1, include = "const", estim = "ML", exogen = exog_varsPL)
summary(PLendogen_5)

vecmresid2 <- as.data.frame(resid(PLendogen_5))
lapply(vecmresid2, function(i)Box.test(i,lag=15,type = "Ljung-Box"))

#VECM -> VAR
PLendogen_5VAR2 <- vec2var(jotestPLendogen2)

#Serial correlation
SerialPLendogen2 <- serial.test(PLendogen_5VAR2, lags.pt = 1, type= "PT.asymptotic")
SerialPLendogen2 #p=0,1655 H0=nincs autokorreláció
#lag.pt=1 nél NA-kat produkál a teszt

#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchPLendogen2 <- arch.test(PLendogen_5VAR2, lags.multi = 15, multivariate.only = TRUE)
ArchPLendogen2 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormPLendogen2 <- normality.test(PLendogen_5VAR2, multivariate.only = TRUE)
NormPLendogen2 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF
ERirfPLendogen2 <- irf(PLendogen_5VAR2, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERirfPLendogen2, ylab= "ER", main= "TB hatása az ER-re")


CPIirfPLendogen2 <- irf(PLendogen_5VAR2, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIirfPLendogen2, ylab= "CPI", main= "TB hatása az CPI-re")

TBirfPLendogen2 <- irf(PLendogen_5VAR2, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBirfPLendogen2, ylab= "TB", main= "ER hatása az TB-re")

#Variancia dekompozíció
FEVDPLendogen3 <- genFEVD(PLendogen_5VAR2, n.ahead = 10)
FEVDPLendogen3
#első a TB önmagából 81,44-et magyaráz er ből 4,27-et, cpi ból 14,28-at