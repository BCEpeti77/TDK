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

RO_endogen <- as.data.frame(read.csv("Románia_endogen.csv", sep = ","))
RO_endogen <- ts(RO_endogen[,2:4],frequency = 4)
plot.ts (RO_endogen)

#stacionaritás vizsgálata
lapply(RO_endogen, function(x){kpss.test(x)}) # Egységgyök
lapply(RO_endogen, function(x){adf.test(x)}) # Egységgyök
lapply(RO_endogen, function(x){pp.test(x)}) # Egységgyök

#differenciálás
ROendogendiff <- diff(RO_endogen, lag=1, differences =1)
lapply(ROendogendiff,function(x){kpss.test(x)})
lapply(ROendogendiff,function(x){adf.test(x)})
lapply(ROendogendiff, function(x){pp.test(x)})

plot.ts(ROendogendiff)

#2. Késleltetés meghatátozása a modellből
lagselectionROendogen <- VARselect(ROendogendiff, lag.max = 8, type = "const")
lagselectionROendogen$selection
#AIC=1, HQ=1, SC=1, FPE=1
#Mindegy, hogy melyiket választom -> 1-es lag #nem kell levonni

#3.johansen teszt
jotestROendogen <- ca.jo(RO_endogen, ecdet = "const", type = "eigen")
jotestROendogen2 <- ca.jo(RO_endogen, ecdet = "const", type = "trace")
summary(jotestROendogen)
summary(jotestROendogen2)
#Eigen és trace szerint 1 kointegráció van (r=0)


RO_exogen <- as.data.frame(read.csv("Románia_exogen.csv", sep = ","))
RO_exogen <- ts(RO_exogen[,2:5],frequency = 4)
exog_varsRO <- RO_exogen[, c("Unemployement", "Total.reserve", "GDP", "GOV.DEBT")]


#VECM modell lag=1
ROendogen_1 <- VECM(RO_endogen, lag = 1, include = "const", estim = "ML", exogen = exog_varsRO)
summary(ROendogen_1)
#ER negatív hatás, CPI negatív
#exogéneket fel kell venni

vecmresid <- as.data.frame(resid(ROendogen_1))
lapply(vecmresid, function(i)Box.test(i,lag=15,type = "Ljung-Box"))
#mindegyiknél elfogadjuk a Ho=fehérzaj-t

#VECM modell lag=1 tesztek

#VECM -> VAR
ROendogen_1VAR <- vec2var(jotestROendogen2)

#Serial correlation
SerialROendogen1 <- serial.test(ROendogen_1VAR, lags.pt = 3, type= "PT.asymptotic")
SerialROendogen1 #p=0,043 H0=nincs autokorreláció -> itt van autokorreláció

#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchROendogen1 <- arch.test(ROendogen_1VAR, lags.multi = 15, multivariate.only = TRUE)
ArchROendogen1 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormROendogen1 <- normality.test(ROendogen_1VAR, multivariate.only = TRUE)
NormROendogen1 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF
ERirfROendogen <- irf(ROendogen_1VAR, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERirfROendogen, ylab= "ER", main= "TB sokk hatása az ER-re")


CPIirfROendogen <- irf(ROendogen_1VAR, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIirfROendogen, ylab= "CPI", main= "TB sokk hatása az CPI-re")

TBirfROendogen <- irf(ROendogen_1VAR, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBirfROendogen, ylab= "TB", main= "ER sokk hatása az TB-re")

ERCPIirfROendogen <- irf(ROendogen_1VAR, impulse = "ER", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERCPIirfROendogen, ylab= "CPI", main= "ER sokk hatása a CPI-ra")

CPIERirfROendogen <- irf(ROendogen_1VAR, impulse = "CPI", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIERirfROendogen, ylab= "ER", main= "CPI sokk hatása az ER-re")

CPITBirfROendogen <- irf(ROendogen_1VAR, impulse = "CPI", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPITBirfROendogen, ylab= "TB", main= "CPI sokk hatása a TB-re")

#Variancia dekompozíció
FEVDROendogen2 <- genFEVD(ROendogen_1VAR, n.ahead = 10)
FEVDROendogen2
#első a TB önmagából 76,8-at magyaráz, er ből 18,9-et, cpi-ból 4,17-et
