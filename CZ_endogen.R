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
setwd("C:/Users/Dell/Desktop/TDK/Adatok/Rendes adatok/Végleges")

CZ_endogen <- as.data.frame(read.csv("Csehország_endogen.csv", sep = ","))
CZ_endogen <- ts(CZ_endogen[,2:4],frequency = 4, start = 2006)
plot.ts (CZ_endogen)

#stacionaritás vizsgálata
lapply(CZ_endogen, function(x){kpss.test(x)}) # Egységgyök
lapply(CZ_endogen, function(x){adf.test(x)}) # Egységgyök
lapply(CZ_endogen, function(x){pp.test(x)}) # Egységgyök

#differenciálás
CZendogendiff <- diff(CZ_endogen, lag=1, differences =1)
lapply(CZendogendiff,function(x){kpss.test(x)})
lapply(CZendogendiff,function(x){adf.test(x)})
lapply(CZendogendiff, function(x){pp.test(x)})

plot.ts(CZendogendiff)
?plot.ts

#2. Késleltetés meghatátozása a modellből
lagselectionCZendogen <- VARselect(CZendogendiff, lag.max = 8, type = "const")
lagselectionCZendogen$selection
#AIC=8, HQ=4, SC=1, FPE=5
#HQ-t választom -> 4-es lag #nem kell levonni

#3.johansen teszt
jotestCZendogen <- ca.jo(CZ_endogen, ecdet = "const", type = "eigen")
jotestCZendogen2 <- ca.jo(CZ_endogen, ecdet = "const", type = "trace")
summary(jotestCZendogen)
summary(jotestCZendogen2)
#Eigen és trace szerint 1 kointegráció van (r=0)


CZ_exogen <- as.data.frame(read.csv("Csehország_exogen.csv", sep = ","))
CZ_exogen <- ts(CZ_exogen[,2:5],frequency = 4)
exog_varsCZ <- CZ_exogen[, c("Unemployement", "Total.reserve", "GDP", "GOV.DEBT")]


#VECM modell lag=4
CZendogen_1 <- VECM(CZ_endogen, lag = 4, include = "const", estim = "ML", exogen = exog_varsCZ)
summary(CZendogen_1)
#ER pozitív hatás, CPI negatív
#exogéneket fel kell venni

####CZendogen_1 <- VECM(CZendogendiff, lag = 4, include = "const", estim = "ML", exogen = exog_varsCZ)
######summary(CZendogen_1)

vecmresid <- as.data.frame(resid(CZendogen_1))
lapply(vecmresid, function(i)Box.test(i,lag=15,type = "Ljung-Box"))
#mindegyiknél elfogadjuk a Ho=fehérzaj-t

#VECM modell lag=4 tesztek

#VECM -> VAR
CZendogen_1VAR <- vec2var(jotestCZendogen2)

#Serial correlation
SerialCZendogen1 <- serial.test(CZendogen_1VAR, lags.pt = 4, type= "PT.asymptotic")
SerialCZendogen1 #p=0,011 H0=nincs autokorreláció

#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchCZendogen1 <- arch.test(CZendogen_1VAR, lags.multi = 15, multivariate.only = TRUE)
ArchCZendogen1 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormCZendogen1 <- normality.test(CZendogen_1VAR, multivariate.only = TRUE)
NormCZendogen1 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF
ERirfCZendogen <- irf(CZendogen_1VAR, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERirfCZendogen, ylab= "ER", main= "TB sokk hatása az ER-re")


CPIirfCZendogen <- irf(CZendogen_1VAR, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIirfCZendogen, ylab= "CPI", main= "TB sokk hatása az CPI-re")

TBirfCZendogen <- irf(CZendogen_1VAR, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBirfCZendogen, ylab= "TB", main= "ER sokk hatása az TB-re")

ERCPIirfCZendogen <- irf(CZendogen_1VAR, impulse = "ER", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERCPIirfCZendogen, ylab= "CPI", main= "ER sokk hatása a CPI-ra")

CPIERirfCZendogen <- irf(CZendogen_1VAR, impulse = "CPI", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIERirfCZendogen, ylab= "ER", main= "CPI sokk hatása az ER-re")

CPITBirfCZendogen <- irf(CZendogen_1VAR, impulse = "CPI", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPITBirfCZendogen, ylab= "TB", main= "CPI sokk hatása a TB-re")



#Variancia dekompozíció
FEVDCZendogen2 <- genFEVD(CZendogen_1VAR, n.ahead = 10)
FEVDCZendogen2
#első a TB önmagából 70 et magyaráz er ből 3 at, cpi ból 27 et
