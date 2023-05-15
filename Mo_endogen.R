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

Mo_endogen <- as.data.frame(read.csv("Magyarország_endogen.csv", sep = ","))
Mo_endogen <- ts(Mo_endogen[,2:4],frequency = 4)
plot.ts (Mo_endogen)

#stacionaritás vizsgálata
lapply(Mo_endogen, function(x){kpss.test(x)}) # Egységgyök
lapply(Mo_endogen, function(x){adf.test(x)}) # Egységgyök
lapply(Mo_endogen, function(x){pp.test(x)}) # Egységgyök

#differenciálás
Moendogendiff <- diff(Mo_endogen, lag=1, differences =1)
lapply(Moendogendiff,function(x){kpss.test(x)})
lapply(Moendogendiff,function(x){adf.test(x)})
lapply(Moendogendiff, function(x){pp.test(x)})

plot.ts(Moendogendiff)

#2. Késleltetés meghatátozása a modellből
lagselectionMoendogen <- VARselect(Moendogendiff, lag.max = 8, type = "const")
lagselectionMoendogen$selection
#AIC=8, HQ=1, SC=1, FPE=3
#Mindegy, hogy melyiket választom -> 1-es lag #nem kell levonni

#3.johansen teszt
jotestMoendogen <- ca.jo(Mo_endogen, ecdet = "const", type = "eigen")
jotestMoendogen2 <- ca.jo(Mo_endogen, ecdet = "const", type = "trace")
summary(jotestMoendogen)
summary(jotestMoendogen2)
#Eigen és trace szerint 1 kointegráció van (r=0), trace szerint 2




Mo_exogen <- as.data.frame(read.csv("Magyarország_exogen.csv", sep = ","))
Mo_exogen <- ts(Mo_exogen[,2:5],frequency = 4)
exog_varsMo <- Mo_exogen[, c("Unemployement", "Total.reserve", "GDP", "GOV.DEBT")]


#VECM modell lag=1
Moendogen_1 <- VECM(Mo_endogen, lag = 1, include = "const", estim = "ML", exogen = exog_varsMo)
summary(Moendogen_1)
#ER pozitív hatás, CPI negatív
#exogéneket fel kell venni

vecmresid <- as.data.frame(resid(Moendogen_1))
lapply(vecmresid, function(i)Box.test(i,lag=15,type = "Ljung-Box"))
#mindegyiknél elfogadjuk a Ho=fehérzaj-t

#VECM modell lag=1 tesztek

#VECM -> VAR
Moendogen_1VAR <- vec2var(jotestMoendogen2)

#Granger okság
Granger1 <- causality(Moendogen_1VAR,cause = "CPI")
Granger1
Granger2 <- causality(Moendogen_1VAR,cause = "ER")
Granger2
?causality


#Serial correlation
SerialMoendogen1 <- serial.test(Moendogen_1VAR, lags.pt = 3, type= "PT.asymptotic")
SerialMoendogen1 #p=0,005496 H0=nincs autokorreláció -> itt van autokorreláció


#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchMoendogen1 <- arch.test(Moendogen_1VAR, lags.multi = 15, multivariate.only = TRUE)
ArchMoendogen1 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormMoendogen1 <- normality.test(Moendogen_1VAR, multivariate.only = TRUE)
NormMoendogen1 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF

ERirfMoendogen <- irf(Moendogen_1VAR, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERirfMoendogen, ylab= "ER", main= "TB sokk hatása az ER-re")


CPIirfMoendogen <- irf(Moendogen_1VAR, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIirfMoendogen, ylab= "CPI", main= "TB sokk hatása az CPI-re")

TBirfMoendogen <- irf(Moendogen_1VAR, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBirfMoendogen, ylab= "TB", main= "ER sokk hatása az TB-re")

ERCPIirfMoendogen <- irf(Moendogen_1VAR, impulse = "ER", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERCPIirfMoendogen, ylab= "CPI", main= "ER sokk hatása a CPI-ra")

CPIERirfMoendogen <- irf(Moendogen_1VAR, impulse = "CPI", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPIERirfMoendogen, ylab= "ER", main= "CPI sokk hatása az ER-re")

CPITBirfMoendogen <- irf(Moendogen_1VAR, impulse = "CPI", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPITBirfMoendogen, ylab= "TB", main= "CPI sokk hatása a TB-re")


#Variancia dekompozíció
FEVDMoendogen2 <- genFEVD(Moendogen_1VAR, n.ahead = 10)
FEVDMoendogen2
#első a TB önmagából 54,25-öt magyaráz, er ből 0,29-et, cpi-ból 45,45-öt
