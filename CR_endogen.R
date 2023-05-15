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

CR_endogen <- as.data.frame(read.csv("Horvátország_endogen.csv", sep = ","))
CR_endogen <- ts(CR_endogen[,2:4],frequency = 4, start = 2006)
plot.ts (CR_endogen)

#stacionaritás vizsgálata
lapply(CR_endogen, function(x){kpss.test(x)}) # Egységgyök
lapply(CR_endogen, function(x){adf.test(x)}) # Egységgyök
lapply(CR_endogen, function(x){pp.test(x)}) # Egységgyök

#differenciálás
CRendogendiff <- diff(CR_endogen, lag=1, differences =1)
lapply(CRendogendiff,function(x){kpss.test(x)})
lapply(CRendogendiff,function(x){adf.test(x)})
lapply(CRendogendiff, function(x){pp.test(x)})

plot.ts(CRendogendiff)

#2. Késleltetés meghatátozása a modellből
lagselectionCRendogen <- VARselect(CRendogendiff, lag.max = 8, type = "const")
lagselectionCRendogen$selection
#AIC=2, HQ=2, SC=2, FPE=2
#Mindegy melyiket választom -> 2-es lag #nem kell levonni

#3.johansen teszt
jotestCRendogen <- ca.jo(CR_endogen, ecdet = "const", type = "eigen")
jotestCRendogen2 <- ca.jo(CR_endogen, ecdet = "const", type = "trace")
summary(jotestCRendogen)
summary(jotestCRendogen2)
#Eigen és trace szerint 1 kointegráció van (r=0)


CR_exogen <- as.data.frame(read.csv("Horvátország_exogen.csv", sep = ","))
CR_exogen <- ts(CR_exogen[,2:5],frequency = 4)
exog_varsCR <- CR_exogen[, c("Unemployement", "Total.reserve", "GDP", "GOV.DEBT")]


#VECM modell lag=2
CRendogen_1 <- VECM(CR_endogen, lag = 2, include = "const", estim = "ML", exogen = exog_varsCR)
summary(CRendogen_1)
#ER pozitív hatás, CPI negatív
#exogéneket fel kell venni

vecmresid <- as.data.frame(resid(CRendogen_1))
lapply(vecmresid, function(i)Box.test(i,lag=15,type = "Ljung-Box"))
#mindegyiknél elfogadjuk a Ho=fehérzaj-t

#VECM modell lag=2 tesztek

#VECM -> VAR
CRendogen_1VAR <- vec2var(jotestCRendogen2)

#Serial correlation
SerialCRendogen1 <- serial.test(CRendogen_1VAR, lags.pt = 2, type= "PT.asymptotic")
SerialCRendogen1 #p=kicsi H0=nincs autokorreláció

#ARCH Effects <- Ho:homoszkedasztikus H1:heteroszkedasztikus
ArchCRendogen1 <- arch.test(CRendogen_1VAR, lags.multi = 15, multivariate.only = TRUE)
ArchCRendogen1 #p=1 jó nekünk, homoszkedasztikusak a hibatagok -> heteroszkedaszticitást néz

#variancia nem klasztereződik -> jó a VECM model

#Rezidumok normalitása
NormCRendogen1 <- normality.test(CRendogen_1VAR, multivariate.only = TRUE)
NormCRendogen1 #kicsi, nem jó de nem is tragikus -> jobb lehetne a specifikáció, pl sorrend csere


#IRF
TBSHOCKTOERirfCRendogen <- irf(CRendogen_1VAR, impulse = "TB", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBSHOCKTOERirfCRendogen, ylab= "ER", main= "TB sokk hatása az ER-re")


TBSHOCKTOCPIirfCRendogen <- irf(CRendogen_1VAR, impulse = "TB", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(TBSHOCKTOCPIirfCRendogen, ylab= "CPI", main= "TB sokk hatása a CPI-ra")

ERSHOCKTOTBirfCRendogen <- irf(CRendogen_1VAR, impulse = "ER", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERSHOCKTOTBirfCRendogen, ylab= "TB", main= "ER sokk hatása az TB-re")

ERSHOCKTOCPIirfCRendogen <- irf(CRendogen_1VAR, impulse = "ER", response = "CPI", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(ERSHOCKTOCPIirfCRendogen, ylab= "CPI", main= "ER sokk hatása az CPI-re")

CPISHOCKTOTBirfCRendogen <- irf(CRendogen_1VAR, impulse = "CPI", response = "TB", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPISHOCKTOTBirfCRendogen, ylab= "TB", main= "CPI sokk hatása az TB-re")

CPISHOCKTOERirfCRendogen <- irf(CRendogen_1VAR, impulse = "CPI", response = "ER", ortho = FALSE, n.ahead = 10, boot = TRUE)
plot(CPISHOCKTOERirfCRendogen, ylab= "ER", main= "CPI sokk hatása az ER-re")
#95%-os megbízhatósági szint

#Variancia dekompozíció
FEVDCRendogen2 <- genFEVD(CRendogen_1VAR, n.ahead = 10)
FEVDCRendogen2
#első a TB önmagából 74,72-őt magyaráz, ER-ből 8,85-öt, CPI-ból 16,42-őt
