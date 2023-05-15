################################################
###################PANELÖKO#####################
################################################

install.packages("plm")
install.packages("Formula")
library(plm)
library(readxl)
library(Formula)
library(ggplot2)
library(lmtest)
library(readxl)
library(ggplot2)
setwd("C:/Users/Dell/Desktop/TDK/Adatok/Rendes adatok/Végleges")
Paneldata <- read_excel("Panel_struktura.xlsx")

##################################################DATA FRAMEK ORSZÁGOKRA LEBONTVA########################################################

setwd("C:/Users/Dell/Desktop/TDK/Adatok/Rendes adatok/Végleges/VECM eredmények")
Horvátország <- read_excel("Horvátország.xlsx")
Csehország <- read_excel("Csehország.xlsx")
Magyarország <- read_excel("Magyarország.xlsx")
Lengyelország <- read_excel("Lengyelország.xlsx")
Románia <- read_excel("Románia.xlsx")


library(stringr)
library(lubridate)


#############################Idő konvertálása######################################

quaterly <- function(x){
  year <- as.numeric(substr(x,1,4))
  month <- (as.numeric(substr(x,7,8))-1)*3+1
  make_date(year,month,day=1)
}

Paneldata$datum <- sapply(Paneldata$Idő, quaterly)
Paneldata$datum <- as.Date(Paneldata$datum)

Horvátország$datum <- sapply(Horvátország$Idő, quaterly)
Horvátország$datum <- as.Date(Horvátország$datum)

Csehország$datum <- sapply(Csehország$Idő, quaterly)
Csehország$datum <- as.Date(Csehország$datum)

Magyarország$datum <- sapply(Magyarország$Idő, quaterly)
Magyarország$datum <- as.Date(Magyarország$datum)

Lengyelország$datum <- sapply(Lengyelország$Idő, quaterly)
Lengyelország$datum <- as.Date(Lengyelország$datum)

Románia$datum <- sapply(Románia$Idő, quaterly)
Románia$datum <- as.Date(Románia$datum)

str(Paneldata)
Paneldata$datum <- as.factor(Paneldata$datum)
str(Paneldata)

FEmodel <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
REmodel <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "random")
summary(FEmodel)
summary(REmodel)

hausman_test <- phtest(FEmodel, REmodel)
hausman_test
#p=nagy random effects kell? <- ez jó így?
#random effects technikailag nem becsülhető, nem találja sem az R sem a Gretl, ezért Fixed Effects modell kell
 

#Az outputok eleje kell a pár változó, utána leírni, hogy ezen kívül voltak még időváltozók xy bázissal, ezek közül ez az és amaz volt szignifikáns, 
#Hivatkozás wooldridge könyv

FEmodel2 <- plm(TB ~  ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel2)#0,59964

robust_se <- sqrt(diag(vcovHC(FEmodel2, type = "HC0")))
coef_with_se1 <- cbind(coef(FEmodel2), robust_se)
colnames(coef_with_se1) <- c("Koefficiens", "Robusztus standard hiba")
print(coef_with_se1)

#logaritmizálás

FEmodel3 <- plm(TB ~  log(ER) + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel3)

FEmodel4 <- plm(TB ~  log(ER) + log(Total_reserve) + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel4)

FEmodel5 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + GDP + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel5)

FEmodel6 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel6)# talán legjobb

FEmodel7 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + log(CPI) + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel7)#ront a korrigált r^2-en

FEmodel8 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + CPI + log(GOV_DEBT) + datum, data = Paneldata, model = "within")
summary(FEmodel8)

FEmodel9 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + log(CPI) + log(GOV_DEBT) + datum, data = Paneldata, model = "within")
summary(FEmodel9)

FEmodel10 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + CPI + GOV_DEBT + datum, data = Paneldata, model = "within")
summary(FEmodel10)#0,70937-es korrigált R^2

FEmodelvegso <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + log(CPI) + log(GOV_DEBT) + datum, data = Paneldata, model = "within")
summary(FEmodelvegso)

robust_se <- sqrt(diag(vcovHC(FEmodelvegso, type = "HC0")))
coef_with_se <- cbind(coef(FEmodelvegso), robust_se)
colnames(coef_with_se) <- c("Koefficiens", "Robusztus standard hiba")
print(coef_with_se)



#előjel koefficiens kell értelmezéshet
#Államadósság növekedése pozitívan hat a külkermérlegre
#Államadósság és TB ábrán


##################################################Lengyelország nélkül#######################################################

setwd("C:/Users/Dell/Desktop/TDK/Adatok/Rendes adatok/Végleges")
Paneldata_Lo <- read_excel("Panel_struktura_Lengyelországnélkül.xlsx")

Paneldata_Lo$datum <- sapply(Paneldata_Lo$Idő, quaterly)
Paneldata_Lo$datum <- as.Date(Paneldata_Lo$datum)
Paneldata_Lo$datum <- as.factor(Paneldata_Lo$datum)
str(Paneldata_Lo)

FEmodel_2 <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum , data = Paneldata_Lo, model = "within")
REmodel_2 <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum , data = Paneldata_Lo, model = "random")
summary(FEmodel_2)
summary(REmodel_2)

hausman_test <- phtest(FEmodel_2,REmodel_2)
hausman_test
#p=kicsi tehát fixed effects modell kell

FEmodelLo1 <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
summary(FEmodelLo1)
#minden szignifikáns ***

robust_se_Lo <- sqrt(diag(vcovHC(FEmodelLo1, type = "HC0")))
coef_with_se_Lo <- cbind(coef(FEmodelLo1), robust_se)
colnames(coef_with_se_Lo) <- c("Koefficiens", "Robusztus standard hiba")
print(coef_with_se_Lo)




#Eredményváltozót nem lehet logaritmizálni, mert vannak negatív értékek is

robust_se_Lo <- sqrt(diag(vcovHC(FEmodelLo8, type = "HC0")))
coef_with_se_Lo <- cbind(coef(FEmodelLo8), robust_se)
colnames(coef_with_se_Lo) <- c("Koefficiens", "Robusztus standard hiba")
print(coef_with_se_Lo)

#érdmeben nem nagyon változik semmi, de megemlíthetem, hogy megcsináltam
#LO-nak nagy belső piaca van, de az eredmények nem változnak, ezt ebben a mellékletben láthatja













































##################################################################COVID előtt és COVID UTÁN##########################################

#setwd("C:/Users/Gép/Desktop/TDK/Adatok/Rendes adatok/Végleges")
#PaneldataCovid <- read_excel("Panel_struktura_covid.xlsx")
#
#
##
#quaterly <- function(x){
#  year <- as.numeric(substr(x,1,4))
#  month <- (as.numeric(substr(x,7,8))-1)*3+1
#  make_date(year,month,day=1)
#}
#
#PaneldataCovid$datum <- sapply(PaneldataCovid$Idő, quaterly)
#PaneldataCovid$datum <- as.Date(PaneldataCovid$datum)
#
#Horvátország$datum <- sapply(Horvátország$Idő, quaterly)
#Horvátország$datum <- as.Date(Horvátország$datum)
#
#Csehország$datum <- sapply(Csehország$Idő, quaterly)
#Csehország$datum <- as.Date(Csehország$datum)
#
#Magyarország$datum <- sapply(Magyarország$Idő, quaterly)
#Magyarország$datum <- as.Date(Magyarország$datum)
#
#Lengyelország$datum <- sapply(Lengyelország$Idő, quaterly)
#Lengyelország$datum <- as.Date(Lengyelország$datum)
#
#Románia$datum <- sapply(Románia$Idő, quaterly)
#Románia$datum <- as.Date(Románia$datum)
#
#str(PaneldataCovid)
#PaneldataCovid$datum <- as.factor(PaneldataCovid$datum)
#str(PaneldataCovid)
#
#FEmodelcov <- plm(TB ~ ER + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = PaneldataCovid, model = "within")
#summary(FEmodelcov)
#
#FEmodelvegsocov <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + CPI + log(GOV_DEBT) + datum, data = PaneldataCovid, model = "within")
#summary(FEmodelvegsocov)
#
#FEmodelvegsocov2 <- plm(TB ~  log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) + log(CPI) + log(GOV_DEBT) + datum, data = PaneldataCovid, model = "within")
#summary(FEmodelvegsocov2)






#Logaritmikus tagok

#FEmodelLo2 <- plm(TB ~ log(ER) + Total_reserve + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo2)#ront

#FEmodelLo3 <- plm(TB ~ ER + log(Total_reserve) + Unemployement + GDP + CPI + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo3)#ront

#FEmodelLo4 <- plm(TB ~ ER + Total_reserve + log(Unemployement) + GDP + CPI + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo4)#javít

#FEmodelLo5 <- plm(TB ~ ER + Total_reserve + log(Unemployement) + log(GDP) + CPI + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo5)#ront

#FEmodelLo6 <- plm(TB ~ ER + Total_reserve + log(Unemployement) + GDP + log(CPI) + GOV_DEBT + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo6)#javít

#FEmodelLo7 <- plm(TB ~ ER + Total_reserve + log(Unemployement) + GDP + log(CPI) + log(GOV_DEBT) + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo7)#legjobb korrigált r^2

#FEmodelLo8 <- plm(TB ~ ER + log(Total_reserve) + log(Unemployement) + log(GDP) +  CPI + log(GOV_DEBT) + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo8)#ugyanaz a specifikáció, mint a Lenygelországgal modellben

#FEmodelLo9 <- plm(TB ~ log(ER) + log(Total_reserve) + log(Unemployement) + log(GDP) +  log(CPI) + log(GOV_DEBT) + datum, data = Paneldata_Lo, model = "within")
#summary(FEmodelLo9)#teljesen log os









################################################################PLOTOK##########################################################
#hist(Horvátország$CPI)
#hist(Csehország$CPI)
#hist(Magyarország$CPI)
#hist(Lengyelország$CPI)
#hist(Románia$CPI)

#ggplot(Lengyelország, aes(x = TB, y = ER)) +
#  geom_line(color = "blue") +
#  labs(title = "ER változása a TB függvényében",
 #      x = "TB",
#       y = "ER") +
#  theme_minimal()

#ggplot(Horvátország, aes(x = CPI, y = GDP)) +
#  geom_point(color = "purple") +
#  labs(title = "Infláció és GDP közötti összefüggés",
#       x = "CPI (Infláció)",
#       y = "GDP") +
#  theme_minimal()

#ggplot(Horvátország, aes(x = datum, y = Unemployement)) +
#  geom_bar(stat = "identity", fill = "steelblue") +
#  labs(title = "Munkanélküliség negyedéves lebontásban",
#       x = "Quarter",
#       y = "Unemployment Rate")

#ggplot(Horvátország, aes(x = Total_reserve, y = GOV_DEBT)) +
#  geom_point(color = "steelblue") +
#  labs(title = "Teljes tartalék és államadósság kapcsolata",
#       x = "Total Reserve",
#       y = "Government Debt")

#
#ggplot(Horvátország, aes(x = datum, y = Unemployement, fill = datum)) +
#  geom_bar(stat = "identity") +
#  labs(title = "Munkanélküliség negyedéves lebontásban", x = "Negyedévek", y = "Munkanélküliség") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

##################################################################################################################################