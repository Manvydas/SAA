library(data.table)
library(lubridate)
library(ggplot2)

source("functions.R")

nvda <- fread("data/NVDA.csv")
jpm <- fread("data/JPM.csv")
gcf <- fread("data/GCF.csv")
spy <- fread("data/SPY.csv")
dat1 <- fread("data/datatable.csv") # gyventojai, naujagymiai, mirtys
dat2 <- fread("data/datatable2.csv") # atlyginimas
ivykiai <- fread("data/ivykiai.csv")[ , c(1, 5), with = T]
covid <- fread("data/covid.csv")[ , 1:4, with = T]
nhyear <- read.table("https://archive.uea.ac.uk/~gj/book/data/nhyear.dat", header=F)
myecg <- fread("data/ecg_2021-04-12.csv", fill=T)

dat1[ , .N, Rodiklis]
dat1[ , data := myDate(Laikotarpis)]

dd <- data.frame(dat1[Rodiklis=="Nuolatini\305\263 gyventoj\305\263 skai\304\215ius m\304\227nesio prad\305\276ioje"])
myCor(dd[ , 4])
acf(dd[ , 4], lag.max = 100)
plot(dd[ , "data"], dd[ , 4], type = "l")

dd <- data.frame(dat1[Rodiklis=="M\304\227nesinis gyv\305\263 gimusi\305\263 k\305\253diki\305\263 skai\304\215ius"])
myCor(dd[ , 4])
acf(dd[ , 4], lag.max = 100)
plot(dd[ , "data"], dd[ , 4], type = "l")

dd <- data.frame(dat1[Rodiklis=="M\304\227nesinis mirusi\305\263j\305\263 skai\304\215ius"])
myCor(dd[ , 4])
acf(dd[ , 4], lag.max = 100)
plot(dd[ , "data"], dd[ , 4], type = "l")



gg <- acf(as.numeric(data.frame(ivykiai)[,2]), lag.max = 60, plot = F)
plot(gg, main = "Pavadinimas", xlab = "Vėlyniai (mėnesiais)", ylab = "Korealiacijos koeficientas")
