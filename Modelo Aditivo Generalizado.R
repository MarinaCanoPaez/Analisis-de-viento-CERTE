rm(list = ls())
library(zoo)
library(xts)
library(lubridate, warn.conflicts = FALSE)
library(mgcv)
library(dplyr)
library(ggplot2)
#Modelo Aditivo Generalizado sin rellenar los datos faltantes de las velocidades máximas estimadas
#Lectura de datos
VvmaxDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento maximas agrupadas 1 a 10 mts cada Dia.csv")
pander::pander(VvmaxDia)
plot(maxDia,col="blue",lwd=2, main="Velocidades de viento maximas agrupadas 1 a 10 mts cada Dia")
fechas <- as.Date(paste(VvmaxDia$Year, VvmaxDia$Mes, VvmaxDia$Dia, sep = "-"))
dosmetrosdealtura <- zoo(VvmaxDia$X2.mts, order.by = fechas)
index(dosmetrosdealtura)
head(predi)
plot(predi$obs,col="blue", type = "p",main="Velocidad de viento predecida a 2 mts de altura")
predictions <- zoo(predict(gam_1, newdata = df[, 2:5]), order.by = datetxt)
datetxt <- seq(min(fechas), max(fechas), by = 1)
df <- data.frame(date = datetxt,
                 Año = as.numeric(format(datetxt, format = "%Y")),
                 Mes = as.numeric(format(datetxt, format = "%m")),
                 Día = as.numeric(format(datetxt, format = "%d")), 
                 WD = as.numeric(format(datetxt, format = "%w")))
head(df[, 2:5])
lines(predi,col="red", lwd=2,main="Velocidad de viento predecida a 2 mts de altura")
head(VvmaxDia)
ggplot(predi, aes(x=fechas)) +
  geom_line(aes(y = VvmaxDia$X2.mts), color = "blue") +
  geom_line(aes(y = pred), color="red", linetype="twodash")
#Modelo Aditivo Generalizado rellenando datos faltantes Velocidades máximas
VvmaxDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento maximas agrupadas 1 a 10 mts cada Dia.csv")
head(VvmaxDia)
fechas <- as.Date(paste(VvmaxDia$Year, VvmaxDia$Mes, VvmaxDia$Dia, sep = "-"))
dosmetrosdealtura <- zoo(VvmaxDia$X2.mts, order.by = fechas)
index(dosmetrosdealtura)
matrix_gam_max = data.frame(X2 = VvmaxDia$X2.mts, Año = VvmaxDia$Year, Mes = VvmaxDia$Mes, Día = VvmaxDia$Dia, WD = wday(fechas))
head(matrix_gam_max)
gam_1_max <- gam(X2 ~ s(Mes, bs = "ps", k = 12) +
               s(Día, bs = "ps", k = 31) +
             s(WD, bs = "ps", k = 5),
             data = matrix_gam_max,
             family = gaussian)
summary(gam_1_max)

Pred <- zoo(data.frame(pred = predict(gam_1_max), obs = VvmaxDia$X2.mts), order.by = fechas)
datetxt <- seq(min(fechas), max(fechas), by = 1)
df <- data.frame(date = datetxt,
                 Año = as.numeric(format(datetxt, format = "%Y")),
                 Mes = as.numeric(format(datetxt, format = "%m")),
                 Día = as.numeric(format(datetxt, format = "%d")), 
                 WD = as.numeric(format(datetxt, format = "%w")))
head(df[, 2:5])
predictions <- zoo(predict(gam_1_max, newdata = df[, 2:5]), order.by = datetxt)
plot(Pred$obs,col="blue", type = "p",main="Velocidad de viento predecida a 2 mts de altura")
lines(predictions,col="red", lwd=2,main="Velocidad de viento predecida a 2 mts de altura")
plot(gam_1_max$residuals)
qqnorm(gam_1_max$residuals)
qqline(gam_1_max$residuals, col = "red")
hist(gam_1_max$residuals)
plot(density(gam_1_max$residuals))
shapiro.test(gam_1_max$residuals)

#Modelo Aditivo Generalizado rellenando datos faltantes Velocidades medias
VvmeanDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento medias agrupadas 1 a 10 mts cada Dia.csv")
head(VvmeanDia)
pander::pander(VvmeanDia)
VvmeanDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento medias agrupadas 1 a 10 mts cada Dia.csv")
plot(meanDia,col="blue",lwd=2, main="Velocidades de viento medias agrupadas 1 a 10 mts cada Dia")
fechas2 <- as.Date(paste(VvmeanDia$Year, VvmeanDia$Mes, VvmeanDia$Dia, sep = "-"))
dosmetrosdealtura2 <- zoo(VvmeanDia$X2.mts, order.by = fechas2)
index(dosmetrosdealtura2)
matrix_gam_mean = data.frame(X2mean = VvmeanDia$X2.mts, Año = VvmeanDia$Year, Mes = VvmeanDia$Mes, Día = VvmeanDia$Dia, WD = wday(fechas2))
head(matrix_gam_mean)
gam_1_mean <- gam(X2mean ~ s(Mes, bs = "cr", k = 12) +
                   s(Día, bs = "ps", k = 31), #+
                   #s(WD, bs = "ps", k = 7),
                 data = matrix_gam_mean,
                 family = gaussian)
summary(gam_1_mean)
Predmean <- zoo(data.frame(predmean = predict(gam_1_mean), obs = VvmeanDia$X2.mts), order.by = fechas2)
datetxtmean <- seq(min(fechas2), max(fechas2), by = 1)
dfmean <- data.frame(datemean = datetxtmean,
                 Año = as.numeric(format(datetxtmean, format = "%Y")),
                 Mes = as.numeric(format(datetxtmean, format = "%m")),
                 Día = as.numeric(format(datetxtmean, format = "%d")), 
                 WD = as.numeric(format(datetxtmean, format = "%w")))
head(dfmean[, 2:5])
predictionsmean <- zoo(predict(gam_1_mean, newdata = dfmean[, 2:5]), order.by = datetxtmean)
plot(Predmean$obs,col="blue", type = "p",main="Velocidad de viento media estimada a 2 mts de altura")
lines(predictionsmean,col="red", lwd=2,main="Velocidad de viento predecida a 2 mts de altura")
plot(gam_1_mean$residuals)
qqnorm(gam_1_mean
       $residuals)
qqline(gam_1_mean$residuals, col = "red")
hist(gam_1_mean$residuals)
plot(density(gam_1_mean$residuals))
shapiro.test(gam_1_mean$residuals)
#Modelo Aditivo Generalizado rellenando datos faltantes Velocidades minimas
VvminDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento minimas agrupadas 1 a 10 mts cada Dia.csv")
head(VvminDia)
VvminDia <- read.csv("~/Documents/colpos Hidrociencias/Quinto cuatrimestre/Velocidades de viento minimas agrupadas 1 a 10 mts cada Dia.csv")
fechas3 <- as.Date(paste(VvminDia$Year, VvminDia$Mes, VvminDia$Dia, sep = "-"))
dosmetrosdealtura3 <- zoo(VvminDia$X2.mts, order.by = fechas3)
index(dosmetrosdealtura3)
matrix_gam_min = data.frame(X2min = VvminDia$X2.mts, Año = VvminDia$Year, Mes = VvminDia$Mes, Día = VvminDia$Dia, WD = wday(fechas3))
head(matrix_gam_min)
gam_1_min <- gam(X2min ~ s(Mes, bs = "ps", k = 12) +
                    s(Día, bs = "ps", k = 31), #+
                  #s(WD, bs = "ps", k = 7),
                  data = matrix_gam_min,
                  family = gaussian)
summary(gam_1_min)
Predmin <- zoo(data.frame(predmin = predict(gam_1_min), obs = VvminDia$X2.mts), order.by = fechas3)
datetxtmin <- seq(min(fechas3), max(fechas3), by = 1)
dfmin <- data.frame(datemin = datetxtmin,
                     Año = as.numeric(format(datetxtmin, format = "%Y")),
                     Mes = as.numeric(format(datetxtmin, format = "%m")),
                     Día = as.numeric(format(datetxtmin, format = "%d")), 
                     WD = as.numeric(format(datetxtmin, format = "%w")))
head(dfmin[, 2:5])
predictionsmin <- zoo(predict(gam_1_min, newdata = dfmin[, 2:5]), order.by = datetxtmin)
plot(Predmin$obs,col="blue", type = "p",main="Velocidad de viento minima estimada a 2 mts de altura")
lines(predictionsmin,col="red", lwd=2,main="Velocidad de viento predecida a 2 mts de altura")
plot(gam_1_min$residuals)
qqnorm(gam_1_min$residuals)
qqline(gam_1_min$residuals, col = "red")
hist(gam_1_min$residuals)
plot(density(gam_1_min$residuals))
shapiro.test(gam_1_min$residuals)
