## Introduccion a herramientas para la evaluación de la calidad del aire
## Trabajo práctico 2
##

# Para bajar entre las líneas apretamos: crtl + enter

#install.packages("openxlsx")
#install.packages("openair")
#install.packages("ggplot2") 

library(openxlsx)
library(openair)
library(ggplot2)

## Definimos carpeta de trabajo
getwd()
setwd("C:/Users/solre/Desktop/IHECA")

## Abrimos el .xlsx
datos <- read.xlsx("TP2_SO2.xlsx", sheet = 1, startRow = 1)
class(datos) #para conocer la clase de la variable
str(datos)

## Reemplazamos la fila date con una secuencia de fechas
datos$date <- seq.POSIXt(from = ISOdate(2012,1,1,0), to = ISOdate(2014,12,31,23), by="hours")

## Explorar nuestro data.frame
names(datos) # arroja los nombres de las variables
names(datos) <- c("date", "hora", "so2") #cambiamos los nombres de las variables

datos[1,]  #nos devuelve toda la fila 1
datos[1,3] #nos devuelve el valor para la fila 1 y la columna 3

datos <- datos[-2]  # Borramos la columna 2

head(datos) # muestra la primera parte de la tabla
tail(datos)   # muestra el final de la tabla

length(datos) # numero de columnas
nrow(datos)  # numero de filas

## Análisis exploratorio

# parametros de posición
mean(datos$so2, na.rm = TRUE)
median(datos$so2, na.rm = TRUE)
quantile(datos$so2, probs = 0.90, na.rm = TRUE)  # Cuantil del 90%
quantile(datos$so2, probs = c(0.90, 0.95, 0.99), na.rm = TRUE)  # Cuantil del 90%, 95% y 99%

# parámetros de dispersión
range(datos$so2, na.rm = TRUE)
sd(datos$so2, na.rm = TRUE)
var(datos$so2, na.rm = TRUE)

summary(datos$so2)

## ¿Cuál es el día con la mayor concentración?

which(datos$so2 == max(datos$so2, na.rm=TRUE)) # Atención! nos arroja la fila
datos[21732,]

mayor_99 <- which(datos$so2 > 15) # Atención! nos arroja la fila
datos[mayor_99,]


## Nuestras primeras gráficas

hist(datos$so2, nclass=80)
hist(log(datos$so2), nclass=80) #Qué podemos decir de la distribución?

hist(datos$so2, nclass=80, main="Histograma", ylab= "Frecuencia", xlab = expression("Concentracion de SO"[2]))

boxplot(datos$so2)
boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]) )


boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]), range=0 )
boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]), outline = FALSE )

par(mfrow=c(3,1))   #Armamos grafica con varios plots
boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]) )
boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]), range=0 )
boxplot(datos$so2, horizontal = TRUE, xlab = expression("Concentracion de SO"[2]), outline = FALSE )

# Qué boxplot representa mejor los datos que tenemos?

summaryPlot(datos[,c(1,2)])  # Funcion resumen del paquete openair


# Series de tiempo y box-plot

plot(datos$date, datos$so2, ylab = expression("Concentracion de SO"[2]), xlab = "Fecha")

plot(datos$date, datos$so2, type="l", ylab = expression("Concentracion de SO"[2]), xlab="Fecha")


plot(as.factor(format(datos$date, "%Y-%m")), datos[,2], 
     ylab = expression("Concentracion de SO"[2]), range=0)

plot(as.factor(format(datos$date, "%Y")), datos[,2],     # Analisis por año
     ylab = expression("Concentracion de SO"[2]), range=0)

plot(as.factor(format(datos$date, "%m")), datos[,2],      # Analisis por mes   
     ylab = expression("Concentracion de SO"[2]), range=0)

plot(as.factor(format(datos$date, "%w")), datos[,2],      # Analisis por dia de la semana
     ylab = expression("Concentracion de SO"[2]), range=0)



####################################################################################################

## Abrimos el dat set de condiciones meteorológicas
meteo <- read.xlsx("TP2_DATOSMETEO.xlsx", sheet = 1, startRow = 1)
meteo$fecha <- seq.POSIXt(from = ISOdate(2012,1,1,0), to = ISOdate(2014,12,31,23), by="hours")
datos$ws <- meteo$`ws.(km/h)`
datos$wd <- meteo$`wd.(grados)`

# ROSA DE VIENTOS

windRose(datos)
windRose(datos, grid.line=10, breaks= c(0,1,2,5,6,11,12,19), key.position = "right")
windRose(datos, grid.line=10, type="year", breaks= c(0,1,2,5,6,11,12,19), key.position = "right")
windRose(datos, grid.line=10, type="month", breaks= c(0,1,2,5,6,11,12,19), key.position = "right")
windRose(datos, grid.line=10, type="daylight", breaks= c(0,1,2,5,6,11,12,19), key.position = "right")

# ROSA DE CONCENTRACION

pollutionRose(datos, pollutant="so2", grid.line=10)
pollutionRose(datos, pollutant="so2", statistic="prop.mean")

pollutionRose(datos, pollutant="so2", type="year", statistic="prop.mean")

percentileRose(datos, pollutant="so2", key.position = "right")


##################################################################################

# Relación entre variables

plot(datos$so2, datos$ws)                  
plot(datos$so2, datos$ws, xlab = expression("Concentracion de SO"[2]), ylab="Velocidad del viento" ) 

datos$year <- format(datos$date, "%Y")

ggplot(data=datos) + geom_point(aes(x=so2, y=ws))
ggplot(data=datos) + geom_point(aes(x=so2, y=ws, col=year))
ggplot(data=datos) + geom_point(aes(x=so2, y=ws, col=year)) + theme_bw()
