#                 MODELOS DE SUPERVIVIENCIA Y SERIES DE TIEMPO
#
# --------------------- Proyecto 1: Series de Tiempo -------------------------
# Intregantes:
#       Mendez García Diego
#       Hernández Martínez Alan
#       Salvador Canela Nelly


# Paqueterias * -------------------------------------------------------------
library(fGarch)
library(nortest)
library(forecast)
require(forecast)
library(fpp)
library(Ecdat)
library(ggplot2)
library(Ecfun)
library(fTrading)
library(timeSeries)
library(tseries)
library(moments)
library(car)
library(modeest)
library(stats)
library(rugarch)
library(imputeTS)
library(lubridate)
library(stats)
library(plotly)



# Cargando Base de Datos * ----------------------------------------------------------
setwd("C:/Users/Abigail/Documents/ModelacionProyecto/Entrega Proyecto 1")
Base <- data.frame(read.csv("NFLX.csv", header = T, sep = ","))
View(Base) #visualizamos la base


# Creando Serie de Tiempo * ------------------------------------------------
Precios <- subset(Base,select=-c(High,Low,Close,Open,Volume)) #delimitamos la base
View(Precios) #visualizamos la dilimitacion
class(Precios$Date) #verificamos que nuestras fechas sean tipo fecha
Precios$Date = as.Date(Precios$Date) #Convertimos a fecha
class(Precios$Date) #verificamos el tipo

Precios<-ts(Base$Adj.Close, start = 2002, end = 2020, frequency = 251) 
View(Precios)


# Revisando Datos Faltantes * ----------------------------------------------
anyNA(Precios)  
unique(is.na(Precios)) 
is.na(Precios)
sapply(Precios, function(x) sum(is.na(x))) 
# no tenemos datos faltantes


# Analisis Descriptivo * ---------------------------------------------------
class(Precios) #es de clase serie de tiempo
summary(Precios) #resumen de la informacion
start(Precios) #inicia en 2002
end(Precios) #termina en 2020
length(Precios) #longitud de la serie, 144 observaciones
cycle(Precios) #posicion en el ciclo de cada observacion
frequency(Precios) #frequencia o periocidad


# Graficas * ---------------------------------------------------------------
#Serie de tiempo NFLX.
ts.plot(Precios, xlab='Tiempo', ylab='Precio', col='blue',
        main='Serie de tiempo Precios NFLX.')

#Precios estabilizados de NFLX.Log Precios 
ts.plot(log(Precios), xlab='Tiempo', ylab='Log(Precio)', col='purple',
        main='Serie de tiempo Log Precios NFLX.')

#Rendimientos de los log(Precios)
rendimiento <- diff(log(Precios))
ts.plot(rendimiento, xlab='Tiempo', ylab='Log Rendimientos', col='steelblue4',
        main='Serie de tiempo Log Rendimientos NFLX.')
#Vemos que esta serie no tiene tendencia, ya que los valores oscilan alrededor del 0,
# y que la variabilidad va cambiando, por lo que podria modelarse con alguno de los 
#modelos GARCH


# Histogramas y Densidad Empirica * ------------------------------------------------------------
#Precios
hist(Precios, freq = FALSE, breaks=15, col = "magenta", 
     main="Histograma de Precios NFLX.", xlab = "Precio")
lines(density(Precios), col="blue",lwd=3)
#Log Precios
hist(log(Precios), freq = FALSE, breaks=15, col = "blue", 
     main="Histograma de Log Precios de NFLX.", xlab = "Log(Precio)")
lines(density(log(Precios)), col="red",lwd=3)
#Log Rendimiento
hist(rendimiento, freq = FALSE, breaks=15, col = "green", 
     main="Histograma de Log Rendimientos NFLX.", xlab = "Log Rendimientos")
lines(density(rendimiento), col="purple",lwd=3)


# Calculando Kurtosis * ----------------------------------------------------
moments::kurtosis(rendimiento)


# Realizando Descomposicion * ----------------------------------------------
Descomposicion<- decompose(rendimiento, type = "additive")
plot(Descomposicion, col='blue')

#Obtenemos la tendencia 
plot(Descomposicion$trend,main="Tendencia de los Rendimientos",col=terrain.colors(5),
     xlab="Tiempo",ylab="Tendencia")
#Obtenemos los ciclos estacionales
plot(Descomposicion$seasonal,main="Ciclos estacionales",col=topo.colors(5),
     xlab="Tiempo")
#Obtenemos el componente aleatorio
plot(Descomposicion$random,main="Componente aleatorio",col=rainbow(3),
     xlab="Tiempo")


# Ajuste del Modelo * ----------------------------------------------------

#Verificamos la correlacion de orden cuadratico  de los rendimientos 

acf(rendimiento)
pacf(rendimiento)
acf(rendimiento^2) 
pacf(rendimiento^2) 
acf(abs(rendimiento)) 
pacf(abs(rendimiento)) 


#Verificamos con la prueba Ljung-Box
Box.test(rendimiento,1) #Ho: Los residuos no estan correlacionados
Box.test(rendimiento^2, 1)
Box.test(abs(rendimiento))

# Reconocemos el sesgo y la pesadez de las colas 
RENDIMIENTOS=as.numeric(rendimiento)
ggplot(as.data.frame(rendimiento),aes(x=RENDIMIENTOS))+
  geom_histogram(bins = 40,color="red",fill="darkblue")+
  geom_vline(xintercept = 0,size=1.5,color="yellow") +
  theme_light()

length(rendimiento[-0.08< rendimiento & rendimiento < 0.08])


#          Para llevar a acabo  la propuesta de un modelo eficiente  se probaron los siguientes

#                                                 Modelos AParch   

#s<- ugarchspec(mean.model = list(armaOrder=c(1,1),include.mean=T ), 
#               variance.model = list(model ="apARCH",garchOrder= c(2,0)),  distribution.model = "sstd")


#2s<- ugarchspec(mean.model = list(armaOrder=c(1,1),include.mean=T ), 
#               variance.model = list(model ="apARCH",garchOrder= c(1,0)), distribution.model = "sstd")

#3 s<- ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=T ),
#                 variance.model = list(model ="apARCH",garchOrder= c(2,0)), distribution.model = "sstd")

#4s<- ugarchspec(mean.model = list(armaOrder=c(0,1),include.mean=T ), 
#               variance.model = list(model ="apARCH",garchOrder= c(1,0)), distribution.model = "sstd")


#                                                 Modelos Egarch   

#e<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
#             variance.model = list(model="eGARCH",garchOrder=c(1,1)),
#            distribution.model = 'sstd')


# e<- ugarchspec(mean.model = list(armaOrder=c(1,1),include.mean=T ),
#                variance.model = list(model ="eGARCH",garchOrder= c(1,2)),
#                distribution.model = "sstd")

# e<- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=T ),
#                variance.model = list(model ="eGARCH",garchOrder= c(2,1)),
#                distribution.model = "sstd")


#                                                Modelos csGarch 

# cs<- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=T ),
#                variance.model = list(model ="csGARCH",garchOrder= c(0,1)),
#                distribution.model = "sstd")

# cs<- ugarchspec(mean.model = list(armaOrder=c(1,1),include.mean=T ),
#                variance.model = list(model ="csGARCH",garchOrder= c(0,2)),
#                distribution.model = "sstd")

#                                                Modelos fgarch 

# cs<- ugarchspec(mean.model = list(armaOrder=c(1,1),include.mean=T ),
#                variance.model = list(model ="fGARCH",garchOrder= c(0,2)),
#                distribution.model = "sstd")

# cs<- ugarchspec(mean.model = list(armaOrder=c(0,0),include.mean=T ),
#                variance.model = list(model ="fGARCH",garchOrder= c(1,1)),
#                distribution.model = "sstd")


#                     Al final pudmos osberver que los mas optimos eran los Modelos SGarch 

#1
#s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
#             variance.model = list(model="sGARCH",garchOrder=c(1,1)),
#            distribution.model = 'sstd')

#2
# s<-ugarchspec(mean.model=list(armaOrder=c(1,1)),
#               variance.model = list(model="sGARCH",garchOrder=c(1,0)),
#               distribution.model = 'sstd')

#3
# s<-ugarchspec(mean.model=list(armaOrder=c(0,0)),
#              variance.model = list(model="sGARCH",garchOrder=c(1,0)),
#             distribution.model = 'sstd')



#                                              MEJOR MODELO 

s<-ugarchspec(mean.model=list(armaOrder=c(1,1)),
              variance.model = list(model="sGARCH",garchOrder=c(1,2)),
              distribution.model = 'sstd')

m<-ugarchfit(data=rendimiento,spec=s)


# Coeficientes y p-values * ----------------------------------------------------
m 
plot(m, which = "all")

#Revision de supuestos del modelo *------------------------------------------
# Notemos que las priebas de Ljung Box de no correlacion se obtienen al imprimir m 
# Verifiquemos la media cero y  varianza constante

Residuales=as.numeric(residuals(m))
maximo=max(Residuales)
minimo=min(Residuales)

# Prueba grafica de no correlación de los residuos 
acfPlot(Residuales)
pacfPlot(Residuales)

# Prueba t-student. Ho: la media de los residuos es 0 
mean(Residuales)
t.test(Residuales, mu=0) #No rechazamos la hipotesis nula uff

# Varianza cte  de residuos.
var(Residuales)
Fechas=seq(as.Date("2002-01-01"), by="day", length=4518)

y<-ggplot(as.data.frame(Residuales),aes(x=Fechas,y=Residuales))+
  geom_line(col="darkblue")+theme_light()+
  geom_hline(yintercept = maximo,size=2,color="red")+
  geom_hline(yintercept = minimo,size=2,color="red")+
  ggtitle ("Residuales")
y # Podemos encerrar entre dos bandas 



#Pronóstico * ------------------------------------------------

# Pronosticamos 20 nuevos rendimientos.
predic<- ugarchforecast(m,n.ahead = 20)
plot(predic) # Seleccionar los graficos 1 y 3 





