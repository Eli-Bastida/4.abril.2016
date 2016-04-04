# 4.abril.2016
graficas seasonplot para identificar estacionalidad, tendencia y ciclicidad

### siguendo con los datos de la clase pasada de las acciones

install.packages("fpp")
require (fpp)
CEMEX <- read.csv("C:\\Users\\SALA-C9\\Downloads\\CEMEX.csv", header = T)
STcemex <- ts(CEMEX[,5], start = 2000, frequency = 12)
seasonplot(STcemex, s =12, year.label = T, main = "valores de accion cemex", ylab = "valor de cierre",
           col = rainbow(20), year.labels.left = T, left= T, pch= 10)
## rainboww es arcoiris year label etiquetas para los años y tambien a la derecha (left), 
## pch es para la figurita en los puntito

monthplot(STcemex, s =12, year.label = T, main = "valores de accion cemex", ylab = "valor de cierre",col = rainbow(20), year.labels.left = T, left= T, pch= 10)
## es la misma grafica pero ahora por mes

## funciones utilizadas
ST graficas
plot or plot.ts()
monthplot()
graficas de subseries temporales

### correlacion y covarianza ###

# covarianza y correlacion: medida del grado de relacion lienal enttre dos variables (X y Y)
# AUTOCAVARIANZA Y AUTO CORRLACION: medida relaion lienal entre valores retardados de una serie de tiempo
# y se mide la relacion entre : YT y YT-1, YT y YT-2, YT y YT-3, etc
# para desestacionalizar y eliminar la tendencia necesitamos conocer la correlacion de las variables
# para realizar mejores pronoisticos de nuestra ST
# en muchos casos la varibales estan correlacionadas entonces si nodotros logramos identificar la 
# correlacion podemos mejorar los pronosticos si las correlaciones son altas para la ST obvservada
# autocerrelacion:
# a veces sucede que los valores que toma una variable en el tiempo no son independientes entre si
# si no que un valor determinado depende de los valores anteriores
# para obtener la correlacion hay que obtener primero la covarianza

#COVARIANZA
#EJERCICIO:
# verifiquen que la formula y la covarianza y la correlacion dan el mismo resultado que las funciones
# cov (x,y) y cor (x,y)
# la covarianza es una medida de asociacion entre dos variables
# cor (x,y) = cov (x,y) / sd(x)sd(y)
# cov (x,y) = (suma((xi-xmedia)(yi-ymedia))) / (n-1)

LIVERPOOL <- read.csv("C:\\Users\\SALA-C9\\Downloads\\LIVERPOOL.csv", header = T)
STliverpool <- ts(LIVERPOOL[,5], start = 2000, frequency = 12)
CEMEX <- read.csv("C:\\Users\\SALA-C9\\Downloads\\CEMEX.csv", header = T)
STcemex <- ts(CEMEX[,5], start = 2000, frequency = 12)
   
n <- length(STcemex) ## NUMERO DE  DATOS
sum(((STcemex-mean(STcemex))*(STliverpool-mean(STliverpool))))/(n-1)
cov(STcemex,STliverpool)
cor(STcemex,STliverpool)


### ejercicio:

### analizar los años de las 3 empresas elegidas (seasonplot) y elegir los tres años que ubiquen 
## con estacionalidad, ciclo o tendecnia, interpretar esos tres años
## calcular con la formula de covarianza y correlacion entre las tes empesas
## es decir empresa 1 = x, empresa 2 = y y empresa 3 = z
## covarianza y correlacion de (x,y), (x,z), (y,z)
## en el caso de que las base de daros no tenga el mimo numero de datos ocupar la funcion window (partir la base)

LIVERPOOL <- read.csv("C:\\Users\\SALA-C9\\Downloads\\LIVERPOOL.csv", header = T)
STliverpool <- ts(LIVERPOOL[,5], start = 2000, frequency = 12)
CEMEX <- read.csv("C:\\Users\\SALA-C9\\Downloads\\CEMEX.csv", header = T)
STcemex <- ts(CEMEX[,5], start = 2000, frequency = 12)
BIMBO <- read.csv("C:\\Users\\SALA-C9\\Downloads\\BIMBO.csv", header = T)
STbimbo <- ts(BIMBO[,5], start = 2000, frequency = 12)

seasonplot(STcemex, s =12, year.label = T, main = "valores de accion cemex", ylab = "valor de cierre",col = rainbow(20), year.labels.left = T, left= T, pch= 10)
seasonplot(STliverpool,s =12, year.label = T, main = "valores de accion liverpool", ylab = "valor de cierre",col = rainbow(20), year.labels.left = T, left= T, pch= 10)
seasonplot(STbimbo, s =12, year.label = T, main = "valores de accion bimbo", ylab = "valor de cierre",col = rainbow(20), year.labels.left = T, left= T, pch= 10)

### CEMEX: observamos tendencia en el año 2002 una tendencia negativa de abril a aseptiembre y tambien
 ##        en el año 2004 de septiempre a diciembre, en losde mas años no se observa tendencia
##         hay dos grandes puntos de ciclicidadn en el año 2005 de mayo a junio y 2006 de junio a julio
##         en esta grafica observamos estacionalidad en los años 2012 y 2014, con incrementos y decrementos muy pequeños
### LIVERPOOL : observamos tendencia en el año 2015 una tendencia positiva de abril a octubre  y tambien
##        una tendencia negativa de octubre  a diciembre, en los demas años no se observa una tendencia definida
##        no se observan grandes puntos de cilicidad
##         en esta grafica demuestran estacionalidad los años 2002, 2005 y 2008 con un comportamiento muy
##        similar durante todo el año
### BIMBO : observamos tendencia en el año 2009 una tendencia positiva de febrero a diciembre  y tambien
##        una tendencia negativa en el año 2010 de marzo a junio y en el mimo año una tendencia positiva de agosto a junio, en los demas años no se observa una tendencia definida
##        se observan un gran ponto de ciclicidad en el año 2011 de marzo a abril
##         en esta grafica demuestran estacionalidad los años 2007 y 2008 con un comportamiento muy
##        similar durante todo el año con incrementos y decrementos notorios

### COVARIANZA
cov(STcemex, STliverpool)
cov(STcemex, STbimbo)
cov(STliverpool, STbimbo)

comprobando:
sum(((STcemex-mean(STcemex))*(STliverpool-mean(STliverpool))))/(n-1)
sum(((STcemex-mean(STcemex))*(STbimbo-mean(STbimbo))))/(n-1)
sum(((STliverpool-mean(STliverpool))*(STbimbo-mean(STbimbo))))/(n-1)

## CORRELACION 
cor(STcemex, STliverpool)
cor(STcemex, STbimbo)
cor(STliverpool, STbimbo)

comprobando:
cov(STcemex, STliverpool) / (sd(STcemex)*sd(STliverpool))
cov(STcemex, STbimbo) / (sd(STcemex)*sd(STbimbo))
cov(STliverpool, STbimbo) / (sd(STliverpool)*sd(STbimbo))
