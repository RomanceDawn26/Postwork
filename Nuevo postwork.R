###############POSTWORK 1 ################################################################################################

'Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R.

Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)

Consulta cómo funciona la función table en R al ejecutar en la consola ?table

Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
  
La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)'

BL <- read.csv('https://www.football-data.co.uk/mmz4281/1920/SP1.csv')
BL_df <- BL[, c('FTHG', 'FTAG')]
Partidos <- length(BL$FTHG)

#Tabla de frecuencias equipo que juega en casa
FTHG <- BL_df %>%group_by(FTHG) %>% summarise(frequency = n())
FTHG_marginal = FTHG$frequency / Partidos
FTHG_df <- data.frame(FTHG$FTHG, FTHG$frequency, FTHG_marginal)

#Tabla de frecuencias equipo visitante
FTAG <- BL_df %>%group_by(FTAG) %>% summarise(frequency = n())
FTAG_marginal = FTAG$frequency / Partidos
FTAG_df <- data.frame(FTAG$FTAG, FTAG$frequency, FTAG_marginal)

#Tabla de frecuencias conjuntas
library(tidyr)
Tabla <- unite(BL_df, Goles,c(1:2),  sep = " ", remove = TRUE)
Tabla_conjunta <- Tabla %>%group_by(Goles) %>% summarise(frequency = n())
Prob_conj <- Tabla_conjunta$frequency / Partidos
Tabla_conjunta <- data.frame(Tabla_conjunta, Prob_conj)

###############POSTWORK 2 ################################################################################################

'Ahora vamos a generar un cúmulo de datos mayor al que se tenía, esta es una situación habitual que se 
puede presentar para complementar un análisis, siempre es importante estar revisando las características o 
tipos de datos que tenemos, por si es necesario realizar alguna transformación en las variables y poder hacer 
operaciones aritméticas si es el caso, además de sólo tener presente algunas de las variables, no siempre se 
requiere el uso de todas para ciertos procesamientos.'

'1. Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga 
española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php'


B1.1920 <- "https://www.football-data.co.uk/mmz4281/1920/D1.csv"
B1.1819 <- "https://www.football-data.co.uk/mmz4281/1819/D1.csv"
B1.1718 <- "https://www.football-data.co.uk/mmz4281/1718/D1.csv"


download.file(url = B1.1920, destfile = "B1.1920.csv", mode = "wb")
download.file(url = B1.1819, destfile = "B1.1819.csv", mode = "wb")
download.file(url = B1.1718, destfile = "B1.1718.csv", mode = "wb")

BL.1920 <- read.csv("B1.1920.csv")
Bl.1819 <- read.csv("B1.1819.csv")
Bl.1718 <- read.csv("B1.1718.csv")

'2. Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary'

str(BL.1920); str(Bl.1718); str(Bl.1819)
head(BL.1920); head(Bl.1718); head(Bl.1819)
summary(BL.1920); summary(Bl.1718); summary(Bl.1819)

'3. Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, 
FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).'

BL.1920 <- select(BL.1920,Date, HomeTeam, AwayTeam, FTHG,FTAG,FTR)
BL.1819 <- select(Bl.1819,Date, HomeTeam, AwayTeam, FTHG,FTAG,FTR)
BL.1718 <- select(Bl.1718,Date, HomeTeam, AwayTeam, FTHG,FTAG,FTR)

'4. Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo 
tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). Con ayuda de la función rbind forma un único data 
frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).'

str(BL.1920); str(BL.1819); str(BL.1718)

BL.1920 <- mutate(BL.1920, Date = as.Date(Date, "%d/%m/%y"))
BL.1819 <- mutate(BL.1819, Date = as.Date(Date, "%d/%m-/y"))
BL.1718 <- mutate(BL.1718, Date = as.Date(Date, "%d/%m/%y"))
head(data)
BL<- rbind(BL.1920,BL.1819,BL.1718)


###############POSTWORK 3 ################################################################################################

'Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan en un partido el equipo de casa o el equipo visitante.'

'Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
  La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)'

#Realiza lo siguiente:
# 1. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.

library(ggplot2)


###############POSTWORK 3 ################################################################################################

'Ahora graficaremos probabilidades (estimadas) marginales y conjuntas para el número de goles que anotan en un partido el equipo de casa o el equipo visitante.'

'Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
  La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)

La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)

La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)'

#Realiza lo siguiente:
# 1. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa.


Games <- length(BL[,1])


Tabla_BL_FTHG <- table(BL$FTHG)
BL_FTHG_marginal <- Tabla_BL_FTHG[1:length(Tabla_BL_FTHG)]/Games
locales <- data.frame(frecuencia = Tabla_BL_FTHG, probabilidad = BL_FTHG_marginal)
locales <- locales[, -c(3)]
locales <- rename(locales, goles = frecuencia.Var1, frecuencia = frecuencia.Freq, probabilidad = probabilidad.Freq)



ggplot(locales, aes(x= locales$goles , y= locales$probabilidad)) +
  geom_bar(stat="identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
  xlab('Núm de goles') +  
  ylab('Probabilidad marginal') +
  ggtitle("Probabilidad marginal de goles que anota el equipo de casa") 

# 2. Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.


Tabla_BL_FTAG <- table(BL$FTAG)
BL_FTAG_marginal <- Tabla_BL_FTAG[1:length(Tabla_BL_FTAG)]/Games
visitantes <- data.frame(frecuencia = Tabla_BL_FTAG, probabilidad = BL_FTAG_marginal)
visitantes <- visitantes[, -c(3)]
visitantes
visitantes <- rename(visitantes, goles = frecuencia.Var1, frecuencia = frecuencia.Freq, probabilidad = probabilidad.Freq)



ggplot(visitantes, aes(x= visitantes$goles , y= visitantes$probabilidad)) +
  geom_bar(stat="identity", color="blue", fill=rgb(0.1,0.4,0.5,0.7) )+
  xlab('Núm de goles') +  
  ylab('Probabilidad marginal') +
  ggtitle("Probabilidad marginal de goles que anota el equipo visitante") 


#3. Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.



T_conjunt <- unite(BL_df, Goles,c(1:2),  sep = " ", remove = TRUE)
Tabla_conjunta <- Tabla %>%group_by(Goles) %>% summarise(frequency = n())



require(utils)

loc <- locales$goles
vis <- visitantes$goles
conjunt <- expand.grid(loc, vis)
conjunt[, 3] = rep(0, length(conjunt[, 1]))
conjunt <- rename(conjunt, goles_local = Var1, goles_visita = Var2, frecuencia_conjunta = V3)

for(i in 1:length(BL[, 1])){
  for(j in 1:length(conjunt[, 1])){
    if((BL[i,4] == conjunt[j, 1]) & (BL[i,5] == conjunt[j, 2])){
      conjunt[j, 3] = conjunt[j, 3] + 1 
    }
  }
}

conjunt[,4] = conjunt$frecuencia_conjunta / Games
conjunt = rename(conjunt, probabilidad = V4)

ggplot(conjunt, aes(x = goles_local, y = goles_visita, fill = probabilidad)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "firebrick4") +
  ggtitle("Heatmap de la probabilidad conjunta") +
  xlab("Goles de locales") + 
  ylab("Goles de visitantes")

###############POSTWORK 4 ################################################################################################

'Ahora investigarás la dependencia o independencia del número de goles anotados por el equipo de casa y el número de 
goles anotados por el equipo visitante mediante un procedimiento denominado bootstrap, revisa bibliografía en 
internet para que tengas nociones de este desarrollo.'

'1. Ya hemos estimado las probabilidades conjuntas de que el equipo de casa anote X=x goles (x=0,1,... ,8), y el 
equipo visitante anote Y=y goles (y=0,1,... ,6), en un partido. Obtén una tabla de cocientes al dividir estas 
probabilidades conjuntas por el producto de las probabilidades marginales correspondientes.'

conjunt[, 5] = rep(0, length(conjunt[, 1]))
for(i in 1:length(conjunt$goles_local )){
  for(j in 1:length(locales$goles)){
    for(k in 1:length(visitantes$goles)){
      if((conjunt[i,1] == locales[j,1]) & (conjunt[i,2] == visitantes[k,1])){
        conjunt[i,5] = locales[j,3] * visitantes[k, 3]
      }
    }
  }
}
conjunto <- rename(conjunt, producto = V5)
conjunto <- conjunto %>% mutate(Cocientes = (conjunto$probabilidad/conjunto$producto) )

'2. Mediante un procedimiento de boostrap, obtén más cocientes similares a los obtenidos en la tabla del punto 
anterior. Esto para tener una idea de las distribuciones de la cual vienen los cocientes en la tabla anterior. 
Menciona en cuáles casos le parece razonable suponer que los cocientes de la tabla en el punto 1, son iguales a 
1 (en tal caso tendríamos independencia de las variables aleatorias X y Y).'

prom <- c()
for(i in 1:9999){
  set.seed(2*i)
  prom[i] = mean(sample(conjunto$Cocientes, length(conjunto$producto), replace = TRUE))
  
}

t.test(prom, alternative = "two.sided", mu = 1)
#La hipotesis nula va hacia la igualdad, lo que nos dice que la mu (media poblacional) es igual a 1 si se acepta.
#En esta ocasion la p  es de < 2.2e-16, menor a 0.05 por lo que rechazamos la hipotesis nula y aceptamos 
#la alternativa, la mu es diferente a 1. Lo que nos indica que la variable X y Y no son independientes. 