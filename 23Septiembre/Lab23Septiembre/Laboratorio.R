library(nycflights13)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)

## Primer inciso
# Ignorando zona horaria que no es importante en fenómenos astronómicos
# Creando la fecha base
x <- '21 de agosto del 2017 a las 18:26:40'
x <- dmy_hms(x)

# Creando las dimensoiones astronómicas medievales
synodic <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)

saros <- 223*synodic

# Fecha final 
pred <- x + saros

resp <- paste0("La fecha del siguiente eclipse es ", pred, sep=" ")

resp


## Segundo inciso
data <- read_excel('data.xlsx')

convertir <- function(x){
  if(typeof(x) != "character") {
    return(as.Date(x))
  } else {
    return(dmy(x))
  }
}

data$`Fecha Creación` <- guess_formats(data$`Fecha Creación`, c('ymd'))



# Tercer inciso
library(DescTools)

calcular_zodiaco <- function() {
  variable_entrada <- readline(prompt="Ingrese su fecha de nacimiento (Día-Mes_Año): ")
  fecha_entrada <- dmy(variable_entrada)
  zodiaco_salida <- Zodiac(fecha_entrada)
  print(zodiaco_salida)
}


## Cuarto Inciso
## Filtrando vuelos con fechas y horas válidas
vuelos <- flights %>% filter(!is.na(dep_time), !is.na(arr_time), dep_time >= 100, arr_time >=100)

vuelos$Hora_salida <-  ymd_hm(paste(vuelos$year, vuelos$month, vuelos$day, vuelos$dep_time %/% 100, ':', vuelos$dep_time %% 100))

vuelos$Hora_llegada <- ymd_hm(paste(vuelos$year, vuelos$month, vuelos$day, vuelos$arr_time %/% 100, ':', vuelos$arr_time %% 100))

vuelos$Hora_salida_estimada <- ymd_hm(paste(vuelos$year, vuelos$month, vuelos$day, vuelos$sched_dep_time %/% 100, ':', vuelos$sched_dep_time %% 100))

vuelos$Hora_llegada_estimada <- ymd_hm(paste(vuelos$year, vuelos$month, vuelos$day, vuelos$sched_arr_time %/% 100, ':', vuelos$sched_arr_time %% 100))

vuelos$Retraso = vuelos$dep_delay + vuelos$arr_delay


