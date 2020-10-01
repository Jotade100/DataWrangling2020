library(nycflights13)
library(tidyverse)
library(lubridate)
library(readr)
library(readxl)
library(openxlsx)
library(chron)

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
data <- read.csv("data.csv", sep=";")

data$`Fecha.Creación` <- dmy(data$`Fecha.Creación`)
#data$`Hora.Creación` <- strptime(data$`Hora.Creación`, "%I:%M %p")

data$`Fecha.Final` <- dmy(data$`Fecha.Final`)
#data$`Hora.Final` <-  strptime(data$`Hora.Final`, "%I:%M %p")


primer <- data %>% group_by(month(Fecha.Creación), Cod) %>% summarise(llamadas = sum(Call)) %>% filter(llamadas > 0) %>% arrange(desc(llamadas))
primer <- primer %>% rename(mes = `month(Fecha.Creación)`)


segundo <- data %>% group_by(wday(Fecha.Creación, label = TRUE)) %>% summarise(llamadas = sum(Call), correo = sum(Email), mensaje = sum(SMS), total = llamadas + correo + mensaje) %>%  arrange(desc(total))

tercero <- data %>% group_by(month(Fecha.Creación)) %>% summarise(llamadas = sum(Call), correo = sum(Email), mensaje = sum(SMS), total = llamadas + correo + mensaje) %>%  arrange(desc(total))

library(ggplot2)
theme_set(theme_minimal())
cuarto <- data %>% group_by(Fecha.Creación) %>% summarise(llamadas = sum(Call), correo = sum(Email), mensaje = sum(SMS), total = llamadas + correo + mensaje) %>%  arrange(Fecha.Creación)
ggplot(data = cuarto, aes(x = Fecha.Creación, y = llamadas))+
  geom_line(color = "#00AFBB", size = 1)


data$Día.Creación <- paste(data$Fecha.Creación, data$Hora.Creación)
data$Día.Creación <- parse_date_time(data$Día.Creación, c('%Y-%m-%d %I:%M %p'))
data$Día.Final <- paste(data$Fecha.Final, data$Hora.Final)
data$Día.Final <- parse_date_time(data$Día.Final, c('%Y-%m-%d %I:%M %p'))


quinto_data <- data %>% filter(Call > 0) 

quinto <- mean(
  difftime(quinto_data$Día.Final, quinto_data$Día.Creación, units = 'mins')
  )


sexto <- quinto_data
sexto$tiempo_de_llamada <- difftime(sexto$Día.Final, sexto$Día.Creación, units = 'mins')
sexto <- sexto %>% group_by(tiempo_de_llamada) %>% summarise(llamada = sum(Call)) %>%  arrange(tiempo_de_llamada) %>% filter(tiempo_de_llamada > -1)



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


