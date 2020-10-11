library(readr)
library(dplyr)
library(stringr)
library(formattable)
library(devtools)
library(plotly)
library(lubridate)
library(matrixStats)

# Leyendo la tabla
df <- read.csv("c1.csv")

# Limpiando los datos un poco
df$Fecha <- dmy(df$Fecha)

# quitando las Q y limpiando los campos con dinero
convierteNumero <- function(x) {
  if(x == ' Q-   '){
    return(as.integer(0))
    } 
  else {
    return(as.double(str_replace(x, "Q", "")))
    } 
}

# Limpieza de campos
df$Camion_5 <- lapply(df$Camion_5, convierteNumero)
df$Moto <- lapply(df$Moto, convierteNumero)
df$Pickup <- lapply(df$Pickup, convierteNumero)
df$directoCamion_5 <- lapply(df$directoCamion_5, convierteNumero)
df$directoMoto <- lapply(df$directoMoto, convierteNumero)
df$directoPickup <- lapply(df$directoPickup, convierteNumero)
df$factura <- lapply(df$factura, convierteNumero)
df$fijoCamion_5 <- lapply(df$fijoCamion_5, convierteNumero)
df$fijoMoto <- lapply(df$fijoMoto, convierteNumero)
df$fijoPickup <- lapply(df$fijoPickup, convierteNumero)

df$Camion_5 <- as.numeric(df$Camion_5)
df$Moto <- as.numeric(df$Moto)
df$Pickup <- as.numeric(df$Pickup)
df$directoCamion_5 <- as.numeric(df$directoCamion_5)
df$directoMoto <- as.numeric(df$directoMoto)
df$directoPickup <- as.numeric(df$directoPickup)
df$factura <- as.numeric(df$factura)
df$fijoCamion_5 <- as.numeric(df$fijoCamion_5)
df$fijoMoto <- as.numeric(df$fijoMoto)
df$fijoPickup <- as.numeric(df$fijoPickup)


for(i in 1:nrow(df)) {
  df$max[i] <- max(df[i,3:5])
} 

df$Costo <- df$max

df$Directo <- max(df[1,3:5])

for(i in 1:nrow(df)) {
  df$Directo[i] <- max(df[i,11:13])
} 


df$Fijo <- max(df[1,3:5])

for(i in 1:nrow(df)) {
  df$Fijo[i] <- max(df[i,14:16])
}

#Medio
convierteMedioPickup <- function(x, y) {
  retorno <- 'Camion'
  if(x> 0){
    return('Pickup')
  } ifelse(y> 0) {
    return('Moto')
  } else {
    return(retorno)
  }
}


df$Medio <- 'Camión'

for(i in 1:nrow(df)) {
  df$Medio[i] <- if(df$Pickup[i]> 0){'Pickup'} else { if(df$Camion_5[i]> 0) {
    'Camión'
  } else {
    'Moto'
  }}
}

df$Duracion <- '0'


for(i in 1:nrow(df)) {
  if(grepl('x', df$X5.30[i])){
    df$Duracion[i] <- '5 a 30'
    
  }
}

for(i in 1:nrow(df)) {
  if(grepl('x', df$X30.45[i])){
    df$Duracion[i] <- '30 a 45'
    
  }
}

for(i in 1:nrow(df)) {
  if(grepl('x', df$X45.75[i])){
    df$Duracion[i] <- '45 a 75'
    
  }
}

for(i in 1:nrow(df)) {
  if(grepl('x', df$X75.120[i])){
    df$Duracion[i] <- '75 a 120'
    
  }
}

for(i in 1:nrow(df)) {
  if(grepl('x', df$X120.[i])){
    df$Duracion[i] <- 'más de 120'
    
  }
}



### Seleccionando las columnas útiles

data <- df %>% select(Fecha, ID, Cod, origen, Lat, Long, factura, height, max, Directo, Fijo, Medio, Duracion)


## Haciendo el estado de resultados
Totalfacturas <- sum(data$factura)
TotalGastosD <- sum(data$Directo)
TotalGastosF <- sum(data$Fijo)
TotalISR <- 0.05 * (Totalfacturas - TotalGastosD - TotalGastosF)
TotalIVA <- 0.12 * Totalfacturas

# Cálculo de tarifas
TarifarioPromedio <- data %>% group_by(month(Fecha), Cod) %>% summarise(PrecioPromedioPorMetro = mean(factura/height), Demanda = n(), Desviacion = sd(factura/height), RazonPrecioDemanda = PrecioPromedioPorMetro/Demanda) %>% 
    arrange(desc(Cod, `month(Fecha)`)) %>% rename (Mes = `month(Fecha)`)
# Guardarlo para hacer tablas en el informe
library("writexl")
write_xlsx(TarifarioPromedio,"tarifario2.xlsx")


# Cálculo de pérdidas
IngPorMes <- data %>% group_by(month(Fecha), Cod) %>% summarise(Ingresos = sum(factura), Egresos = sum(Fijo + Directo), Ganancia = Ingresos - Egresos) %>% 
  arrange(desc(Cod, `month(Fecha)`)) %>% rename (Mes = `month(Fecha)`)
# Guardarlo para hacer tablas en el informe
library("writexl")
write_xlsx(IngPorMes,"ingresos.xlsx")


# Pérdida
Perdidas <- data %>% filter(factura < max)

#Pérdida promedio
PerdidaPromedio <- data %>% filter(Cod == "VISITA_POR_CORRECCION", factura/height < 15.81433167-7.33344059095454, month(Fecha) == 1)
PerdidaPromedio <- PerdidaPromedio %>% select(Fecha, ID, Cod, factura, height, max, Directo, Fijo, Medio, Duracion)
PerdidaPromedio$Precio <- PerdidaPromedio$factura/PerdidaPromedio$height
PerdidaPromedio$PrecioSugerido <- 15.81433167-7.33344059095454
library("writexl")
write_xlsx(PerdidaPromedio,"casosdeperdida.xlsx")


## Centros de distribución
CentrosDist <- data %>% group_by(month(Fecha), origen) %>% summarise(Ingresos = sum(factura), Egresos = sum(Fijo + Directo), Ganancia = Ingresos - Egresos, Casos = n()) %>% 
  arrange(desc(origen, `month(Fecha)`)) %>% rename (Mes = `month(Fecha)`)

CentrosDistProm <- CentrosDist %>% group_by(origen) %>% summarise(IngresosProm = mean(Ingresos), EgresosProm = mean(Egresos), GananciaPromedio = mean(Ganancia), PromCasos = mean(Casos), DesvCasos = sd(Casos)) %>% 
  arrange(desc(origen)) 

library("writexl")
write_xlsx(CentrosDist,"Apendice3.xlsx")
library("writexl")
write_xlsx(CentrosDistProm,"TablaCentros.xlsx")

## 80/20 de postes
Postes <- data %>% group_by(ID) %>% summarise(Ingresos = sum(factura), Egresos = sum(Fijo + Directo), Ganancia = Ingresos - Egresos, Casos = n())
Postes <- Postes %>% arrange(desc(Ingresos))




barplot(height=head(Postes, 5)$Ingresos, main="Postes con más ingresos", xlab="Postes", ylab="Ingreso en factura", 
        names.arg=head(Postes, 5)$ID,
        border="blue", density=c(10,20,30,40,50))

Postes <- Postes %>% arrange(desc(Ganancia))
barplot(height=head(Postes, 5)$Ganancia, main="Postes con más ganancias", xlab="Postes", ylab="Ganancia bruta", 
        names.arg=head(Postes, 5)$ID,
        border="green", density=c(10,20,30,40,50))


Postes <- Postes %>% arrange(desc(Casos))
barplot(height=head(Postes, 5)$Casos, main="Postes que necesitan más atenciones", xlab="Postes", ylab="Casos", 
        names.arg=head(Postes, 5)$ID,
        border="orange", density=c(10,20,30,40,50))


Postes$Eficiencia <- Postes$Ganancia/Postes$Casos
PromedioDeCporPoste <- mean(Postes$Casos)
PostesEficientes <- Postes %>% filter(Casos > PromedioDeCporPoste)
PostesEficientes <- PostesEficientes %>% arrange(desc(Eficiencia))
barplot(height=head(PostesEficientes, 5)$Eficiencia, main="Postes más eficientes", xlab="Postes", ylab="Eficiencia en  Q", 
        names.arg=head(PostesEficientes, 5)$ID,
        border="red", density=c(10,20,30,40,50))


## Viajes más eficientes
ViajesEficientes <- data %>% group_by(Fecha, origen, Medio) %>% summarise(Ingresos = sum(factura), Egresos = sum(Fijo + Directo), Ganancia = Ingresos - Egresos, Casos = n())
ViajesEficientes <-ViajesEficientes %>% arrange(desc(Ganancia))
library("writexl")
write_xlsx(head(ViajesEficientes, 10),"TablaViajesEficientes.xlsx")


## Medios más eficientes
MediosAgrupados <- data %>% group_by(Medio) %>% summarise(Ingresos = sum(factura), Egresos = sum(Fijo + Directo), Ganancia = Ingresos - Egresos, Casos = n())
pie(MediosAgrupados$Ingresos, labels = MediosAgrupados$Medio, main="Distribución de ingresos por medios")
pie(MediosAgrupados$Ganancia, labels = MediosAgrupados$Medio, main="Distribución de ganacias por medios")
pie(MediosAgrupados$Casos, labels = MediosAgrupados$Medio, main="Distribución de casos por medios")

library("writexl")
write_xlsx(MediosAgrupados,"TablaMedios.xlsx")