library(readr)
library(dplyr)
library(stringr)
library(formattable)
library(devtools)
library(plotly)



# Leyendo la tabla
df <- read.csv("tabla_completa.csv")

# Haciendo una expansión de los datos y un poco de limpieza

df$precio <- df$Q / df$CANTIDAD

df$Despacho <- ifelse(grepl("despacho a cliente", tolower(df$CLIENTE)), 1, 0)

df$Faltante <- ifelse(grepl("faltante", tolower(df$CLIENTE)), 1, 0)

df$Devolucion <- ifelse(grepl("DEVOLUCION", toupper(df$CLIENTE)), 1, 0)

df$Normal <- ifelse(df$Faltante+df$Despacho+df$Devolucion < 1, 1, 0)

df$NombreCliente <- sapply(strsplit(df$CLIENTE, "/"), `[`, 1)
df$NombreCliente <- as.character(df$NombreCliente)
df$NombreCliente <- sapply(strsplit(df$NombreCliente, "\\|"), `[`, 1)


# Distribución por unidades
df %>% group_by(UNIDAD) %>% summarise(`Venta Promedio en Q` = mean(Q), `Cantidad Promedio UDS` = mean(CANTIDAD))


# Pilotos por mes
formattable(df %>% group_by(PILOTO, MES) %>% summarise(`Cantidad de viajes` =  n(), `Venta Promedio` = mean(Q),`Cantidad Promedio` = mean(CANTIDAD)) 
            
            )

# Eficiencia de pilotos
formattable(df %>% group_by(PILOTO) %>% summarise(`Cantidad de viajes` =  n(), `Venta Promedio` = mean(Q),`Cantidad Promedio` = mean(CANTIDAD), `Razón de eficiencia` = `Cantidad Promedio`/`Cantidad de viajes`) %>% arrange(desc(`Razón de eficiencia`))
            
)

# Viajes por precio
formattable(df %>% group_by(precio) %>% summarise(`Cantidad de viajes` =  n()) 
            
)


# Viajes Clientes
df <- df %>% rename(
  `Nombre del cliente` = NombreCliente
)

formattable(
  
  df %>% group_by(`Nombre del cliente`) %>% summarise(`Venta Promedio` = mean(Q),`Cantidad Promedio` = mean(CANTIDAD)) %>% arrange(desc(`Cantidad Promedio`))
            
)


formattable(
  
  df %>% group_by(`Nombre del cliente`) %>% filter(Devolucion == 0) %>% summarise(`Ventas totales en Q` = sum(Q),`Cantidad Total en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Total en UDS`))
  
)


# Devolución
formattable(
  
  df %>% group_by(`Nombre del cliente`) %>% filter(Devolucion == 1) %>% summarise(`Venta Promedio en Q` = sum(Q),`Cantidad Promedio en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Promedio en UDS`))
  
)

# Faltantes
formattable(
  
  df %>% group_by(UNIDAD) %>% filter(Faltante == 1) %>% summarise(`Cantidad de viajes` =  n(), `Venta Promedio en Q` = sum(Q),`Cantidad Promedio en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Promedio en UDS`))
  
)

# Unidades
formattable(
  
  df %>% group_by(UNIDAD) %>% summarise(`Cantidad de viajes` =  n(), `Venta Promedio en Q` = sum(Q),`Cantidad Promedio en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Promedio en UDS`))
  
)

formattable(
  
  df %>% group_by(PILOTO, UNIDAD) %>% summarise(`Cantidad de viajes` =  n(),`Cantidad Promedio en UDS` = sum(CANTIDAD)) 
  
)


#Análisis exploratorio
hist(df$Q, col = "beige", main = "Distribución de ventas por viaje",
     xlab = "Ventas en Q por viaje", ylab = "Frecuencia")

barplot(table(df$UNIDAD), main="Viajes por unidad", horiz=FALSE,
        names.arg=c("C. Gran.", "C. Peq.", "Panel"), col = c("green", "green", "gray"),  xlab = "Unidad", ylab = "Frecuencia (Viajes)")

barplot(table(df$CREDITO), main="Crédito en días", horiz=TRUE, xlab = "Frecuencia (Viajes)", ylab = "Días de crédito")

barplot(table(df$CREDITO), main="Crédito en días", horiz=TRUE, xlab = "Frecuencia (Viajes)", ylab = "Días de crédito", col = c("green", "green", "gray"))

barplot(table(df$MES), main="Distribución de viajes por mes", horiz=TRUE, xlab = "Frecuencia (Viajes)", ylab = "MES", col = c( "gray",  "gray", "green",  "gray", "green", "gray",  "gray",  "gray",  "gray",  "gray",  "gray",  "gray" ))


## Gráficas
tabla_clientes_may <- df %>% group_by(`Nombre del cliente`) %>% filter(Devolucion == 0) %>% summarise(`Ventas totales en Q` = sum(Q),`Cantidad Total en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Total en UDS`))
tabla_clientes_may$`Nombre del cliente` <- factor(tabla_clientes_may$`Nombre del cliente`, levels = unique(tabla_clientes_may$`Nombre del cliente`)[order(tabla_clientes_may$`Ventas totales en Q`, decreasing = TRUE)])
fig <- plot_ly(tabla_clientes_may , x = ~`Nombre del cliente`, y = ~`Ventas totales en Q`, type = 'bar', name = 'Distribución de clientes por mes', marker = list(color = c(
  'rgba(222,45,38,0.8)', 'rgba(222,45,38,0.8)', 
  'rgba(222,45,38,0.8)', 'rgba(204,204,204,1)',
  'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
  'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
  'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
  'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                                                                                                                                                            'rgba(204,204,204,1)', 'rgba(204,204,204,1)',
                                                                                                                                                                            'rgba(204,204,204,1)'))) 
fig <- fig %>% layout(yaxis = list(title = 'Ventas en Q'), barmode = 'group')

fig


tabla_clientes_may <- df %>% group_by(`Nombre del cliente`) %>% filter(Devolucion == 0) %>% summarise(`Ventas totales en Q` = sum(Q),`Cantidad Total en UDS` = sum(CANTIDAD)) %>% arrange(desc(`Cantidad Total en UDS`))
tabla_clientes_may$`Nombre del cliente` <- factor(tabla_clientes_may$`Nombre del cliente`, levels = unique(tabla_clientes_may$`Nombre del cliente`)[order(tabla_clientes_may$`Ventas totales en Q`, decreasing = TRUE)])
fig <- plot_ly(tabla_clientes_may , x = ~`Nombre del cliente`, y = ~`Ventas totales en Q`, type = 'bar', name = 'Distribución de clientes por mes')
fig <- fig %>% layout(yaxis = list(title = 'Ventas en Q'), barmode = 'group')

fig


unique(df$NombreCliente)

unique(df$UNIDAD)

unique(df$UBICACION)