library(ggplot2)
library(dplyr)
library(DescTools)
library(ggcorrplot)

#descripcion

class(superstore_data)
dim(superstore_data)
str(superstore_data)
head(superstore_data)

#cambiar nombres a español

colnames(superstore_data) <- c("Id",
                  "Año_Nacimiento",
                  "Educacion",
                  "Estado_Civil",
                  "Ingreso",
                  "Hijos_Niños",
                  "Hijos_Adolescentes",
                  "Fecha_Cliente",
                  "Recencia",
                  "Gasto_Vinos",
                  "Gasto_Frutas",
                  "Gasto_Carnes",
                  "Gasto_Pescado",
                  "Gasto_Dulces",
                  "Gasto_ProductosOro",
                  "Compras_Descuento",
                  "Compras_Web",
                  "Compras_Catalogo",
                  "Compras_Tienda",
                  "Visitas_Web_Mensuales",
                  "Respuesta",
                  "Queja")


table(superstore_data$Estado_Civil)
table(superstore_data$Educacion)

superstore_data <- superstore_data %>%
  mutate(Estado_Civil = recode(Estado_Civil, "Alone" = "Soltero",
                            "Divorced" = "Divorciado", "Married" = "Casado",
                            "Single" = "Soltero", "Together" = "En pareja",
                            "Widow" = "Viudo", "YOLO" = "Soltero", "Absurd" = NA_character_ ))

superstore_data <- superstore_data %>%
  mutate(Educacion = recode(Educacion, "2n Cycle" = "Ciclo Superior", "Basic" = "Básico",
                               "Graduation" = "Grado", "Master" = "Master",
                               "PhD" = "Doctorado" ))

superstore_data$Fecha_Cliente <- as.Date(superstore_data$Fecha_Cliente, format = "%m/%d/%Y")

superstore_data <- superstore_data %>%
  mutate(Respuesta = recode(Respuesta, "0" = "No", "1" = "Sí"))

superstore_data <- superstore_data %>%
  mutate(Queja = recode(Queja, "0" = "No", "1" = "Sí"))

object.size(superstore_data)

#Calidad de datos


superstore_data[duplicated(superstore_data), ] #identifica duplicados
Abstract (superstore_data)
PlotMiss(superstore_data, main = "Valores faltantes por variable")
superstore_data$Ingreso[is.na(superstore_data$Ingreso)]<-mean(superstore_data$Ingreso, na.rm=TRUE)
superstore_data <- na.omit(superstore_data) 
Abstract (superstore_data)


#Identificar datos atípicos de cada variable
Outlier(superstore_data$Año_Nacimiento, method = "boxplot") #extraemos los valores atípicos
boxplot(superstore_data$Año_Nacimiento, main="Año de Nacimiento") #gráfico de bigotes

Outlier(superstore_data$Ingreso, method = "boxplot") 
boxplot(superstore_data$Ingreso, main="Ingreso anual")

Outlier(superstore_data$Hijos_Niños, method = "boxplot") 
boxplot(superstore_data$Hijos_Niños, main="Hijos Niños")

Outlier(superstore_data$Hijos_Adolescentes, method = "boxplot") 
boxplot(superstore_data$Hijos_Adolescentes, main="Hijos Adolescentes")

Outlier(superstore_data$Recencia, method = "boxplot") 
boxplot(superstore_data$Recencia, main="Recencia")

Outlier(superstore_data$Gasto_Vinos, method = "boxplot") 
boxplot(superstore_data$Gasto_Vinos, main="Gasto en vinos en los últimos dos años")

Outlier(superstore_data$Gasto_Frutas, method = "boxplot") 
boxplot(superstore_data$Gasto_Frutas, main="Gasto en fruta en los últimos dos años")

Outlier(superstore_data$Gasto_Carnes, method = "boxplot") 
boxplot(superstore_data$Gasto_Carnes, main="Gasto en carne en los últimos dos años")

Outlier(superstore_data$Gasto_Pescado, method = "boxplot") 
boxplot(superstore_data$Gasto_Pescado, main="Gasto en pescado en los últimos dos años")

Outlier(superstore_data$Gasto_Dulces, method = "boxplot") 
boxplot(superstore_data$Gasto_Dulces, main="Gasto en dulces en los últimos dos años")

Outlier(superstore_data$Gasto_ProductosOro, method = "boxplot") 
boxplot(superstore_data$Gasto_ProductosOro, main="Gasto en productos de oro en los últimos dos años")

Outlier(superstore_data$Compras_Descuento, method = "boxplot")
boxplot(superstore_data$Compras_Descuento, main="Número de compras con descuento")

Outlier(superstore_data$Compras_Web, method = "boxplot") 
boxplot(superstore_data$Compras_Web, main="Número de compras en la web")

Outlier(superstore_data$Compras_Catalogo, method = "boxplot") 
boxplot(superstore_data$Compras_Catalogo, main="Número de compras con el catálogo")

Outlier(superstore_data$Compras_Tienda, method = "boxplot") 
boxplot(superstore_data$Compras_Tienda, main="Número de compras en tienda")

Outlier(superstore_data$Visitas_Web_Mensuales, method = "boxplot") 
boxplot(superstore_data$Visitas_Web_Mensuales, main="Número de visitas a la web en el último mes")

#Estudio de los atípicos de Año_Nacimiento e Ingreso

valores_atipicos_Ingreso <- c(157146, 160803, 666666, 162397, 157733, 153924, 156924, 157243)
valores_atipicos_Año <- c(1893, 1899, 1900)


indices_atipicos_Año <- which(superstore_data$Año_Nacimiento %in% valores_atipicos_Año)
indices_atipicos_Año
registros_atipicos_Año <- superstore_data[indices_atipicos_Año, ]
print(registros_atipicos_Año)

indices_atipicos_Ingreso <- which(superstore_data$Ingreso %in% valores_atipicos_Ingreso)
indices_atipicos_Ingreso
registros_atipicos_Ingreso <- superstore_data[indices_atipicos_Ingreso, ]
print(registros_atipicos_Ingreso)

#Eliminar atípicos seleccionados
superstore_data <- superstore_data[-indices_atipicos_Año, ]
superstore_data <- superstore_data[superstore_data$Ingreso != 666666, ]

#Análisis univariante

#id
summary(superstore_data$Id)

#Año_Nacimiento
summary(superstore_data$Año_Nacimiento)
IQR(superstore_data$Año_Nacimiento)
var(superstore_data$Año_Nacimiento)
sd(superstore_data$Año_Nacimiento)
cv_Año_Nacimiento<- sd(superstore_data$Año_Nacimiento)/mean(superstore_data$Año_Nacimiento)
cv_Año_Nacimiento

hist(superstore_data$Año_Nacimiento ,
     main = "Distribución de los Años de Nacimiento",
     xlab = "Años",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "darkblue")

abline(v = mean(superstore_data$Año_Nacimiento), col = "red", lty = 2)  # Línea de la media en rojo
legend("topright", legend = c("Media"), col = "red", lty = 2)  

#Educacion
summary(superstore_data$Educacion)


barplot(table(superstore_data$Educacion ), 
        main="Distribución de Educación", 
        ylab = "Frecuencia",
        col = c("lightgreen", "lightblue", "lightpink", "lightyellow", "lightgray")
)

#Estado_Civil
summary(superstore_data$Estado_Civil)

barplot(table(superstore_data$Estado_Civil ), 
        main="Distribución del Estado Civil", 
        ylab = "Frecuencia",
        col = c("lightgreen", "lightblue", "lightpink", "lightyellow", "lightgray")
    )

#Ingreso
summary(superstore_data$Ingreso)
IQR(superstore_data$Ingreso)
var(superstore_data$Ingreso)
sd(superstore_data$Ingreso)
cv_Ingreso<- sd(superstore_data$Ingreso)/mean(superstore_data$Ingreso)
cv_Ingreso

hist(superstore_data$Ingreso ,
     main = "Distribución de Ingresos",
     xlab = "Ingresos",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "darkgreen")

abline(v = mean(superstore_data$Ingreso), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2)  

#Hijos_Niños
summary(superstore_data$Hijos_Niños)
IQR(superstore_data$Hijos_Niños)
var(superstore_data$Hijos_Niños)
sd(superstore_data$Hijos_Niños)
cv_Hijos_Niños<- sd(superstore_data$Hijos_Niños)/mean(superstore_data$Hijos_Niños)
cv_Hijos_Niños

ggplot(superstore_data, aes(x = as.factor(Hijos_Niños))) +
  geom_bar(fill = "lightpink", color = "#C2185B") +
  labs(title = "Número de Hijos Pequeños",
       x = "Cantidad de Hijos Pequeños",
       y = "Frecuencia") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    panel.grid.major.x = element_blank(),  # Eliminar las líneas verticales de la cuadrícula
    ) 


#Hijos_Adolescentes
summary(superstore_data$Hijos_Adolescentes)
IQR(superstore_data$Hijos_Adolescentes)
var(superstore_data$Hijos_Adolescentes)
sd(superstore_data$Hijos_Adolescentes)
cv_Hijos_Adolescentes<- sd(superstore_data$Hijos_Adolescentes)/mean(superstore_data$Hijos_Adolescentes)
cv_Hijos_Adolescentes

ggplot(superstore_data, aes(x = as.factor(Hijos_Adolescentes))) +
  geom_bar(fill = "lightpink", color = "#C2185B") +
  labs(title = "Número de Hijos Adolescentes",
       x = "Cantidad de Hijos Adolescentes",
       y = "Frecuencia") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    panel.grid.major.x = element_blank(),  
  ) 

#Fecha_Cliente
summary(superstore_data$Fecha_Cliente)

ggplot(superstore_data, aes(x = Fecha_Cliente)) +
  geom_histogram(binwidth = 30, fill = "skyblue", color = "darkblue") +
  labs(title = "Distribución de Clientes por Fecha",
       x = "Fechas de Suscripción (Mes y Año)",
       y = "Frecuencia de Clientes") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Para una mejor lectura de las fechas
  )

#Recencia
summary(superstore_data$Recencia)
IQR(superstore_data$Recencia)
var(superstore_data$Recencia)
sd(superstore_data$Recencia)
cv_Recencia<- sd(superstore_data$Recencia)/mean(superstore_data$Recencia)
cv_Recencia

hist(superstore_data$Recencia ,
     main = "Recencia (días desde la última visita del cliente)",
     xlab = "Días",
     ylab = "Frecuencia",
     col = "yellow",
     border = "orange")

abline(v = mean(superstore_data$Recencia), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2)  

#Gasto_Vinos
summary(superstore_data$Gasto_Vinos)
IQR(superstore_data$Gasto_Vinos)
var(superstore_data$Gasto_Vinos)
sd(superstore_data$Gasto_Vinos)
cv_Gasto_Vinos<- sd(superstore_data$Gasto_Vinos)/mean(superstore_data$Gasto_Vinos)
cv_Gasto_Vinos

hist(superstore_data$Gasto_Vinos ,
     main = "Distribución de Gastos en Vinos",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "#C39BD3",
     border = "#7D3C98")

abline(v = mean(superstore_data$Gasto_Vinos), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2)  


#Gasto_Frutas
summary(superstore_data$Gasto_Frutas)
IQR(superstore_data$Gasto_Frutas)
var(superstore_data$Gasto_Frutas)
sd(superstore_data$Gasto_Frutas)
cv_Gasto_Frutas<- sd(superstore_data$Gasto_Frutas)/mean(superstore_data$Gasto_Frutas)
cv_Gasto_Frutas

hist(superstore_data$Gasto_Frutas ,
     main = "Distribución del Gasto en Frutas",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "darkgreen")

abline(v = mean(superstore_data$Gasto_Frutas), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2) 

#Gasto_Carnes
summary(superstore_data$Gasto_Carnes)
IQR(superstore_data$Gasto_Carnes)
var(superstore_data$Gasto_Carnes)
sd(superstore_data$Gasto_Carnes)
cv_Gasto_Carnes<- sd(superstore_data$Gasto_Carnes)/mean(superstore_data$Gasto_Carnes)
cv_Gasto_Carnes

hist(superstore_data$Gasto_Carnes ,
     main = "Distribución del Gasto en Carne",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "#F1948A",              
     border = "#C0392B")

abline(v = mean(superstore_data$Gasto_Carnes), col = "darkred", lty = 2)
legend("topright", legend = c("Media"), col = "darkred", lty = 2) 

#Gasto_Pescado
summary(superstore_data$Gasto_Pescado)
IQR(superstore_data$Gasto_Pescado)
var(superstore_data$Gasto_Pescado)
sd(superstore_data$Gasto_Pescado)
cv_Gasto_Pescado<- sd(superstore_data$Gasto_Pescado)/mean(superstore_data$Gasto_Pescado)
cv_Gasto_Pescado

hist(superstore_data$Gasto_Pescado ,
     main = "Distribución del Gasto en Pescado",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "darkblue")

abline(v = mean(superstore_data$Gasto_Pescado), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2) 

#Gasto_Dulces
summary(superstore_data$Gasto_Dulces)
IQR(superstore_data$Gasto_Dulces)
var(superstore_data$Gasto_Dulces)
sd(superstore_data$Gasto_Dulces)
cv_Gasto_Dulces<- sd(superstore_data$Gasto_Dulces)/mean(superstore_data$Gasto_Dulces)
cv_Gasto_Dulces

hist(superstore_data$Gasto_Dulces ,
     main = "Distribución del Gasto en Dulces",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "lightpink",
     border = "#C2185B")

abline(v = mean(superstore_data$Gasto_Dulces), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2) 

#Gasto_ProductosOro
summary(superstore_data$Gasto_ProductosOro)
IQR(superstore_data$Gasto_ProductosOro)
var(superstore_data$Gasto_ProductosOro)
sd(superstore_data$Gasto_ProductosOro)
cv_Gasto_ProductosOro<- sd(superstore_data$Gasto_ProductosOro)/mean(superstore_data$Gasto_ProductosOro)
cv_Gasto_ProductosOro

hist(superstore_data$Gasto_ProductosOro ,
     main = "Distribución del Gasto en Productos de Oro",
     xlab = "Gasto",
     ylab = "Frecuencia",
     col = "yellow",
     border = "orange")

abline(v = mean(superstore_data$Gasto_ProductosOro), col = "red", lty = 2)
legend("topright", legend = c("Media"), col = "red", lty = 2) 

#Compras_Descuento
summary(superstore_data$Compras_Descuento)
IQR(superstore_data$Compras_Descuento)
var(superstore_data$Compras_Descuento)
sd(superstore_data$Compras_Descuento)
cv_Compras_Descuento<- sd(superstore_data$Compras_Descuento)/mean(superstore_data$Compras_Descuento)
cv_Compras_Descuento

barplot(table(superstore_data$Compras_Descuento ), 
        main="Distribución de Compras con Descuento", 
        xlab = "Número de Compras con Descuento",
        ylab = "Frecuencia",
        col="lightgreen",              
        border = "darkgreen",)

#Compras_Web
summary(superstore_data$Compras_Web)
IQR(superstore_data$Compras_Web)
var(superstore_data$Compras_Web)
sd(superstore_data$Compras_Web)
cv_Compras_Web<- sd(superstore_data$Compras_Web)/mean(superstore_data$Compras_Web)
cv_Compras_Web
mean(superstore_data$Compras_Web)

barplot(table(superstore_data$Compras_Web ), 
        main="Distribución de Compras Web",
        xlab = "Número de Compras por la Web",
        ylab = "Frecuencia",
        col="skyblue",              
        border = "darkblue", )


#Compras_Catalogo
summary(superstore_data$Compras_Catalogo)
IQR(superstore_data$Compras_Catalogo)
var(superstore_data$Compras_Catalogo)
sd(superstore_data$Compras_Catalogo)
cv_Compras_Catalogo<- sd(superstore_data$Compras_Catalogo)/mean(superstore_data$Compras_Catalogo)
cv_Compras_Catalogo

barplot(table(superstore_data$Compras_Catalogo ), 
        main="Distribución de Compras por el Catálogo", 
        xlab = "Número de Compras mediante el Catálogo",
        ylab = "Frecuencia",
        col="#C39BD3",              
        border = "#7D3C98", 
        )


#Compras_Tienda
summary(superstore_data$Compras_Tienda)
IQR(superstore_data$Compras_Tienda)
var(superstore_data$Compras_Tienda)
sd(superstore_data$Compras_Tienda)
cv_Compras_Tienda<- sd(superstore_data$Compras_Tienda)/mean(superstore_data$Compras_Tienda)
cv_Compras_Tienda

barplot(table(superstore_data$Compras_Tienda ), 
        main="Distribución de las Compras en Tienda", 
        xlab = "Número de Compras en Tienda",
        ylab = "Frecuencia",
        col = "#D2B48C",              
        border = "#8B4513", )


#Visitas_Web_Mensuales
summary(superstore_data$Visitas_Web_Mensuales)
IQR(superstore_data$Visitas_Web_Mensuales)
var(superstore_data$Visitas_Web_Mensuales)
sd(superstore_data$Visitas_Web_Mensuales)
cv_Visitas_Web_Mensuales<- sd(superstore_data$Visitas_Web_Mensuales)/mean(superstore_data$Visitas_Web_Mensuales)
cv_Visitas_Web_Mensuales

barplot(table(superstore_data$Visitas_Web_Mensuales ), 
        main="Distribución de Visitas Web Mensuales", 
        xlab = "Número de Visitas Web Mensuales",
        ylab = "Frecuencia",
        col = "yellow",
        border = "orange")


#Respuesta

barplot(table(superstore_data$Respuesta ), 
        main="Distribución de las Respuestas en la última campaña", 
        xlab = "Respuestas",
        ylab = "Frecuencia",
        col = c("lightgreen", "lightblue")
        )


#Queja

barplot(table(superstore_data$Queja ), 
        main="Distribución de las Quejas en el último año", 
        xlab = "Quejas",
        ylab = "Frecuencia",
        col = c("lightgreen", "lightblue"))





#bivariante


# Calcular la correlación
correlaciones<- cor(superstore_data[sapply(superstore_data, is.numeric)])

# Mapa de calor de la matriz de correlación

ggcorrplot(correlaciones, 
           outline.color = "grey",
           title = "Mapa de Calor de Correlación",
           )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),   # Centrar el título y hacerlo en negrita
    axis.text.x = element_text(angle = 45, hjust = 1),       # Rotación de etiquetas en el eje X
    axis.text.y = element_text(size = 10)                    # Tamaño de las etiquetas en el eje Y
  )


#cuantitativos

#gasto carne e ingresos
cov(superstore_data$Gasto_Carnes, superstore_data$Ingreso)
cor(superstore_data$Gasto_Carnes, superstore_data$Ingreso)

ggplot(superstore_data, aes(x = Ingreso, y = Gasto_Carnes)) +
  geom_point(aes(color = Gasto_Carnes), size = 3) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ingreso vs Gasto en Carne",
       x = "Ingreso",
       y = "Gasto en Carne") +
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"),
)


divisionescarne <- superstore_data #vamos a crear un dataframe en el que dividir los gastos por grupos
divisionescarne$Gasto_Carnes <- cut(divisionescarne$Gasto_Carnes, breaks = 4)  # Crear los grupos 

ggplot(divisionescarne, aes(x = Gasto_Carnes, y = Ingreso)) +
  geom_boxplot(aes(fill = Gasto_Carnes)) +
  labs(title = "Ingreso y Gasto en Carne",
       x = "Gasto en Carne",
       y = "Ingreso") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))

#gasto vinos e ingresos
cov(superstore_data$Gasto_Vinos, superstore_data$Ingreso)
cor(superstore_data$Gasto_Vinos, superstore_data$Ingreso)

ggplot(superstore_data, aes(x = Ingreso, y = Gasto_Vinos)) +
  geom_point(aes(color = Gasto_Vinos),  size = 3) +
  scale_color_gradient(low = "lavender", high = "purple") +
  labs(title = "Ingreso vs Gasto en Vinos",
       x = "Ingreso",
       y = "Gasto en Vinos") +
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"),
  )




divisionesvino <- superstore_data #vamos a crear un dataframe en el que dividir los gastos por grupos
divisionesvino$Gasto_Vinos <- cut(divisionesvino$Gasto_Vinos, breaks = 5)  # Crear grupos de visitas web

ggplot(divisionesvino, aes(x = Gasto_Vinos, y = Ingreso)) +
  geom_boxplot(aes(fill = Gasto_Vinos)) +
  labs(title = "Ingreso y Gasto en Vino",
       x = "Gasto en Vino",
       y = "Ingreso") +
  theme_minimal() +
  scale_fill_brewer(palette = "Purples")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))

#compras catalogo e ingresos
cov(superstore_data$Compras_Catalogo, superstore_data$Ingreso)
cor(superstore_data$Compras_Catalogo, superstore_data$Ingreso)

ggplot(superstore_data, aes(x = Ingreso, y = Compras_Catalogo)) +
  geom_point(aes(color = Compras_Catalogo), size = 3) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Ingreso vs Compras mediante el Catalogo",
       x = "Ingreso",
       y = "Compras mediante el Catalogo") +
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"),
  )

divisionescatalogo <- superstore_data #vamos a crear un dataframe en el que dividir las compras por grupos
divisionescatalogo$Compras_Catalogo <- cut(divisionescatalogo$Compras_Catalogo, breaks = 3)  # Crear grupos de visitas web

ggplot(divisionescatalogo, aes(x = Compras_Catalogo, y = Ingreso)) +
  geom_boxplot(aes(fill = Compras_Catalogo)) +
  labs(title = "Ingreso según las Compras en el Catálogo",
       x = "Compras en el Catálogo",
       y = "Ingreso") +
  theme_minimal() +
  scale_fill_brewer(palette = "Greens")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))

#compras catalogo y gasto carne
cov(superstore_data$Compras_Catalogo, superstore_data$Gasto_Carnes)
cor(superstore_data$Compras_Catalogo, superstore_data$Gasto_Carnes)

ggplot(superstore_data, aes(x = Compras_Catalogo, y = Gasto_Carnes)) +
  geom_point(aes(color = Compras_Catalogo), size = 3) +
  scale_color_gradient(low = "coral", high = "darkred") +
  labs(title = "Compras en el Catálogo y Gasto en Carne",
       x = "Compras mediante el Catalogo",
       y = "Gasto en Carne") +
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"),
  )

divisionescatalogo <- superstore_data #vamos a crear un dataframe en el que dividir las compras por grupos
divisionescatalogo$Compras_Catalogo <- cut(divisionescatalogo$Compras_Catalogo, breaks = 4)  # Crear grupos de visitas web

ggplot(divisionescatalogo, aes(x = Compras_Catalogo, y = Gasto_Carnes)) +
  geom_boxplot(aes(fill = Compras_Catalogo)) +
  labs(title = "Gasto en Carne según las Compras en el Catálogo",
       x = "Compras en el Catálogo",
       y = "Gasto en Carne") +
  theme_minimal() +
  scale_fill_brewer(palette = "Reds")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))



#visitas web e ingresos
cov(superstore_data$Visitas_Web_Mensuales, superstore_data$Ingreso)
cor(superstore_data$Visitas_Web_Mensuales, superstore_data$Ingreso)

ggplot(superstore_data, aes(x = Visitas_Web_Mensuales, y = Ingreso)) +
  geom_point(aes(color = Visitas_Web_Mensuales), size = 3) +
  scale_color_gradient(low = "lightyellow", high = "gold") +
  labs(title = "Ingreso vs Visitas Web Mensuales",
       x = "Visitas Web Mensuales",
       y = "Ingreso") +
  theme_minimal() +
  theme (plot.title = element_text(hjust = 0.5, face = "bold"),
  )

divisiones <- superstore_data #vamos a crear un dataframe en el que dividir las visitas por grupos
divisiones$Visitas_Web <- cut(divisiones$Visitas_Web_Mensuales, breaks = 3)  # Crear grupos de visitas web

ggplot(divisiones, aes(x = Visitas_Web, y = Ingreso)) +
  geom_boxplot(aes(fill = Visitas_Web)) +
  labs(title = "Ingreso según las Visitas Web Mensuales",
       x = "Visitas Web Mensuales",
       y = "Ingreso") +
  theme_minimal() +
  scale_fill_brewer(palette = "YlOrRd")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"))


#Comparcion Respuestas
# Con Estado civil
tabla_contingencia_resp_estado <- as.data.frame(table(superstore_data$Estado_Civil, superstore_data$Respuesta))
colnames(tabla_contingencia_resp_estado) <- c("Estado_Civil", "Respuesta", "Frecuencia")

# Calcular proporciones dentro de cada Estado Civil
tabla_contingencia_resp_estado <- tabla_contingencia_resp_estado %>%
  group_by(Estado_Civil) %>%
  mutate(Proporcion = Frecuencia / sum(Frecuencia))

ggplot(tabla_contingencia_resp_estado, aes(x = Estado_Civil, y = Proporcion, fill = Respuesta)) +
  geom_bar(stat = "identity") + 
  labs(title = "Distribución Relativa de Respuestas por Estado Civil",
       x = "Estado Civil",
       y = "Proporción",
       fill = "Respuesta") +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar eje y como porcentaje
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

# Con Educacion
tabla_contingencia_resp_educ <- as.data.frame(table(superstore_data$Educacion, superstore_data$Respuesta))
colnames(tabla_contingencia_resp_educ) <- c("Educacion", "Respuesta", "Frecuencia")

tabla_contingencia_resp_educ <- tabla_contingencia_resp_educ %>%
  group_by(Educacion) %>%
  mutate(Proporcion = Frecuencia / sum(Frecuencia))

ggplot(tabla_contingencia_resp_educ, aes(x = Educacion, y = Proporcion, fill = Respuesta)) +
  geom_bar(stat = "identity") + 
  labs(title = "Distribución Relativa de Respuestas por Educación",
       x = "Educación",
       y = "Proporción",
       fill = "Respuesta") +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar eje y como porcentaje
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

#Comparcion Queja
# Con Estado civil
tabla_contingencia_queja_estado <- as.data.frame(table(superstore_data$Estado_Civil, superstore_data$Queja))
colnames(tabla_contingencia_queja_estado) <- c("Estado_Civil", "Queja", "Frecuencia")

# Calcular proporciones dentro de cada Estado Civil
tabla_contingencia_queja_estado <- tabla_contingencia_queja_estado %>%
  group_by(Estado_Civil) %>%
  mutate(Proporcion = Frecuencia / sum(Frecuencia))

ggplot(tabla_contingencia_queja_estado, aes(x = Estado_Civil, y = Proporcion, fill = Queja)) +
  geom_bar(stat = "identity") + 
  labs(title = "Distribución Relativa de Quejas por Estado Civil",
       x = "Estado Civil",
       y = "Proporción",
       fill = "Queja") +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar eje y como porcentaje
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )

# Con Educacion
tabla_contingencia_queja_educ <- as.data.frame(table(superstore_data$Educacion, superstore_data$Queja))
colnames(tabla_contingencia_queja_educ) <- c("Educacion", "Queja", "Frecuencia")

tabla_contingencia_queja_educ <- tabla_contingencia_queja_educ %>%
  group_by(Educacion) %>%
  mutate(Proporcion = Frecuencia / sum(Frecuencia))

ggplot(tabla_contingencia_queja_educ, aes(x = Educacion, y = Proporcion, fill = Queja)) +
  geom_bar(stat = "identity") + 
  labs(title = "Distribución Relativa de Quejas por Educación",
       x = "Educación",
       y = "Proporción",
       fill = "Queja") +
  scale_fill_manual(values = c("lightcoral", "skyblue")) +
  scale_y_continuous(labels = scales::percent_format()) +  # Mostrar eje y como porcentaje
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13)
  )


#variables diferentes




#Gasto vino y educacion
boxplot(superstore_data$Gasto_Vinos ~ superstore_data$Educacion, main="Gasto en Vinos según el Nivel Educativo", xlab="Educacion", ylab="Gasto en Vinos", col="lavender")


#crear nueva variable
superstore_data$Tiene_Hijos <- ifelse(superstore_data$Hijos_Niños > 0 | superstore_data$Hijos_Adolescentes > 0, "Sí", "No")

media_gasto_dulces <- aggregate(Gasto_Dulces ~ Tiene_Hijos, data = superstore_data, FUN = mean)

print(media_gasto_dulces)



#FASE 2

#creacion y analisis de la nueva variable

superstore_data_lm <- superstore_data
superstore_data_lm <- superstore_data_lm %>%
  mutate(Respuesta = recode(Respuesta, "No" = "0", "Sí" = "1"))

superstore_data_lm <- superstore_data_lm %>%
  mutate(Queja = recode(Queja, "No" = "0", "Sí" = "1"))

superstore_data_lm <- superstore_data_lm %>%
  select(-Tiene_Hijos)

superstore_data_lm <- superstore_data_lm %>%
  select(-Id)

superstore_data_lm$Total_Gastos <- rowSums(superstore_data_numerico[, c("Gasto_Vinos", "Gasto_Frutas", "Gasto_Carnes", "Gasto_Pescado", "Gasto_Dulces", "Gasto_ProductosOro")])

summary(superstore_data_lm$Total_Gastos)
hist(superstore_data_lm$Total_Gastos, main="Gastos totales", ylab="Frecuencia", xlab="Total_Gastos", breaks=20)


library(psych)
pairs.panels(superstore_data_lm,gap=0)

#crear modelos


#Caso 1
# Crear el modelo de regresión múltiple - total gastos
modelo <- lm(Total_Gastos ~  Hijos_Niños +Compras_Web +Compras_Catalogo+Gasto_Carnes+Gasto_Pescado+Gasto_Dulces+Gasto_Vinos+Gasto_Frutas, data = superstore_data_lm)

summary(modelo)

plot(modelo$fitted.values, superstore_data_lm$Total_Gastos,
     main = "Valores Reales vs Predichos",
     xlab = "Valores Predichos",
     ylab = "Valores Reales",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Línea de referencia ideal


#Definimos los coeficientes

bo = 10.252512
b1 = -6.701912
b2 = 4.575039 
b3= 3.335032  
b4=	0.983120 
b5= 1.155259
b6= 1.051046
b7= 1.002738
b8=1.144613

niños = 1
web = 5
catalogo= 6
carne= 180
pescado=10
vinos=200
dulces= 15
frutas=50

#Predicción

caso_1 =paste("El gasto total en este caso es:",round( bo + (b1*niños) + (b2*web)+(b3*catalogo)+(b4*carne)+(b5*pescado)+(b6*vinos)+(b7*dulces)+(b8*frutas),2))
caso_1

#Caso 2
# Crear el modelo de regresión múltiple - compras catalogo

modelo2 <- lm(Compras_Catalogo ~  Hijos_Niños  +Compras_Descuento+Visitas_Web_Mensuales+Gasto_Pescado+Gasto_Carnes+Gasto_Dulces+Gasto_Vinos, data = superstore_data_lm)

summary(modelo2)

plot(modelo2$fitted.values, superstore_data_lm$Compras_Catalogo,
     main = "Valores Reales vs Predichos",
     xlab = "Valores Predichos",
     ylab = "Valores Reales",
     pch = 19, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Línea de referencia ideal



#Definamos los coeficientes

bo = 1.780780
b1 = -0.605628
b2 = 0.208048 
b3= -0.206218  
b4=	0.004624
b5= 0.004986
b6= 0.002996
b7= 0.002220

niños = 1
descuento = 1
visitasweb = 5
pescado=10
carne= 180
vinos=200
dulces= 15

#Predicción

caso_2 =paste("El número de compras mediante el catálogo en este caso es:",round( bo + (b1*niños) + (b2*descuento)+(b3*visitasweb)+(b4*pescado)+(b5*carne)+(b6*vinos)+(b7*dulces),2))
caso_2




# Definamos las variables# Definamos las variablesGasto_Carnes
bo = -429.3
b1 = 0.011
b2 = -98.91
b3= -171.1
b4=	20.43
b5= 24.45
b6=35.67
b7=71.3

#caso 1

ingreso = 60000
niños = 0
adolescentes = 1
visitas = 9
web = 5
tienda = 7
catalogo= 6

summary(superstore_data_lm$Compras_Catalogo)

nuevo_tree_caso_1 =paste("El gasto web del caso 1 es de:",round( bo + (b1*ingreso) + (b2*niños)+(b3*adolescentes)+(b4*visitas)+(b5*web)+(b6*tienda)+(b7*catalogo),2))
nuevo_tree_caso_1




modelo = lm (Visitas_Web_Mensuales ~ Ingreso + Gasto_Carnes+Gasto_Vinos , data = superstore_data)
modelo
summary(modelo) #si el modelo tiene variables q no son relevantes, los resultados van a tener consecuencias y hay q eliminarlas







#ANALISIS COMPONENTES PRINCIPALES
library(factoextra)

superstore_data_acp <- superstore_data

superstore_data_acp <- superstore_data_acp %>%
  mutate(Respuesta = recode(Respuesta, "No" = 0, "Sí" = 1))

superstore_data_acp <- superstore_data_acp %>%
  mutate(Queja = recode(Queja, "No" = 0, "Sí" = 1))


superstore_data_numerico <- superstore_data_acp %>%
  select_if(is.numeric)

datosacp <- superstore_data_numerico %>%
  select(-Id)

datosacp <- scale(datosacp)
datosacp <- as.data.frame(datosacp)


ACP1<- prcomp(datosacp)
ACP1
summary(ACP1)

fviz_eig(ACP1, addlabels = TRUE, ylim = c(0, 50), ncp = 16, xlab="Dimensiones", ylab="Porcentaje de variaciones explicadas")


fviz_pca_var(ACP1, 
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 2)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC2(10,9%)"
)

fviz_pca_var(ACP1, 
             axes = c(1, 3),    
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 3)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC3(7,5%)"
)


fviz_pca_var(ACP1, 
             axes = c(1, 4),    
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 4)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC4(6,3%)"
)

fviz_pca_var(ACP1, 
             axes = c(1, 5),    
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 5)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC5(5,6%)"
)

fviz_pca_var(ACP1, 
             axes = c(1, 6),    
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 6)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC6(4,9%)"
)

fviz_pca_var(ACP1, 
             axes = c(1, 7),    
             col.var = "contrib",     # Usar la contribución como color
             gradient.cols = c("red", "yellow", "green"), 
             repel = TRUE,            # Evitar que las etiquetas se sobrepongan
             labelsize = 5,           # Tamaño del texto de las etiquetas
             title = "Gráfico de las Variables del PCA (1 y 7)",  # Añadir título
             alpha.var = 0.7 ,         # Ajustar transparencia de las flechas
             xlab="PC1(34,9%)",
             ylab="PC7(4,3%)"
)





