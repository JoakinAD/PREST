# Cargar las librerías necesarias
library(readxl)

# Le datos desde el archivo Excel
nacimientos <- read_excel("/home/luis/Escritorio/PRESTvis/PREST-2324-DatosTrabajo.xlsx", sheet = 1, skip = 8) 
nacimientos_totales <- nacimientos[,1:4]


#nombra columnas
names(nacimientos_totales) <- c("Edad", "Galicia", "Madrid", "Murcia")

#(a) Calcular medias y (cuasi)desviaciones tı́picas muestrales
#(b) Calcular cuartiles, y detectar valores atı́picos, si los hay
stats_galicia <- summary(nacimientos_totales$Galicia)
stats_madrid <- summary(nacimientos_totales$Madrid)
stats-murcia <- summary(nacimientos_totales$Murcia)

# Calcular IQR para detectar valores atípicos
iqr_galicia <- IQR(nacimientos_totales$Galicia)
iqr_madrid <- IQR(nacimientos_totales$Madrid)
iqr_murcia <- IQR(nacimientos_totales$Murcia)


# Histogramas para cada ciudad
hist(nacimientos_totales$Galicia, main="Nacimientos en Galicia", xlab="Nacimientos",ylab = , col="lightblue")
hist(nacimientos_totales$Madrid, main="Nacimientos en Madrid", xlab="Nacimientos", col="orange")
hist(nacimientos_totales$Murcia, main="Nacimientos en Murcia", xlab="Nacimientos", col="palegreen")

#(a) Calcular medias y (cuasi)desviaciones tı́picas muestrales

# Estadística descriptiva: Medias y desviaciones estándar
summary_stats <- sapply(data_clean$Galicia[, -1], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
print(summary_stats)

#(b) Calcular cuartiles, y detectar valores atı́picos, si los hay
quartiles <- apply(data_clean$Galicia[], 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
q1 <- quartiles[1, ]
q2 <- quartiles[2, ]
q3 <- quartiles[3, ]

iqr <- q3 - q1

# Definimos los límites para considerar un dato como atípico
lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

# Identificamos los valores atípicos
v_atipicos <- data_clean$Galicia[data_clean$Galicia < lim_inf | data_clean$Galicia > lim_sup]
print(v_atipicos)

#(c) Confeccionar los correspondientes histogramas.
# Histogramas para cada comunidad
par(mfrow = c(3, 1))  # Configura el layout para 3 filas de gráficos
hist(data_clean$Galicia, main = "Histograma de Nacimientos en Galicia", xlab = "Nacimientos", col = "blue")
hist(data_clean$Madrid, main = "Histograma de Nacimientos en Madrid", xlab = "Nacimientos", col = "red")
hist(data_clean$Murcia, main = "Histograma de Nacimientos en Murcia", xlab = "Nacimientos", col = "green")
#tiene que dividirse, esta apareciendo en el mismo plot 
par(mfrow = c(1,1))
# Diagrama de cajas
boxplot(data_clean$Galicia, data_clean$Madrid, data_clean$Murcia, names = c("Galicia", "Madrid", "Murcia"), main = "Comparación de Nacimientos por Comunidad", col = c("lightblue","orange","palegreen"), outline = TRUE, pch =19, col.out = c("blue","red","green"),cex.out = 1.5)

# Diagrama de sectores para mostrar la contribución de cada comunidad
total_nacimientos <- colSums(data_clean[, -1], na.rm = TRUE)
pie(total_nacimientos, labels = names(total_nacimientos), main = "Contribución de Cada Comunidad al Total de Nacimientos en 2022", col = c("blue", "red", "green"))


#analisis de c y d con hombre y mujer

data <- read_excel("/home/luis/Escritorio/PRESTvis/PREST-2324-DatosTrabajo.xlsx", sheet = 1, skip = 8) 

# Supongamos que las columnas están organizadas como sigue: Edades, Galicia, Madrid, Murcia, Galicia, Madrid, Murcia
data_clean_mujeres <- data[, c(1, 7:9)]
data_clean_hombres <- data[, c(1, 12:14)]
colnames(data_clean_mujeres) <- c("Edad", "Galicia", "Madrid", "Murcia")
colnames(data_clean_hombres) <- c("Edad","Galicia", "Madrid", "Murcia")

# Convertir las columnas a tipo numérico
data_clean_mujeres <- sapply(data_clean_mujeres, function(x) as.numeric(as.character(x)))
data_clean_hombres <- sapply(data_clean_hombres, function(x) as.numeric(as.character(x)))

# Diagramas de cajas para hombres y mujeres por comunidad
boxplot(data_clean_mujeres[,c("Galicia", "Madrid", "Murcia")], 
        main = "Nacimientos de Mujeres por Comunidad",
        xlab = "Comunidad", ylab = "Nacimientos", col = c("pink", "red", "orange"), names = c("Galicia", "Madrid", "Murcia"))

boxplot(data_clean_hombres[,c("Galicia", "Madrid", "Murcia")], 
        main = "Nacimientos de Hombres por Comunidad",
        xlab = "Comunidad", ylab = "Nacimientos", col = c("lightblue", "blue", "cyan"), names = c("Galicia", "Madrid", "Murcia"))

# Calculando el total de nacimientos por sexo y comunidad
total_mujeres <- colSums(data_clean_mujeres[, c("Galicia", "Madrid", "Murcia")], na.rm = TRUE)
total_hombres <- colSums(data_clean_hombres[, c("Galicia", "Madrid", "Murcia")], na.rm = TRUE)

#analisis maternidad en distintas comunidades

# Asumiendo que data_clean es el DataFrame con las columnas adecuadas
# Ejemplo de nombres de columnas: "Edad", "Galicia", "Madrid", "Murcia"

# Transformar frecuencias en listas de edades

edad_galicia <- rep(data_clean_mujeres[, "Edad"], times = data_clean_mujeres[, "Galicia"])
edad_madrid <- rep(data_clean_mujeres[, "Edad"], times = data_clean_mujeres[, "Madrid"])
edad_murcia <- rep(data_clean_mujeres[, "Edad"], times = data_clean_mujeres[, "Murcia"])


# Calcular estadísticas descriptivas
summary_galicia <- summary(edad_galicia)
summary_madrid <- summary(edad_madrid)
summary_murcia <- summary(edad_murcia)

# Visualización con histogramas
hist(edad_galicia, breaks = 30, main = "Distribución de Edad de Maternidad en Galicia", xlab = "Edad")
hist(edad_madrid, breaks = 30, main = "Distribución de Edad de Maternidad en Madrid", xlab = "Edad")
hist(edad_murcia, breaks = 30, main = "Distribución de Edad de Maternidad en Murcia", xlab = "Edad")

# Diagramas de cajas para comparar
boxplot(edad_galicia, edad_madrid, edad_murcia, names = c("Galicia", "Madrid", "Murcia"), main = "Comparación de Edad de Maternidad por Comunidad")

