# Cargar las librerías necesarias
library(readxl)



# Cargar los datos desde un archivo Excel
data <- read_excel("/home/luis/Escritorio/PRESTvis/PREST-2324-DatosTrabajo.xlsx", sheet = 1, skip = 8)  # Ajusta la ruta y el número de fila desde donde deseas comenzar a leer

# Seleccionar y renombrar las columnas relevantes
data_clean <- data[, 1:4]
colnames(data_clean) <- c("Edad", "Galicia", "Madrid", "Murcia")

# Convertir las columnas a tipo numérico, omitiendo posibles valores no numéricos
data_clean$Galicia <- as.numeric(as.character(data_clean$Galicia))
data_clean$Madrid <- as.numeric(as.character(data_clean$Madrid))
data_clean$Murcia <- as.numeric(as.character(data_clean$Murcia))

# Estadística descriptiva: Medias y desviaciones estándar
summary_stats <- sapply(data_clean[, -1], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
print(summary_stats)

# Cuartiles
quartiles <- apply(data_clean[, -1], 2, quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
iqr <- quartiles[3, ] - quartiles[1, ]

# Histogramas para cada comunidad
par(mfrow = c(3, 1))  # Configura el layout para 3 filas de gráficos
hist(data_clean$Galicia, main = "Histograma de Nacimientos en Galicia", xlab = "Nacimientos", col = "blue")
hist(data_clean$Madrid, main = "Histograma de Nacimientos en Madrid", xlab = "Nacimientos", col = "red")
hist(data_clean$Murcia, main = "Histograma de Nacimientos en Murcia", xlab = "Nacimientos", col = "green")
#tiene que dividirse, esta apareciendo en el mismo plot 
# Diagrama de cajas
boxplot(data_clean$Galicia, data_clean$Madrid, data_clean$Murcia, names = c("Galicia", "Madrid", "Murcia"), main = "Comparación de Nacimientos por Comunidad")

# Diagrama de sectores para mostrar la contribución de cada comunidad
total_nacimientos <- colSums(data_clean[, -1], na.rm = TRUE)
pie(total_nacimientos, labels = names(total_nacimientos), main = "Contribución de Cada Comunidad al Total de Nacimientos en 2022", col = c("blue", "red", "green"))


#analisis de c y d con hombre y mujer

data <- read_excel("/home/luis/Escritorio/PRESTvis/PREST-2324-DatosTrabajo.xlsx", sheet = 1, skip = 8) 

# Supongamos que las columnas están organizadas como sigue: Edades, Galicia Mujeres, Madrid Mujeres, Murcia Mujeres, Galicia Hombres, Madrid Hombres, Murcia Hombres
data_clean_mujeres <- data[, c(1, 7:9)]
data_clean_hombres <- data[, c(1, 12:14)]
colnames(data_clean_mujeres) <- c("Edad", "Galicia Mujeres", "Madrid Mujeres", "Murcia Mujeres")
colnames(data_clean_hombres) <- c("Edad","Galicia Hombres", "Madrid Hombres", "Murcia Hombres")

# Convertir las columnas a tipo numérico
data_clean_mujeres[,-1] <- sapply(data_clean_mujeres[,-1], function(x) as.numeric(as.character(x)))
data_clean_hombres[,-1] <- sapply(data_clean_hombres[,-1], function(x) as.numeric(as.character(x)))

# Diagramas de cajas para hombres y mujeres por comunidad
boxplot(data_clean_mujeres[,c("Galicia Mujeres", "Madrid Mujeres", "Murcia Mujeres")], 
        main = "Nacimientos de Mujeres por Comunidad",
        xlab = "Comunidad", ylab = "Nacimientos", col = c("pink", "red", "orange"), names = c("Galicia", "Madrid", "Murcia"))

boxplot(data_clean_hombres[,c("Galicia Hombres", "Madrid Hombres", "Murcia Hombres")], 
        main = "Nacimientos de Hombres por Comunidad",
        xlab = "Comunidad", ylab = "Nacimientos", col = c("lightblue", "blue", "cyan"), names = c("Galicia", "Madrid", "Murcia"))

# Calculando el total de nacimientos por sexo y comunidad
total_mujeres <- colSums(data_clean_mujeres[, c("Galicia Mujeres", "Madrid Mujeres", "Murcia Mujeres")], na.rm = TRUE)
total_hombres <- colSums(data_clean_hombres[, c("Galicia Hombres", "Madrid Hombres", "Murcia Hombres")], na.rm = TRUE)

# Diagramas de sectores para mostrar la contribución por sexo y comunidad
par(mfrow = c(1, 2))
pie(total_mujeres, labels = names(total_mujeres), main = "Contribución de Mujeres al Total de Nacimientos", col = c("pink", "red", "orange"))
pie(total_hombres, labels = names(total_hombres), main = "Contribución de Hombres al Total de Nacimientos", col = c("lightblue", "blue", "cyan"))

