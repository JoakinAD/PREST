# Una vez escogido el filepath del archivo mediante file_path <- "/path/to/your/PREST-2324-DatosTrabajo.xlsx", en mi caso:
file_path <- "~/Documents/3/2/PREST/PREST-2324-DatosTrabajo.xlsx"

# Cargar la librería readxl
library(readxl)

#Cargamos la segunda pestaña del archivo
datos <- read_excel(file_path, sheet = 2)

#APARTADO A
# Usamos la función plot() para crear el gráfico
plot(datos$Concentración, datos$Absorbencia, main="Diagrama de Dispersión",
     xlab="Concentración", ylab="Absorbencia", pch=19, col='blue')

# Ajustamos el modelo lineal
modelo <- lm(Absorbencia ~ Concentración, data=datos)

# Añadimos la recta de regresión al diagrama de dispersión
abline(modelo, col="red")

# Ver el resumen del modelo
resumen <- summary(modelo)
resumen

# Para extraer directamente los coeficientes
coeficientes <- coef(modelo)
intercepto <- coeficientes[1]
pendiente <- coeficientes[2]

# Imprimimos la ecuación de la recta
cat(sprintf("La ecuación de la recta de regresión es: Absorbencia = %.4f + %.4f * Concentración\n", intercepto, pendiente))

#APARTADO B
# Extraemos el valor de R cuadrado del resumen
r_cuadrado <- resumen$r.squared

# Imprimimos R cuadrado
print(paste("El valor de R^2 es:", r_cuadrado))

#APARTADO C
# Calculamos los residuos
residuos <- residuals(modelo)

# Visualizamos los primeros residuos
head(residuos)

# Creamos un histograma con 5 intervalos
hist(residuos, breaks = 5, main = "Histograma de Residuos", xlab = "Residuos", col = "cyan")

# Calculamos la media de los residuos
media_residuos <- mean(residuos)

# Calculamos la desviación estándar de los residuos
desviacion_std_residuos <- sd(residuos)

# Imprimimos los resultados
print(paste("Media de los residuos:", media_residuos))
print(paste("Desviación estándar de los residuos:", desviacion_std_residuos))

#APARTADO D
#Con la función predict()
# Realizamos una predicción para una concentración de 20
aux_data <- data.frame(Concentración = 20)
prediccion <- predict(modelo, newdata = aux_data)

# Mostramos la predicción
print(paste("La predicción de la absorbencia para una concentración de 20 es:", prediccion))

#Usando la ecuación de la recta de regresión
# Calcular la predicción manualmente para una concentración de 20
concentración <- 20
absorbencia_predicha <- intercepto + pendiente * concentración

# Mostrar la predicción
print(paste("La predicción de la absorbencia para una concentración de 20 es:", absorbencia_predicha))
