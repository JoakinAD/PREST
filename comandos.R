# Una vez escogido el filepath del archivo mediante file_path <- "/path/to/your/PREST-2324-DatosTrabajo.xlsx", en mi caso:
file_path <- "~/Documents/3/2/PREST/PREST-2324-DatosTrabajo.xlsx"

# Cargar la librería readxl
library(readxl)

#EJERCICIO 1

datos1 <- read_excel(file_path, sheet = 1, skip = 8)
datos1_total <- datos1[, 1:4]
colnames(datos1_total) <- c("Edad", "Galicia", "Madrid", "Murcia")
datos1_mujeres <- datos1[, 6:9]
colnames(datos1_mujeres) <- c("Edad", "Galicia", "Madrid", "Murcia")
datos1_hombres <- datos1[, 11:14]
colnames(datos1_hombres) <- c("Edad", "Galicia", "Madrid", "Murcia")

#APARTADO A)
# Calcular la media para cada comunidad
media_galicia <- mean(datos1_total$Galicia)
media_madrid <- mean(datos1_total$Madrid)
media_murcia <- mean(datos1_total$Murcia)

# Calcular la desviación estándar para cada comunidad
sd_galicia <- sd(datos1_total$Galicia)
sd_madrid <- sd(datos1_total$Madrid)
sd_murcia <- sd(datos1_total$Murcia)

# Crear un data frame para visualizar mejor los resultados
resultados_total <- data.frame(
  Comunidad = c("Galicia", "Madrid", "Murcia"),
  Media_Edad = c(media_galicia, media_madrid, media_murcia),
  SD_Edad = c(sd_galicia, sd_madrid, sd_murcia)
)

# Imprimir los resultados
print(resultados_total)

#APARTADO B)
# Calcular cuartiles para cada comunidad
cuartiles_galicia <- quantile(datos1_total$Galicia, probs = c(0.25, 0.5, 0.75))
cuartiles_madrid <- quantile(datos1_total$Madrid, probs = c(0.25, 0.5, 0.75))
cuartiles_murcia <- quantile(datos1_total$Murcia, probs = c(0.25, 0.5, 0.75))

# Crear un data frame para visualizar mejor los cuartiles
resultados_cuartiles <- data.frame(
  Comunidad = rep(c("Galicia", "Madrid", "Murcia"), each = 3),
  Cuartil = rep(c("Q1", "Q2", "Q3"), 3),
  Valor = c(cuartiles_galicia, cuartiles_madrid, cuartiles_murcia)
)

# Imprimir los cuartiles
print(resultados_cuartiles)
# Calcular el rango intercuartílico (IQR) y definir límites para detectar valores atípicos en cada comunidad
IQR_galicia <- IQR(datos1_total$Galicia)
limite_inferior_galicia <- cuartiles_galicia[1] - 1.5 * IQR_galicia
limite_superior_galicia <- cuartiles_galicia[3] + 1.5 * IQR_galicia
valores_atipicos_galicia <- datos1_total$Galicia[datos1_total$Galicia < limite_inferior_galicia | datos1_total$Galicia > limite_superior_galicia]

IQR_madrid <- IQR(datos1_total$Madrid)
limite_inferior_madrid <- cuartiles_madrid[1] - 1.5 * IQR_madrid
limite_superior_madrid <- cuartiles_madrid[3] + 1.5 * IQR_madrid
valores_atipicos_madrid <- datos1_total$Madrid[datos1_total$Madrid < limite_inferior_madrid | datos1_total$Madrid > limite_superior_madrid]

IQR_murcia <- IQR(datos1_total$Murcia)
limite_inferior_murcia <- cuartiles_murcia[1] - 1.5 * IQR_murcia
limite_superior_murcia <- cuartiles_murcia[3] + 1.5 * IQR_murcia
valores_atipicos_murcia <- datos1_total$Murcia[datos1_total$Murcia < limite_inferior_murcia | datos1_total$Murcia > limite_superior_murcia]

# Mostrar valores atípicos
print("Valores atípicos en Galicia:")
print(valores_atipicos_galicia) #Se muestra numeric(0) en el resultado ya que no hay valores atípicos
print("Valores atípicos en Madrid:")
print(valores_atipicos_madrid) #Se muestra numeric(0) en el resultado ya que no hay valores atípicos
print("Valores atípicos en Murcia:")
print(valores_atipicos_murcia) #Se muestra numeric(0) en el resultado ya que no hay valores atípicos

#APARTADO C)
# Configurar el layout para mostrar 3 gráficos en una sola fila
par(mfrow = c(1, 3))

# Histograma para Galicia
hist(datos1_total$Galicia,
     main = "Histograma para Galicia",
     xlab = "Valores",
     col = "lightblue",
     breaks = 10)  # Define el número de barras en el histograma

# Histograma para Madrid
hist(datos1_total$Madrid,
     main = "Histograma para Madrid",
     xlab = "Valores",
     col = "lightgreen",
     breaks = 10)

# Histograma para Murcia
hist(datos1_total$Murcia,
     main = "Histograma para Murcia",
     xlab = "Valores",
     col = "lightcoral",
     breaks = 10)

# Barplot para Galicia
barplot(datos1_total$Galicia,
        names.arg = datos1_total$Edad,
        main = "Nacimientos en Galicia por Edad",
        xlab = "Edad",
        ylab = "Número de Nacimientos",
        col = "lightblue",
        las = 1,
        space = 0)

# Barplot para Madrid
barplot(datos1_total$Madrid,
        names.arg = datos1_total$Edad,
        main = "Nacimientos en Madrid por Edad",
        xlab = "Edad",
        ylab = "Número de Nacimientos",
        col = "lightgreen",
        las = 1,
        space = 0)

# Barplot para Murcia
barplot(datos1_total$Murcia,
        names.arg = datos1_total$Edad,
        main = "Nacimientos en Murcia por Edad",
        xlab = "Edad",
        ylab = "Número de Nacimientos",
        col = "lightcoral",
        las = 1,
        space = 0)

#APARTADO D)

# Restablecer la configuración gráfica a su valor predeterminado
par(mfrow = c(1, 1))

# Combina los datos en un nuevo data frame para un boxplot combinado
datos_combinados <- data.frame(Galicia = datos1_total$Galicia, Madrid = datos1_total$Madrid, Murcia = datos1_total$Murcia)

# Crear un boxplot combinado
boxplot(datos_combinados, 
        main = "Boxplot por Comunidad", 
        ylab = "Numero de nacimientos", 
        col = c("lightblue", "lightgreen", "lightcoral"),
        las = 1,   # Orienta las etiquetas del eje x horizontalmente
        outline = TRUE,  # Muestra valores atípicos
        names = c("Galicia", "Madrid", "Murcia"))


#APARTADO E)

# Sumar los nacimientos en cada comunidad
total_galicia <- sum(datos1_total$Galicia)
total_madrid <- sum(datos1_total$Madrid)
total_murcia <- sum(datos1_total$Murcia)

# Crear un vector con los totales
totales <- c(Galicia = total_galicia, Madrid = total_madrid, Murcia = total_murcia)

# Crear el diagrama de sectores
pie(totales,
    main = "Contribución de Cada Comunidad al Total de Nacimientos",
    col = c("lightblue", "lightgreen", "lightcoral"),
    labels = paste(names(totales), ": ", round(100 * totales/sum(totales), 1), "%"))


#APARTADO D MUJERES)
datos_combinados <- data.frame(Galicia = datos1_mujeres$Galicia, Madrid = datos1_mujeres$Madrid, Murcia = datos1_mujeres$Murcia)

# Crear un boxplot combinado
boxplot(datos_combinados, 
        main = "Boxplot por Comunidad - Mujeres", 
        ylab = "Valores", 
        col = c("lightblue", "lightgreen", "lightcoral"),
        las = 1,   # Orienta las etiquetas del eje x horizontalmente
        outline = TRUE,  # Muestra valores atípicos
        names = c("Galicia", "Madrid", "Murcia"))
#APARTADO E MUJERES)
# Sumar los nacimientos en cada comunidad
total_galicia <- sum(datos1_mujeres$Galicia)
total_madrid <- sum(datos1_mujeres$Madrid)
total_murcia <- sum(datos1_mujeres$Murcia)

# Crear un vector con los totales
totales <- c(Galicia = total_galicia, Madrid = total_madrid, Murcia = total_murcia)

# Crear el diagrama de sectores
pie(totales,
    main = "Contribución de Cada Comunidad al Total de Nacimientos - Mujeres",
    col = c("lightblue", "lightgreen", "lightcoral"),
    labels = paste(names(totales), ": ", round(100 * totales/sum(totales), 1), "%"))
#APARTADO D HOMBRES)
datos_combinados <- data.frame(Galicia = datos1_hombres$Galicia, Madrid = datos1_hombres$Madrid, Murcia = datos1_hombres$Murcia)

# Crear un boxplot combinado
boxplot(datos_combinados, 
        main = "Boxplot por Comunidad - Hombres", 
        ylab = "Valores", 
        col = c("lightblue", "lightgreen", "lightcoral"),
        las = 1,   # Orienta las etiquetas del eje x horizontalmente
        outline = TRUE,  # Muestra valores atípicos
        names = c("Galicia", "Madrid", "Murcia"))
#APARTADO E HOMBRES)

# Sumar los nacimientos en cada comunidad
total_galicia <- sum(datos1_hombres$Galicia)
total_madrid <- sum(datos1_hombres$Madrid)
total_murcia <- sum(datos1_hombres$Murcia)

# Crear un vector con los totales
totales <- c(Galicia = total_galicia, Madrid = total_madrid, Murcia = total_murcia)

# Crear el diagrama de sectores
pie(totales,
    main = "Contribución de Cada Comunidad al Total de Nacimientos - Hombres",
    col = c("lightblue", "lightgreen", "lightcoral"),
    labels = paste(names(totales), ": ", round(100 * totales/sum(totales), 1), "%"))



#EJERCICIO 2
#Cargamos la segunda pestaña del archivo
datos2 <- read_excel(file_path, sheet = 2)

#APARTADO A)
# Usamos la función plot() para crear el gráfico
plot(datos2$Concentración, datos2$Absorbencia, main="Diagrama de Dispersión",
     xlab="Concentración", ylab="Absorbencia", pch=19, col='blue')

# Ajustamos el modelo lineal
modelo <- lm(Absorbencia ~ Concentración, data=datos2)

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

#APARTADO B)
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
hist(residuos, breaks = 5, main = "Histograma de Residuos", xlab = "Residuos", freq = FALSE, col = "lightblue")
# Añadir una curva normal al histograma
curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)), add = TRUE, col = "red")

# Calculamos la media de los residuos
media_residuos <- mean(residuos)

# Calculamos la desviación estándar de los residuos
desviacion_std_residuos <- sd(residuos)

# Imprimimos los resultados
print(paste("Media de los residuos:", media_residuos))
print(paste("Desviación estándar de los residuos:", desviacion_std_residuos))

#APARTADO D)
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


#EJERCICIO 3
#Cargamos la segunda pestaña del archivo
datos3 <- read_excel(file_path, sheet = 3)

#APARTADO A)
# Seleccionamos todos los datos de la tabla base con Sexo = 0 (femenino)
datos_mujer <- subset(datos3, Sexo==0)

# Realizamos la suma y división
apartado_1 <- sum(datos_mujer$Altura > 170) / nrow(datos_mujer)

# Resultado
print(paste("La probabilidad de que la altura sea mayor que 170 cm es de ", apartado_1, " en el grupo femenino"))

#APARTADO B)
# Primero seleccionamos todos los datos de P(B), y dividimos su cantidad entre el n total
datos170_30 <- subset(datos3, Altura > 170 & Edad < 30)
probabilidad_170y30 <- nrow(datos170_30)/nrow(datos3)

# Similarmente, seleccionamos los datos de P(A∩B) y dividimos
datosM_170_30 <- subset(datos3, Altura > 170 & Edad < 30 & Sexo == 0)
probabilidad_M_170_30 <- nrow(datosM_170_30)/nrow(datos3)

# Finalmente, dividimos P(A∩B) entre P(B)
apartado_2 <- probabilidad_M_170_30/probabilidad_170y30

# Resultado
print(paste("La probabilidad de que, dada una persona mayor de 170 cm y menor de 30 años, sea del grupo femenino es de: ", apartado_2))

#APARTADO C)
# Tanto la media como la desviación típica se pueden calcular con una sola función, aplicada a la tabla de datos
datos_alt_muj <- as.numeric(datos_mujer$Altura)
apartado_3m <- mean(datos_alt_muj)
apartado_3sd <- sd(datos_alt_muj)

# Resultado
print(paste("Asumiendo una distribución normal, se tiene que para la altura del grupo femenino μ=",apartado_3m," y σ=",apartado_3sd))

# Crear el histograma
hist(datos_alt_muj, main = "Distribución de valores de altura en grupo femenino", xlab="Altura (cm)", ylab="Densidad", ylim = c(0, 0.06) ,freq = FALSE, col = "lightblue")

# Añadir la curva normal
curve(dnorm(x, mean = mean(datos_alt_muj), sd = sd(datos_alt_muj)), add = TRUE, col = "red", lwd = 2)

#APARTADO D)
# Calculamos la probabilidad acumulada
prob_acumulada <- pnorm(170, mean=apartado_3m, sd=apartado_3sd)

# Luego restamos esto a 1 para obtener nuestra probabilidad deseada
apartado_4 <- 1-prob_acumulada

# Resultado:
print(paste("La probabilidad de que la altura supere 170 cm en este grupo es de: ",apartado_4))

#APARTADO E)
# Primero se calcula p
probsPeso_90 <- sum(datos3$Peso > 90)/nrow(datos3)

# Seguido calculamos la probabilidad de 0 y 1 personas usando la funcion pbinom
prob_0_a_1 <- pbinom(1,200,prob=probsPeso_90)

# Por ultimo restamos esto al total para obtener la probabilidad deseada
apartado_5 <- 1-prob_0_a_1

# Resultado
print(paste("La probabilidad de que 2 o mas personas pesen lo indicado de entre las 200 es igual a: ", apartado_5))


