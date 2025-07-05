# Instalar paquetes necesarios
install.packages("ggplot2")
install.packages("dplyr")

# Cargar librerías
library(ggplot2)
library(dplyr)

# cargamos train desde el environment y la copiamos a una variable data
data <- train


# Estructura del dataset
str(data) # parecido al info() de python

# Resumen de variables
summary(data) # parecido al describe() de python


# Promedios y medianas
mean(data$Age, na.rm = TRUE)
median(data$Age, na.rm = TRUE)


# Contar supervivientes
table(data$Survived) # parecido al value_counts() de python


# Proporción de supervivientes
prop.table(table(data$Survived))


# Gráfico de barras de supervivencia por sexo
ggplot(data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(fill = "Sobrevivió") +
  theme_minimal()

# Histograma de edades
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal()


# Boxplot de tarifas (Fare) por clase de pasajero
ggplot(data, aes(x = factor(Pclass), y = Fare)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(x = "Clase", y = "Tarifa")


# grafico de densidad
ggplot(data, aes(x = Age, fill = factor(Survived))) +
  geom_density(alpha = 0.5) +
  labs(fill = "Sobrevivió") +
  theme_minimal()


# Grafico de dispersion
ggplot(data, aes(x = Age, y = Fare, color = factor(Survived))) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(color = "Sobrevivió")



install.packages("plotly")
library(plotly)


# boxplot utlilizando plotly
p <- ggplot(data, aes(x = factor(Pclass), y = Fare, color = factor(Survived))) +
  geom_boxplot() +
  theme_minimal()

ggplotly(p)  # lo vuelve interactivo


# mosaicoplot 
mosaicplot(table(data$Sex, data$Survived),
           main = "Supervivencia según sexo",
           color = TRUE)


# Esta libreria crea un reporte estadistico del dataset
install.packages("DataExplorer")
library(DataExplorer)
create_report(data)


# Mapa de correlacion
install.packages("corrplot")
library(corrplot)
# Seleccionar solo columnas numéricas
numericas <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numericas, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black",
         number.cex = 0.7)



# verificar datos nulos por columna
colSums(is.na(data))

# Reemplazar NA de Age con su media
data$Age[is.na(data$Age)] <- mean(data$Age, na.rm = TRUE)

# Reemplazar NA de Cabin
data$Cabin[is.na(data$Cabin)] <- "Sin información"

# Reemplazar NA de Embarked
data$Embarked[is.na(data$Embarked)] <- "Sin información"


# Verificar nuevamente
colSums(is.na(data))


# masculino = 0, femenino = 1
data$Sex <- ifelse(data$Sex == "male", 0, 1)

data$Embarked <- ifelse(data$Embarked == "C", 1,
                           ifelse(data$Embarked == "Q", 2,
                                  ifelse(data$Embarked == "S", 3, 0)))

data$Cabin <- ifelse(data$Cabin == "Sin información", 0, 1)

# revisar el resultado
str(data)


# eliminar columnas innecesarias
data <- data[, !(names(data) %in% c("PassengerId", "Name", "Ticket"))]

# revisar resultado
str(data)


# en clasificacion en R se recomienda transformar la columna objetivo a FACTOR
data$Survived <- as.factor(data$Survived)



# MACHINE LEARNING 

# ARBOL DE DECISION
install.packages("rpart")
install.packages("rpart.plot")  # para graficar el árbol

library(rpart)
library(rpart.plot)


#DIVIDIR LOS DATOS EN TRAIN Y TEST
set.seed(123)  # para reproducibilidad
indices <- sample(1:nrow(data), size = 0.7 * nrow(data))

train_data <- data[indices, ]
test_data  <- data[-indices, ]


arbol <- rpart(Survived ~ ., data = train_data, method = "class")
rpart.plot(arbol, extra = 106)
predicciones <- predict(arbol, newdata = test_data, type = "class")
table(Predicho = predicciones, Real = test_data$Survived)
#ACCURACY
mean(predicciones == test_data$Survived)







