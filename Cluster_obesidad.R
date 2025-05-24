
# Establecemos la ruta de trabajo

setwd("C:/Users/Rihard/Documents/GitHub/Cluster_obesity")

install.packages("caret")

# Cargar librerías
library(tidyverse)
library(cluster)
library(factoextra)
# Cargar paquetes necesarios
library(tidyverse)
library(caret)
library(FactoMineR)
library(factoextra)
library(recipes)
library(dplyr)


# Leer archivo
# Fuente: https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster?resource=download
data <- read.csv("ObesityDataSet.csv")

# Analisis exploratorio

table(data$Gender)
table(data$family_history_with_overweight)
table(data$FAVC)
table(data$CAEC)
table(data$SMOKE)
table(data$SCC)
table(data$CALC)
table(data$MTRANS)

# Eliminamos las 

data <- data %>% select(-MTRANS, -CAEC, -CALC, NObeyesdad)

# Ver primeras filas
head(data)
# Ver estructura
glimpse(data)

# Verificar valores faltantes
colSums(is.na(data))

# Estadísticas descriptivas
summary(data)


# Convertir variables categóricas a factores y luego a numéricas
# data_clean <- data %>%
#   mutate(across(where(is.character), as.factor)) %>%
#   mutate(across(where(is.factor), as.numeric))
# 

rec <- recipe(~., data = data %>% select(-NObeyesdad)) %>%
  step_dummy(all_nominal()) %>%      # convierte a dummies
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

data_clean <- bake(prep(rec), new_data = NULL)


# Escalar datos
data_scaled <- scale(data_clean)
colSums(is.na(data_scaled))

# Elegir número óptimo de clusters (elbow method) 5
fviz_nbclust(data_scaled, kmeans, method = "wss")

wssplot <- function(data_scaled, nc=15, seed=1234){
  wss <- (nrow(data_scaled)-1)*sum(apply(data_scaled,2,var))
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data_scaled, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(data_scaled, nc=10)


# Ejecutar K-means con 5 clustwers
set.seed(123)
km <- kmeans(data_scaled, centers = 3, nstart = 25)

# Visualizar clusters
fviz_cluster(km, data = data_scaled)

#clusplot(data_scaled, km$cluster, main='2D representation of the Cluster solution',
#         color=TRUE, shade=TRUE,
#         labels=5, lines=0)


# Añadir resultados al dataset original
data_clustered <- data %>% mutate(Cluster = factor(km$cluster))

# Ver primeros resultados
head(data_clustered)

# Calcular promedios por cluster (solo numéricas)
promedios <- data_clustered %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Mostrar tabla
print(promedios)




# Segunda parte

# Eliminar la variable objetivo
df_cluster <- data %>% select(-NObeyesdad)

# Crear receta para procesar los datos
rec <- recipe(~., data = df_cluster) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # codificar variables categóricas
  step_center(all_numeric()) %>%                  # centrar
  step_scale(all_numeric())                       # escalar

# Aplicar la receta
prep_rec <- prep(rec)
df_proc <- bake(prep_rec, new_data = NULL)

# Ver dimensiones del dataset procesado
dim(df_proc)


library(tidyverse)
library(recipes)

# Cargar los datos
df <- read_csv("ObesityDataSet.csv")

# Eliminar la variable objetivo
df_cluster <- df %>% select(-NObeyesdad)

# Crear una receta de preprocesamiento
rec <- recipe(~ ., data = df_cluster) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Convierte factores a dummies
  step_center(all_numeric()) %>%                  # Centra variables numéricas
  step_scale(all_numeric())                       # Escala variables numéricas

# Preparar la receta
rec_prep <- prep(rec)

# Aplicar la receta al conjunto de datos
df_proc <- bake(rec_prep, new_data = NULL)

# Verificar resultado
glimpse(df_proc)


prep()




library(tidyverse)
library(recipes)
library(factoextra)

# 1. Cargar datos
data <- read_csv("ObesityDataSet.csv")

# 2. Eliminar variable objetivo
data_cluster <- data %>% select(-NObeyesdad)

# 3. Crear receta de preprocesamiento
rec <- recipe(~ ., data = data_cluster) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%  # Dummies para todas las categóricas
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# 4. Preparar y aplicar receta
prep_rec <- prep(rec)
data_proc <- bake(prep_rec, new_data = NULL)

# 5. Determinar número óptimo de clusters (método del codo)
fviz_nbclust(data_proc, kmeans, method = "wss") +
  labs(title = "Número óptimo de clusters - Método del Codo")

# 6. Aplicar K-Means con k = 4 (puedes cambiar este valor según resultado anterior)
set.seed(123)
kmeans_result <- kmeans(data_proc, centers = 4, nstart = 25)

# 7. Visualizar clusters con PCA
fviz_cluster(kmeans_result, data = data_proc,
             palette = "jco",
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal(),
             main = "Clusters por K-Means")



data$cluster <- factor(kmeans_result$cluster)

# Calcular promedios por cluster (solo numéricas)
promedios <- data_clustered %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

# Mostrar tabla
print(promedios)











