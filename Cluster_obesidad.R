
# Establecemos la ruta de trabajo

setwd("C:/Users/Rihard/Documents/GitHub/Cluster_obesity")


# Cargar librerías
library(tidyverse)
library(cluster)
library(factoextra)

# Leer archivo
# Fuente: https://www.kaggle.com/datasets/aravindpcoder/obesity-or-cvd-risk-classifyregressorcluster?resource=download
data <- read.csv("ObesityDataSet.csv")

# Eliminar la variable objetivo (etiqueta)
data <- data %>% select(-NObeyesdad)

# Convertir variables categóricas a factores y luego a numéricas
data_clean <- data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric))

# Escalar datos
data_scaled <- scale(data_clean)
colSums(is.na(data_scaled))

# Elegir número óptimo de clusters (elbow method) 5
fviz_nbclust(data_scaled, kmeans, method = "wss")

# wssplot <- function(data_scaled, nc=15, seed=2345){
#   wss <- (nrow(data_scaled)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data_scaled, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
# 
# wssplot(data_scaled, nc=10)


# Ejecutar K-means con 5 clustwers
set.seed(123)
km <- kmeans(data_scaled, centers = 5, nstart = 25)

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


