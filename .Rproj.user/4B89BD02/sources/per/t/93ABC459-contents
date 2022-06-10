# invocamos las librerias
pacman::p_load(sf, tidyverse, spdep)
options(scipen = 999)

# cargamos los datos de prueba que vienen en la libreria sf
nc <- st_read(system.file("shape/nc.shp", package="sf"))

# extraemos las variables numericas que vamos a estudiar
# tasa de natalidad
# tasa de natalidad no caucasica
# tasa de mortalidad al nacer (sudden infant death)
nc_vars <- nc %>% select(BIR74, NWBIR74, SID74) %>% st_drop_geometry()

# exploramos la distribucion de las variables
hist(nc_vars$BIR74)
hist(nc_vars$NWBIR74)
hist(nc_vars$SID74)

# usamos el metodo mas sencillo, kmeans, para agrupar los condados en 10 clusters segun las variables
clusters_k <- kmeans(nc_vars, 10)

# creamos la variable cluster en el conjunto nc
nc$cluster_kmeans <- as.factor(clusters_k$cluster)

# visualizamos
ggplot(nc) +
  geom_sf(aes(fill = cluster_kmeans))

clusters_k$betweenss / mean(dist(nc_vars))

# no se ve mucha informacion
# puede ser que la escala este afectando el analisis

# exploramos la escala de las variables
summary(nc_vars)

# vemos que algunas van hasta 21588 y otras hasta 44
# esto afecta en el calculo de las distancias de atributos

# repetimos el proceso escalando las variables como x-mu/sd
scaled_vars <- scale(nc_vars)

summary(scaled_vars)

# volvemos a clusterizar con las variables escaladas
clusters_k_sc <- kmeans(scaled_vars, 10)

nc$cluster_kmeans_sc <- as.factor(clusters_k_sc$cluster)

ggplot(nc) +
  geom_sf(aes(fill = cluster_kmeans_sc))

clusters_k_sc$betweenss / mean(dist(scaled_vars))


# aun se ve ruidoso
# analizamos las correlaciones entre las variables
cor(nc_vars)
# es esperable el # de muertes correlaciones con los nacimientos


# creamos la variable de tasas de muerte por tipo de natalidad
nc_vars_rate <- 
  nc_vars %>% 
  mutate(SID_BIR = SID74 / BIR74,
         SID_NW = SID74 / NWBIR74) 

# volvemos a analizar las correlaciones
cor(nc_vars_rate)

# seleccionamos las variables sin correlacion relevante
nc_vars_rate <- 
  nc_vars_rate %>% 
  select(BIR74, SID_BIR, SID_NW)

cor(nc_vars_rate)

# vemos que la escala sigue siendo relevante
summary(nc_vars_rate)

# escalamos las variables nuevas
scaled_vars_rate <- scale(nc_vars_rate)

summary(scaled_vars_rate)

cor(scaled_vars_rate)

# volvemos a calcular el kmeans con datos transformados
clusters_k_sc_rate <- kmeans(scaled_vars_rate, 10)

nc$cluster_kmeans_sc_rate <- as.factor(clusters_k_sc_rate$cluster)

ggplot(nc) +
  geom_sf(aes(fill = cluster_kmeans_sc_rate))

clusters_k_sc_rate$betweenss / mean(dist(scaled_vars_rate))

# intentemoslo con clustering jerarquico

# calculamos distancia de atributos en base a las variables escaladas
dist_sc_rate <- dist(scaled_vars_rate)

# generamos modelo jerarquico
hclust <- hclust(dist_sc_rate)

plot(hclust)

# cortamos modelo en 10 clusters
clusters_hier <- factor(cutree(hclust, k = 10))

ggplot(nc) +
  geom_sf(aes(fill = clusters_hier))

# tampoco se ve mucha informacion ya que estan contiguos

# ahora hagamos un cluster con vecindad

# calculamos la matriz de vecindades
matriz_vecindad <- poly2nb(nc)

# viene como una lista, asi que la transformamos en una matriz
matriz_vec <- map(1:nrow(nc), function(i) as.numeric(1:nrow(nc) %in% matriz_vecindad[[i]])) %>% 
  unlist() %>% 
  matrix(nrow=nrow(nc))

# la matriz de atributos tambien viene como un objeto de distancia, la pasamos a matriz
matriz_atr <- dist_sc_rate %>% as.matrix()

# ahora construimos una matriz de vecindad-atributo
# donde la matriz de vecindad es 0 (no vecino), le asignamos un valor muy grande (10000), de lo contrario, 
# mantiene el valor de distancia de atributos
matriz_atr_vec <- (1-matriz_vec)*10000 + matriz_vec*matriz_atr

# pasamos la matriz a objeto distancia (lo que recibe el hclust)
dist_atr_vec <- matriz_atr_vec %>% as.dist()

# ejecutamos el hclust con esta matriz de vecindad-atributo
hclust2 <- hclust(dist_atr_vec)

plot(hclust2)

# tiene una distribucion extraña, ya que utiliza complete linkage

# veamos como queda el corte con 10 clusters
clusters_hier2 <- factor(cutree(hclust2, k = 10))

ggplot(nc) +
  geom_sf(aes(fill = clusters_hier2))

# probamos otro metodo de distancia jerarquica
hclust3 <- hclust(dist_atr_vec, method = "average")

plot(hclust3)

# el arbol esta mas distribuido
clusters_hier3 <- factor(cutree(hclust3, k = 10))

ggplot(nc) +
  geom_sf(aes(fill = clusters_hier3))

# probamos otro metodo de distancia jerarquica
hclust4 <- hclust(dist_atr_vec, method = "single")

plot(hclust4)

clusters_hier4 <- factor(cutree(hclust4, k = 10))

ggplot(nc) +
  geom_sf(aes(fill = clusters_hier4))

# probamos un metodo mas de distancia jerarquica
hclust5 <- hclust(dist_atr_vec, method = "ward.D")

plot(hclust5)

clusters_hier5 <- factor(cutree(hclust5, k = 10))

ggplot(nc) +
  geom_sf(aes(fill = clusters_hier5))



## veamos la animacion

for(i in 1:100) {
  nc <- nc %>% mutate(k = factor(cutree(hclust5, k = 101-i)))
  
  ggplot(nc) +
    geom_sf(aes(fill = k)) +
    theme(legend.position = "none")
  
  ggsave(paste0("Figuras/cluster",i,".png"))
}


filenames <- paste0("Figuras/cluster",1:100,".png")
m <- magick::image_read(filenames[1])
for (i in 2:100) {
  m <- c(m, magick::image_read(filenames[i]))
}
m <- magick::image_animate(m, fps = 10, loop = 1, dispose = "previous")
magick::image_write(m, "Figuras/movie.gif")


