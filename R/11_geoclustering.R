# invocamos las librerias
pacman::p_load(sf, tidyverse, spdep)
options(scipen = 999)

# cargamos los datos 
mz_lc = read_rds("data/MBHT_LC.rds") |> 
  drop_na() 

# extraemos las variables numericas que vamos a estudiar
lc_vars = mz_lc |> 
  dplyr::select(ibt:E65YMAS) |> 
  drop_na() |> 
  st_drop_geometry() |> 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

# exploramos la distribucion de las variables
hist(lc_vars$dim_acc)
hist(lc_vars$dim_amb)
hist(lc_vars$dim_seg)
hist(lc_vars$dim_soc)

# usamos el metodo mas sencillo, kmeans, para agrupar los condados en 10 clusters segun las variables
clusters_k = kmeans(lc_vars, 10)

# creamos la variable cluster en el conjunto nc
mz_lc$cluster_kmeans = as.factor(clusters_k$cluster)

# visualizamos
ggplot(mz_lc) +
  geom_sf(aes(fill = cluster_kmeans))

clusters_k$betweenss / mean(dist(lc_vars))


# intentemoslo con clustering jerarquico

# calculamos distancia de atributos 
dist_attr = dist(lc_vars)

# generamos modelo jerarquico
hclust = hclust(dist_attr)

plot(hclust)

# cortamos modelo en 10 clusters
clusters_hier = factor(cutree(hclust, k = 10))

ggplot(mz_lc) +
  geom_sf(aes(fill = clusters_hier)) +
  scale_fill_viridis_d()

# tampoco se ve mucha informacion ya que estan contiguos

# ahora hagamos un cluster con vecindad

# calculamos la matriz de vecindades
matriz_vecindad  = spdep::nb2listw(neighbours = spdep::knn2nb(
  knn = spdep::knearneigh( x = st_centroid(mz_lc), k = 8, longlat = F)), 
  style = "W")

# viene como una lista, asi que la transformamos en una matriz
matriz_vec = map(1:nrow(mz_lc), function(i) as.numeric(1:nrow(mz_lc) %in% matriz_vecindad$neighbours[[i]])) |> 
  unlist() |> 
  matrix(nrow=nrow(mz_lc))

# la matriz de atributos tambien viene como un objeto de distancia, la pasamos a matriz
matriz_atr = dist_attr |> as.matrix()

# ahora construimos una matriz de vecindad-atributo
# donde la matriz de vecindad es 0 (no vecino), le asignamos un valor muy grande (10000), de lo contrario, 
# mantiene el valor de distanci a de atributos
matriz_atr_vec = (1-matriz_vec)*10000 + matriz_vec*matriz_atr

# pasamos la matriz a objeto distancia (lo que recibe el hclust)
dist_atr_vec = matriz_atr_vec |> as.dist()

# ejecutamos el hclust con esta matriz de vecindad-atributo
hclust2 = hclust(dist_atr_vec, method= "average")

plot(hclust2)

# tiene una distribucion extraña, ya que utiliza complete linkage

# veamos como queda el corte con 10 clusters
clusters_hier2 = factor(cutree(hclust2, k = 10))

ggplot(mz_lc) +
  geom_sf(aes(fill = clusters_hier2))

