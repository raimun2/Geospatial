# Cargar Librerias
pacman::p_load(tidyverse, sf, MASS, gstat, raster, 
               spdep, spatialreg, patchwork)


# manzanas las condes
mz_lc = read_rds("data/MBHT_LC.rds") 

# generamos lista de vecinos
nb = poly2nb(mz_lc, queen=TRUE)

# generamos pesos a partir de los vecinos
lw = nb2listw(nb, style="W", zero.policy=TRUE)

plot(lw, coords = st_coordinates(st_centroid(mz_lc)), col='red')

# resolvermos de 3 maneras: radio, buffer y knn

# buffer
poly_buff = 
  mz_lc |> 
  st_buffer(dist=50) |> 
  st_cast("MULTIPOLYGON")

buff_nb = mat2listw(st_overlaps(poly_buff, sparse=FALSE))

plot(buff_nb, coords = st_coordinates(st_centroid(mz_lc)), col='red')


# knn
nvec = 12
knn_nb = spdep::nb2listw(neighbours = spdep::knn2nb(
  knn = spdep::knearneigh( x = st_centroid(mz_lc), k = nvec, longlat = F)),
  style = "W")

plot(knn_nb, coords = st_coordinates(st_centroid(mz_lc)), col='red')

# radio
radius = 500
# calcula matriz de distancias
dist_mat = 
  st_distance(st_centroid(mz_lc), st_centroid(mz_lc)) 

# calcula el inverso y asigna 0 en la diagonal
distancias.inv = 1/dist_mat
diag(distancias.inv) = 0

# asigna 0 si distancia es mayor al radio
distancias.inv[as.numeric(dist_mat) > radius] = 0

# matriz de pesos
sp_w = matrix(as.numeric(distancias.inv), nrow =  nrow(distancias.inv))

# lista de pesos
rad_nb = mat2listw(sp_w)

plot(rad_nb, coords = st_coordinates(st_centroid(mz_lc)), col='red')

