pacman::p_load(sf, tidyverse, elevatr, terra)

poligonos <- read_rds("data/MBHT_LC.rds")

# extraemos la geometria 
LC_pol <- st_geometry(poligonos)
class(LC_pol)

# funciones de validacion
st_is_valid(LC_pol)
st_is_simple(LC_pol)

# generamos subconjunto para ejemplos 
LC_sub <- LC_pol[1:8]
plot(LC_sub, col = 1:8)

# transformamos en lineas 
LC_lin <- st_cast(LC_sub,"LINESTRING")

# generamos los centroides
LC_pnt <- st_centroid(LC_sub)
class(LC_pnt)

# calculo de parametros
st_area(LC_sub)
st_area(LC_pnt)
st_length(LC_pnt)
st_length(LC_lin)

# calculamos un buffer de 30 metros
LC_buff <- st_buffer(LC_sub, 30)
plot(LC_buff, col = 1:8)
LC_lin2 <- st_cast(LC_buff,"LINESTRING")
LC_lin3 <- st_boundary(LC_buff)
plot(LC_lin2)

# operaciones binarias

st_intersects(LC_buff)
st_intersects(LC_buff, sparse = FALSE)
st_intersects(LC_buff, sparse = FALSE)
st_disjoint(LC_buff, sparse = FALSE)
st_within(LC_buff, sparse = FALSE)
st_contains(LC_buff, sparse = FALSE)
st_overlaps(LC_buff, sparse = FALSE)
st_equals(LC_buff, sparse = FALSE)
st_covers(LC_buff, sparse = FALSE)
st_covered_by(LC_buff, sparse = FALSE)
st_crosses(LC_lin, sparse = FALSE)


# calculo de distancias
st_distance(LC_sub)
st_distance(LC_pnt)
st_distance(LC_lin)
st_distance(LC_buff)

st_relate(LC_sub)


# operaciones generativas
# unimos todos los polinonos buffereados
LC_u <- st_union(LC_buff)

# comparamos fronteras y centroides 
plot(LC_u)
plot(LC_sub, add = TRUE, col = "red")
plot(st_centroid(LC_sub), add = TRUE, col = 'blue')
plot(st_centroid(LC_u), add = TRUE, col = 'green')

# casco convexo
plot(st_convex_hull(LC_sub), col ="green")
plot(LC_sub, add=TRUE)


# interseccion
LC_inter_buf <- st_intersection(LC_buff[1:4], LC_buff[5:8])
plot(LC_inter_buf, col=1:34)

LC_dif_buf <- st_difference(LC_buff[1:4], LC_buff[5:8])
plot(LC_dif_buf, col=1:8)
LC_dif_buf2 <- st_difference(LC_buff[5:8], LC_buff[1:4])
plot(LC_dif_buf2, col=1:8)
LC_dif_buf3 <- st_sym_difference(LC_buff[1:4], LC_buff[5:8])
plot(LC_dif_buf3, col=1:8)


# triangulamos 
lc_triang <- st_triangulate(LC_sub)
plot(lc_triang, col=NULL, border="black")

## Voronoi 

# creamos polignos de voronoi sobre la misma geometria de antes
LC_voron <- st_voronoi(LC_sub)
plot(LC_voron, border = "white")
plot(LC_sub, add=TRUE, border = "red")


## Simplificar 

# dibujamos la primera geometria
plot(LC_sub)
plot(st_simplify(LC_sub, dTolerance = 15), add=TRUE, border="red") # la simplificacion con umbral de 15m
plot(st_simplify(LC_sub, dTolerance = 30), add=TRUE, border="green") # 30 m


## Segmentar 

# segmentamos poligono 
LC_seg <- st_segmentize(LC_sub, 10)
plot(LC_seg)


# segmentamos lineas
LC_seg2 <- st_segmentize(LC_lin, 2)
# dibujamos resultados
plot(LC_lin)
plot(LC_seg2, add=TRUE, col = "red")


## Acoplar ----

# acoplamos poligonos
LC_snapped <- st_snap(LC_sub, LC_sub, tolerance = 15)
plot(LC_sub)
plot(LC_snapped, col='red', add=TRUE)

## Poligonizar ----

# creamos poligono a partir de lineas
LC_pol2 <- st_polygonize(LC_lin)

# visualizamos
plot(LC_lin)
plot(LC_pol2, add=TRUE, col = 1:8) 



# Operaciones sobre raster ----


## Bajar resolucion ----

# identificamos un objeto raster de la libreria terra

DEM <- 
  get_elev_raster(st_transform(LC_sub,"EPSG:3857"), z=13) %>% 
  crop(st_bbox(st_transform(LC_sub,"EPSG:3857")))

# exploramos su resolucion
res(DEM)

# disminuimos la resolucion utilizando diferentes funciones de agregacion
rmean <- aggregate(DEM, fact = 4, fun=mean)
rmin <- aggregate(DEM, fact = 4, fun=min)
rmax <- aggregate(DEM, fact = 4, fun=max)

# validamos resolucion
res(rmax)

# graficamos original y 3 agregaciones
par(mfrow=c(2,2), mar = c(0,0,1,0))
plot(DEM)
plot(rmean)
plot(rmin) 
plot(rmax)


## Mejorar la resolucion -----

# creamos un raster vacio en funcion de DEM
x <- DEM
# definimos su resolucion
res(x) <- c(1, 1)
# hacemos un mapeo de los datos de menor resolucion a mayor utilizando diferentes metodos
xbilin <- resample(DEM, x, method="bilinear")


# graficamos el original y los 3 desagregaciones
par(mfrow=c(1,2), mar = c(0,0,1,0))
plot(DEM)
plot(xbilin)

