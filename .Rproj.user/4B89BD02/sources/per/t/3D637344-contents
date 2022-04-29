pacman::p_load(sf, tidyverse, sfnetworks, openrouteservice, terra, leaflet)
#remotes::install_github("GIScience/openrouteservice-r")

# ********************************
# Analisis vectorial avanzado ----
# ********************************

## Simplificar ----

# cargamos un shape de poligonos
nc <- st_read(system.file("shape/nc.shp", package="sf"))

# extramos la geometria
nc_g <- st_geometry(nc)

# dibujamos la primera geometria
plot(nc_g[1], lwd = 3)
plot(st_simplify(nc_g[1], dTolerance = 1e3), add=TRUE, border="red") # la simplificacion con umbral de 1000m
plot(st_simplify(nc_g[1], dTolerance = 5e3), add=TRUE, border="green") # 5000m

## Segmentar ----

# generamos poligono
pol <- st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))
# segmentamos poligono con parametro de distancia 0.3
pol.seg <- st_segmentize(pol, 0.3)
# dibujamos resultados
plot(pol.seg, col = 'grey')
points(pol.seg[[1]])

# generamos linea
ls <- st_linestring(rbind(c(0,0),c(1,0),c(2,1),c(3,1)))
# segmentamos lineas
ls.seg <- st_segmentize(ls, 0.3)
# dibujamos resultados
plot(ls)
points(ls.seg)


## Acoplar ----

# creamos lineas
lines <- st_multilinestring(list(
  cbind(c(0, 1), c(1, 1.05)),
  cbind(c(0, 1), c(0, -.05)),
  cbind(c(1, .95, 1), c(1.05, .5, -.05))
))
# visualizamos
plot(lines, lwd=2, col='blue')

# creamos polinono
poly <- st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 1, 1, 0, 0))))
plot(poly, add=TRUE, pch = 2)

# acoplamos poligono a lineas
snapped <- st_snap(poly, lines, tolerance=.1)
# agregamos al grafico
plot(snapped, col='red', add=TRUE, alpha=0.5)

## Poligonizar ----

# creamos multilinea
mls <- st_multilinestring(list(matrix(c(0,0,0,1,1,1,0,0), 4 ,2,byrow=TRUE)))
# poligonizamos
st_polygonize(st_sfc(mls))

# visualizamos
plot(mls)
plot(st_polygonize(st_sfc(mls)), add=TRUE) 

## Triangular ----

# triangulizamos la primera geometria del shape
triang <- st_triangulate(nc_g[1])
# dibujamos forma original y triangulos encima
plot(nc_g[1], col="black")
plot(triang, col=NULL, border="white", add=TRUE)


## Voronoi ----

# creamos polignos de voronoi sobre la misma geometria de antes
voron <- st_voronoi(nc_g[1])
# repetimos el grafico
plot(nc_g[1], col="black")
plot(voron, col=NULL, border="white", add=TRUE)

# *********************************
# Analisis de redes ----
# *********************************

## Camino mas corto ----

# obtenemos red de roxel, disponible en la libreria
net <- as_sfnetwork(roxel, directed = FALSE)
plot(net)

# utilizamos funcion para encontrar ruta entre nodos 1 y 9
path <- igraph::shortest_paths(net, 1, 9)$vpath[[1]]

# extraemos del objeto net los puntos del camino mas corto
path_net <- st_geometry(net)[path %>% as.numeric()] %>% 
  st_as_sf("POINT") 

# agregamos puntos al mapa, de color rojo
plot(path_net, add=TRUE, col="red", lwd  = 8)

## Impedancia de viaje ----

# creamos funcion con impedancia segun regla de Tobler
tobler_impedance <- function(slope){
  w <- 6*exp(-3.5*abs(slope + 0.05))
  return(w)
}

# cargamos ruta de cerro el plomo
plomo <- read_rds("data/ruta_plomo.rds")

# calculamos las metricas de distancia, diferencia de altitud de cada medicion
# calculamos la velocidad segun impedancia de toble, y luego la duracion
plomo <- plomo %>% 
  mutate(slope = pmin(0.4, pmax(-0.4,tan(grade_smooth/360*2*pi))),
         tobler_speed = tobler_impedance(slope),
         delta_distance = c(0,diff(distance)),
         duration = (delta_distance)/tobler_speed) %>% 
  filter(delta_distance > 0)

# calculamos la diferencia entre medicion real y calculada
sum(plomo$duration)
max(plomo$time) / 3600


## Isocronas ----

# para utilizar la libreria debemos obtener una llave a su API
# esta es mi llave, deberian obtener las suyas en la pagina para que no colapse
# https://openrouteservice.org/dev/#/signup
ors_api_key("5b3ce3597851110001cf6248db68c78edc7b41538810b09141f8cb60")

# utilizamos la funcion isochrones para calcular las isocronas de la coordenada a 20 minutos
drivetime <- ors_isochrones(
  # definimos punto de inicio
  locations = c(-70.51, -33.49),
  # usamos perfil de ciclista
  profile = "cycling-mountain",
  # 20 minutos
  range = 20*60,
  # devuelve un objeto sf
  output = "sf"
)

# creamos mapa dinamico para visualizarlo
leaflet() %>%
  addProviderTiles("OpenStreetMap") %>% 
  addAwesomeMarkers(
    lat = -33.49,
    lng = -70.51,
    label = "UAI") %>%
  addPolygons(data = drivetime)


# ****************************
# Operaciones sobre raster ----
# ****************************

## Bajar resolucion ----

# identificamos un objeto raster de la libreria terra
f <- system.file("ex/elev.tif", package="terra")
# lo almacenamos como raster
r <- rast(f)
# exploramos su resolucion
res(r)

# disminuimos la resolucion utilizando diferentes funciones de agregacion
rmean <- aggregate(r, c(4,5), fun=mean)
rmin <- aggregate(r, c(4,5), fun=min)
rmax <- aggregate(r, c(4,5), fun=max)
# validamos resolucion
res(rmin)

# graficamos original y 3 agregaciones
par(mfrow=c(2,2), mar = c(0,0,1,0))
plot(r)
plot(rmean)
plot(rmin) 
plot(rmax)


## Mejorar la resolucion -----

# creamos un raster vacio en funcion de r
x <- rast(r)
# definimos su resolucion
res(x) <- c(0.0005, 0.0005)
# hacemos un mapeo de los datos de menor resolucion a mayor utilizando diferentes metodos
xbilin <- resample(r, x, method="bilinear")
xcubic <- resample(r, x, method="cubic")
xcubicspline <- resample(r, x, method="cubicspline")

# graficamos el original y los 3 desagregaciones
par(mfrow=c(2,2), mar = c(0,0,1,0))
plot(r)
plot(xbilin)
plot(xcubic)
plot(xcubic)

