library(tidyverse)

# transformacion entre tipos de datos

# puntos
c(1,1) %>% 
  st_point() %>% 
  st_cast("MULTIPOINT")

rbind(c(1,1),c(2,2)) %>% 
  st_multipoint() %>% 
  st_cast("POINT")

rbind(c(1,1),c(2,2)) %>%   
  st_multipoint() %>%  
  list() %>% 
  st_geometrycollection() 


# lineas
ls <- st_linestring(rbind(c(0,0),c(1,1),c(2,1)))
mls1 <- st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))
mls2 <- st_multilinestring(list(rbind(c(4,4),c(4,3)), rbind(c(2,2),c(2,1),c(3,1))))
(sfc <- st_sfc(ls,mls1,mls2))
st_cast(sfc, "MULTILINESTRING")

# poligonos


# multigeometrias
gc1 <- st_geometrycollection(list(st_linestring(rbind(c(0,0),c(1,1),c(2,1)))))
gc2 <- st_geometrycollection(list(st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))))
gc3 <- st_geometrycollection(list(st_multilinestring(list(rbind(c(4,4),c(4,3)), rbind(c(2,2),c(2,1),c(3,1))))))
(sfc <- st_sfc(gc1,gc2,gc3))

# tranformaciones algebraicas
(p <- st_point(c(0,2)))
p + 1 # sumo el mismo valor a ambas coordenadas
p + c(1,2) #sumo diferentes valores por coordenadas
p + p # sumo 2 veces el mismo vector
p * p # multiplico cada valor por si mismo

# creo una funcion de rotacion


puntos <- read_rds("data/puntos.rds")

mean(table(puntos$id))


puntos$id %>% 
  table() %>% 
  mean()


# extraigo 1 actividad
actividad <- puntos %>% filter(id == unique(puntos$id)[1])

# vectores

# vemos a que clase corresponde
class(actividad)

# transformamos a objeto espacial utilizando libreria sf
library(sf)

actividad_sf <- sf::st_as_sf(actividad, coords = c("lng","lat"))
class(actividad_sf)

# transformamos a objeto espacial utilizando libreria sp
actividad_sp <- sp::SpatialPointsDataFrame(actividad[,c("lng", "lat")], actividad)
class(actividad_sp)

plot(actividad_sf)

plot(actividad_sp@coords)

# tipos de datos
plot(actividad)

ggplot(actividad_sf) +
  geom_sf()

# lineas
actividad_linea <- actividad_sf %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  sf::st_cast("LINESTRING") %>% 
  sf::st_set_crs("WGS84")

class(actividad_linea)

plot(actividad_linea, col ="black")

ggplot() +
  geom_sf(data = actividad_linea, lwd = 8, lineend = "round")


# poligono
actividad_polygon <- actividad_sf %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>%
  sf::st_cast("POLYGON") %>% 
  sf::st_set_crs("WGS84")

class(actividad_polygon)

plot(actividad_polygon, col ="black")

ggplot() +
  geom_sf(data = actividad_polygon, lwd = 3, lineend = "round", aes(fill = 1))



## cargo imagen raster
library(raster)
gee <- read_rds("data/rgee_img.rds")

plot(gee)
plotRGB(gee, stretch = "lin")

gee[1,1]


# ejercicio georeferenciar puntos ----

library(tidygeocoder)


dirs <- c("Cardenal Newman 1480, Las Condes, Chile",
          "Juan XXIII 6821, Vitacura, Chile",
          "Los Dibujantes 1585, vitacura, Chile",
          "La Cortada 2099, Vitacura, Chile",
          "jose M perceval 10253, Vitacura, chile",
          "Capitan Hemmendinger 9595, Las Condes, Chile",
          "Luxemburgo 9595, Las Condes, Chile")

p2 <- geo(dirs, method="arcgis") %>% as_tibble()


# validar la georeferencia
dirs_rev <- reverse_geocode(p2, lat = lat, long = long,  method = "arcgis")


dirs_rev$diferencias <- mapply(adist, dirs_rev$address...4, dirs_rev$address...1)


direcciones <- read_csv2("data/mediciones.csv")

