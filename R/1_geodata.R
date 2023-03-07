
library(tidyverse)
library(sf)

# definimos las fronteras de la zona de analisis
latitudes = c(-33.58, -33.38, -33.28)
longitudes <- c(-70.55, -70.4, -70.45)

# transformacion entre tipos de datos

# puntos
c(longitudes[1],latitudes[1]) %>% 
  st_point() %>% 
  st_cast("MULTIPOINT")

cbind(longitudes, latitudes) %>% 
  st_multipoint() %>% 
  st_cast("POINT")

cbind(longitudes, latitudes) %>%   
  st_multipoint() %>%  
  list() %>% 
  st_geometrycollection() 


# lineas
ls <- st_linestring(cbind(longitudes, latitudes))
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

actividad_sf <- sf::st_as_sf(actividad, coords = c("lng","lat"))
class(actividad_sf)

plot(actividad_sf)


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



# accedemos a data externa ----
library(chilemapas)

poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      dplyr::select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

ggplot(comunas_los_rios) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")), 
                       name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region de los Rios") +
  theme_minimal(base_size = 13)


# modelo de elevacion digital (DEM) ----

library(elevatr)
library(raster)

DEM <- get_elev_raster(actividad_polygon, z = 11)

# dibujamos el DEM
plot(DEM)

