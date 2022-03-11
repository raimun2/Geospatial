install.packages("remotes")
remotes::install_github("raimun2/GPStream")

library(tidyverse)

# traemos data de la libreria GPStream
puntos <- GPStream::strava_streams
write_rds(puntos, "data/puntos.rds")

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


# raster imagen
imagen <- read_rds("data/mapa.rds")

plot(imagen)

ggmap::ggmap(imagen)

imagen[1,1]

## cargo imagen satelital
gee <- read_rds("data/rgee_img.rds")

plot(gee)
plotRGB(gee, stretch = "lin")


# rasterizo los puntos
actividad_raster <- stars::st_rasterize(actividad_sf[,"id"], nx = 50, ny = 50)
class(actividad_raster)

plot(actividad_raster, breaks = "equal")

# analizo metadata 
metadata <- GPStream::strava_metadata

summary(metadata)

library(googlePolylines)
decode_poly <- metadata$map.summary_polyline[1] %>% decode() 

plot(decode_poly[[1]])
