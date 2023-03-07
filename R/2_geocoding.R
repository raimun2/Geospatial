# georeferenciacion -----

library(tidygeocoder)
library(sf)
library(mapview)
library(tidyverse)
library(osmdata)

# leemos poligonos de Las Condes
poligonos = read_rds("data/MBHT_LC.rds") 
LasCondes = read_sf("data/shapefile/LasCondes.shp")

# leemos direcciones
direcciones = read_csv("data/direcciones.csv")

#exploramos listado
direcciones

# intentamos georeferenciar direcciones asi tal como vienen
p1 = geo(direcciones$DIRECCION, method="arcgis")

# vemos los resultados preliminares
p1 %>% summary()

# visualizamos en un mapa.. hay resultados en todo el mundo
p1 %>% drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapview(legend = FALSE)

# agregamos el nombre de la comuna de las condes al texto de busqueda
p2 = geo(paste0(direcciones$DIRECCION," Las Condes"), method="arcgis") 

# vemos si hay cambios respecto a p1
p2 %>% summary()

# visualizamos en el mapa
p2 %>% drop_na() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  mapview(legend = FALSE)

# intentamos agregando el nombre de Chile tambien
p3 = geo(paste0(direcciones$DIRECCION," Las Condes, Chile"), method="arcgis") 

# vemos las estadisticas descriptivas
p3 %>% summary()

# generamos objeto espacial con la misma CRS que el objeto base
p3_sf = p3 %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(st_crs(poligonos)) 

# visualizamos
p3_sf %>% mapview(legend = FALSE)

# validar la georeferencia ----

# hacemos geocoding inverso, para validar las direcciones
dirs_rev = reverse_geocode(p3, lat = lat, long = long,  method = "arcgis")

# vemos las diferencias en el texto de las direcciones
dirs_rev$diferencias = mapply(adist, dirs_rev$address...4, dirs_rev$address...1)

hist(dirs_rev$diferencias)


# limpiamos un poco
# quitamos el numero de departamento de las direcciones y preservamos casos unicos
dirs_dp = sub(" DP.*", "", direcciones$DIRECCION) %>% unique()

# geocodificamos valores unicos
p4 = geo(paste0(dirs_dp," Las Condes, Chile"), method="arcgis") %>% as_tibble()

# geocoding inverso nuevamente
dirs_rev2 = reverse_geocode(p4, lat = lat, long = long,  method = "arcgis")

# quitamos las unidades superiores para comparar solo nombre de calle y numero
dirs_rev2$dir4 = gsub(", Las Condes, Santiago, Región Metropolitana de Santiago, 7550000, CHL", "", dirs_rev2$address...4)

# volvemos a calcular las diferencias
dirs_rev2$diferencias = mapply(adist, dirs_rev2$dir4, dirs_dp)

hist(dirs_rev2$diferencias)
