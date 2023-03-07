# georeferenciacion -----
library(tidygeocoder)
library(sf)
library(mapview)
library(tidyverse)
library(osmdata)
library(igraph)
library(nngeo)
library(sfnetworks)

# leemos poligonos de Las Condes
poligonos <- read_rds("data/MBHT_LC.rds") 
LasCondes <- read_sf("data/shapefile/LasCondes.shp")

# leemos direcciones
direcciones <- read_csv("data/direcciones.csv")

#exploramos listado
direcciones = direcciones[1:10,]

# intentamos agregando el nombre de Chile tambien
p3 <- geo(paste0(direcciones$DIRECCION," Las Condes, Chile"), method="arcgis") 

# generamos objeto espacial con la misma CRS que el objeto base
p3_sf <- p3 %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(st_crs(poligonos)) 

# visualizamos
p3_sf %>% mapview(legend = FALSE)

# red de calles

# usaremos la libreria de osm para extraer las calles principales de Las Condes
# (asumiremos que por estas pasa el transporte publico)

# identificamos el bbox de las condes
LC_bbox <- getbb("Las Condes, Santiago, Chile")

# vemos las categorias presentes en la libreria OSM
available_tags("highway")

# extraigo las calles principales y sus nodos desde OSM
LC_hway <- opq(LC_bbox) %>% # identifico bbox
  add_osm_feature( # agrego atributo del listado anterior
    key = "highway",
    #    value = c("motorway", "primary", "residential", "secondary", "terciary", "living_street") # se pueden agregar despues secondary terciary available
  ) %>% 
  osmdata_sf() %>% # transformo en sf
  pluck("osm_lines") %>% # preservo solo elementos de osm_lines
  st_transform(crs=st_crs(poligonos))  # dejo en misma proyeccion que objeto base

mapview(LasCondes)

# extramos solo las calles que intersectan con las condes
calles_LC <- 
  LC_hway %>% # tomamos toda la base
  filter(LC_hway %>%  #fitramos cuando hay interseccion entre las calles y 
           st_intersects(LasCondes %>% # el poligono del perimetro de las condes
                           st_buffer(dist = 15)) %>%  # con un margen de 15 metros
           summary() %>% # el resumen de intersect devuelve 1 si hay interseccion 0 eoc
           .[,1] %>% # este valor de 1 viene en la primera columna
           as.numeric() == 1) %>% # verifico si el resultado de esta operacion es 1
  drop_na(name) %>% 
  filter(surface %in% c("asphalt", "concrete", "paved", "paving_stones", "cobblestone", "cobblestone:flattened"))

# calles_LC = read_sf("data/red_vial_LC.gpkg")

# visualizamos recorridos de transporte publico
mapview(calles_LC)

# generamos red de calles
net <- as_sfnetwork(calles_LC, directed = FALSE)
plot(net)

# calculamos una ruta en particular, del 1 al 9
path <- shortest_paths(net, 1, 9, algorithm = "dijkstra")$vpath[[1]]

# extraemos del objeto net los puntos del camino mas corto
path_net <- st_geometry(net)[path %>% as.numeric()] %>% 
  st_as_sf("POINT") 

# agregamos puntos al mapa, de color rojo
plot(path_net, add=TRUE, col="red", lwd  = 8)

# identificamos vecinos mas cercanos a entidades espaciales y preservamos los casos unicos
nn_direcciones <- st_nn(p3_sf, net)
nn_dir_unicos <- unique(nn_direcciones)

nn_poligonos <- st_nn(st_centroid(poligonos), net)
nn_pol_unicos <- unique(nn_poligonos)

# calculamos la ruta de todos con todos
distancia_red <- shortest_paths(net, from = nn_dir_unicos , to = nn_pol_unicos, output = "epath")

# podemos calcular la distancia de cada poligono a cada junta de vecinos
distancia_euclideana <- st_distance(poligonos, p3_sf[3,])

# preservamos la menor distancia para asociar una manzana a una JJVV
# generamos indicador de accesibilidad a jjvv
poligonos <- 
  poligonos %>% 
  mutate(accesibilidad = apply(distancia_euclideana, 1, function(x) min(x)))

# visualizamos
ggplot() +
  geom_sf(data=poligonos, aes(fill=accesibilidad))+
  geom_sf(data = p3_sf[3,], size = 2, col = "white") +
  scale_fill_viridis_c(direction = -1)
