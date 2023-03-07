# preparamos entorno de trabajo ----

## invocamos librerias
pacman::p_load(tidyverse, ggmap, sp, sf, elevatr, raster, rayshader,
               tmap, mapview, RColorBrewer, viridis, mapdeck)

## definimos proyeccion segun el formato proj4
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

## cargamos puntos de datos
puntos <- read_rds("data/puntos.rds") %>% 
  filter(id == unique(.$id)[20]) %>% 
  dplyr::select(lng,lat) %>% 
  st_as_sf(coords = c("lng","lat")) %>% 
  st_set_crs(prj_dd)

## cargamos archivo shp con poligonos de LasCondes
las_condes <- read_rds("data/MBHT_LC.rds")

chilemapas::codigos_territoriales %>% view()


# creamos una paleta que nos va a servir para los colores
pal <- magma(n = length(unique(las_condes$PERSONAS)), direction = -1)


# cual es el objetivo de esta visualizacion? -----


# visualizamos con funcion nativa ----
plot(puntos)
plot(las_condes)


# visualizacion inicial con ggplot
ggplot() + 
  geom_sf(data = las_condes, aes(fill = PERSONAS), col = NA) +
  geom_sf(data = puntos) 



# bbox del objeto espacial
bbox <- st_bbox(puntos) %>% as.numeric()


# mapa de elevaciones ----
# obtenemos elevaciones de toda el area
elev <- get_elev_raster(puntos, prj = prj_dd, z = 12)

# visualizamos las elevaciones
plot(elev)

# traducimos elevaciones a curvas de nivel, con 100 niveles
curvas_nivel <- rasterToContour(elev, nlevels = 100) %>% st_as_sf()

# visualizamos curvas de nivel
plot(curvas_nivel)

# las visualizamos con ggplot
ggplot() +
  geom_sf(data= curvas_nivel) + 
  geom_sf(data = las_condes, aes(fill = PERSONAS), alpha = .5, col = NA) +
  geom_sf(data = puntos, col = "yellow") 



# obtencion de mapas base -----
maptype <- 'terrain'
map <- ggmap::get_stamenmap(bbox, maptype = maptype, zoom=12, source="stamen")

# visualizamos utilizando ggmap
ggmap(map)

# visualizamos agregando la ruta
ggmap(map) +
  geom_point(data=data.frame(st_coordinates(puntos$geometry)), aes(X, Y), col="yellow",size = 2) 


bboxexp <- bbox
bboxexp[1] <- bboxexp[1]-0.05
bboxexp[2] <- bboxexp[2]-0.02
bboxexp[3] <- bboxexp[3]+0.05
bboxexp[4] <- bboxexp[4]+0.02

map2 <- ggmap::get_stamenmap(bboxexp, maptype = maptype, zoom=12, source="stamen")

ggmap(map2) +
  geom_point(data=data.frame(st_coordinates(puntos$geometry)), aes(X, Y), col="yellow",size = 2) 


# mapa base custiomizado -----
# uso de rayshader

# paso las elevaciones a matriz
elev_mat <- raster_to_matrix(elev)

# achico la matriz (para que corra mas rapido)
elev_mat_small <- resize_matrix(elev_mat, 0.5)

# creo capas de visualizacion y sombreado con rayshader
elev_mat_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(elev_mat_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer = 0.8) %>%    
  add_overlay(generate_point_overlay(st_as_sf(curvas_nivel), 
                                     color = viridisLite::magma(100, direction = -1), size = 1, pc = 19, 
                                     raster::extent(elev), heightmap = elev_mat_small), alphalayer = 0.9) %>% 
  add_overlay(generate_point_overlay(puntos, color = "yellow", size = 1, pc = 19, 
                                     raster::extent(elev), heightmap = elev_mat_small)) %>% 
  plot_map()




# mapas interactivos ----


# primera libreria que probaremos es tmap
# tmap permite hacer graficos estaticos e interactivos

tmap_mode("plot")
tm_shape(las_condes) + 
  tm_polygons(col = "PERSONAS", palette = pal)

# para interactivos cambiamos el modo de visualizacion a "view"
tmap_mode("view")

# repetimos el mismo plot
tm_shape(las_condes) + 
  tm_polygons(col = "PERSONAS", palette = pal)

# cambiamos el mapa base
tm_shape(las_condes) +
  tm_polygons(col = "PERSONAS", palette = pal) +
  tm_basemap("Stamen.Watercolor")

# graficamos los objetos espaciales como puntos
tm_shape(las_condes) +
  tm_dots(col = "PERSONAS", palette = pal,
          popup.vars = TRUE) 



# otra buena opcion es mapview

# graficamos usando funcion mapview
mapview(las_condes)

# especificamos variable para colorear
mapview(las_condes, zcol = "PERSONAS")

# especificamos paleta
mapview(las_condes, zcol = "PERSONAS", col.regions = pal)

? mapview

# coloreamos de acuerdo al nombre del distrito y generamos un popup
mapview(
  las_condes,
  zcol = "ZONA",
  col.regions = pal,
  popup = leafpop::popupTable(
    las_condes,
    zcol = c(
      "PERSONAS",
      "AREA",
      "E65YMAS"
    )
  )
)

