# invocamos librerias
pacman::p_load(tidyverse, ggmap, sp, sf, elevatr, raster, rayshader)

# cargamos puntos de datos
data <- read_rds("data/ruta_plomo.rds")

# visualizacion inicial
ggplot(data, aes(lon,lat)) + geom_point() 

# cual es el objetivo de esta visualizacion?

# definimos proyeccion segun el formato proj4
prj_dd = "+proj=longlat +ellps=WGS84 +datum=WGS84  +no_defs"

# creamos objeto espacial
spatial_data <- SpatialPoints(data[,c("lon","lat")], proj4string = CRS(prj_dd))

#visualizamos con funcion nativa
plot(spatial_data)


# expandimos el bbox del objeto espacial
bboxexp <- bbox(spatial_data)
bboxexp[1] <- bboxexp[1]-0.05
bboxexp[2] <- bboxexp[2]-0.02
bboxexp[3] <- bboxexp[3]+0.05
bboxexp[4] <- bboxexp[4]+0.02
spatial_data@bbox <- bboxexp

#visualizamos nuevamente con funcion nativa
plot(spatial_data)

# obtenemos elevaciones de toda el area
elev <- get_elev_raster(spatial_data, prj = prj_dd, z = 14)

# visualizamos las elevaciones
plot(elev)

# traducimos elevaciones a curvas de nivel, con 100 niveles
curvas_nivel <- rasterToContour(elev, nlevels = 100)

# visualizamos curvas de nivel
plot(curvas_nivel)

# las visualizamos con ggplot
ggplot() +
  geom_sf(data=st_as_sf(curvas_nivel))



# buscamos un mapa base apropiado
maptype <- 'terrain'
map <- ggmap::get_stamenmap(bboxexp, maptype = maptype, zoom=12, source="stamen")

# visualizamos utilizando ggmap
ggmap(map)

# visualizamos agregando la ruta
ggmap(map) +
  geom_path(data=data, col="yellow", aes(lon,lat), size = 2) 

# agregamos lineas de contorno (ERROR)
ggmap(map) +
  geom_path(data=data, col="yellow", aes(lon,lat), size = 2) +
  geom_sf(data=st_as_sf(curvas_nivel))



# para agregar las lineas de contorno a ggmap, tendremos que transformarlo a un dataframe normal

### transformo spatiallines a dataframe
# para cada nivel de la curva identifico todos los puntos que construyen esa linea
puntos_contorno <- do.call(rbind, lapply(curvas_nivel$level,
                                        function (y) do.call(rbind,lapply(
                                          coordinates(curvas_nivel[curvas_nivel$level==y,])[[1]]  ,
                                          function(x) data.frame(level=y,x)))))

# identifico el largo de cada linea
linea_contorno <- do.call(rbind, lapply(curvas_nivel$level,
                                       function (y) do.call(rbind,lapply(
                                         coordinates(curvas_nivel[curvas_nivel$level==y,])[[1]]  ,
                                         function(x) nrow(data.frame(level=y,x))))))

# ordeno los nombres
colnames(puntos_contorno) = c("ele","lon","lat")
puntos_contorno$ele = as.numeric(as.character(puntos_contorno$ele))
# identifico las lineas 
puntos_contorno$linea = rep(1:nrow(linea_contorno),linea_contorno)



# ahora podemos agregarlas a la visualizacion
ggmap(map) +
  geom_path(data=puntos_contorno,  aes(lon,lat, group=linea, col=ele)) +
  geom_path(data=data, col="yellow",aes(lon,lat), size = 2) 

# ahora agregaremos mas detalles y formateamos el mapa

ggmap(map) +
  geom_path(data=puntos_contorno,  aes(lon,lat, group=linea, col=ele)) +
  geom_path(data=data,col="yellow",aes(lon,lat), size = 2) +
  theme_void() +
  theme(legend.position = "none") + 
  scale_colour_viridis_c(option = "A", direction =  -1) +
  annotate("text", x = -70.33, y = -33.228, hjust = 0, size = 9, fontface =2,
           label = "Gradient ascent in \nCerro el Plomo") +
  annotate("text", x = -70.33, y = -33.34, hjust = 0, size = 5, 
           label = "Author: @raimun2") 



# mapa base customizado

# paso las elevaciones a matriz
stream_mat <- raster_to_matrix(elev)

# achico la matriz (para que corra mas rapido)
stream_mat_small <- resize_matrix(stream_mat, 0.2)

# creo capas de visualizacion y sombreado con rayshader
stream_mat_small %>% 
  height_shade() %>% 
  add_overlay(sphere_shade(stream_mat_small, texture = "desert", 
                           zscale=4, colorintensity = 5), alphalayer = 0.8) %>%    
  add_overlay(generate_point_overlay(st_as_sf(curvas_nivel), 
                                     color = viridisLite::magma(100, direction = -1), size = 1, pc = 19, 
                                     raster::extent(elev), heightmap = stream_mat_small), alphalayer = 0.9) %>% 
  add_overlay(generate_point_overlay(st_as_sf(spatial_data), color = "yellow", size = 1, pc = 19, 
                                     raster::extent(elev), heightmap = stream_mat_small)) %>% 
  plot_map()


