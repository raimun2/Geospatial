pacman::p_load(raster, mapview, sf)

# Cargamos imagen satelital de las condes ----
LC <- brick("data/landsat8_30m/l8_2013.tif")
LC

# Número de bandas asignado a la Imagen OLI Landsat
# 
# - aerosol = 1
# - blue    = 2
# - green   = 3
# - red     = 4
# - nir     = 5
# - swir1   = 6
# - swir2   = 7
# - thermal  = 8

# asigno nombres a las bandas
names(LC) <- c("B1","B2", "B3", "B4", "B5", "B6", "B7", "B8" )

### utm utilizado a nivel regional (depende de la zona y hemisferio)
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Reproyección de Imagen  
LC_ll <- projectRaster(LC, crs = crs_utm)

# Mapeamos la imagen ----

# Color Natural
plotRGB(LC_ll, r = 4, g = 3, b = 2)

# Color Natural con contraste lineal
plotRGB(LC_ll, r = 4, g = 3, b = 2, stretch = "lin")

# Color Natural con contraste de quiebres naturales
plotRGB(LC_ll, r = 4, g = 3, b = 2, stretch = "hist")

# recortamos extension de la imagen

# definimos fronteras (extent)
ext <- extent(c(350638, 358235,  6299157, 6304228))
# recortamos
LC_crop <- crop(x = LC, y = ext, snap="out")
# visualizamos
plotRGB(LC_crop, r = 4, g = 3, b = 2, stretch = "lin")




# Visualizamos falso color ----

# Infrarojo (5,4,3)
plotRGB(LC, r = 5, g = 4, b = 3, stretch = "hist")

# Agricultura (6,5,2)
plotRGB(LC, r = 6, g = 5, b = 2, stretch = "hist")

# Penetración de la Radiación en la Atmósfera (7,6,5)
plotRGB(LC, r = 7, g = 6, b = 5, stretch = "hist")

# Uso del Suelo / Masas de Agua (5,6,4)
plotRGB(LC, r = 5, g = 6, b = 4, stretch = "hist")

# Infrarojo de Onda Corta (7,5,4)
plotRGB(LC, r = 7, g = 5, b = 4, stretch = "hist")

# Análisis de Vegetación (6,5,4)
plotRGB(LC, r = 6, g = 5, b = 4, stretch = "hist")

# Análisis de Vegetación Sana (5,6,2)
plotRGB(LC, r = 5, g = 6, b = 2, stretch = "hist")

# Exploramos el canal infrarojo

# normalizamos los valores del canal 5 (x-mean)/sd
infrared <- scale(LC[[5]])

# dibujamos el infrarojo
plot(infrared)

# extraemos zonas con infrarojo alto, que aproximan a la nieve
nieve <- calc(infrared, fun = function(x) ifelse(x <= 3, NA, x))

pal_green <- colorRampPalette(c("green","springgreen4", "darkgreen"))
plot(nieve , col = pal_green( 200 ))

# pasamos pixeles a poligonos
poligonos_infrarojo <- rasterToPolygons(nieve, digits = 16) %>% st_as_sf()
plot(poligonos_infrarojo, pal = pal_green)

# veamos que pasa al unirlos
merged_poligonos_infrarojo <- poligonos_infrarojo  %>% 
  st_union() %>% # unimos vecinos
  st_cast("POLYGON")  # aislamos los poligonos resultantes

plot(merged_poligonos_infrarojo)

# st_union solo junta las geometrias, para unir los valores hay que ir un paso mas alla

# extraemos los valores del raster original sobre cada poligono resultante y lo guardamos en un df
nieve_poly <- data.frame(nieve = raster::extract(nieve, 
                                                     st_as_sf(merged_poligonos_infrarojo), 
                                                     fun=mean))

# le asignamos al df las geometrias de los poligonos
st_geometry(nieve_poly) <- st_sfc(merged_poligonos_infrarojo)

# visualizamos
plot(nieve_poly, pal = pal_green)

# agregar la frontera de Las Condes
LasCondes <- sf::st_read("data/shapefile/LasCondes.shp")
plot(LasCondes$geometry)
plot(nieve_poly, pal = pal_green, add = TRUE)

# visualizamos en mapa interactivo
mview <- mapview(LasCondes, color = "#05A39B", alpha.region =0)+
  viewRGB(LC, r = 4, g = 3, b = 2, na.color = "transparent") +
  mapview(nieve_poly, na.color = "transparent", col.regions = pal_green) 
mview

# guardamos el mapa como pagina html
mapshot(x = mview, url = "mapa_veg.html")


# llamo funciones de indices espectrales
source("R/6_indices_espectrales.R")

nieve2 <- NDSI(LC)
plot(nieve2)

nieve2 <- calc(nieve2, fun = function(x) ifelse(x <= 0.9, NA, x))

plot(nieve2)

