pacman::p_load(raster, mapview)

# Cargamos imagen satelital de las condes ----
LC <- brick("data/OLI_LC.tif")
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
names(LC) <- c("aerosol","blue", "green", "red", "nir", "swir1", "swir2", "tir1" )

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
plotRGB(LC, r = 5, g = 4, b = 3, stretch = "lin")

# Agricultura (6,5,2)
plotRGB(LC, r = 6, g = 5, b = 2, stretch = "lin")

# Penetración de la Radiación en la Atmósfera (7,6,5)
plotRGB(LC, r = 7, g = 6, b = 5, stretch = "lin")

# Uso del Suelo / Masas de Agua (5,6,4)
plotRGB(LC, r = 5, g = 6, b = 4, stretch = "lin")

# Infrarojo de Onda Corta (7,5,4)
plotRGB(LC, r = 7, g = 5, b = 4, stretch = "lin")

# Análisis de Vegetación (6,5,4)
plotRGB(LC, r = 6, g = 5, b = 4, stretch = "lin")

# Análisis de Vegetación Sana (5,6,2)
plotRGB(LC, r = 5, g = 6, b = 2, stretch = "lin")

# Spoiler de la proxima clase: Indice espectral NDVI ----

# indice normalizado de vegetacion
ndvi <- (LC[[5]] - LC[[4]]) / (LC[[4]] + LC[[5]]) 

# dibujamos el nvdi
plot(ndvi , main = "NDVI Las Condes")

# extraemos zonas con ndvi alto
vegetacion <- calc(ndvi, fun = function(x) ifelse(x <= 0.3, NA, x))

pal_green <- colorRampPalette(c("green","springgreen4", "darkgreen"))( 200 )
plot(vegetacion , main = "NDVI Vegetación Alta", col = pal_green)

## agregar los polígonos de Las Condes
LasCondes <- sf::st_read("data/LasCondes.shp")

plot(LasCondes$geometry, add = T, )


# visualizamos en mapa interactivo

mview <- mapview(LasCondes, color = "#05A39B", alpha.region =0)+
  viewRGB(LC, r = 4, g = 3, b = 2, na.color = "transparent")+
  mapview(vegetacion, na.color = "transparent", col.regions=pal_green) 
mview
# guardamos el mapa como pagina html
mapshot(x = mview, url = "mapa_veg.html")

