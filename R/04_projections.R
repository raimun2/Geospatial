# cargamos librerias
library(sf)
library(tmap)
library(vapour)

# leemos datos
s_sf <- readr::read_rds("data/s.rds")

plot(s_sf)

# consultamos versiones de librerias
sf_extSoftVersion()[1:3]

# vemos la documentacion de la funcion st_crs
? st_crs

# consultamos CRS con estandar WKT
st_crs(s_sf)

# consultamos CRS con estandar proj4
raster::crs(s_sf)

# transformamos de proj4 a WKT
projeccion <- "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
vapour_srs_wkt(projeccion)

# +a         Semimajor radius of the ellipsoid axis
# +b         Semiminor radius of the ellipsoid axis
# +datum     Datum name 
# +ellps     Ellipsoid name 
# +lat_0     Latitude of origin
# +lat_1     Latitude of first standard parallel
# +lat_2     Latitude of second standard parallel
# +lat_ts    Latitude of true scale
# +lon_0     Central meridian
# +over      Allow longitude output outside -180 to 180 range, disables wrapping 
# +proj      Projection name 
# +south     Denotes southern hemisphere UTM zone
# +units     meters, US survey feet, etc.
# +x_0       False easting
# +y_0       False northing
# +zone      UTM zone

# cambio la proyeccion de un objeto
s_sf2 <- st_set_crs(s_sf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 
st_crs(s_sf2)

plot(s_sf2)


# transformamos un objeto a otra proyeccion
s_sf_gcs <- st_transform(s_sf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
st_crs(s_sf_gcs)

plot(s_sf_gcs)

# probamos con datos del todo el mundo
data(World)  

st_crs(World)

tm_shape(World) + tm_fill() 

# transformamos a azimuthal equidistant
World_ae <- st_transform(World, "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(World_ae) 

tm_shape(World_ae) + tm_fill() 

# azimuthal equidistant centrada en Maine
World_aemaine <- st_transform(World, "+proj=aeqd +lat_0=44.5 +lon_0=-69.8 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World_aemaine) + tm_fill()  

# robinson
World_robin <- st_transform(World,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World_robin) + tm_fill()  

# sinusoidal
World_sin <- st_transform(World,"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World_sin) + tm_fill()  

# mercator
World_mercator <- st_transform(World,"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World_mercator) + tm_fill()  

# https://proj.org/operations/projections/index.html
