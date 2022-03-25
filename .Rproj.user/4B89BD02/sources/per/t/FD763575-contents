# cargamos librerias
library(sf)   
library(tmap)
library(vapour)

# leemos datos
s.sf <- readr::read_rds("data/s.rds")

plot(s.sf)

# consultamos versiones de librerias
sf_extSoftVersion()[1:3]

# vemos la documentacion de la funcion st_crs
? st_crs

# consultamos CRS con estandar WKT
st_crs(s.sf)

# consultamos CRS con estandar proj4
raster::crs(s.sf)

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
s.sf2 <- st_set_crs(s.sf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 
st_crs(s.sf2)

plot(s.sf2)


# transformamos un objeto a otra proyeccion
s.sf.gcs <- st_transform(s.sf, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
st_crs(s.sf.gcs)

plot(s.sf.gcs)

# probamos con datos del¿ todo el mundo
data(World)  

st_crs(World)

# transformamos a azimuthal equidistant
World.ae <- st_transform(World, "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(World.ae) 

tm_shape(World.ae) + tm_fill() 

# azimuthal equidistant centrada en Maine
World.aemaine <- st_transform(World, "+proj=aeqd +lat_0=44.5 +lon_0=-69.8 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World.aemaine) + tm_fill()  

# robinson
World.robin <- st_transform(World,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.robin) + tm_fill()  

# sinusoidal
World.sin <- st_transform(World,"+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.sin) + tm_fill()  

# mercator
World.mercator <- st_transform(World,"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(World.mercator) + tm_fill()  

# https://proj.org/operations/projections/index.html
