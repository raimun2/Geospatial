# cargamos librerias
pacman::p_load(sf, stars, tmap, tidyverse)

# transformamos entre wkt y proj4 -----

# antiguo estandar de proyecciones proj4
crs_utm <-   "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs"

crs_ll  <-   "+proj=longlat +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

crs_utm2 <-  "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

crs_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


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

# codigos ESPG
ESPG_stereo <- "EPSG:28992"

ESPG_webmercator <- "EPSG:3857"

ESPG_tmercator <- 26912
# raster vs vector ----

# cargamos datos vectoriales
data_vector <- read_rds("data/MBHT_LC.rds")

# verificamos si viene con una proyeccion
st_crs(data_vector)

# cargamos datos raster
data_raster <- read_rds("data/rgee_img.rds") %>% 
  st_as_stars()

# verificacmos proyeccion
st_crs(data_raster)

# vemos la documentacion de la funcion st_crs
? st_crs

# funcion para pasar proj4 a wkt ----
proj42wkt <- function(crs_p4){
  st_point(1:2) %>% 
    st_sfc() %>% 
    st_sf(crs = crs_p4) %>% 
    st_crs() %>% return()
}

proj42wkt("EPSG:26912")

# set vs transform ----

# cambio la proyeccion de un objeto
data_vector2 <- 
  data_vector %>% 
  st_set_crs(crs_ll) 

# consulto crs
st_crs(data_vector2)

# ploteamos
plot(data_vector2$geometry)


# transformamos un objeto a otra proyeccion
data_vector3 <- 
  data_vector %>% 
  st_transform(crs_ll) 

# verificamos
st_crs(data_vector3)

# ploteamos
plot(data_vector3$geometry)


# transformamos un objeto raster
data_raster2 <- data_raster %>% 
  st_transform("EPSG:26912")

# chequeamos crs
st_crs(data_raster2)

# ploteamos los canales r g b
image(data_raster, rgb = c(1,2,3))


# efecto en cartografia ----

# probamos con datos del todo el mundo
data(World)  

st_crs(World)

tm_shape(World) + 
  tm_fill() 

# transformamos a azimuthal equidistant
World_ae <-
  World %>% 
  st_transform("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(World_ae) 

tm_shape(World_ae) + 
  tm_fill() 

# azimuthal equidistant centrada en Maine
World_aemaine <- 
  World %>%  
  st_transform( "+proj=aeqd +lat_0=44.5 +lon_0=-69.8 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World_aemaine) + 
  tm_fill()  

# robinson
World_robin <- 
  World %>% 
  st_transform("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World_robin) + 
  tm_fill()  

# sinusoidal
World_sin <- 
  World %>%  
  st_transform("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World_sin) + 
  tm_fill()  

# mercator
World_mercator <- 
  World %>% 
  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_shape(World_mercator) + 
  tm_fill()  

# https://proj.org/operations/projections/index.html
