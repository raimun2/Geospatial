library(raster)
library(sf)
library(sp)
library(elevatr)
library(tidyverse)
library(ggmap)
library(ggridges)

# esto es un comentario

# definimos las fronteras de la zona de analisis
latitudes = c(-33.58, -33.38)
longitudes <- c(-70.55, -70.4)

# creamos un objeto espacial
spatial_ramon <- SpatialPoints(coords=cbind(longitudes, latitudes), proj4string=CRS("+proj=longlat +datum=WGS84"))

# exploramos los limites
spatial_ramon@bbox

# extraemos un mapa
mapa <- get_map(location = spatial_ramon@bbox, maptype = "terrain", zoom = 11)

# dibujamos el mapa
ggmap(mapa) 

# extraemos el modelo de elevacion digital (DEM)
DEM <- get_elev_raster(spatial_ramon, z= 11)

# dibujamos el DEM
plot(DEM)
points(longitudes, latitudes)

# recortamos el DEM
DEM_recortado <- crop(DEM, extent(spatial_ramon))

# dibujamos el DEM recortado
plot(DEM_recortado)
points(longitudes, latitudes)

# dibujamos el DEM usando otras herramientas

# primero ggplot tradicional
# para esto debemos adaptar la data a un df
spdf <- as(DEM_recortado, "SpatialPixelsDataFrame")
raster_df <- as.data.frame(spdf)

# convencion de nombres utilizada por mi
colnames(raster_df) <- c("ele", "lon", "lat")

# ejemplo ggplot clasico
ggplot(raster_df, aes(x = lon, y = lat, fill = ele)) + 
  geom_tile()
  
# ejemplo ggridges
n_slices <- 100
DEM_slices <- DEM_recortado[1+0:(n_slices-1)*round(nrow(DEM_recortado)/n_slices), , drop=FALSE]
spdf_ridges <- as(DEM_slices, "SpatialPixelsDataFrame")
raster_df_ridges <- as.data.frame(spdf_ridges)
colnames(raster_df_ridges) <- c("ele", "lon", "lat")

# ejemplo ggridges
ggplot(raster_df_ridges, aes(x = lon, y = lat, group = lat, height = ele)) + 
  geom_density_ridges(stat = "identity", scale = 20, fill=NA, color = "black") +
  coord_equal() + 
  theme_void() +
  ggtitle("Ridge profile of Sierra de Ramon")+
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5))


