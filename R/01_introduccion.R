# base R
mean(iris$Sepal.Length)

# tidy R
library(tidyverse)
iris %>% 
  pull(Sepal.Length) %>% 
  mean()


# creamos un objeto espacial ----
library(sf)
library(sp)

# definimos las fronteras de la zona de analisis
latitudes = c(-33.58, -33.38)
longitudes <- c(-70.55, -70.4)

spatial_ramon <- SpatialPoints(coords=cbind(longitudes, latitudes), proj4string=CRS("+proj=longlat +datum=WGS84"))

# exploramos los limites
spatial_ramon@bbox

# dibujamos el mapa ----- 
library(ggmap)

mapa <- get_map(location = spatial_ramon@bbox, maptype = "terrain", zoom = 11)

ggmap(mapa) 

# accedemos a data externa ----
library(chilemapas)

poblacion_adulto_mayor_comunas <- censo_2017_comunas %>% 
  filter(as.integer(edad) >= 14) %>% 
  group_by(codigo_comuna) %>% 
  summarise(pob_adulto_mayor = sum(poblacion))

comunas_los_rios <- mapa_comunas %>% 
  filter(codigo_region == 14) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(poblacion_adulto_mayor_comunas)

ggplot(comunas_los_rios) + 
  geom_sf(aes(fill = pob_adulto_mayor, geometry = geometry)) +
  geom_sf_label(aes(label = nombre_comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(c("#DCA761", "#CFB567", "#BFBC71", "#9EA887", "#819897")), 
                       name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 anios y mas en la Region de los Rios") +
  theme_minimal(base_size = 13)


# modelo de elevacion digital (DEM) ----

library(elevatr)
library(raster)

DEM <- get_elev_raster(spatial_ramon, z = 11)

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
  

# otras representaciones de objetos espaciales ----
library(ggridges)


n_slices <- 100
DEM_slices <- DEM_recortado[1+0:(n_slices-1)*round(nrow(DEM_recortado)/n_slices), , drop=FALSE]
spdf_ridges <- as(DEM_slices, "SpatialPixelsDataFrame")
raster_df_ridges <- as.data.frame(spdf_ridges)
colnames(raster_df_ridges) <- c("ele", "lon", "lat")

ggplot(raster_df_ridges, aes(x = lon, y = lat, group = lat, height = ele)) + 
  geom_density_ridges(stat = "identity", scale = 20, fill=NA, color = "black") +
  coord_equal() + 
  theme_void() +
  ggtitle("Ridge profile of Sierra de Ramon")+
  theme(plot.title = element_text(size = 30, face = "bold", hjust = 0.5))


