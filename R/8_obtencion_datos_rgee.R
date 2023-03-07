pacman::p_load(tidyverse, rgee, sf, raster)

ee_Initialize(drive = T)

# defino una region de interes ----
roi <- 
  c(-70.518333, -33.416667) %>%  # las condes
  st_point(dim = "XYZ") %>% 
  st_buffer(dist = 0.09) %>% 
  sf_as_ee()

anio = 2014

# captura de imagenes satelitales por año ----
disponible <- ee$ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
  filterDate(paste0(anio,'-07-01'),paste0(anio,'-11-30'))$
  filterBounds(roi)$
  filterMetadata('CLOUD_COVER','less_than', 15)

# ordeno las fechas
df_disponible <- ee_get_date_ic(disponible) %>%
  arrange(time_start)

# extraigo el id de la primera fecha
escena <- df_disponible$id[1]

# defino las bandas que me interesa extraer
l8_bands <- ee$Image(escena)$select(c("B1", "B2", "B3", "B4", 
                                      "B5", "B6", "B7", "B9"))
# B1: Aerosol, B2: Blue, B3: Green, B4: Red
# B5: NIR, B6: SWIR 1, B7: SWIR 2, B9: Cirrus

# extraigo imagenes satelitales 
l8_img <- ee_as_raster(
  image = l8_bands,
  region = roi$bounds(),
  scale = 30)

plotRGB(l8_img, r = 4, g = 3, b = 2, stretch = "hist")

# cargo toda la data en memoria
l8_img <- readAll(l8_img)

# guardo raster en carpeta
writeRaster(LC_ll, filename=paste0("data/landsat8_30m/l8_",anio,".tif"), format="GTiff", overwrite=TRUE)
