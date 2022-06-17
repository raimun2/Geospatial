### Calculo de Hotspots de Violencia ----

# Cargar Librerias
pacman::p_load(rgdal, rgeos, stars, spatstat, spdep, 
               spatialreg, tidyverse, vapour, gstat, MASS)

# defino CRS que voy a usar
crs_utm <-  vapour_srs_wkt("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
crs_ll <- vapour_srs_wkt("+proj=longlat +datum=WGS84 +no_defs")

# ***************************
# Cargar y ordenar datos ----

# Datos de delitos
  violencia <- read_rds("data/casos_violencia.rds")  %>%  
  SpatialPointsDataFrame(coords = .[,c("x","y")], 
                                   proj4string = CRS(crs_utm)) %>% 
  st_as_sf() 

# Cargar datos censales de nivel educativo en Las Condes a nivel de personas
censo_lc <- readRDS("data/censo_lc.rds") %>% 
  mutate(poblacion = 1,
         CODINE011 = as.character(IDMZ)) %>% 
  select(-IDMZ)

# Calcular poblacion por manzana
poblacion <- 
  censo_lc %>% 
  group_by(CODINE011) %>% 
  summarise(poblacion = sum(poblacion)) 

# Calcular Nivel Educacional de jefes de hogar por manzana
nived <- censo_lc %>% 
  filter(DSOST==1) %>%  # Filtar sostenedores
  group_by(CODINE011) %>% 
  summarise(EDUC = mean(EDUC))

# Cargar Poligonos de Manzanas de Las Condes (Censo 2012)
# acoplar con datos de nivel educacional a manzanas
mz_lc <- readRDS("data/manzanas_lc.rds") %>% 
  st_as_sf() %>% 
  left_join(poblacion, by = "CODINE011") %>% 
  left_join(nived, by = "CODINE011") %>% 
  st_as_sf(wkt = crs_utm) %>% 
  mutate(area = st_area(.)/10000,
         densidad = poblacion/area,
         violencia = lengths(st_intersects(geometry, violencia))) %>% 
  drop_na() 

# manzanas en version puntos
mz_point <- mz_lc %>% 
  st_centroid() 

# visualizamos
ggplot() +
  geom_sf(data=mz_lc) + 
  geom_sf(data = violencia) 



# *************************
# Kernel density ----
# *************************

# Calculo de Hotspots con radios mas y menos extensos de agregacion
# extraigo puntos para la funcion kde2d
pts <- violencia$geometry %>% unlist() %>% matrix(nrow=2) %>% t()

del_hotspots_1 <- kde2d(pts[,1], pts[,2], h = 1500, n = 100)
image(del_hotspots_1, col = viridis::viridis(100), main='Densidad de Delitos Violentos 0.06')


del_hotspots_2 <- kde2d(pts[,1], pts[,2], h = 3000, n = 100)
image(del_hotspots_2, col = viridis::viridis(100), main='Densidad de Delitos Violentos 0.03')


# *************************
# Ponderacion por distancia ----
# *************************

gs <- gstat(formula = violencia~1, locations = mz_lc)

rast <- raster(mz_lc, res=100)

idw <- interpolate(rast, gs)

plot(idw, col = viridis::viridis(100), main='Densidad de Delitos KNN')


# ************
# Kriging ----
# ************


formMod <- EDUC ~ 1
variog_empirico <- variogram(formMod, mz_point)
plot(variog_empirico)

variog_teorico <- fit.variogram(variog_empirico, 
                                model = vgm(model  = "Exp", nugget = 0.8))
plot(variog_teorico, cutoff = 4300)

# Prediccion tipo Kriging
modelo_krige <- krige(formula = formMod ,
                locations = mz_point, 
                model = variogFitOLS,
                newdata = mz_point,
                debug.level = 0)

plot(modelo_krige)

# ****************
# Regresiones ----
# ****************


# modelo de regresion convencional
modviol <- lm(violencia ~ log(densidad) + EDUC, data = mz_point)
summary(modviol)

## Crear matriz de pesos espaciales
nb <- nb2listw(neighbours = knn2nb(
  knn = knearneigh(x = mz_point, k = 12)), 
  style = "W")

# Modelos de regresion espacial

#Error espacial
fit.errdurb <- errorsarlm(violencia ~ log(densidad) + EDUC, data = mz_point, 
                         listw = nb, etype="error", method="eigen")
summary(fit.errdurb)
moran.test(fit.errdurb$residuals, nb) ## Test Moran residuos

#Lag espacial
fit.durb <- lagsarlm(violencia ~ log(densidad) + EDUC, data = mz_point,
                  listw = nb ,type="lag",method="eigen") 
summary(fit.durb)
moran.test(fit.durb$residuals, nb)

#Error y Lag espacial
fit.sac <- sacsarlm(violencia ~ log(densidad) + EDUC, data = mz_point,
                 listw=nb, type="sac", method="eigen")
summary(fit.sac)
moran.test(fit.sac$residuals, nb)


