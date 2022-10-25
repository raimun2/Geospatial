### Calculo de Hotspots de Violencia ----

# Cargar Librerias
pacman::p_load(rgdal, rgeos, stars, spatstat, spdep, sf, raster,
               spatialreg, tidyverse, gstat, MASS)


# ***************************
# Cargar y ordenar datos ----

# Datos de delitos
violencia <- read_rds("data/casos_violencia.rds")  %>%  
  st_as_sf()

# Cargar datos censales de nivel educativo en Las Condes a nivel de personas
censo_lc <- readRDS("data/censo_lc.rds") %>% 
  mutate(poblacion = 1,
         CODINE011 = as.character(IDMZ)) %>% 
  dplyr::select(-IDMZ)

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
mz_lc <- readRDS("data/MBHT_LC.rds") %>% 
  st_as_sf() %>% 
  rename(CODINE011 = ID_MANZ) %>% 
  left_join(poblacion, by = "CODINE011") %>% 
  left_join(nived, by = "CODINE011") %>%  
  st_set_crs("+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  mutate(area = st_area(.)/10000,
         densidad = poblacion/area,
         violencia = lengths(st_intersects(geometry, violencia))) 

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

rast <- raster(mz_lc, res=30)

idw <- interpolate(rast, gs)

plot(idw, col = viridis::viridis(100), main='Densidad de Delitos KNN')


# ************
# Kriging ----
# ************

formMod <- violencia ~ 1
variog_empirico <- variogram(formMod, mz_point)

variog_teorico <- fit.variogram(variog_empirico, 
                                model = vgm(model  = "Exp", nugget = 0.8))

plot(variog_teorico, cutoff = 4300, add=TRUE)
plot(variog_empirico)

# Prediccion tipo Kriging
modelo_krige <- krige(formula = formMod ,
                      locations = mz_point, 
                      model = variog_teorico,
                      newdata = mz_point,
                      debug.level = 0)

ggplot(data=mz_lc, aes(fill=modelo_krige$var1.pred)) + 
  geom_sf() + 
  scale_fill_viridis_c()


# ****************
# Regresiones ----
# ****************

dataviolencia = mz_point %>% 
  mutate(violencia = ifelse(is.na(violencia),0,violencia)) %>% 
  drop_na(densidad)

mz_violencia = mz_lc %>% filter(CODINE011 %in% dataviolencia$CODINE011)

# modelo de regresion convencional
modviol <- lm(violencia ~ log(densidad) + EDUC, data = dataviolencia)
summary(modviol)


## Crear matriz de pesos espaciales
nb <- nb2listw(neighbours = knn2nb(
  knn = knearneigh(x = dataviolencia, k = 12)), 
  style = "W")

moran.test(modviol$residuals, nb) ## Test Moran residuos


# Modelos de regresion espacial

#Error espacial
fit.errdurb <- errorsarlm(violencia ~ log(densidad) + EDUC, data = dataviolencia, 
                          listw = nb, etype="error", method="eigen")
summary(fit.errdurb)
moran.test(fit.errdurb$residuals, nb) ## Test Moran residuos


#Lag espacial
fit.durb <- lagsarlm(violencia ~ log(densidad) + EDUC, data = dataviolencia,
                     listw = nb ,type="lag",method="eigen") 
summary(fit.durb)
moran.test(fit.durb$residuals, nb)

#Error y Lag espacial
fit.sac <- sacsarlm(violencia ~ log(densidad) + EDUC, data = dataviolencia,
                    listw=nb, type="sac", method="eigen")
summary(fit.sac)
moran.test(fit.sac$residuals, nb)

mz_violencia <- 
  mz_violencia %>% 
  mutate(reg_lin = predict(modviol), 
         errsar = fitted(fit.errdurb),
         lagsar = fitted(fit.durb),
         sacsar = fitted(fit.sac))

library(patchwork)

p1 <- ggplot(data=mz_violencia) + 
  geom_sf(aes(fill=reg_lin)) +
  scale_fill_viridis_c()

p2 <- ggplot(data=mz_violencia) + 
  geom_sf(aes(fill=errsar)) +
  scale_fill_viridis_c()

p3 <- ggplot(data=mz_violencia) + 
  geom_sf(aes(fill=lagsar)) +
  scale_fill_viridis_c()

p4 <- ggplot(data=mz_violencia) + 
  geom_sf(aes(fill=sacsar)) +
  scale_fill_viridis_c()

(p1 + p2) / (p3 + p4)


