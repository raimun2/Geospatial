# Cargar Librerias
pacman::p_load(tidyverse, sf, MASS, gstat, raster, 
               spdep, spatialreg, patchwork)


# manzanas las condes
mz_lc = read_rds("data/MBHT_LC.rds") 

# verificamos proyeccion
st_crs(mz_lc)

# visualizamos
ggplot() +
  geom_sf(data = mz_lc, aes(fill=ibt)) 

# exploramos los valores de ingresos
mz_lc$ibt
hist(mz_lc$ibt, main=NULL)
boxplot(mz_lc$ibt, horizontal = TRUE)
mz_lc$ibt |> summary()


# variograma ----

# manzanas en version puntos
mz_point = mz_lc |>
  st_centroid()

formMod = ibt ~ 1
variog_empirico = variogram(formMod, mz_point, cutoff=5000)

variog_teorico = fit.variogram(variog_empirico, 
                                model = vgm(model  = "Sph", nugget = 0.48))


ggplot() + aes(dist, gamma) +
  geom_point(data=variog_empirico) + 
  geom_line(data=variogramLine(variog_teorico, maxdist = max(5000)))

nvec = 12
knn_nb = spdep::nb2listw(neighbours = spdep::knn2nb(
  knn = spdep::knearneigh( x = st_centroid(mz_lc), k = nvec, longlat = F)),
  style = "W")

## moran ----


# calculamos el indice global de moran
I = moran(mz_lc$ibt, knn_nb, length(knn_nb), Szero(knn_nb))
I

# calculamos el pvalue del indice
moran.test(mz_lc$ibt, knn_nb, alternative="greater")

# Moran local y significancia

lmoran = localmoran(mz_lc$ibt, knn_nb) |> as.data.frame()

# generamos lo cuadrantes de moran
matrix = data.frame(value = ifelse(mz_lc$ibt > mean(mz_lc$ibt), "H", "L"),
                     correlation = ifelse(lmoran$Ii > mean(lmoran$Ii),"H","L"),
                     significance = lmoran$`Pr(z != E(Ii))`) |> 
  mutate(cuadrant = ifelse(significance < 0.1,paste0(value,correlation),NA))

# traspasamos variable a objeto original
mz_lc$cuadrant = matrix$cuadrant

ggplot() +
  geom_sf(data=mz_lc, aes(fill=cuadrant))

