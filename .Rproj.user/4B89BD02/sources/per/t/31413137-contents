pacman::p_load(sf, spdep, tmap, tidyverse)
set.seed(42)

# leemos poligonos desde archivo web
poligonos <- readRDS(url("https://github.com/mgimond/Data/raw/gh-pages/Exercises/nhme.rds"))

# vemos nombres de las variables
names(poligonos)

tm_shape(poligonos) + 
  tm_fill(col="NAME")+
  tm_legend(outside=TRUE) +
  tm_borders()

# exploramos los valores de ingresos
poligonos$Income
hist(poligonos$Income, main=NULL)
boxplot(poligonos$Income, horizontal = TRUE)

# visualizamos graficamente el ingreso
tm_shape(poligonos) + 
  tm_fill(col="Income", style="quantile", n=8, palette="Greens") +
  tm_legend(outside=TRUE)


# comparacion de datos reales con datos random
poligonos$rand1 <- sample(poligonos$Income, length(poligonos$Income), replace = FALSE)


tm_shape(poligonos) + 
  tm_fill(col=c("Income", "rand1"),
          style="quantile", n=8, palette="Greens", legend.show = FALSE) +
  tm_facets(nrow=1)


# generamos lista de vecinos
nb <- poly2nb(poligonos, queen=TRUE)

# generamos pesos a partir de los vecinos
lw <- nb2listw(nb, style="W", zero.policy=TRUE)

# calculamos el indice global de moran
I <- moran(poligonos$Income, lw, length(nb), Szero(lw))[1]
I

# calculamos el pvalue del indice
moran.test(poligonos$Income,lw, alternative="greater")


# calculamos el moran local
lmoran <- localmoran(poligonos$Income, lw) %>% as.data.frame()

# generamos lo cuadrantes de moran
matrix <- data.frame(value = ifelse(poligonos$Income > mean(poligonos$Income), "H", "L"),
                     correlation = ifelse(lmoran$Ii > mean(lmoran$Ii),"H","L"),
                     significance = lmoran$`Pr(z != E(Ii))`) %>% 
  mutate(cuadrant = ifelse(significance < 0.1,paste0(value,correlation),NA))

# traspasamos variable a objeto original
poligonos$cuadrant <- matrix$cuadrant

# dibujamos los cuadrantes
tm_shape(poligonos) + 
  tm_fill(col="cuadrant") +
  tm_legend(outside=TRUE) + 
  tm_borders()

