pacman::p_load(sf, tidyverse)

# transformacion entre tipos de datos

# puntos
st_point(c(1,1)) %>% st_cast("MULTIPOINT")
st_multipoint(rbind(c(1,1))) %>% st_cast("POINT")
st_multipoint(rbind(c(1,1),c(2,2))) %>% st_cast("POINT")
st_geometrycollection(list(st_point(c(1,1)))) %>% st_cast("POINT")

# lineas
ls <- st_linestring(rbind(c(0,0),c(1,1),c(2,1)))
mls1 <- st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))
mls2 <- st_multilinestring(list(rbind(c(4,4),c(4,3)), rbind(c(2,2),c(2,1),c(3,1))))
(sfc <- st_sfc(ls,mls1,mls2))
st_cast(sfc, "MULTILINESTRING")

# poligonos
shp <- system.file("shape/nc.shp", package="sf")
class(st_geometry(st_read(shp, quiet = TRUE)))
class(st_geometry(st_read(shp, quiet = TRUE, type = 3)))
class(st_geometry(st_read(shp, quiet = TRUE, type = 1)))

# multigeometrias
gc1 <- st_geometrycollection(list(st_linestring(rbind(c(0,0),c(1,1),c(2,1)))))
gc2 <- st_geometrycollection(list(st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))))
gc3 <- st_geometrycollection(list(st_multilinestring(list(rbind(c(4,4),c(4,3)), rbind(c(2,2),c(2,1),c(3,1))))))
(sfc <- st_sfc(gc1,gc2,gc3))

# tranformaciones algebraicas
(p <- st_point(c(0,2)))
p + 1 # sumo el mismo valor a ambas coordenadas
p + c(1,2) #sumo diferentes valores por coordenadas
p + p # sumo 2 veces el mismo vector
p * p # multiplico cada valor por si mismo

# creo una funcion de rotacion
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
p * rot(pi/4)
p * rot(pi/2)
p * rot(pi)

# centroides
nc <- st_read(shp)
ncg <- st_geometry(nc)
plot(ncg, border = 'grey')
cntrd <- st_centroid(ncg)
ncg2 <- (ncg - cntrd) * rot(pi/2) * .75 + cntrd
plot(ncg2, add = TRUE)
plot(cntrd, col = 'red', add = TRUE, cex = .5)

# operaciones geometricas

# operaciones simples
b0 <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(0,-1), c(-1,-1))))
st_is_valid(st_sfc(b0,b1))
plot(b1)

s <- st_sfc(st_linestring(rbind(c(0,0), c(1,1))), 
            st_linestring(rbind(c(0,0), c(1,1),c(0,1),c(1,0))))
st_is_simple(s)

# operaciones unitarias

b0 <- st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))
b1 <- b0 + 2
b2 <- b0 + c(-0.2, 2)
x <- st_sfc(b0, b1, b2)
a0 <- b0 * 0.8
a1 <- a0 * 0.5 + c(2, 0.7)
a2 <- a0 + 1
a3 <- b0 * 0.5 + c(2, -0.5)
y <- st_sfc(a0,a1,a2,a3)
plot(x, border = 'red')
plot(y, border = 'green', add = TRUE)

# calculo de area y longitud
st_area(x)
st_area(st_sfc(st_point(c(0,0))))
st_length(st_sfc(st_linestring(rbind(c(0,0),c(1,1),c(1,2))), st_linestring(rbind(c(0,0),c(1,0)))))

# calculo de distancias
st_distance(x,y)
st_relate(x,y)

# comparaciones
st_intersects(x,y)
st_intersects(x, x, sparse = FALSE)
st_intersects(x, y, sparse = FALSE)
st_disjoint(x, y, sparse = FALSE)
st_touches(x, y, sparse = FALSE)
st_crosses(s, s, sparse = FALSE)
st_within(x, y, sparse = FALSE)
st_contains(x, y, sparse = FALSE)
st_overlaps(x, y, sparse = FALSE)
st_equals(x, y, sparse = FALSE)
st_equals_exact(x, y, 0.001, sparse = FALSE)
st_covers(x, y, sparse = FALSE)
st_covered_by(x, y, sparse = FALSE)
st_covered_by(y, y, sparse = FALSE)


# operaciones binarias
(u <- st_union(x))
plot(u)
# 
plot(x)
plot(st_centroid(x), add = TRUE, col = 'red')
plot(u)
plot(st_centroid(u), add = TRUE, col = 'red')

# buffer
(b <- st_buffer(x, 0.2))
plot(b)
plot(st_buffer(x, -0.2))

# frontera
plot(st_boundary(x))
plot(st_boundary(u))

# casco convexo
plot(st_convex_hull(x))
plot(st_convex_hull(u))

# circunferencia inscrita
plot(st_inscribed_circle(x))
plot(st_inscribed_circle(u))

# interseccion
plot(x)
plot(y, add = TRUE)
plot(st_intersection(st_union(x),st_union(y)), add = TRUE, col = 'red')

# differencias
par(mfrow=c(2,2), mar = c(0,0,1,0))
plot(x, col = '#ff333388'); 
plot(y, add=TRUE, col='#33ff3388')
title("x: red, y: green")
plot(x, border = 'grey')
plot(st_difference(st_union(x),st_union(y)), col = 'lightblue', add = TRUE)
title("difference(x,y)")
plot(x, border = 'grey')
plot(st_difference(st_union(y),st_union(x)), col = 'lightblue', add = TRUE)
title("difference(y,x)")
plot(x, border = 'grey')
plot(st_sym_difference(st_union(y),st_union(x)), col = 'lightblue', add = TRUE)
title("sym_difference(x,y)")

