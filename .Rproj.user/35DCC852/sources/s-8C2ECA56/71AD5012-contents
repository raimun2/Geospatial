library(geoR)

data(package = "geoR")    

data(wolfcamp)              # carga el archivo de datos wolfcamp
summary(wolfcamp)

# Los gráficos de dispersión de los datos frente a las coordenadas nos pueden ayudar a determinar si hay una tendencia. 

plot(wolfcamp)

# También, en lugar del histograma, nos puede interesar un gráfico de dispersión 3D


plot(wolfcamp, lowess = TRUE, scatter3d = TRUE) 


# Si se asume que hay una tendencia puede interesar eliminarla:

plot(wolfcamp, trend=~coords)

# El comando points(geodata) (función points.geodata) genera un gráfico con las posiciones de los datos 
# (y por defecto con el tamaño de los puntos proporcional al valor):
  
points(wolfcamp)

# Se pueden establecer los tamaños de los puntos, simbolos y colores a partir de los valores de los datos
points(wolfcamp, col = "gray", pt.divide = "equal")


## modelando la tendencia


data(s100) # Cargar datos estacionarios
summary(s100)


plot(s100)

# variogramas empiricos

oldpar <- par(mfrow=c(1,2)) 
plot(variog(s100))


plot(variog(s100, max.dist = 0.6))


# La recomendación es considerar solo saltos hasta la mitad de la máxima distancia (ver ‘Distance summary’ en resultados del sumario).

vario <- variog(s100, max.dist = 0.6)

names(vario)

# Los resultados pueden ser nubes de puntos (semivarianzas), valores discretizados (binned) o suavizados, dependiendo del parámetro op

vario.b <- variog(s100, max.dist = 0.6, op = "bin") #discretizado

vario.c <- variog(s100, max.dist=0.6, op="cloud")  #nube

vario.bc <- variog(s100, max.dist=0.6, bin.cloud=TRUE)  #discretizado+nube

vario.s <- variog(s100, max.dist=0.6, op="sm", band=0.2)  #suavizado

oldpar<-par(mfrow=c(2,2)) # Preparar para 4 gráficos por ventana
plot(vario.b, main="Variograma empírico")
plot(vario.c, main="Nube de puntos variograma")
plot(vario.bc, bin.cloud=TRUE, main="Graficos de cajas")
title("Gráficos de cajas") # Corregir fallo del comando anterior
plot(vario.s, main="Variograma suavizado")

par(oldpar) # Restaurar opciones de gráficos

# Si hay valores atípicos (o la distribución de los datos es asimétrica) puede ser preferible utilizar el estimador robusto
varior.b <- variog(s100, estimator.type = "modulus", max.dist=0.6)
varior.bc <- variog(s100, estimator.type = "modulus", max.dist=0.6, bin.cloud=TRUE)

oldpar<-par(mfrow=c(2,2)) #Preparar para 4 gráficos por ventana
plot(vario.b, main="Estimador clásico")
plot(varior.b, main="Estimador robusto")
plot(vario.bc, bin.cloud=TRUE)
plot(varior.bc, bin.cloud=TRUE)

par(oldpar) #Restaurar opciones de gráficos

# En el caso de anisotropía, también se pueden obtener variogramas direccionales con la función variog mediante los argumentos direction y tolerance

vario.60 <- variog(s100, max.dist = 0.6, direction = pi/3) #variograma en la dirección de 60 grados


# Para estudiar si hay anisotropía, se pueden cálcular de forma rápida variogramas direccionales con la función variog4. 
# Por defecto calcula cuatro variogramas direccionales, correspondientes a los ángulos 0, 45, 90 y 135 grados:
vario.4 <- variog4(s100, max.dist = 0.6)

oldpar <- par(mfrow=c(1,2))
plot(vario.60)
title(main = expression(paste("direccional, angulo = ", 60 * degree)))
plot(vario.4, lwd = 2)

par(oldpar)


# variograma teorico


# se ajusta el modelo teorico a la data usando minimos cuadrados ordinarios o ponderados

# ajuste al ojo
vario.b <- variog(s100, max.dist=0.6)

vario.s <- variog(s100, max.dist=0.6,option = "smooth", kernel = "normal", band = 0.2)  #suavizado

plot(vario.b)
lines(vario.s, type = "l", lty = 2)

lines.variomodel(cov.model = "exp", cov.pars = c(1,0.3), nugget = 0, max.dist = 0.6, lwd = 3)
legend(0.3, 0.3, c("empirico", "suavizado", "modelo exponencial"), lty = c(1, 2, 1), lwd = c(1, 1, 3))

# otros modelos
plot(vario.b)
lines.variomodel(cov.model = "exp", cov.pars = c(0.9,0.3), nug = 0.1, max.dist = 0.6)
lines.variomodel(cov.model = "mat", cov.pars = c(0.85,0.2), nug = 0.1, kappa = 1, max.dist = 0.6,lty = 2)
lines.variomodel(cov.model = "sph", cov.pars = c(0.8,0.8), nug = 0.1, max.dist = 0.6, lwd = 2)


# ajuste con OLS
vario.ols <- variofit(vario.b, ini = c(1, 0.5), weights = "equal")  #ordinarios

# ajuste con wls
vario.wls <- variofit(vario.b, ini = c(1, 0.5), weights = "cressie")  #ponderados


summary(vario.wls)

# ajuste con verosimilitud

vario.ml <- likfit(s100, ini = c(1, 0.5)) #Modelo exponencial con par ini umbral y escala (1/3 rango)

vario.ml

summary(vario.ml)

# ajuste con max verosimilitud resrtingida
vario.reml <- likfit(s100, ini = c(1, 0.5), lik.method = "RML")

summary(vario.reml)

plot(vario.b, main = "Estimador empírico y modelos ajustados")
lines(vario.ml, max.dist = 0.6)
lines(vario.reml, lwd = 2, max.dist = 0.6)
lines(vario.ols, lty = 2, max.dist = 0.6)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.6)
legend(0.3, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1, 1, 2, 2), lwd = c(1, 2,1, 2)) 


# inferencia sobre el variograma

# usando envolventes independientes
env.indep <- variog.mc.env(s100, obj.var = vario.b)

# usando modelo de variograma teorico
env.model <- variog.model.env(s100, obj.var = vario.b, model = vario.wls)

oldpar <- par(mfrow = c(1, 2))
plot(vario.b, envelope = env.indep)
plot(vario.b, envelope = env.model)
lines(vario.wls, lty = 2, lwd = 2, max.dist = 0.6)

par(oldpar)     

# validacion cruzada
xv.wls <- xvalid(s100, model = vario.wls)
summary(xv.wls)

xv.reml <- xvalid(s100, model = vario.reml)
summary(xv.reml)


oldpar <- par(mfrow = c(2, 5))
plot(xv.wls, ask = FALSE)

par(oldpar)

