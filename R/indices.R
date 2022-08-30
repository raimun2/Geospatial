GCI <- function(rasterstack_l8){
  return(rasterstack_l8$B5/rasterstack_l8$B3 - 1)
}

NVDI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - rasterstack_l8$B4)/(rasterstack_l8$B5 + rasterstack_l8$B4))
}

GNVDI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - rasterstack_l8$B3)/(rasterstack_l8$B5 + rasterstack_l8$B3))
}

EVI <- function(rasterstack_l8){
  return( 2.5 * (rasterstack_l8$B5 - rasterstack_l8$B4) / 
                                 (rasterstack_l8$B5 + 6*rasterstack_l8$B4 - 7.5*rasterstack_l8$B2 + 1))
}

SAVI <- function(rasterstack_l8){
  return((1+0.5)*(rasterstack_l8$B5 - rasterstack_l8$B4)/(rasterstack_l8$B5 + rasterstack_l8$B4 + 0.5))
}

AVI <- function(rasterstack_l8){
  return((rasterstack_l8$B5*(1 - rasterstack_l8$B4)*(rasterstack_l8$B5 + rasterstack_l8$B4))^(1/3))
}

ARVI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - 2*rasterstack_l8$B4 + rasterstack_l8$B2)/
           (rasterstack_l8$B5 + 2*rasterstack_l8$B4 + rasterstack_l8$B2))
}

SIPI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - rasterstack_l8$B2)/(rasterstack_l8$B5 + rasterstack_l8$B4))
}

BSI <- function(rasterstack_l8){
  return(((rasterstack_l8$B4 + rasterstack_l8$B6) - (rasterstack_l8$B5 + rasterstack_l8$B2)) /
           ((rasterstack_l8$B4 + rasterstack_l8$B6) + (rasterstack_l8$B5 + rasterstack_l8$B2)))
}

NDMI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - rasterstack_l8$B6)/(rasterstack_l8$B5 + rasterstack_l8$B6))
}

MSI <- function(rasterstack_l8){
  return((rasterstack_l8$B6)/(rasterstack_l8$B5))
}

NBRI <- function(rasterstack_l8){
  return((rasterstack_l8$B5 - rasterstack_l8$B6)/(rasterstack_l8$B5 + rasterstack_l8$B6))
}

NDWI <- function(rasterstack_l8){
  return((rasterstack_l8$B3 - rasterstack_l8$B5)/(rasterstack_l8$B3 + rasterstack_l8$B5))  
}

NDSI <- function(rasterstack_l8){
  return((rasterstack_l8$B3 - rasterstack_l8$B6)/(rasterstack_l8$B3 + rasterstack_l8$B6))
}

NDGI <- function(rasterstack_l8){
  return((rasterstack_l8$B3 - rasterstack_l8$B5)/(rasterstack_l8$B3 + rasterstack_l8$B5))
}

Ca<- function(rasterstack_l8){
  return(-3.533 + 0.455*rasterstack_l8$B4 + 0.321*rasterstack_l8$B5)
}

SS<- function(rasterstack_l8){
  return(-8.093 + 0.278*rasterstack_l8$B3 + 0.429*rasterstack_l8$B4)
}

Tu<- function(rasterstack_l8){
  return(-8.2 + 0.266*rasterstack_l8$B3 + 0.716*rasterstack_l8$B4 + 0.094*rasterstack_l8$B5)
}
