install.packages("raster")
library(raster)

#NDVI Formula: (NIR-red)/(NIR+red)
NDVI = function(x,y){
  result = (x-y)/(x+y)
  return (result)
}

#return result
NDVI_Result = function(x,y){
  overlay(raster(x), raster(y), fun = NDVI)
}

Test = function(){}
  
