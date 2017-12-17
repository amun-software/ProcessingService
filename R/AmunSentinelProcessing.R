library(raster)
library(graphics)

#NDVI Formula: (NIR-red)/(NIR+red) j
NDVI = function(x,y){
  result = (x-y)/(x+y)
  return (result)
}

#return result
NDVI_Result = function(x,y){
  output = overlay(raster(x), raster(y), fun = NDVI)
  raster::plot(output,axes=FALSE, box=FALSE, legend=FALSE)
  
  
}
  
