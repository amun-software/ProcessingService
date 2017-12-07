library(raster)

#NDVI Formula: (NIR-red)/(NIR+red) j
NDVI = function(x,y){
  result = (x-y)/(x+y)
  return (result)
}

#return result
NDVI_Result = function(x,y){
  output = overlay(raster(x), raster(y), fun = NDVI)
  return(output)
  
  
}
  
