library(raster)
library(graphics)

graphics::par(bg=NA)

test = function(number){
  return (number)
  }

##########################Kanal-Mapping########################################
TCI = function(channel1,channel2,channel3){
  r = raster::raster(channel1)
  g = raster::raster(channel2)
  b = raster::raster(channel3)
  RGB_brick= raster::brick(r,g,b)
  raster::plotRGB(RGB_brick,r=1,g=2,b=3,stretch="lin",colNA=FALSE,bgalpha=0)
}

TCI_grayscale = function(img){
  rasterIMG = raster::raster(img)
  grayscale = gray.colors(100, # number of colors
                          start = 0.0, #black
                          end = 1.0, # white
                          gamma=2.2, #correction from linear to nonlinear (conversion from camera to human eye)
                          alpha = NULL)# transparency
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image(rasterIMG,col=grayscale,axes=FALSE,legend=FALSE, frame=FALSE)

}

#var = TCI(Band1,Band2,Band3)
#TCI_grayscale(Band1)

##########################Contrast Adjustments#################################################

Contrast = function(band,min,max){
  rasterBand = raster::raster(band)
  newMin = min
  newMax = max
  rasterBand[rasterBand<=newMin] = NA
  rasterBand[rasterBand>=newMax] = NA
  minValue = raster::minValue(rasterBand)
  maxValue = raster::maxValue(rasterBand)
  scale = scaling(rasterBand,minValue,maxValue)
  
}

scaling = function (rasterBand,minValue,maxValue){
  ((rasterBand-minValue)*255 / (maxValue -minValue) + 0)
}

ContrastRGB = function(red,green,blue,rmin,rmax,gmin,gmax,bmin,bmax){
  r = Contrast(red,rmin,rmax)
  g = Contrast(green,gmin,gmax)
  b = Contrast(blue,bmin,bmax)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::plotRGB(RGB_brick,r=1,g=2,b=3,stretch="lin")
}


#NDVI Formula: (NIR-red)/(NIR+red) j
NDVI = function(x,y){
  result = (x-y)/(x+y)
  return (result)
}

#return result
NDVI_Result = function(x,y){
  output = raster::overlay(raster::raster(base::normalizePath(x)), raster::raster(base::normalizePath(y)), fun = NDVI)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image(output,axes=FALSE,legend=FALSE, frame=FALSE, col=base::rev(terrain.colors(3)))
  
  
}
  
