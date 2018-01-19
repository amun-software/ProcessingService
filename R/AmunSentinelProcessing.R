library(raster)
library(graphics)


graphics::par(bg=NA)

test = function(number){
  return (number)
  }

##########################Kanal-Mapping########################################
#' TCI Function
#' @param channel1 First Channel of the image
#' @param channel2 Second Channel of the image
#' @param channel3 Third Channel of the image 
#' @return returns an RGB image consisting of the input channels as R,G and B
TCI = function(channel1,channel2,channel3){
  r = raster::raster(channel1)
  g = raster::raster(channel2)
  b = raster::raster(channel3)
  RGB_brick= raster::brick(r,g,b)
  RGB_brick[RGB_brick<=0] = NA
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::plotRGB(RGB_brick,r=1,g=2,b=3,stretch="hist",bgalpha=0)
}

#' Delivers the TCI image as grayscale
#' @param img The image which will be displayed as grayscale
#' @return returns grayscale image
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

#' Function to adjust the contrast of an image
#' @param band Band to be adjusted
#' @param min new min value
#' @param max new max value
#' @return new scaled image
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

#' Display the image with new contrast
#' @param band Band to be displayed
#' @param min min contrast value
#' @param max max contrast value
#' @return Displays the image with new Contrast
getContrastSeperatly = function(band,min,max){
  Contrast(band,min,max)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image(output,axes=FALSE,legend=FALSE, frame=FALSE, col=base::rev(terrain.colors(3)))
  
}

#' Scales the image from 0 to 255
#' @param rasterBand image which contrast should be adjusted
#' @param minValue new min value of image
#' @param maxValue new max value of image
#' @return scaled values (0 to 255)
scaling = function (rasterBand,minValue,maxValue){
  ((rasterBand-minValue)*255 / (maxValue -minValue) + 0)
}

#' Plots RGB with new contrast
#' @param red redBand
#' @param green greenBand
#' @param blue blueBand
#' @param rmin RedBand min value
#' @param rmax redBand max value
#' @param gmin greenBand min value
#' @param gmax greenBand max value
#' @param bmin blueBand min value
#' @param bmax blueBand max value
#' @return returns RGB image with new Contrast
ContrastRGB = function(red,green,blue,rmin,rmax,gmin,gmax,bmin,bmax){
  r = Contrast(red,rmin,rmax)
  g = Contrast(green,gmin,gmax)
  b = Contrast(blue,bmin,bmax)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::plotRGB(RGB_brick,r=1,g=2,b=3,stretch="lin")
}


#NDVI Formula: (NIR-red)/(NIR+red) 
#' returns values based on NDVI formula (NIR-red)/(NIR+red)
#' @param x NIR band
#' @param y Red band
NDVI = function(x,y){
  result = (x-y)/(x+y)
  return (result)
}

#' Returns the NDVI image
#' @param x NIR Band
#' @param y Red band
#' @return return NDVI image
NDVI_Result = function(x,y){
  output = raster::overlay(raster::raster(base::normalizePath(x)), raster::raster(base::normalizePath(y)), fun = NDVI)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image(output,axes=FALSE,legend=FALSE, frame=FALSE, col=base::rev(terrain.colors(3)))
  
  
}
