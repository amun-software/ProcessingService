library(raster)
library(graphics)
library(gsubfn)


graphics::par(bg=NA)

test = function (...){
  base::exists("...")
  a = raster::raster(B01)
  b = raster::raster(B02)
  return(a+b)
  }

##########################Kanal-Mapping########################################
#' TCI Function
#' @param channel1 First Channel of the image
#' @param channel2 Second Channel of the image
#' @param channel3 Third Channel of the image 
#' @return returns an RGB image consisting of the input channels as R,G and B
RGB = function(channel1,channel2,channel3,rmin,rmax,gmin,gmax,bmin,bmax){
  contrast = ContrastRGB(channel1,channel2,channel3,rmin,rmax,gmin,gmax,bmin,bmax)
  contrast[contrast<=0] = NA
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::plotRGB(contrast,r=1,g=2,b=3,bgalpha=0)
}

#' Delivers the TCI image as grayscale
#' @param img The image which will be displayed as grayscale
#' @return returns grayscale image
TCI_grayscale = function(img,min,max){
  contrast = getContrastSeperatly(img,min,max)
  contrast[contrast<=0] = NA
  grayscale = gray.colors(100, # number of colors
                          start = 0.0, #black
                          end = 1.0, # white
                          gamma=2.2, #correction from linear to nonlinear (conversion from camera to human eye)
                          alpha = NULL)# transparency
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image(contrast,col=grayscale,axes=FALSE,legend=FALSE, frame=FALSE)
  
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
  rasterBand[rasterBand<=newMin] = newMin
  rasterBand[rasterBand>=newMax] = newMax
  minValue = raster::minValue(rasterBand)
  maxValue = raster::maxValue(rasterBand)
  scale = scaling(rasterBand,minValue,maxValue)
  return (scale)
  
}

#' Display the image with new contrast
#' @param band Band to be displayed
#' @param min min contrast value
#' @param max max contrast value
#' @return Displays the image with new Contrast
getContrastSeperatly = function(band,min,max){
  Contrast(band,min,max)  
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
  raster = raster::brick(r,g,b)
  return(raster)
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
#' Returns the Image of the defined expression, e.g. B01+B02-B03
#' @param string expression
#' @param ... different Bands
#' @return result Image
expressionResult = function(string,...){
  list = base::list(...)
  test = base::do.call(base::cbind,list)
  regex = "(B[0-1][0-9]|A[126]0mB[0-1][0-9])"
  firstCheck = base::gsub("m/","m",string)
  check = base::gsub(regex, "raster::raster(\\1)", firstCheck)
  sapply1 = base::lapply(check,function(x) gsubfn::gsubfn(regex,list,x))
  addQuotesF = base::gsub("raster\\(","raster('",sapply1)
  addQuotesB = base::gsub(".png",".png'",addQuotesF)
  result = base::lapply(addQuotesB, function(x) base::eval(base::parse(text=x)))
  result2= base::do.call(base::cbind, result)
  graphics::par(bg=NA,mar=c(0,0,0,0),oma=c(0,0,0,0))
  raster::image((result2[[1]]),axes=FALSE,legend=FALSE, frame=FALSE, col=base::rev(terrain.colors(3))) 
}
                        
  

