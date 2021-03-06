% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AmunSentinelProcessing.R
\name{scaling}
\alias{scaling}
\title{Scales the image from 0 to 255}
\usage{
scaling(rasterBand, minValue, maxValue)
}
\arguments{
\item{rasterBand}{image which contrast should be adjusted}

\item{minValue}{new min value of image}

\item{maxValue}{new max value of image}
}
\value{
scaled values (0 to 255)
}
\description{
Scales the image from 0 to 255
}
