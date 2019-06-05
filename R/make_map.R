#plots lines
library(sf)
library(raster)
library(ggplot2)

source("R/get_lines.R")


for (i in list.files(here::here("gis"))){
  get_lines(i, )
}

