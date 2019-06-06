library(sf)
library(ggmap)
library(ggplot2)
library(leaflet)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(janitor)

nips <- read.csv('data/nip_data.csv')
load(file = "data/flrt_data.rdata")

falmouth <- c(-70.693531,41.53, -70.448287,  41.625940)


leaflet() %>%
  setView(lng = -70.617672, lat = 41.564279, zoom = 12) %>%
  addTiles() %>% 
  addPolylines(data = flrt_nonrandom_surv1, color = "black", popup = ~paste("Nips found:", as.character(count))) %>% 
  addPolylines(data = flrt_random_surv, color = "red") 