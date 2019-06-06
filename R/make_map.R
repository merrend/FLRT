library(sf)
library(ggmap)
library(ggplot2)
library(leaflet)

# Add FLRT data to a map of Falmouth. Note: You won't be able to use ggmap without first getting an
# API key to query google
# See https://github.com/dkahle/ggmap

falmouth <- c(-70.693531,41.53, -70.448287,  41.625940)

load(file = "data/flrt_data.rdata")

falma <- get_map(falmouth,source = "google",maptype = "roadmap") 
ggmap(falma) +
  geom_sf(data = flrt_nonrandom_surv1, inherit.aes = F, color = "red") +
  geom_sf(data = flrt_random_surv, inherit.aes = F, color = "black") +
  ylab("Latitude") +
  xlab("Longitude") +
  ggtitle("FLRT Litter Survey") +
  guides(col = guide_colorbar(title = "Sampling design"))
# coord_sf(xlim = c(-70.675,-70.5),ylim = c(41.525,41.65))