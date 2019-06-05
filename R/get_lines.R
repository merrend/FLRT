#Script to make GIS data from FLRT sampling efforts usable

library(plotKML)
library(stringr)
library(dplyr)
library(sf)
library(rgdal)
library(ggplot2)


#Find all .GPX files (random samples)
survey_data <- NULL
for (i in list.files(here::here("gis"))){
  if (str_detect(i,"gpx")){
    d <- readGPX(paste0("gis/",i))$tracks[[1]]
    assign("survey_data", rbind(survey_data, d))
  }
}

#Make them into an SPDF
survey_df <- bind_rows(survey_data, .id = "column_label")
coordinates(survey_df) <- ~lon + lat
proj4string(survey_df) <- CRS("+proj=longlat +datum=NAD27 +no_defs")

#Convert to SF
flrt_random_surv <- survey_df %>% as("sf") %>% 
  dplyr::group_by(column_label) %>%
  dplyr::summarise(do_union=FALSE) %>%
  sf::st_cast("LINESTRING")  %>% 
  st_transform(4267)

#Find all KMZ files (created in Google Earth)
flrt_nonrandom_surv <- NULL
for (i in list.files(here::here("gis"))){
  if(str_detect(i,"kmz")){
    # file.rename(from = paste0("gis/",i), to = paste0("gis/",str_replace_all(i, " ", "_")))
    print(i)
    df <- maptools::getKMLcoordinates(textConnection(system(paste0("unzip -p ",here::here("gis",i)), intern = TRUE)))
    df <- data.frame(lon = df[[1]][,1],
                     lat = df[[1]][,2],
                     fn = i)
    assign("flrt_nonrandom_surv",rbind(flrt_nonrandom_surv,df))
  }
}

#Convert to SPDF
coordinates(flrt_nonrandom_surv) <- ~lon + lat
proj4string(flrt_nonrandom_surv) <- CRS("+proj=longlat +datum=NAD27 +no_defs")

#Convert to SF
flrt_nonrandom_surv1 <- flrt_nonrandom_surv %>% as("sf") %>% 
  dplyr::group_by(fn) %>%
  dplyr::summarise(do_union=FALSE) %>%
  sf::st_cast("LINESTRING") %>% 
  st_transform(4267)

#Save output
save(flrt_random_surv, flrt_nonrandom_surv1, file = "data/flrt_data.rdata")
