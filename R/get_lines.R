#Script to make GIS data from FLRT sampling efforts usable

library(plotKML)
library(stringr)
library(dplyr)
library(sf)
library(rgdal)
library(ggplot2)
library(janitor)
library(readxl)


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
                     location = i)
    assign("flrt_nonrandom_surv",rbind(flrt_nonrandom_surv,df))
  }
}

flrt_nonrandom_surv <- flrt_nonrandom_surv %>% 
  mutate(location = str_remove(location,".kmz"))

td <- read_excel("data/FLRT_survey.xlsx", sheet = 2, col_names = F, na = "") %>% 
  dplyr::rename(Var = `...1`,
                Var2 = `...2`,
                Var3 = `...3`) %>% 
  tidyr::unite(., Var, c("Var","Var2","Var3"), sep = " ") %>% 
  mutate(Var = str_replace_all(Var, "NA", " ")) %>% 
  mutate(Var = str_squish(Var)) %>% 
  gather(L, Value, -Var)

dates <- unique(td[td$Var == c("Date"),]) %>% 
  dplyr::select(L, Value) 

location <- unique(td[td$Var == c("Location from / to"),]) %>% 
  dplyr::select(L, Value) 

distance <- unique(td[td$Var == c("Road Length miles"),]) %>% 
  dplyr::select(L, Value) 

out <- td %>% 
  left_join(., dates, "L") %>% 
  left_join(., location, "L") %>% 
  left_join(., distance, "L") %>% 
  dplyr::select(-L) %>% 
  dplyr::rename(count = Value.x,
                date = Value.y,
                location = Value.x.x,
                distance = Value.y.y) %>% 
  filter(!str_detect(Var,"Date|Location|Road Length miles|Weight \\(lbs\\) lbs - bathroom scale")) %>% 
  mutate(date = excel_numeric_to_date(as.numeric(date)),
         count = ifelse(is.na(count),0,count),
         class = ifelse(str_detect(Var, "Nips"), "Nips",
                        ifelse((str_detect(Var, "bottles") & !str_detect(Var, "Nips")),"bottles",
                               ifelse(str_detect(Var, "cans"),"cans",
                                      ifelse(str_detect(Var, "bags"), "bags",
                                             ifelse(str_detect(Var,"cups"),"cups",NA))))),
         use = ifelse(str_detect(Var, "lcohol|Nips"), "alcohol",
                      ifelse(str_detect(Var, "food"), "Food service",NA)),
         count = as.numeric(count)) %>% 
  unite(use_class, c("use","class"), remove = F, sep = " ") %>% 
  mutate(use_class = str_squish(str_replace_all(use_class,"NA |NA","")),
         Var = ifelse(str_detect(Var, "paraphernalia"),"Cig. para.",
                      ifelse(str_detect(Var, "Nips"),"Nips",
                             ifelse(str_detect(Var, "food service"),
                                    "Food service packaging",Var)))) %>% # Add FLRT data to a map of Falmouth. Note: You won't be able to use ggmap without first getting an
  filter(Var == "Nips") %>% 
  as.data.frame()

flrt_nonrandom_surv <- flrt_nonrandom_surv %>% 
  left_join(.,out,by = "location")

#Convert to SPDF
coordinates(flrt_nonrandom_surv) <- ~lon + lat
proj4string(flrt_nonrandom_surv) <- CRS("+proj=longlat +datum=NAD27 +no_defs")

#Convert to SF
flrt_nonrandom_surv1 <- flrt_nonrandom_surv %>% as("sf") %>% 
  dplyr::group_by(location, count) %>%
  dplyr::summarise(do_union=FALSE) %>%
  sf::st_cast("LINESTRING") %>% 
  st_transform(4267)

#Save output
save(flrt_random_surv, flrt_nonrandom_surv1, file = "data/flrt_data.rdata")
