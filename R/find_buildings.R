library(osmdata)
library(sf)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)

#Get nip data
data.dir <- here::here('data')
nips <- read.csv(file.path(data.dir, "nip_data.csv"))

#Get polygons for buildings
falmouth_bb <- getbb("falmouth ma")
falmouth_q <- opq(falmouth_bb)
fal_buildings <- 
  falmouth_q %>%
  add_osm_feature(key = "building")%>%
  osmdata_sf()
fal_buildings <- fal_buildings$osm_polygons %>% 
  st_transform(3035)

#transform nips data to sf object and reproject to draw circles with accurate radii
s1 <- st_as_sf(nips, coords = c("center_lon", "center_lat"), 
               crs = 4326) %>% 
  st_transform(3035) %>% 
  mutate(id = 1:length(geometry))

dates <- s1 %>%  
  dplyr::select( date, sample) %>% 
  st_set_geometry(NULL)

nip_df <- s1 %>% 
  dplyr::select(date, sample, nips) %>% 
  st_set_geometry(NULL)


mod_df <- NULL
for (i in seq(10,200,10)){
  
  #Draw circle with 0.1 mile radius
  s1_circles <- st_buffer(s1, dist = i) 
  
  #Intersect circles with building polygons. How many fall into each circle?
  found_buildings <- s1_circles %>%
    st_intersection(fal_buildings) %>% 
    st_transform(4326) %>%  
    group_by(date, sample) %>% 
    dplyr::summarise(n_buildings = n()) %>% 
    right_join(.,dates, by = c("date","sample")) %>% 
    mutate(n_buildings = ifelse(is.na(n_buildings), 0, n_buildings)) %>% 
    st_set_geometry(NULL)
  
   
    
   mod <- glm(nip_df$nips ~ found_buildings$n_buildings, family = "poisson")  
    print(mod)
   mod_summ <- summary(mod)
   
   mod_collect <- data.frame(AIC = mod_summ$aic,
              P = mod_summ$coefficients[8],
              circle_radius = i)
   
   assign("mod_df",rbind(mod_df, mod_collect))
}

#Plot model performance against circle radii
ggplot(data = mod_df) +
  geom_point(aes(y = AIC, x = circle_radius))

best_dist <- dplyr::filter(mod_df, AIC == min(AIC))

################## Fit best model again and plot results

#Draw circle with 0.1 mile radius
s1_circles <- st_buffer(s1, dist = best_dist$circle_radius) 

#Intersect circles with building polygons. How many fall into each circle?
found_buildings <- s1_circles %>%
  st_intersection(fal_buildings) %>% 
  st_transform(4326) %>%  
  group_by(date, sample) %>% 
  dplyr::summarise(n_buildings = n()) %>% 
  right_join(.,dates, by = c("date","sample")) %>% 
  mutate(n_buildings = ifelse(is.na(n_buildings), 0, n_buildings)) %>% 
  st_set_geometry(NULL)

#fit model
mod <- glm(nip_df$nips ~ found_buildings$n_buildings, family = "poisson")  
null_mod <- glm(nip_df$nips ~ 1, family = "poisson")

#create new df for plotting
plot_df <- data.frame(nips = nip_df$nips,
                      buildings = found_buildings$n_buildings,
                      prediction = predict(mod))

#Get inverse of link function to transform model output back to interpretable data
ilink <- family(mod)$linkinv

## add fit and se.fit on the link scale
ndata <- plot_df
ndata <- bind_cols(plot_df, 
                   setNames(as_tibble(predict(mod, ndata, se.fit = TRUE)[1:2]),
                            c('fit_link','se_link')))

## create the confidence interval for the prediction (fit_resp)
ndata2 <- ndata %>% 
  mutate(fit_resp  = ilink(fit_link),
         right_upr = ilink(fit_link + (2 * se_link)),
         right_lwr = ilink(fit_link - (2 * se_link)))

anova(mod, null_mod, test = "Chisq")

#Plot it
ggplot(data = ndata2) +
  geom_point(aes(x = buildings,
                 y = nips)) +
  geom_line(aes(x = buildings, y = fit_resp)) +
  geom_ribbon(aes(x = buildings, 
                  ymin = right_lwr, ymax = right_upr), alpha = 0.1) +
  ggtitle("Nip litter ~ building density")
