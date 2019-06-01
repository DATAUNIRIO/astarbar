library(osmdata)
library(mapview)
library(tidyverse)
library(sf)

st_x = function(x) st_coordinates(x)[,1]
st_y = function(x) st_coordinates(x)[,2]

bbox <-  getbb('limoilou, canada')

mespoints <- tibble(name = c("start", "end"),
                    lat = c(bbox[2,1], bbox[2,2]),
                    lon = c(bbox[1,1], bbox[1,2])) %>%
  st_as_sf(., coords= c("lon", "lat"), crs = 4326)
mespoints$x <- st_x(mespoints)
mespoints$y <- st_y(mespoints)


pubs <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "pub")  %>%
  osmdata_sf(.) %>% 
  .$osm_points %>%
  select(name)


bars <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "bar")  %>%
  osmdata_sf(.) %>% 
  .$osm_points %>%
  select(name)

pubs_bars <- pubs %>% rbind(bars)
pubs_bars$x <- st_x(pubs_bars)
pubs_bars$y <- st_y(pubs_bars)

points <- pubs_bars %>% 
  filter(!is.na(name))%>%
  select(name, x ,y) %>%
  st_set_geometry(NULL) 
  
start <- mespoints %>% filter(name == "start")
end <- mespoints %>% filter(name == "end")
  
#initialiser les trajets
l <- vector("list", 1000)
l[[1]] <- "start"

# get distance to data



# trouver lequel expand


# expand start
k_to_expand <- 1

# get list of possible destinations
dests <- points$name

# find empty spots in list
A = map_lgl(l, is.null) %>% which() %>% .[1:length(dests)]
i = 1 
for (dest in dests){
  l[[A[i]]] <- c(l[[k_to_expand]], dest)
  i <- i + 1
  
}
# effacer le k qu'on vient d'expand
l[k_to_expand] <- list(NULL) 

l[1:100]
l[1:20]
l[[20]]
