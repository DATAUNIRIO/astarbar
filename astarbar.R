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
  select(name, x ,y)  %>%
  rbind(mespoints %>% filter(name == "start"))

end <- mespoints %>% filter(name == "end")

list.of.samples <- replicate(1000, sample(1:100,size=10), simplify=FALSE)
#initialiser les trajets
open <- vector("list", 10000)
#l <- list(replicate(1000,NA_character_))
#l <- map(1:1000, ~ NULL)
#l <- map(1:1000, ~ NA_character_)
open[[1]] <- "start"

# get distance to data
d_parcouru <- c(rep(NA_integer_, 10000))
d_parcouru[1] <- 0

#get distance to end
d_left <- c(rep(NA_integer_, 10000))
d_left[1] = as.numeric(st_distance(end, points %>% filter(name== "start")))

# get total distance
d_total <- c(rep(NA_integer_, 10000))
d_total[1] = d_parcouru[1] + d_left[1]

# expand best trip
k_to_expand <- which.min(d_total)
open[k_to_expand]
#last_point <- lapply(l[[k_to_expand]], tail,1)
last_point <- open[[k_to_expand]][length(open[[k_to_expand]])]
  
  
# get list of possible destinations  (do not go back to already visited, and only go to end after 4 points including start)
if(length(open[[k_to_expand]] )==4){
  dests <- "end"} else{
    dests <- points %>% filter(!(name %in% open[[k_to_expand]])) %>% pull(name)
  }
dests

# find empty spots in list
A = map_lgl(open, is.null) %>% which() %>% .[1:length(dests)]
i = 1 
for (dest in dests){ 
  open[[A[i]]] <- c(open[[k_to_expand]], dest)
  d_parcouru[A[i]] <- d_parcouru[k_to_expand] + 
    as.numeric(st_distance(points %>% filter(name == last_point), 
                           points %>% filter(name == dest)))
  d_left[A[i]] <- as.numeric(st_distance(points %>% filter(name == dest), end))
  d_total[A[i]] <- d_parcouru[A[i]] + d_left[A[i]]
  if(dest == "end" & d_total[A[i]] <= min(d_total) & d_total[A[i]] <= min(closed_d_total)){
    win==1}
  
  i <- i + 1
}
# effacer le k qu'on vient d'expand


#l[k_to_expand] <- NA_character_
open[k_to_expand] <- list(NULL)
d_parcouru[k_to_expand]<- NA_integer_
d_left[k_to_expand] <-  NA_integer_
d_total[k_to_expand] <-  NA_integer_


open[1:20]
d_parcouru[1:20]
d_left[1:20]
d_total[1:20]

