
remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
#ors_api_key("apikey")
library(osmdata)
library(mapview)
library(tidyverse)
library(sf)
library(leaflet)
n_bars <- 3
st_x = function(x) st_coordinates(x)[,1]
st_y = function(x) st_coordinates(x)[,2]

bbox <-  getbb('limoilou, québec')

mespoints <- tibble(name = c("start", "end"),
                    lat = c(bbox[2,1], bbox[2,2]),
                    lon = c(bbox[1,1], bbox[1,2]),
                    osm_id = c("start", "end")) %>%
  st_as_sf(., coords= c("lon", "lat"), crs = 4326)
mespoints$x <- st_x(mespoints)
mespoints$y <- st_y(mespoints)


pubs <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "pub")  %>%
  osmdata_sf(.) %>% 
  .$osm_points  %>% 
  select(name, osm_id)


bars <- opq(bbox = bbox)%>%
  add_osm_feature(key = "amenity", value = "bar")  %>%
  osmdata_sf(.) %>% 
  .$osm_points  %>% 
  select(name, osm_id)

pubs_bars <- pubs %>% rbind(bars)
pubs_bars$x <- st_x(pubs_bars)
pubs_bars$y <- st_y(pubs_bars)

points <- pubs_bars %>% 
  filter(!is.na(name))%>%
  rbind(mespoints %>% filter(name == "start"))

end <- mespoints %>% filter(name == "end")

distance_matrix_input  <-  pubs_bars %>% 
  filter(!is.na(name))%>%
  rbind(mespoints) %>%
  st_set_geometry(NULL)

z <- distance_matrix_input %>% 
  select(x,y) %>%
  ors_matrix(., 
             metrics = c("duration", "distance"), 
             units = "km",
             profile = "foot-walking")

zz <- z$durations %>% as_tibble()
colnames(zz)  <- distance_matrix_input$osm_id
zz$origin = distance_matrix_input$osm_id

durations <- zz %>% 
  gather(key= destination, value  = duration, -origin)


list.of.samples <- replicate(1000, sample(1:100,size=10), simplify=FALSE)
#initialiser les trajets
open <- vector("list", 10000)
closed <- vector("list", 10000)
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

d_parcouru_closed <- c(rep(NA_integer_, 10000))

# while we havent found the best path and there are still open paths..
win  <- 0 
round = 1
while (win==0){
  
  # expand best trip
  k_to_expand <- which.min(d_total)
  
  message("round ", round, " expanding ", open[k_to_expand])
  
  #last_point <- lapply(l[[k_to_expand]], tail,1)
  last_point <- open[[k_to_expand]][length(open[[k_to_expand]])]
  
  
  # get list of possible destinations  (do not go back to already visited, and only go to end after 4 points including start)
  if(length(open[[k_to_expand]] )== n_bars+1){
    dests <- "end"} else{
      dests <- points %>% filter(!(name %in% open[[k_to_expand]])) %>% pull(name)
    }
  dests
  
  # find empty spots in list
  A = map_lgl(open, is.null) %>% which() %>% .[1:length(dests)]
  B = map_lgl(closed, is.null) %>% which() %>% .[1:length(dests)]
  i = 1 
  for (dest in dests){ 
    if (dest != "end"){
      open[[A[i]]] <- c(open[[k_to_expand]], dest)
      d_parcouru[A[i]] <- d_parcouru[k_to_expand] + 
        as.numeric(st_distance(points %>% filter(name == last_point), 
                               points %>% filter(name == dest)))
      d_left[A[i]] <- as.numeric(st_distance(points %>% filter(name == dest), end))
      d_total[A[i]] <- d_parcouru[A[i]] + d_left[A[i]]
    }
    
    if (dest == "end"){
      closed[[B[i]]] <- c(open[[k_to_expand]], dest)
      d_parcouru_closed[B[i]] <- d_parcouru[k_to_expand] + 
        as.numeric(st_distance(points %>% filter(name == last_point), 
                               end))
      if(d_parcouru_closed[B[i]] <= min(d_total, na.rm= T) & d_parcouru_closed[B[i]] <= min(d_parcouru_closed, na.rm = TRUE)){
        win <- 1
        message(paste0("final path = ", paste0(closed[[B[i]]]), " distance: ", d_parcouru_closed[B[i]]))}
    }
    i <- i + 1
  }
  # effacer le k qu'on vient d'expand
  closed[1:20]
  
  #l[k_to_expand] <- NA_character_
  open[k_to_expand] <- list(NULL)
  d_parcouru[k_to_expand]<- NA_integer_
  d_left[k_to_expand] <-  NA_integer_
  d_total[k_to_expand] <-  NA_integer_
  round = round+1
} # fin while

# create best_path
best_path <- points %>% 
  left_join (tibble(name = closed[[1]]) %>% 
               mutate(rank = row_number())) %>%
  arrange(rank) %>%
  filter(!is.na(rank)) %>%
  select(name) %>%
  rbind(end %>% select(name)) %>%
  mutate(dummy=1) %>%
  summarize(., do_union = FALSE) %>%
  st_cast("LINESTRING")

leaflet(rbind(points,end))  %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(popup = ~ name ) %>%
  addPolylines(data=best_path, color="red")

