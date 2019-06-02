library(shiny)
#remotes::install_github("GIScience/openrouteservice-r")
set.seed(1234)
library(openrouteservice)
library(opencage)
ors_api_key(Sys.getenv("ors_key"))
library(osmdata)
library(mapview)
library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
st_x = function(x) st_coordinates(x)[,1]
st_y = function(x) st_coordinates(x)[,2]

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

IconSet <- awesomeIconList(
  beer   = makeAwesomeIcon(icon= 'beer', markerColor = 'green', iconColor = 'white', library = "fa"),
  nobeer = makeAwesomeIcon(icon= 'beer', markerColor = 'red', iconColor = 'white', library = "fa"),
  start = makeAwesomeIcon(icon= 'flag-o ',  iconColor = 'white', library = "fa"),
  end = makeAwesomeIcon(icon= 'bed',  iconColor = 'white', library = "fa")
  
)

ui <- 
  fluidPage(
    fluidRow(
      column(3,  textInput("input_start", h3("Where am I?"), 
                           value = "Université Laval, Quebec"))  ,
      column(3,  textInput("input_end", h3("Where is my bed?"), 
                           value = "Chateau Frontenac, Quebec"))  ,
      column(3, sliderInput("input_n_bars", h3("How many bars?"),
                            min = 1, max = 6, value = 3)),
      column(3,  radioButtons("input_how", "How am I getting home?",
                              choices = list("crawling" = 1, "biking"= 2, "driving" = 3),selected = 1)
             )),
    fluidRow(
      column(3,
             actionButton("recalc", "Get me home!", 
                          icon("walking"),
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      column(9,sliderInput(
        "input_boundingbox_buffer", 
        h3("Buffer around bounding box, in degrees."),
        min = 0, max = 2, step = 0.05, value = 0))
      ),
    
    fluidRow(textOutput("text")),
    fluidRow(leafletOutput("mapplot"))
    
  )


server <- function(input, output) {
  
  my_results1 <- eventReactive( input$recalc, {
    
    
    how <- input$input_how
    
    if(how == 1){
      my_profile = "foot-walking"
      how_name = "crawling"
    } else if(how == 2){
      my_profile = "cycling-regular"
      how_name= "biking"
    } else if (how == 3){
      my_profile = "driving-car"
      how_name = "driving"
    }
    n_bars <- input$input_n_bars
    
    opencage_return_start <- opencage_forward(input$input_start, 
                                              limit = 1 , # just the best result
    )
    start_lat = opencage_return_start$results$geometry.lat
    start_lon = opencage_return_start$results$geometry.lng
    
    
    opencage_return_end <- opencage_forward(input$input_end, 
                                            limit = 1 , # just the best result
    )
    end_lat = opencage_return_end$results$geometry.lat
    end_lon = opencage_return_end$results$geometry.lng
    
    
    mespoints <- tibble(name = c("start", "end"),
                        lat = c(start_lat, end_lat),
                        lon = c(start_lon, end_lon),
                        osm_id = c("start", "end")) %>%
      st_as_sf(., coords= c("lon", "lat"), crs = 4326)
    mespoints$x <- st_x(mespoints)
    mespoints$y <- st_y(mespoints)
    
    if (as.numeric(st_distance(  mespoints %>% filter(name== "start"),  mespoints %>% filter(name== "end"))) < 100000){
    
    
    pubs <- opq(bbox = c(min(start_lon, end_lon)-input$input_boundingbox_buffer, min(start_lat, end_lat)-input$input_boundingbox_buffer, max(start_lon, end_lon)+input$input_boundingbox_buffer, max(start_lat, end_lat)+input$input_boundingbox_buffer))%>%
      add_osm_feature(key = "amenity", value = "pub")  %>%
      osmdata_sf(.) %>% 
      .$osm_points  %>% 
      select(name, osm_id)
    
    
    bars <- opq(bbox = c(min(start_lon, end_lon)-input$input_boundingbox_buffer, min(start_lat, end_lat)-input$input_boundingbox_buffer, max(start_lon, end_lon)+input$input_boundingbox_buffer, max(start_lat, end_lat)+input$input_boundingbox_buffer))%>%
      add_osm_feature(key = "amenity", value = "bar")  %>%
      osmdata_sf(.) %>% 
      .$osm_points  %>% 
      select(name, osm_id)
    
    pubs_bars <- pubs %>% rbind(bars)
    pubs_bars$x <- st_x(pubs_bars)
    pubs_bars$y <- st_y(pubs_bars)
    
    bar_count <- nrow(pubs_bars)
    
    if(nrow(pubs_bars)> 49){
      message("more than 50 pubs, sampling 49 to allow use of open route services duration matrix api")
      pubs_bars <- pubs_bars[sample(nrow(pubs_bars), 49), ]
    }
    
    
    points <- pubs_bars %>% 
      filter(!is.na(name))%>%
      rbind(mespoints %>% filter(name == "start"))
    
    end <- mespoints %>% filter(name == "end")
    
    allpoints <- rbind(points, end)
    
    distance_matrix_input  <-  pubs_bars %>% 
      filter(!is.na(name))%>%
      rbind(mespoints) %>%
      st_set_geometry(NULL)
    
    z <- distance_matrix_input %>% 
      select(x,y) %>%
      ors_matrix(., 
                 metrics = c("duration", "distance"), 
                 units = "km",
                 profile = my_profile)
    
    zz <- z$durations %>% as_tibble()
    colnames(zz)  <- distance_matrix_input$osm_id
    zz$origin = distance_matrix_input$osm_id
    
    durations <- zz %>% 
      gather(key= destination, value  = duration, -origin)
    
    
    
    #initialiser les trajets
    open <- vector("list", 100000)
    closed <- vector("list", 100000)
    open[[1]] <- "start"
    
    # get distance to data
    d_parcouru <- c(rep(NA_integer_, 100000))
    d_parcouru[1] <- 0
    
    #get distance to end
    d_left <- c(rep(NA_integer_, 100000))
    d_left[1] = durations %>% 
      filter(origin == "start", destination == "end") %>% 
      pull(duration) 
    #as.numeric(st_distance(end, points %>% filter(name== "start")))
    
    # get total distance
    d_total <- c(rep(NA_integer_, 100000))
    d_total[1] = d_parcouru[1] + d_left[1]
    
    d_parcouru_closed <- c(rep(NA_integer_, 100000))
    
    # while we havent found the best path and there are still open paths..
    win  <- 0
    stop <- 0
    round = 1
    while (win==0 & stop == 0){
      
      # expand best trip
      k_to_expand <- which.min(d_total)
      
      message("round ", round, " expanding ", open[k_to_expand])
      
      #last_point <- lapply(l[[k_to_expand]], tail,1)
      last_point <- open[[k_to_expand]][length(open[[k_to_expand]])]
      
      
      # get list of possible destinations  (do not go back to already visited, and only go to end after 4 points including start)
      if(length(open[[k_to_expand]] )== n_bars+1){
        dests <- "end"} else{
          dests <- points %>% filter(!(osm_id %in% open[[k_to_expand]])) %>% pull(osm_id)
        }
      #dests
      
      # find empty spots in list
      A = map_lgl(open, is.null) %>% which() %>% .[1:length(dests)]
      B = map_lgl(closed, is.null) %>% which() %>% .[1:length(dests)]
      i = 1 
      for (dest in dests){ 
        if (dest != "end"){
          open[[A[i]]] <- c(open[[k_to_expand]], dest)
          d_parcouru[A[i]] <- d_parcouru[k_to_expand] + 
            durations %>% 
            filter(origin == last_point, destination == dest) %>% 
            pull(duration)
          # as.numeric(st_distance(points %>% filter(name == last_point), 
          #                        points %>% filter(name == dest)))
          d_left[A[i]] <- durations %>% 
            filter(origin == dest, destination == "end") %>% 
            pull(duration)
          #as.numeric(st_distance(points %>% filter(name == dest), end))
          d_total[A[i]] <- d_parcouru[A[i]] + d_left[A[i]]
        }
        
        if (dest == "end"){
          closed[[B[i]]] <- c(open[[k_to_expand]], dest)
          d_parcouru_closed[B[i]] <- d_parcouru[k_to_expand] + 
            durations %>% 
            filter(origin == last_point, destination == "end") %>% 
            pull(duration)
          # as.numeric(st_distance(points %>% filter(name == last_point), 
          #                        end))
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
      if (round == 500){
        stop == 1
        message("reached round 500, cancelling")
      }
    } # fin while
    
    # create best_path as the crow flies
    # best_path_linestring <- rbind(points,end) %>% 
    #   left_join (tibble(osm_id = closed[[1]]) %>% 
    #                mutate(rank = row_number())) %>%
    #   arrange(rank) %>%
    #   filter(!is.na(rank)) %>%
    #   select(osm_id) %>%
    #   summarize(., do_union = FALSE) %>%
    #   st_cast("LINESTRING")
    
    # create markers by use
    
    markers <- allpoints %>% 
      left_join(tibble(osm_id = closed[[1]]) %>% 
                  mutate(rank = row_number())) %>%
      mutate(type =factor(
        case_when(
          osm_id == "start"~ "start",
          osm_id == "end"~ "end",
          !is.na(rank) ~ "beer",
          TRUE ~ "nobeer")))
    
    best_path_stops <-  markers %>%
      filter(!is.na(rank))%>%
      arrange(rank) %>%
      select(osm_id,x,y) %>%
      rbind(end %>% select(osm_id,x,y))  %>%
      st_set_geometry(NULL)
    
    
    itinerary <- ors_directions(best_path_stops %>% select(x,y),
                                profile= my_profile)
    
    
    beer <- markers %>%
      filter(osm_id != "start", osm_id != "end") %>%
      filter(!is.na(rank))%>%
      arrange(rank)  
    
    if (bar_count>10 & bar_count < 49){
      title <- tags$div(
        tag.map.title, HTML("The shortest path home with ",
                            n_bars, 
                            " bars stops at ", 
                            paste0(beer$name, collapse = "", sep= ", "), 
                            " and will require ",
                            how_name, " ",
                            floor(d_parcouru_closed[1] / 60),
                            " minutes.")
      )  }
    
    if (bar_count<=10){
      title <- tags$div(
        tag.map.title, HTML("Only ", bar_count, " bars found. you may want to increase bounding box size using the slider.",
                            "The shortest path home with ",
                            n_bars, 
                            " bars stops at ", 
                            paste0(beer$name, collapse = "", sep= ", "), 
                            " and will require ",
                            how_name, " ",
                            floor(d_parcouru_closed[1] / 60),
                            " minutes.")
      )  }
    if (bar_count>49){
      title <- tags$div(
        tag.map.title, HTML("Found ", bar_count, " bars. 49 were randomly selected for optimization.  You may want to decrease bounding box size using the slider.",
                            "The shortest path home with ",
                            n_bars, 
                            " bars stops at ", 
                            paste0(beer$name, collapse = "", sep= ", "), 
                            " and will require ",
                            how_name, " ",
                            floor(d_parcouru_closed[1] / 60),
                            " minutes.")
      )  }
    
    list("cancelled" = 0,
         "markers"= markers,
         "itinerary" = itinerary,
         "title"= title,
         "bar_count" = bar_count
    )
    
    } else{
      #plus de 100 km 
      list("cancelled" = 1,
           "markers"= mespoints %>% mutate(type=factor("bed")),
           "itinerary" = NULL,
           "title"= NULL,
           "bar_count" = NULL)
    }
    
  })
  
  output$text <- renderText({
    req(my_results1)
    
    if (my_results1()$cancelled==0){
    paste0("Optimising from ", my_results1()$bar_count, " bars.  A sample of 49 is randomly selected if the number of bars is above 50.")
    } else {"more than 100 km between origin and destination, cancelled"}
    
  })
  
  output$mapplot <- renderLeaflet({
    req(my_results1)
    if (my_results1()$cancelled==0){
    leaflet( my_results1()$markers)  %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addAwesomeMarkers(icon = ~IconSet[type], popup =~ name) %>%
      addGeoJSON(my_results1()$itinerary, fill=FALSE, color = "red")%>%
      addControl(my_results1()$title, position = "topleft")
    } else {
      
      leaflet(my_results1()$markers) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addAwesomeMarkers(icon = ~IconSet[type], popup =~ name) 
        
        
    }
  })
  
  
}
shinyApp(ui, server)
