# Initial data loading and preproccessing
# Results are saved into project_env.RData

library(tidyverse)
library(shiny)
library(DT)
library(leaflet)
library(plotly)
library(shinydashboard)
library(ggrepel)
library(ggtext)
library(scales)
library(plotly)
library(sf)
library(osmdata)
library(ggrepel)
library(tmaptools)
library(htmlTable)
library(osrm)
library(stplanr)

# Assisting functions ---------
trajectory_calculator <- function(start_stop, end_stop, choosable_lines, sisendaeg, number_of_alternatives = 2){
  
  chosen_trips_list <- list()
  for (i in choosable_lines){
    chosen_trips <- stops_with_routes_base %>% 
      mutate(arrival_time = as.character(arrival_time)) %>% 
      mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
      filter(route_long_name == i) %>% 
      arrange(arrival_time) %>%
      filter(arrival_time > sisendaeg) %>% 
      arrange(trip_id) %>% 
      group_by(trip_id) %>% 
      mutate(row_num = row_number()) %>% 
      mutate(cut_place = ifelse(stop_name %in%  c(start_stop, end_stop), row_num, NA)) %>%
      #slice(seq(unique(cut_place))) %>%  
      filter(!all(is.na(unique(cut_place)))) %>% 
      filter(length(unique(cut_place)) != 2) %>% 
      slice(reduce(unique(cut_place)[!is.na(unique(cut_place))], seq)) %>%  
      #filter(.$stop_name[1] == start_stop) %>%  
      select(trip_id, arrival_time, stop_name, row_num, cut_place, route_long_name) %>% 
      mutate(check_col = ifelse(stop_name == end_stop & cut_place == min(cut_place, na.rm = T), 1, 0)) %>% 
      filter(!(1 %in% check_col)) %>% 
      filter(.$stop_name[1] == start_stop) %>%
      ungroup()
    chosen_trips_list[[i]] <- chosen_trips
  }
  
  chosen_trips <- reduce(chosen_trips_list, rbind.data.frame, stringsAsFactors = F)
  
  # first stop only
  # choosing only one stop for each bus stop?
  
  chosen_trips_w_coords <- chosen_trips %>% 
    left_join(
      peatused_df %>% 
        st_coordinates() %>% 
        cbind(peatused_df$name) %>% 
        as.data.frame(., stringsAsFactors = F) %>% 
        mutate(X = as.numeric(X)) %>% 
        mutate(Y = as.numeric(Y)) %>% 
        select(stop_name = V3, X, Y) %>% 
        group_by(stop_name) %>% # Current rule, always choose the one more to the north
        filter(Y == max(Y)) %>% 
        slice(1) %>% 
        ungroup()
    )
  
  # Select 3 shortest trips
  # Form route for each trip_id
  # Those closest to the start time
  
  input_df <- chosen_trips_w_coords %>% 
    group_by(trip_id) %>% 
    mutate(time_to_start = min(arrival_time) - sisendaeg ) %>% 
    ungroup %>% 
    arrange(time_to_start) %>% 
    filter(time_to_start  != 0) %>% 
    filter(trip_id %in% unique(trip_id)[0:(number_of_alternatives + 1)]) %>% 
    as.data.frame(., stringsAsFactors = F)
  
  route_sf_all <- vector(mode = "list")
  
  for (j in unique(input_df$trip_id)){
    input_df2 <- input_df %>% filter(trip_id == j)
    from_cut = input_df2[1:(nrow(input_df2)-1),c("X", "Y")] %>% unlist()
    to_cut = input_df2[2:nrow(input_df2),c("X", "Y")] %>% unlist()
    
    route_sf <- route(from = from_cut,
                      to = to_cut,
                      route_fun = osrmRoute,
                      returnclass = "sf")
    route_sf <- route_sf %>% mutate(linename = i)
    route_sf_all[[as.character(j)]] <- route_sf %>% mutate(trip_id = j)
    
  }
  
  # For each route, we have stop steps for each step
  route_sf_all[["input_df"]] <- input_df
  return(route_sf_all)
  
}
clean_osm_data = function(x, retain_named = F){
  for(i in names(x)){
    if(str_detect(i, "osm") & !is.null(x[[i]])){
      if(nrow(x[[i]]) == 0) { next} 
      if(retain_named){
        x[[i]] = x[[i]] %>% 
          filter(!is.na(name))
      }
      
      # res = x[[i]] %>% keep(~ mean(is.na(na_if(.x, ""))) < 0.2) 
      res = x[[i]] %>% keep(function(.x) if(is.list(.x)){return(T)} else {mean(is.na(na_if(.x, ""))) < 0.2} ) 
      if(("name" %in% colnames(x[[i]])) & (!("name" %in% colnames(res)))){
        name = x[[i]] %>% as.data.frame() %>% select(name) 
        res = res %>% bind_cols(name)
      }
      x[[i]] = res
    }
  }
  
  return(x)
}
get_bus_stop_closest <- function(peatused_df, coords_data_list, start = TRUE){
  st_distance(coords_data_list[[1]]$geometry, peatused_df[1,]$geometry)
  
  if(start){ # input is algpeatus
    distances <- coords_data_list[[1]]$geometry %>% 
      st_cast("POINT") %>% 
      head(1) %>% # start
      st_distance(peatused_df$geometry) # two start stop candidates 
    
    return(peatused_df[reduce(distances == min(distances), c),][1,]) 
  }
  
  if(!start){ # input is algpeatus
    distances <- coords_data_list[[1]]$geometry %>% 
      st_cast("POINT") %>% 
      tail(1) %>% # end
      st_distance(peatused_df$geometry) # two end stop candidates 
    
    return(peatused_df[reduce(distances == min(distances), c),][1,]) 
  }
}
closest <- function(input_stop_name,  points_sf){
  
  distance_to <- (peatused_df %>% 
    filter(name == input_stop_name))$geometry %>% 
    head(1) %>% 
    st_distance(points_sf)
  
  return(c(points_sf$name[which(distance_to == min(distance_to))], paste0(round(min(distance_to), 0), " meetrit")))
}
closest_point <- function(input_stop_name,points_sf){
  
  distance_to <- (peatused_df %>% 
                    filter(name == input_stop_name))$geometry %>% 
    head(1) %>% 
    st_distance(points_sf)
  
  return(points_sf[which(distance_to == min(distance_to)),])
}
renamer <- function(x){
  names(x) <- c(1, 2, 3)
  return(x)
}
name_fixer <- function(x){
  names(x) <- c("", "", "")
  return(x)
}

# Txt files from Tartu city database ----------
# https://avaandmed.eesti.ee/datasets/tartu-linna-uhistranspordi-peatuste-numbrid-ja-nimekiri-asukohad-ja-soiduplaanid

routes <- read.csv2("data/routes.txt", sep = ",")
trips <- read.csv2("data/trips.txt", sep = ",")
stops <-  read.csv2("data/stops.txt", sep = ",")
stop_times <-  read.csv2("data/stop_times.txt", sep = ",")

# OSM data -----------
tartu_bb = getbb("Tartu")

asutused = opq(tartu_bb) %>% 
  add_osm_feature(key = "amenity") %>% 
  osmdata_sf() %>% 
  unname_osmdata_sf() #%>% 
  #clean_osm_data()

teed = opq(tartu_bb) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf() %>% 
  unname_osmdata_sf() %>% 
  clean_osm_data()

peatused = opq(tartu_bb) %>% 
  add_osm_feature(key = "public_transport") %>% 
  osmdata_sf() %>% 
  unname_osmdata_sf() %>% 
  clean_osm_data()

peatused$osm_points <- peatused$osm_points %>% st_set_crs("EPSG:4326")

atm = opq(tartu_bb) %>% 
  add_osm_feature(key = "atm") %>% 
  osmdata_sf() %>% 
  unname_osmdata_sf() %>% 
  clean_osm_data()

tartu_asutused = asutused$osm_points %>% 
  filter(! is.na(name)) %>% 
  bind_rows(st_coordinates(asutused$osm_polygons) %>% 
              as.data.frame())

tartu_asutused_polygon = asutused$osm_polygons %>% 
  filter(! is.na(name))

poed <- opq(tartu_bb) %>% 
  add_osm_feature(key = "shop") %>% 
  osmdata_sf() %>% 
  unname_osmdata_sf()%>% 
  clean_osm_data()

poed_tartu <- poed$osm_points %>% 
  filter(!is.na(name)) %>% 
  filter(name != "Desiree") %>% 
  filter(name != "BonBon Lingerie") %>% 
  filter(name != "SEXMAX") 

poed_tartu <- poed$osm_polygons %>% 
  mutate(geometry = st_centroid(geometry)) %>% 
  bind_rows(poed_tartu)

kohvikud <- asutused$osm_points %>% 
  filter(! is.na(name)) %>% 
  filter(amenity == "cafe")

tartu_poed = poed$osm_polygons
tartu_asutused_polygon = tartu_asutused_polygon %>% bind_rows(tartu_poed %>% 
                                                                rename(amenity = shop))

tartu_bussijaam = tartu_asutused %>% 
  filter(name == "Tartu bussijaam")

tartu_asutused_polygon <- tartu_asutused_polygon %>% 
  filter(amenity %in% c("cafe", "fast_food", "hospital", "post_office", "place_of_worship", "restaurant", "university", "cinema",
                        "college", "supermarket", "mall"))  

tartu_asutused_polygon$amenity <- plyr::mapvalues(tartu_asutused_polygon$amenity, 
                                                  from = c("cafe", "fast_food", "hospital", "post_office", "place_of_worship", "restaurant", "university", "cinema",
                                                           "college", "supermarket", "mall"),
                                                  to = c("Toidukoht", "Toidukoht", "Haigla", "Postkontor", "Kogudus", "Toidukoht", "Ülikool", "Kino", "Ülikool", "Pood", "Kaubanduskeskus"))

stops_with_routes <- stops %>% 
  filter(stop_name %in% peatused$osm_points$name) %>% 
  filter(grepl("Tartu", authority)) %>% 
  filter(stop_area == "Tartu linn") %>% 
  left_join(stop_times %>% select(trip_id, stop_id, arrival_time)) %>% 
  left_join(trips %>% select(route_id, trip_id)) %>% 
  left_join(routes %>% filter(grepl("Tartu LV", competent_authority)) %>% 
              select(route_id, route_short_name, route_long_name, route_color, route_type))

stops_with_routes <- stops_with_routes %>% 
  distinct() %>% 
  select(stop_id, stop_name, route_short_name, route_long_name, 
         route_color, route_type, stop_lat, stop_lon, arrival_time) %>% 
  distinct() %>% 
  filter(!is.na(arrival_time)) %>% 
  group_by(route_long_name, stop_name) %>% 
  mutate(arrival_time = as.character(arrival_time)) %>% 
  mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
  arrange(arrival_time) %>% 
  filter(!is.na(route_long_name)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  mutate(arrival_time = substr(arrival_time, 12, 19))

stops_with_routes_for_input <- stops_with_routes

stops_with_routes <- stops_with_routes %>% left_join(stops_with_routes %>% 
                                                       group_by(route_long_name, stop_name) %>% 
                                                       mutate(arrival_time = as.character(arrival_time)) %>% 
                                                       mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
                                                       arrange(arrival_time) %>% 
                                                       mutate(arrival_time = substr(arrival_time, 12, 16)) %>% 
                                                       summarise(saabumised = reduce(arrival_time, paste)) %>% 
                                                       ungroup() %>%  
                                                       mutate(saabumised = substr(saabumised, 1, 35))) %>% 
  select(-arrival_time) %>% 
  distinct

peatused_df <- peatused$osm_points %>% 
  #filter(highway == "bus_stop") %>% 
  filter(bus == "yes") %>% 
  filter(!is.na(name)) %>% 
  left_join(stops_with_routes %>% rename(name = stop_name)) %>% 
  distinct()

peatused_df <- peatused_df %>% 
  mutate(lat = lapply(peatused_df$geometry, function(x){as.character(x)[[2]]})) %>% 
  mutate(lon = lapply(peatused_df$geometry, function(x){as.character(x)[[1]]}))


stops_with_routes_base <- stops %>% 
  filter(stop_name %in% peatused$osm_points$name) %>% 
  filter(grepl("Tartu", authority)) %>% 
  filter(stop_area == "Tartu linn") %>% 
  left_join(stop_times %>% select(trip_id, stop_id, arrival_time)) %>% 
  left_join(trips %>% select(route_id, trip_id)) %>% 
  left_join(routes %>% filter(grepl("Tartu LV", competent_authority)) %>% 
              select(route_id, route_short_name, route_long_name, route_color, route_type))

stops_with_routes_base <- stops_with_routes_base %>% 
  distinct() %>% 
  select(stop_id, stop_name, route_short_name, route_long_name, route_color, route_type, stop_lat, stop_lon, arrival_time, trip_id) %>% 
  distinct() %>% 
  filter(!is.na(arrival_time)) %>% 
  group_by(route_long_name, stop_name) %>% 
  mutate(arrival_time = as.character(arrival_time)) %>% 
  mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
  arrange(arrival_time) %>% 
  filter(!is.na(route_long_name)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  mutate(arrival_time = substr(arrival_time, 12, 19))


# List of dataframes with stop info --------

koik_abitabelid_list <- list()

for (i in 
     unique(peatused_df$name)){
  peatused_df_pauluse <- peatused_df %>% filter(name == i)
  abitabel <- bind_cols(peatused_df_pauluse %>% select(route_short_name), peatused_df_pauluse %>% select(route_long_name), peatused_df_pauluse %>% select(saabumised)) %>%
    select(route_short_name, route_long_name, saabumised) %>% distinct() %>%
    tidyr::drop_na() %>%
    arrange(route_long_name)
  
  names(abitabel) <- c("Liini nr", "Liin", "Saabumisajad")
  if (nrow(abitabel) != 0){
    koik_abitabelid_list[[i]] <- abitabel
  }
}

# m1 <- leaflet(tartu_asutused_polygon) %>%
#   addTiles() %>%
#   addCircleMarkers(data = peatused_df_pauluse, label = ~name, popup = htmlTable(abitabel, rnames = F),
#                               radius = 2, fillOpacity = 1, opacity = 1, color = "darkblue")

# Default beginning journey ----------

start_stop <- "Riiamäe"
end_stop <- "Aardla"
algaeg <- as.POSIXct("10:00:00", format = "%H:%M:%S")

choosable_lines_input = stops_with_routes_base %>% 
  mutate(route_long_name = as.character(route_long_name)) %>% 
  group_by(route_long_name) %>% 
  filter(start_stop %in% stop_name & end_stop %in% stop_name) %>% 
  ungroup() %>% 
  distinct(route_long_name) %>% 
  tidyr::drop_na() %>% 
  unlist

# For one line - let's go through all the alternatives

pal_for_lines <- RColorBrewer::brewer.pal(length(choosable_lines_input), name = "RdBu") 
counter <- 0
liini_abitabel_list <- c()
liini_abitabel_mitu_list <- c()
coords_data_list <- c()

for (z in c(1)){
  routes_list <- trajectory_calculator(start_stop = start_stop,
                                       end_stop = end_stop,
                                       choosable_lines = choosable_lines_input[z],
                                       sisendaeg = algaeg,
                                       #                                           sisendaeg = as.POSIXct("10:00:00", format = "%H:%M:%S"),
                                       number_of_alternatives = 2)
  for (trip_id_result in names(routes_list)[names(routes_list) != "input_df"]){
    
    route_as_line <- st_combine(routes_list[[trip_id_result]])
    distance <- as.character(round(round(st_length(route_as_line), 0)/1000, 2))
    # trip duration
    trip_info_base <- routes_list[["input_df"]] %>% 
      filter(trip_id == trip_id_result)
    
    trip_info <- trip_info_base %>% 
      filter(!is.na(cut_place)) %>% 
      select(arrival_time, time_to_start) 
    
    to_walk <- trip_info$time_to_start[1]
    drive_duration <- ((trip_info$arrival_time[2]) - trip_info$arrival_time[1]) %>% as.character()
    drive_duration <- paste0(drive_duration, "  min")
    #total_duration <- to_walk + drive_duration
    start_time <- paste0(substr(trip_info$arrival_time[1], 12, 16), "   ")
    end_time <- paste0(substr(trip_info$arrival_time[2], 12, 16), "   ")
    
    liini_nr <- stops_with_routes_base %>% 
      filter(route_long_name == choosable_lines_input[z]) %>% 
      distinct(route_short_name) %>% 
      mutate(route_short_name = as.character(route_short_name)) %>% 
      unlist(., use.names = F)
    
    # vahepeatuste arv
    number_of_stops <- trip_info_base %>% 
      filter(trip_id == trip_info_base$trip_id[1]) %>% 
      filter(is.na(cut_place)) %>% 
      nrow()
    
    liini_abitabel <- data.frame(row.names = c("Start", "Kohal",  "Sõidu kestvus","Vahepeatusi", "Vahemaa"),
                                 c(start_time, end_time,drive_duration, number_of_stops, 
                                   paste0(distance, " "))
    )
    
    names(liini_abitabel) <- ""
    
    liini_abitabel_list[[trip_id_result]] <- liini_abitabel
    
  }
  
  liini_abitabel_mitu <- liini_abitabel_list %>% 
    bind_cols() %>% 
    as.data.frame()
  names(liini_abitabel_mitu) <- rep("", ncol(liini_abitabel_mitu))
  
  ls_coords <- routes_list[[1]] %>% st_coordinates() 

  if (z %% 2 == 0){
    counter <- counter + 1
    ls_coords[,1] = ls_coords[,1] + (counter-1)*0.00008
  } else {
    ls_coords[,1] = ls_coords[,1] - (counter-1)*0.00008
    
  }
  ls_coords_shifted <- ls_coords %>% 
    st_linestring() %>% 
    st_geometry() %>% 
    st_zm() %>% 
    st_set_crs("EPSG:4326") %>% 
    st_sf()
  
  coords_data_list[[z]] <- ls_coords_shifted
  liini_abitabel_mitu_list[[z]] <- liini_abitabel_mitu
}

coords_data_list_vaikimisi <- coords_data_list
liini_abitabel_mitu_list_vaikimisi <- liini_abitabel_mitu_list
trajectory_results_init <- c(coords_data_list_vaikimisi, liini_abitabel_mitu_list_vaikimisi)

algpeatus <- "Riiamäe"
lopppeatus <- "Aardla"

valitud_peatused <- peatused_df %>% filter(name == algpeatus)

abitabel <- bind_cols(valitud_peatused %>% 
                        select(route_short_name), valitud_peatused %>% 
                        select(route_long_name), valitud_peatused %>% 
                        select(saabumised)) %>% 
  select(route_short_name, route_long_name, saabumised) %>% distinct() %>% 
  tidyr::drop_na() %>% 
  arrange(route_long_name) 

names(abitabel) <- c("Liini nr", "Liin", "Saabumisajad")
pal = colorFactor(c("red", "yellow", "green"), tartu_asutused_polygon$amenity)

# Create initial map -----------

m1 <- leaflet(tartu_asutused_polygon) %>%
  addTiles() %>%
  #setView(lng = view_x, lat = view_y, zoom = 13) %>% 
  addPolygons(label = ~name, popup = ~ name, color = "gray", fillColor = ~pal(tartu_asutused_polygon$amenity),
              fillOpacity = 0.4, opacity = 1, weight = 1) %>%
  addLegend("bottomright", pal = pal, values = tartu_asutused_polygon$amenity,
            title = "Asutuse tüüp",
            opacity = 1
  ) %>%
  addCircleMarkers(data = peatused_df %>% 
                     filter(name == algpeatus) %>%
                     mutate(lat = as.numeric(lat)) %>% 
                     filter(lat == max(lat)) %>% 
                     head(1), label = ~name, popup = htmlTable(abitabel, rnames = F),
                   radius = 3.5, fillOpacity = 1, opacity = 1, color = "red")

punased_peatused <- c(algpeatus, lopppeatus)

kohvikud$geometry <- kohvikud$geometry %>% st_set_crs("EPSG:4326")

m1 <- m1 %>% addPolygons(label = ~name, popup = ~ paste0( '<a href = "', paste('http://www.google.com',
                                                                               '/search?hl=en&gl=kr&tbm=nws&authuser=0&q=',
                                                                               name, "&start=",start,sep=''), '">', name, ' </a>'), color = "gray", fillColor = ~pal(tartu_asutused_polygon$amenity),
                         fillOpacity = 0.4, opacity = 1, weight = 1) %>%
  addMarkers(data = closest_point(input_stop_name = lopppeatus, points_sf = kohvikud), 
             label = ~ name, 
             icon = list(
               iconUrl = 'https://icons.iconarchive.com/icons/martin-berube/food/256/coffee-icon.png',
               iconSize = c(40, 40)
             )) %>% 
  addMarkers(data = closest_point(input_stop_name = lopppeatus, points_sf = poed_tartu), 
             label = ~ name,
             icon = list(
               iconUrl = 'https://icons.iconarchive.com/icons/graphicloads/colorful-long-shadow/256/Cart-icon.png',
               iconSize = c(40, 40)
             )) #%>% 

for (i in names(koik_abitabelid_list)){
  if (!(i %in% punased_peatused)){
    
    m1 <- m1 %>% addCircleMarkers(data = peatused_df %>% 
                                    filter(name == i) %>% 
                                    mutate(lat = as.numeric(lat)) %>% 
                                    filter(lat == max(lat)) %>% 
                                    head(1), label = ~name, popup = htmlTable(koik_abitabelid_list[[i]] %>% 
                                                                                mutate(Liin = paste0("&nbsp", Liin, "&nbsp")), rnames = F),
                                  radius = 2, fillOpacity = 1, opacity = 1, color = "darkblue")
    
    
  }
}

peatused_df$geometry <- peatused_df$geometry %>% st_set_crs("EPSG:4326")
kohvikud$geometry <- kohvikud$geometry %>% st_set_crs("EPSG:4326")

m1 <- m1 %>% addPolygons(label = ~name, popup = ~ paste0( '<a href = "', paste('http://www.google.com',
                                                                               '/search?hl=en&gl=kr&tbm=nws&authuser=0&q=',
                                                                               name, "&start=",start,sep=''), '">', name, ' </a>'), color = "gray", fillColor = ~pal(tartu_asutused_polygon$amenity),
                         fillOpacity = 0.4, opacity = 1, weight = 1) %>%
  addMarkers(data = closest_point(input_stop_name = lopppeatus, points_sf = kohvikud), 
             label = ~ name, 
             icon = list(
               iconUrl = 'https://icons.iconarchive.com/icons/martin-berube/food/256/coffee-icon.png',
               iconSize = c(40, 40)
             )) %>% 
  addMarkers(data = closest_point(input_stop_name = lopppeatus, points_sf = poed_tartu), 
             label = ~ name,
             icon = list(
               iconUrl = 'https://icons.iconarchive.com/icons/graphicloads/colorful-long-shadow/256/Cart-icon.png',
               iconSize = c(40, 40)
             ))


for (i in lopppeatus){
  peatused_df_pauluse <- peatused_df %>%
    filter(name == i)
  
  abitabel <- bind_cols(peatused_df_pauluse %>%
                          select(route_short_name), peatused_df_pauluse %>%
                          select(route_long_name), peatused_df_pauluse %>%
                          select(saabumised)) %>%
    select(route_short_name, route_long_name, saabumised) %>% distinct() %>%
    tidyr::drop_na() %>%
    arrange(route_long_name)
  
  names(abitabel) <- c("Liini nr", "Liin", "Saabumisajad")
  m1 <- m1 %>% addCircleMarkers(data = peatused_df_pauluse %>% 
                                  filter(name == lopppeatus) %>% 
                                  get_bus_stop_closest(., coords_data_list = trajectory_results_init, start = F),
                                  label = ~name, popup = htmlTable(abitabel, rnames = F),
                                radius = 2, fillOpacity = 1, opacity = 1, color = "red")
  
}

m1_init <- m1 %>% addPolylines(data = trajectory_results_init[[1]], 
                               popup = ~ htmlTable(trajectory_results_init[[2]] %>% 
                                                     renamer() %>% 
                                                     mutate_all(., function(x){paste0("&nbsp", x, "&nbsp")}) %>% 
                                                     name_fixer(), 
                                                                               caption = paste0("Teekond: ", algpeatus, " - ", lopppeatus)), 
                    label= ~ paste0(liini_nr, ": ", choosable_lines_input[1]),
                    fillColor = "red"
)




# Write initial map ----------

objects <- ls()

#save(list = objects, file = "project_env.RData")
