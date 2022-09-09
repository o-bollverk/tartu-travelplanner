
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

# Time set ---------
Sys.setenv(TZ="Europe/Paris")

# Load project environment from .RData ---------

# Assisting functions ----------
closest <- function(input_stop_name,  points_sf){
  distance_to <- peatused$osm_points %>% 
    filter(name == input_stop_name) %>%
    filter(public_transport == "platform") %>% 
    head(1) %>% 
    st_distance(points_sf)
  
  return(c(points_sf$name[which(distance_to == min(distance_to))], paste0(round(min(distance_to), 0), " meetrit")))
}
closest_point <- function(input_stop_name,  points_sf){
  distance_to <- peatused$osm_points %>% 
    filter(name == input_stop_name) %>%
    filter(public_transport == "platform") %>% 
    head(1) %>% 
    st_distance(points_sf)
  return(points_sf[which(distance_to == min(distance_to)),])
}
get_bus_stop_closest <- function(peatused_df, coords_data_list, start = TRUE){
  st_distance(coords_data_list[[1]]$geometry, peatused_df[1,]$geometry)
  
  if(start){ # input is algpeatus
    distances <- coords_data_list[[1]]$geometry %>% 
      st_cast("POINT") %>% 
      head(1) %>% # start
      st_distance(peatused_df$geometry) # two start stop candidates 
    
    # if (nrow(peatused_df[reduce(distances == min(distances), c),]) != 1){
    #   writeLines("No two stop candidates?")
    #   print(distances == min(distances))
    #   return(NULL)
    # }
    return(peatused_df[reduce(distances == min(distances), c),][1,]) 
  }
  
  if(!start){ # input is algpeatus
    distances <- coords_data_list[[1]]$geometry %>% 
      st_cast("POINT") %>% 
      tail(1) %>% # end
      st_distance(peatused_df$geometry) # two end stop candidates 
    
    # if (nrow(peatused_df[reduce(distances == min(distances), c),]) != 1){
    #   writeLines("No two stop candidates?")
    #   print(distances == min(distances))
    #   return(NULL)
    # }
    
    return(peatused_df[reduce(distances == min(distances), c),][1,]) 
  }
}

server = function(input, output, session){
  
  # For dynamic selectinput
  
  inputstop_reactive <- reactive({
    get(input$start_stop)
  })
  
  # Using the reactive input
  
  observeEvent( c(input$start_stop, input$start_time), {
    updateSelectInput(session, "end_stop", choices = 
                        stops_with_routes_base %>%
                        mutate(arrival_time = as.character(arrival_time)) %>% 
                        mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
                        filter(arrival_time > as.POSIXct(input$start_time)) %>%
                        mutate(stop_name = as.character(stop_name)) %>%
                        filter(!is.na(route_long_name)) %>%
                        group_by(route_long_name) %>%
                        filter(input$start_stop %in% stop_name) %>% 
                        ungroup() %>% 
                        group_by(trip_id) %>% 
                        filter(input$start_stop %in% stop_name) %>% 
                        mutate(trip_index = row_number()) %>% 
                        mutate(start_point = ifelse(stop_name == input$start_stop, trip_index, NA)) %>% 
                        filter(trip_index > unique(start_point)[!is.na(unique(start_point))]) %>% 
                        ungroup() %>% 
                        distinct(stop_name) %>%
                        unlist(., use.names = F) %>% 
                        sort())
    
    
  })
  
  peatused_df_react <- reactive({

    lopppeatus <- isolate(input$end_stop)
    algpeatus <- isolate(input$start_stop)
    algaeg <- isolate(input$start_time) + 60*60*2
    
    stops_with_routes_for_input_filtered2 <- stops_with_routes_for_input %>% 
      mutate(arrival_time = as.character(arrival_time)) %>% 
      mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
      filter(arrival_time > algaeg) 

    stops_with_routes_for_join <- stops_with_routes_for_input_filtered2 %>% left_join(stops_with_routes_for_input_filtered2 %>% 
                                                                                        group_by(route_long_name, stop_name) %>% 
                                                                                        mutate(arrival_time = substr(arrival_time, 12, 16)) %>% 
                                                                                        summarise(saabumised = reduce(arrival_time, paste)) %>% 
                                                                                        ungroup() %>%  
                                                                                        mutate(saabumised = substr(saabumised, 1, 35))) %>% 
      select(-arrival_time) %>% 
      distinct
    
    peatused_df <- peatused$osm_points %>% 
      filter(bus == "yes") %>% 
      filter(!is.na(name)) %>% 
      left_join(stops_with_routes_for_join %>% rename(name = stop_name)) %>% 
      distinct()
    
    peatused_df <- peatused_df %>% 
      mutate(lat = lapply(peatused_df$geometry, function(x){as.character(x)[[2]]})) %>% 
      mutate(lon = lapply(peatused_df$geometry, function(x){as.character(x)[[1]]}))
    
    peatused_df
    
    
    
  })
  
  trajectory_results_rect <- reactive({
    
    if (input$goButton == 0){
      return(c(coords_data_list_vaikimisi, liini_abitabel_mitu_list_vaikimisi))
    }
    input$goButton
    
    start_stop <- isolate(input$start_stop)
    end_stop <- isolate(input$end_stop)
    algaeg <- isolate(input$start_time)

    choosable_lines_input <- stops_with_routes_base %>%
      mutate(arrival_time = as.character(arrival_time)) %>% 
      mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
      filter(arrival_time > as.POSIXct(algaeg)) %>%
      mutate(stop_name = as.character(stop_name)) %>%
      filter(!is.na(route_long_name)) %>%
      group_by(route_long_name) %>%
      filter(start_stop %in% stop_name & end_stop %in% stop_name) %>% 
      ungroup() %>% 
      group_by(trip_id) %>% 
      filter(start_stop %in% stop_name & end_stop %in% stop_name) %>% 
      mutate(trip_index = row_number()) %>% 
      mutate(start_point = ifelse(stop_name == start_stop, trip_index, NA)) %>% 
      #filter(!is.na(start_point)) %>% 
      filter(trip_index > unique(start_point)[!is.na(unique(start_point))]) %>% 
      ungroup() %>% 
      distinct(as.character(route_long_name)) %>%
      unlist(., use.names = F) %>% 
      sort()
    
    
    
    
    # For one line - let's go through all the alternatives
    
    pal_for_lines <- RColorBrewer::brewer.pal(length(choosable_lines_input), name = "RdBu") 
    counter <- 0
    liini_abitabel_list <- c()
    liini_abitabel_mitu_list <- c()
    coords_data_list <- c()
    liini_numbrid <- c()
    #for (z in 1:length(choosable_lines_input)){ 
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
        drive_duration <- paste0(drive_duration, " min  ")
        #total_duration <- to_walk + drive_duration
        start_time <- paste0(substr(trip_info$arrival_time[1], 12, 16), "  ")
        end_time <- paste0(substr(trip_info$arrival_time[2], 12, 16), "  ")
        
        liini_nr <- stops_with_routes_base %>% 
          filter(route_long_name == choosable_lines_input[z]) %>% 
          distinct(route_short_name) %>% 
          mutate(route_short_name = as.character(route_short_name)) %>% 
          unlist(., use.names = F)
        
        liini_numbrid <- c(liini_numbrid, liini_nr)
        
        # vahepeatuste arv
        number_of_stops <- trip_info_base %>% 
          filter(trip_id == trip_info_base$trip_id[1]) %>% 
          filter(is.na(cut_place)) %>% 
          nrow()
        
        liini_abitabel <- data.frame(row.names = c("Start", "Kohal",  "Sõidu kestvus","Vahepeatusi", "Vahemaa (km)"),
                                     c(start_time, end_time,drive_duration, number_of_stops, 
                                       paste0(distance))
        )
        
        names(liini_abitabel) <- ""
        liini_abitabel_list[[trip_id_result]] <- liini_abitabel
        
      }
      
      liini_abitabel_mitu <- liini_abitabel_list %>% 
        bind_cols() %>% 
        as.data.frame()
      names(liini_abitabel_mitu) <- rep("", ncol(liini_abitabel_mitu))
      
      ls_coords <- routes_list[[1]] %>% st_coordinates() #%>% st_linestring()

      if (z %% 2 == 0){
        counter <- counter + 1
        ls_coords[,1] = ls_coords[,1] + (counter-1)*0.00008
      } else {
        ls_coords[,1] = ls_coords[,1] - (counter-1)*0.00008
        
      }
      ls_coords_shifted <- ls_coords %>% st_linestring() %>% st_geometry() %>% st_zm() %>% st_set_crs("EPSG:4326") %>% st_sf()
      coords_data_list[[z]] <- ls_coords_shifted
      liini_abitabel_mitu_list[[z]] <- liini_abitabel_mitu
      
    }
    
    return(c(coords_data_list, liini_abitabel_mitu_list, list(liini_numbrid), list(choosable_lines_input)))
    
  })

  output$mymap =  renderLeaflet({
    
    if (input$goButton == 0)
      return(
        m1_init
      )
    
    lopppeatus <- isolate(input$end_stop)
    algpeatus <- isolate(input$start_stop)
    valitud_peatused <- peatused_df_react() %>% filter(name == algpeatus)
    
    abitabel <- bind_cols(valitud_peatused %>% 
                            select(route_short_name), valitud_peatused %>% 
                            select(route_long_name), valitud_peatused %>% 
                            select(saabumised)) %>% 
      select(route_short_name, route_long_name, saabumised) %>% distinct() %>% 
      tidyr::drop_na() %>% 
      arrange(route_long_name)
    
    names(abitabel) <- c("Liini nr", "Liin", "Saabumisajad")
    abitabel <- abitabel %>% mutate(Liin = paste0("&nbsp", Liin, "&nbsp"))

    pal = colorFactor(c("red", "yellow", "green"), tartu_asutused_polygon$amenity)
    
    view_x <- (peatused_df_react()$geometry %>% st_coordinates())[1,1]
    view_y <- (peatused_df_react()$geometry %>% st_coordinates())[1,2]
    
    m1 <- leaflet(tartu_asutused_polygon) %>%
      addTiles() %>%
      #setView(lng = view_x, lat = view_y, zoom = 13) %>% 
      addPolygons(label = ~name, popup = ~ name, color = "gray", fillColor = ~pal(tartu_asutused_polygon$amenity),
                  fillOpacity = 0.4, opacity = 1, weight = 1) %>%
      #addPolygons(data = tartu_toit$osm_polygons, label = ~name, popup = ~ name, color = ~pal(amenity)) %>%
      addLegend("bottomright", pal = pal, values = tartu_asutused_polygon$amenity,
                title = "Asutuse tüüp",
                opacity = 1
      ) %>%
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
                 )) %>% 
      addCircleMarkers(data = peatused_df_react() %>% 
                         filter(name == algpeatus) %>% 
                         get_bus_stop_closest(., coords_data_list = trajectory_results_rect(), start = T),
                        label = ~name, popup = htmlTable(abitabel, rnames = F),
                       radius = 3.5, fillOpacity = 1, opacity = 1, color = "red")
    
    punased_peatused <- c(algpeatus, lopppeatus)
    
    for (i in names(koik_abitabelid_list)){
      if (!(i %in% punased_peatused)){
        
        m1 <- m1 %>% addCircleMarkers(data = peatused_df %>% 
                                        filter(name == i), label = ~name, 
                                      popup = htmlTable(koik_abitabelid_list[[i]] %>% 
                                                                                              mutate(Liin = paste0("&nbsp", Liin, "&nbsp"))
                                                                                            
                                                                                            , rnames = F),
                                      radius = 2, fillOpacity = 1, opacity = 1, color = "darkblue")
        
      }
    }
    
    for (i in lopppeatus){
      peatused_df_pauluse <- peatused_df_react() %>%
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
                                      get_bus_stop_closest(., coords_data_list = trajectory_results_rect(), start = F), 
                                    label = ~name, popup = htmlTable(abitabel, rnames = F),
                                    radius = 2, fillOpacity = 1, opacity = 1, color = "red")
      
    }
    
    quotes=FALSE
    start=0
    
    trajectory_results_init[[2]] %>% 
      renamer() %>% 
      mutate_all(., function(x){paste0("  ", x, "  ")}) %>% 
      name_fixer()
      
    
    m1 <- m1 %>% addPolylines(data = trajectory_results_rect()[[1]], 
                              popup = ~ htmlTable(trajectory_results_rect()[[2]] %>% 
                                                    renamer() %>% 
                                                    mutate_all(., function(x){paste0("&nbsp", x, "&nbsp")}) %>% 
                                                    name_fixer() %>% 
                                                    #renamer() %>% 
                                                    addHtmlTableStyle( #col.columns = c("none", "#F7F7F7")
                                                                      css.cell = c("width: 600;"))
                                                  
                                                    , 
                                                                                   caption = paste0("Teekond: ", 
                                                                                                    input$start_stop, " - ", 
                                                                                                    input$end_stop)),
                              #addHtmlTableStyle(css.cell = c("width: 50;","width: 100;")) 
                        label= ~ paste0(trajectory_results_rect()[[3]][1], ": ", 
                                        trajectory_results_rect()[[4]][1]),
                        fillColor = "red"#,
                        #popupOptions = popupOptions(minWidth = 400)
    )
    
   
    m1 <- m1 %>% addPolygons(label = ~name, popup = ~ paste0( '<a href = "', paste('http://www.google.com',
                                                 '/search?hl=en&gl=kr&tbm=nws&authuser=0&q=',
                                                 name, "&start=",start,sep=''), '">', name, ' </a>'), color = "gray", fillColor = ~pal(tartu_asutused_polygon$amenity),
                    fillOpacity = 0.4, opacity = 1, weight = 1) %>%
      addMarkers(data = atm$osm_points %>%
                   mutate(name = ifelse(is.na(name), "ATM: Pank pole teada", paste("ATM:", name))),
                 label = ~ name,
                 popup = ~ name)
    
    m1
    
  })
  
  output$vbox2 <- renderValueBox({
    valueBox(
      width = 3,
      subtitle = "kilomeetrit",
      strsplit(as.character(trajectory_results_rect()[[2]][5,1]), " ")[[1]][1],
      icon = icon("bus"),
      color = "blue"
    )
  })
  output$vbox1 <- renderValueBox({
    valueBox(width = 3,
             subtitle = "minutit",
             strsplit(as.character(trajectory_results_rect()[[2]][3,1]), " ")[[1]][1],
             icon = icon("clock"),
             color = "blue"
    )
  })
  
  output$vbox3 <- renderValueBox({
    valueBox(width = 3,
             subtitle = "vahepeatust",
             as.character(trajectory_results_rect()[[2]][4,1]),
             icon = icon("stop-circle"),
             color = "blue"
    )
  })
  
  output$vbox4 <- renderValueBox({
    valueBox(width = 3,
             subtitle = " maksab sama teekond A-Taksoga",
             as.character(1.9 + 
                            as.numeric(strsplit(as.character(trajectory_results_rect()[[2]][4,1]), " ")[[1]])*0.7) %>% 
               as.numeric() %>% 
               round(., 2) %>% 
               as.character() %>% 
               paste0(., "€"),
             icon = icon("euro-sign"),
             color = "blue"
    )
  })
  output$vbox5 <- renderValueBox({
    valueBox(width = 4,
             subtitle =  paste0("peatusele ", input$end_stop," lähim kohvik (", closest(input$end_stop, kohvikud)[2], ")"),
             closest(input$end_stop, kohvikud)[1],
             icon = icon("coffee"),
             color = "blue"
    )
  })
  output$vbox6 <- renderValueBox({
    valueBox(width = 4,
             subtitle = paste0("peatusele ", input$end_stop, " lähim pood (", closest(input$end_stop, poed_tartu)[2], ")"),
             closest(input$end_stop, poed_tartu)[1],
             icon = icon("store"),
             color = "blue"
    )
  })
  
}

 
