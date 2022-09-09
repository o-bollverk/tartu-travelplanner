
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

# shinyapp--------

ui = shinyUI(dashboardPage(title = "Tartu reisiplaneerija",
  dashboardHeader(title = "Tartu reisiplaneerija",
    dropdownMenu(type = "notifications",
      notificationItem(
        text = "Peatus.ee kasutamise võid ära unustada"
      )
    )
  ),
  dashboardSidebar(#title = "test!",
    
    sliderInput("start_time", 
                "Teekonna algusaeg:", 
                min =  as.POSIXct("00:00:00", format = "%H:%M:%S"),
                max =  as.POSIXct("23:59:59", format = "%H:%M:%S"),
                value = c(as.POSIXct("10:00:00", format = "%H:%M:%S")),
                timeFormat = "%H:%M:%S", ticks = F#, animate = T
    ),
    selectInput("start_stop", 
                "Algpeatus",
                choices = stops_with_routes_base$stop_name %>% unique() %>% sort(),
                selected = "Riiamäe"
    ),
    selectInput("end_stop", 
                "Lõpppeatus",
                choices = 
                  stops_with_routes_base %>%
                  mutate(arrival_time = as.character(arrival_time)) %>% 
                  mutate(arrival_time = as.POSIXct(arrival_time, format = "%H:%M:%S")) %>% 
                  filter(arrival_time > as.POSIXct("10:00:00", format = "%H:%M:%S")) %>%
                  mutate(stop_name = as.character(stop_name)) %>%
                  filter(!is.na(route_long_name)) %>%
                  group_by(route_long_name) %>%
                  filter("Riiamäe" %in% stop_name) %>% 
                  ungroup() %>% 
                  group_by(trip_id) %>% 
                  filter("Riiamäe" %in% stop_name)  %>% 
                  mutate(trip_index = row_number()) %>% 
                  mutate(start_point = ifelse(stop_name == "Riiamäe", trip_index, NA)) %>% 
                  filter(trip_index > unique(start_point)[!is.na(unique(start_point))]) %>% 
                  ungroup() %>% 
                  distinct(stop_name) %>% 
                  unlist(., use.names = F) %>% 
                  sort()
                ,
                selected = "Aardla"
    ),
    
    actionButton("goButton", "Värskenda teekonda ja aegu", icon = icon("refresh"))
  ),
  dashboardBody(
    tabItem(
      "vboxes",
      valueBoxOutput("vbox1",
                     width = 3
      ),
      valueBoxOutput("vbox2",
                     width = 3
      ),
      valueBoxOutput("vbox3",
                     width = 3
      ),
      valueBoxOutput("vbox4",
                     width = 3
      )),
    tabItem("vboxes2",
            column(width = 12,
                   fluidRow(
                     valueBoxOutput("vbox5",
                                    width = 6
                     ),
                     valueBoxOutput("vbox6",
                                    width = 6
                     )
                     )
            )
    ),
    tabItem("graph",
            leafletOutput("mymap", height = 700
            )
    )
  )
  
)
)
