#' -----------------------------------------------
#' Gain and Loss Shiny App for the field for COBALT
#' 
#' -----------------------------------------------

# 1. Preamble (loading libraries, data)
library(shiny)
library(shinythemes)
library(leaflet) # map
library(leaflet.extras) # map tools
library(sf) #for geospatial data
library(dplyr)
library(mapview)
library(webshot)

#for install
#install.packages("mapview")
#install.packages("webshot")  # This will also install dependencies
#webshot::install_phantomjs() 

# load the data
max_extent <- readRDS("data/max_seagras_extent_sf.Rds")
loss_gain <- readRDS("data/loss_gain_map_2022.Rds")
seagrass <- readRDS("data/joined_seagrass_cover.Rds")
boat_ramps <- st_read("data/Maine_Boat_Launches_GeoLibrary/Maine_Boat_Launches_GeoLibrary.shp")
recent_coverage <- st_read("data/MaineDEP_Casco_Bay_Seagrass_2022/MaineDEP_Casco_Bay_Seagrass_2022.shp")

# crop the boatramps and most recent data to the extent of max_extent with st_crop()
casco_boat_ramps <- st_crop(boat_ramps, max_extent)
most_recent_coverage <- st_crop(recent_coverage, max_extent)

print(max_extent$cover_pct)
# define color palette
loss_gain_pal <- colorFactor(palette = c("red", "purple"),
                             domain = loss_gain$type)

# 2. Definine a user interface
ui <- fluidPage(
  
  titlePanel("Seagrass Change in Casco Bay"),
  
  theme = shinytheme("spacelab"),
  # add our layout
  sidebarLayout(
    sidebarPanel(
      
      # Selector for cover percent of max extent
      checkboxGroupInput(inputId = "cover_class",
                         label = "Choose a Cover Class for Extent",
                         choices = unique(max_extent$cover_pct),
                         selected = NULL),
      
      # make a radio button input to select a reference year
      radioButtons(inputId = "reference_year",
                   label = "Reference Year for DEP Segrass Mapping",
                   choices = unique(loss_gain$reference_year),
                   selected = unique(loss_gain$reference_year)[1]),
      
      radioButtons(inputId = "toggle_boat_luanches",
                   label = "Toggle the Boat Launches",
                   choices = list("On" , "Off"),
                   selected = list("On" , "Off")[2]),
      
      radioButtons(inputId = "toggle_most_recent_extent",
                   label = "Toggle DEP Seagrass Layer 2022",
                   choices = list("On" , "Off"),
                   selected = list("On" , "Off")[2]),
      
      fileInput("gpx_file", "Upload GPX File",
                accept = c(".gpx"), multiple =TRUE)
    ),
    
    mainPanel(
      # output a map
      leafletOutput(outputId = "map")
    )
  )
)

# 3. define a server - where you do things

server <- function(input, output){
  
  gpx_tracks <- reactiveValues(list = list())
  
  plot_extent <- reactive({
    ret <- max_extent %>%
      filter(cover_pct %in% input$cover_class)
    ret
  })
  
  loss_gain_reference <- reactive({
    selected_cover_values <- which(max_extent$cover_pct %in% input$cover_class)
    ret <- loss_gain %>%
      filter(reference_year == input$reference_year) %>%
      filter(cover %in% selected_cover_values)
    
    validities <- st_is_valid(ret)
    if (any(!validities)) {
      print("INVALID geometries found. Fixing...")
      ret[!validities, ] <- st_make_valid(ret[!validities, ])
      validities <- st_is_valid(ret)
      ret <- ret[validities, ]
    }
    
    return(ret)
  })
  
  #to use points instead of lines 
  # gpx_tracks <- reactiveValues(lines = list(), points = list())
  # 
  # observeEvent(input$gpx_file, {
  #   req(input$gpx_file)
  #   
  #   files <- input$gpx_file$datapath
  #   for (f in files) {
  #     # Try to read track lines
  #     gpx_line <- tryCatch({
  #       sf::st_read(f, layer = "tracks", quiet = TRUE)
  #     }, error = function(e) NULL)
  #     
  #     if (!is.null(gpx_line)) {
  #       gpx_tracks$lines[[length(gpx_tracks$lines) + 1]] <- gpx_line
  #     }
  #     
  #     # Try to read track points or waypoints
  #     gpx_points <- tryCatch({
  #       sf::st_read(f, layer = "track_points", quiet = TRUE)
  #     }, error = function(e) {
  #       tryCatch({
  #         sf::st_read(f, layer = "waypoints", quiet = TRUE)
  #       }, error = function(e2) NULL)
  #     })
  #     
  #     if (!is.null(gpx_points)) {
  #       gpx_tracks$points[[length(gpx_tracks$points) + 1]] <- gpx_points
  #     }
  #   }
  # })
  
  observeEvent(input$gpx_file, {
    req(input$gpx_file)
    
    files <- input$gpx_file$datapath
    for (f in files) {
      gpx_sf <- tryCatch({
        sf::st_read(f, layer = "tracks", quiet = TRUE)
      }, error = function(e) {
        print(paste("Failed to read:", f))
        return(NULL)
      })
      
      if (!is.null(gpx_sf)) {
        gpx_tracks$list[[length(gpx_tracks$list) + 1]] <- gpx_sf
      }
    }
  })
  
  boat_launches <- reactive({
    if (input$toggle_boat_luanches == "On") {
      return(casco_boat_ramps)
    } else {
      return(NULL)
    }
  })
  
  most_recent_extent <- reactive({
    if (input$toggle_most_recent_extent == "On") {
      return(most_recent_coverage)
    } else {
      return(NULL)
    }
  })
  
  output$map <- renderLeaflet({
    b <- st_bbox(max_extent) |> as.numeric()
    
    casco_map <- leaflet() |> 
      addProviderTiles(provider = "Esri.WorldTopoMap") |> 
      fitBounds(b[1], b[2], b[3], b[4]) |> 
      addLegend(pal = loss_gain_pal,
                values = loss_gain$type,
                title = "Loss or Gain") |> 
      addLegend(colors = c("blue", "red"),
                labels = c("Boatramps", "Eelgrass"),
                title = "Toggles",  
                position = "bottomright") |> 
      addControlGPS(
        options = gpsOptions(
          position = "topleft",
          activate = TRUE, 
          autoCenter = TRUE, 
          setView = TRUE)) 
    
    activateGPS(casco_map)
    casco_map
  })
  
  observe({
    leafletProxy(mapId = "map") |> 
      clearShapes() |> 
      clearGroup("boat_launches") |> 
      addPolylines(data = plot_extent(), color = "black", weight = 2) |> 
      addPolygons(data = loss_gain_reference(),
                  fillColor = ~loss_gain_pal(type),
                  stroke = FALSE,
                  fillOpacity = 1)
    
    if (input$toggle_boat_luanches == "On") {
      leafletProxy(mapId = "map") |> 
        addCircleMarkers(data = boat_launches(),
                         color = "blue",
                         stroke = TRUE, 
                         fillOpacity = 0.8,
                         radius = 4,
                         group = "boat_launches")
    }
    
    if (input$toggle_most_recent_extent == "On") {
      leafletProxy(mapId = "map") |> 
        addPolygons(data = most_recent_extent(), 
                    fillColor = "red",
                    stroke = FALSE, 
                    fillOpacity = 0.8)
    }
    
    #to view points instead of lines on the map 
    # if (length(gpx_tracks$points) > 0) {
    #   for (gpx_pt in gpx_tracks$points) {
    #     if (!is.null(gpx_pt)) {
    #       leafletProxy("map") |>  
    #         addCircleMarkers(data = gpx_pt,
    #                          color = "orange",
    #                          radius = 5,
    #                          stroke = TRUE,
    #                          fillOpacity = 0.9)
    #     }
    #   }
    # }
    
    if (length(gpx_tracks$list) > 0) {
      for (gpx_sf in gpx_tracks$list) {
        if (!is.null(gpx_sf)) {
          leafletProxy("map") |>  
            addPolylines(data = gpx_sf, color = "yellow", weight = 4)
        }
      }
      
      last_sf <- gpx_tracks$list[[length(gpx_tracks$list)]]
      centroid <- st_centroid(last_sf)
      coords <- st_coordinates(centroid)
      
      leafletProxy("map") |> 
        flyTo(lng = coords[1], lat = coords[2], zoom = 16)
    }
  })
}

# 4. Call shinyApp() to run the app
shinyApp(ui = ui, server = server)
