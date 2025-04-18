#' -----------------------------------------------
#' Gain and Loss Shiny App for the field for COBALT
#' 
#' TODO:
#'  - Have the option to look at loss, gain, or both - DONE
#'  - Look at boundaries of the bed in the reference - DONE
#'  - Add in the boatramps on the map
#'  - Play with colors and stroke or fill opacity
#'  - Add the dot (Jarrett) - DONE
#' -----------------------------------------------




# 1. Preamble (loading libraries, data)
library(shiny)
library(shinythemes)
library(leaflet) # map
library(leaflet.extras) # map tools
library(sf) #for geospatial data
library(dplyr)

# load the data
max_extent <- readRDS("data/max_seagras_extent_sf.Rds")
loss_gain <- readRDS("data/loss_gain_map_2022.Rds")
seagrass <- readRDS("data/joined_seagrass_cover.Rds")
boat_ramps <- st_read("data/Maine_Boat_Launches_GeoLibrary/Maine_Boat_Launches_GeoLibrary.shp")
recent_coverage <- st_read("data/MaineDEP_Casco_Bay_Seagrass_2022/MaineDEP_Casco_Bay_Seagrass_2022.shp")

# crop the boatramps and most recent data to the extent of max_extent with st_crop()
casco_boat_ramps <- st_crop(boat_ramps, max_extent)
most_recent_coverage <- st_crop(recent_coverage, max_extent)


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
      selectInput(inputId = "cover_class",
                  label = "Choose a Cover Class for Extent",
                  choices = unique(max_extent$cover_pct),
                  selected = unique(max_extent$cover_pct)[1]),
      
      # make a radio button input to select a reference year
      radioButtons(inputId = "reference_year",
                   label = "Choose Reference Year",
                   choices = unique(loss_gain$reference_year),
                   selected = unique(loss_gain$reference_year)[1]),
      
      radioButtons(inputId = "toggle_boat_luanches",
                  label = "Toggle the Boat Launches",
                  choices = list("On" , "Off"),
                  selected = list("On" , "Off")[2]),
      
      radioButtons(inputId = "toggle_most_recent_extent",
                   label = "Toggle the Most Recent Flyover Extent",
                   choices = list("On" , "Off"),
                   selected = list("On" , "Off")[2]),
     
      fileInput("gpx_file", "Upload GPX File",
                accept = c(".gpx"))
    
    ),
    
    mainPanel(
      # output a map
      leafletOutput(outputId = "map")
      
    )
  )
  
)

# 3. define a server - where you do things

server <- function(input, output){
  # create reactives
  
  # filter our max_extent data
  # to the selected cover_class
  plot_extent <- reactive({
    ret <- max_extent |>
      filter(cover_pct == input$cover_class)
    
    ret
  })
  
  # Make a reactive for current gain_loss 
  # using reference_year and the loss_gain object
  loss_gain_reference <- reactive({
    
    cover_class <- which(input$cover_class == max_extent$cover_pct)[1]

    
    ret <- loss_gain |>
      filter(reference_year == input$reference_year) |>
      filter(cover == cover_class)
    
    print(ret)
    
    ret
  
  })
  
  gpx_data <- reactive({
    req(input$gpx_file)  # Ensure a file is uploaded
    
    # Read the GPX file as an sf object
    gpx_sf <- sf::st_read(input$gpx_file$datapath, layer = "tracks", quiet = TRUE)
    
    return(gpx_sf)
  })
  
  #create a reactive for the boat launches
  boat_launches <- reactive({
    if (input$toggle_boat_luanches == "On") {
      return(casco_boat_ramps) # Return the boat launch shapefile
    } else {
      return(NULL) # Return NULL if the toggle is Off
    
    }
  })
    
  #create a reactive for the current seagrass coverage
  most_recent_extent <- reactive({
    if (input$toggle_most_recent_extent == "On") {
      return(most_recent_coverage) # Return the recent_extent shapefile
    } else {
      return(NULL) # Return NULL if the toggle is Off
    }
  })
  
  
  # render outputs
  output$map <- renderLeaflet({
    # get a bounding box
    b <- st_bbox(max_extent) |> as.numeric()
    print(b)
    
    # make a leaflet map
    casco_map <- leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      fitBounds(b[1], b[2], b[3], b[4]) |>
      addLegend(pal = loss_gain_pal,
                values = loss_gain$type,
                title = "Loss or Gain") |>
      addLegend(colors = c("blue", "green"),
                labels = c("Boatramps", "Eelgrass"),  # No variable, just a single color
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
  
  # Observe changes to inputs and update the map
  observe({
    
    leafletProxy(mapId = "map") |>
      clearShapes()|>
      clearGroup("boat_launches") |>
      
      addPolylines(data = plot_extent(), #need to add another one of these for max extent
                  color = "black",
                  weight = 2) |>
      # add Polygons for loss gain relative to a reference year
      addPolygons(data = loss_gain_reference(),
                  fillColor = ~loss_gain_pal(type),
                  stroke = FALSE,
                  fillOpacity = 1) 
    
    if (input$toggle_boat_luanches == "On") { #for toggling the boat launches 
      leafletProxy(mapId = "map") |> 
        addCircleMarkers(data = boat_launches(),
                         color = "blue",
                         stroke = TRUE, 
                         fillOpacity = 0.8,
                         radius = 4,
                         group = "boat_launches"
        )
    } 
    if (input$toggle_most_recent_extent == "On"){
      leafletProxy(mapId = "map") |> 
        addPolygons(data = most_recent_extent(), 
                     fillColor = "green",
                     stroke = FALSE, 
                     fillOpacity = 0.8)  
    }
    req(gpx_data())  
    gpx_sf <- gpx_data()  # ✅ Evaluate the reactive
    
    if (!is.null(gpx_sf)) {  # Check if data is not empty
      bbox <- as.list(st_bbox(gpx_sf))
      print(bbox)
      
      centroid <- st_centroid(gpx_sf)
      centroid_coords <- st_coordinates(centroid)  # Extract coordinates from centroid
      centroid_lon <- centroid_coords[1]  # Longitude
      centroid_lat <- centroid_coords[2]  # Latitude
      
      # Add GPX layer to the map
      leafletProxy("map") |>  
        addPolylines(data = gpx_sf,  
                     color = "yellow", 
                     weight = 4,  
                     layerId = "gpx_layer") |> 
      flyTo(lng = centroid_lon, lat = centroid_lat, zoom = 16)  #zoom to the bounding box of the GPX layer
        
        
    }
  })
  
}

# 4. Call shinyApp() to run the app
shinyApp(ui = ui, server = server)
