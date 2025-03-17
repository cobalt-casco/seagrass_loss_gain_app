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

# crop the boatramps to the extent of max_extent with st_crop()
casco_boat_ramps <- st_crop(boat_ramps, max_extent)


# define color palette
loss_gain_pal <- colorFactor(palette = c("red", "purple"),
                             domain = loss_gain$type)

# 2. Definine a user interface
ui <- fluidPage(
  
  titlePanel("Where has seagrass changed in Casco Bay"),
  
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
  
  #create a reactive for current sea grass coverage
  most_recent_extent <- reactive({
    if (input$toggle_boat_luanches == "On") {
      return(casco_boat_ramps) # Return the boat launch shapefile
    } else {
      return(NULL) # Return NULL if the toggle is Off
    }
  })
  
  # render outputs
  output$map <- renderLeaflet({
    # get a bounding box
    b <- st_bbox(max_extent) |> as.numeric()
 
    
    # make a leaflet map
    casco_map <- leaflet() |>
      addProviderTiles(provider = "Esri.WorldTopoMap") |>
      fitBounds(b[1], b[2], b[3], b[4]) |>
      addLegend(pal = loss_gain_pal,
                values = loss_gain$type,
                title = "Loss or Gain") |>
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
      addPolylines(data = plot_extent(), #need to add another one of these for max extent
                  color = "black") |>
      # add Polygons for loss gain relative to a reference year
      addPolygons(data = loss_gain_reference(),
                  fillColor = ~loss_gain_pal(type),
                  stroke = FALSE,
                  fillOpacity = 1) 
    if (input$toggle_boat_luanches == "On") { #for toggling the boat launches 
      leafletProxy(mapId = "map") |> 
        addCircleMarkers(data = most_recent_extent(),
                         color = "black",
                         stroke = TRUE, 
                         fillOpacity = 0.8,
                         radius = 4,
                         ) 
    
    }
  })
  
  
  
  
  
}

# 4. Call shinyApp() to run the app
shinyApp(ui = ui, server = server)
