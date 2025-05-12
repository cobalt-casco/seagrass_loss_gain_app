#' -----------------------------------------------
#' Gain and Loss Shiny App for the field for COBALT
#' (Addressing loss_gain_reference filtering based on original logic)
#' -----------------------------------------------

# 1. Preamble (loading libraries, data)
library(shiny)
library(shinythemes)
library(leaflet) 
library(leaflet.extras) 
library(sf) 
library(dplyr)

# Load the data - ENSURE THESE PATHS ARE CORRECT
tryCatch({
  max_extent <- readRDS("data/max_seagras_extent_sf.Rds")
  loss_gain <- readRDS("data/loss_gain_map_2022.Rds")
  seagrass <- readRDS("data/joined_seagrass_cover.Rds") # Original had this
  boat_ramps_raw <- st_read("data/Maine_Boat_Launches_GeoLibrary/Maine_Boat_Launches_GeoLibrary.shp")
  recent_coverage_raw <- st_read("data/MaineDEP_Casco_Bay_Seagrass_2022/MaineDEP_Casco_Bay_Seagrass_2022.shp")
}, error = function(e) {
  stop(paste("FATAL ERROR: Error loading initial data files. Please check paths. App cannot start. Original error:", e$message))
})

if (!exists("max_extent") || !inherits(max_extent, "sf") || nrow(max_extent) == 0) {
  stop("FATAL ERROR: max_extent data is not loaded correctly or is empty. App cannot start.")
}
if (!exists("boat_ramps_raw") || !inherits(boat_ramps_raw, "sf")) stop("FATAL ERROR: boat_ramps_raw not loaded. App cannot start.")
if (!exists("recent_coverage_raw") || !inherits(recent_coverage_raw, "sf")) stop("FATAL ERROR: recent_coverage_raw not loaded. App cannot start.")
if (!exists("loss_gain") || !inherits(loss_gain, "sf")) stop("FATAL ERROR: loss_gain data not loaded. App cannot start.")


casco_boat_ramps <- st_crop(boat_ramps_raw, max_extent) 
most_recent_coverage <- st_crop(recent_coverage_raw, max_extent) 

print(max_extent$cover_pct) # From original script

# Define color palette
if (!"type" %in% names(loss_gain)) {
  warning("'type' column not found in loss_gain data. Using default.")
  loss_gain$type <- "default_type" 
}
loss_gain_pal_domain <- unique(na.omit(loss_gain$type))
if (length(loss_gain_pal_domain) == 0) {
  warning("No data for loss_gain_pal domain. Using default.")
  loss_gain_pal_domain <- c("loss", "gain", "default_type") 
}
# MODIFIED PALETTE FOR DISTINCT COLORS
loss_gain_pal <- colorFactor(palette = c("red", "purple", "purple"), 
                             domain = loss_gain_pal_domain)


# 2. Define a user interface 
ui <- fluidPage(
  titlePanel(
    title = div(
      "Seagrass Change in Casco Bay",  
      tags$img(src = "ZOESTRA.jpg",     # First image
               height = "40px",        
               style = "margin-left: 10px; vertical-align: middle;"), # Style for first image
      tags$img(src = "COBALT.jpg", # Second image (assuming it's in 'www' folder)
               height = "40px",        
               style = "margin-left: 5px; vertical-align: middle;")  
    )
  ),
  theme = shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "cover_class",
                         label = "Choose a Cover Class for Extent",
                         choices = unique(max_extent$cover_pct), 
                         selected = NULL), 
      radioButtons(inputId = "reference_year",
                   label = "Reference Year for DEP Seagrass Mapping",
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
      radioButtons(inputId = "gpx_file_type", 
                   label = "Select GPX data type to load:",
                   choices = list("Lines (Yellow Tracks)" = "lines", 
                                  "Points (Orange Waypoints/Track Points)" = "points"),
                   selected = "lines"),
      fileInput("gpx_file", "Upload GPX File",
                accept = c(".gpx"), multiple =TRUE) 
    ),
    mainPanel(
      leafletOutput(outputId = "map") 
    )
  )
)

# 3. define a server
server <- function(input, output){
  
  gpx_tracks <- reactiveValues(lines = list(), points = list())
  last_gpx_feature_to_focus <- reactiveVal(NULL)
  
  plot_extent <- reactive({
    # MODIFICATION: If no cover class is selected, return an empty sf object
    if (is.null(input$cover_class) || length(input$cover_class) == 0) {
      return(max_extent[0, , drop = FALSE]) # Return 0-row version of max_extent
    }
    max_extent %>%
      filter(cover_pct %in% input$cover_class) 
  })
  
  loss_gain_reference <- reactive({
    req(input$reference_year) 
    
    # MODIFICATION: If no cover class is selected, return an empty sf object for loss/gain
    if (is.null(input$cover_class) || length(input$cover_class) == 0) {
      # Return 0-row version of loss_gain, maintaining column structure
      # This ensures downstream addPolygons doesn't error on missing columns like 'type'
      return(loss_gain[0, , drop = FALSE]) 
    }
    
    selected_cover_indices <- which(max_extent$cover_pct %in% input$cover_class)
    
    current_data <- loss_gain %>%
      filter(reference_year == input$reference_year)
    
    if (length(selected_cover_indices) > 0) {
      if ("cover" %in% names(current_data)) {
        current_data <- current_data %>%
          filter(cover %in% selected_cover_indices) 
      } else {
        message("Warning: 'cover' column not found in 'loss_gain' data. Cannot filter by cover class indices.")
        current_data <- current_data[0,] 
      }
    } else { 
      # This means input$cover_class was selected but yielded no matching indices.
      message("Debug: input$cover_class selected, but resulted in no matching indices in max_extent.")
      current_data <- current_data[0,] 
    }
    
    if(nrow(current_data) > 0) {
      are_valid <- st_is_valid(current_data)
      invalid_geoms_present <- any(are_valid == FALSE, na.rm = TRUE)
      if (invalid_geoms_present) { 
        print(paste("Attempting to fix", sum(are_valid == FALSE, na.rm=TRUE), "invalid geometries in loss_gain_reference."))
        to_fix_indices <- which(are_valid == FALSE) 
        if(length(to_fix_indices) > 0) {
          non_empty_to_fix_indices <- to_fix_indices[!st_is_empty(current_data[to_fix_indices,])]
          if(length(non_empty_to_fix_indices) > 0) {
            current_data[non_empty_to_fix_indices, ] <- st_make_valid(current_data[non_empty_to_fix_indices, ])
          }
        }
        current_data <- current_data[!st_is_empty(current_data), ] 
        if(nrow(current_data) > 0){ 
          final_validity <- st_is_valid(current_data)
          current_data <- current_data[final_validity == TRUE | is.na(final_validity), ]
        }
      }
    }
    return(current_data)
  })
  
  observeEvent(input$gpx_file, { 
    req(input$gpx_file, input$gpx_file_type)
    files <- input$gpx_file$datapath
    file_names <- input$gpx_file$name
    gpx_type_to_load <- input$gpx_file_type
    latest_feature_this_upload <- NULL
    for (i in seq_along(files)) {
      f <- files[i]; fname <- file_names[i]; gpx_sf <- NULL
      tryCatch({
        if (gpx_type_to_load == "lines") {
          gpx_sf <- sf::st_read(f, layer = "tracks", quiet = TRUE)
          if (!is.null(gpx_sf) && nrow(gpx_sf) > 0) {
            gpx_tracks$lines[[length(gpx_tracks$lines) + 1]] <- gpx_sf
            latest_feature_this_upload <- gpx_sf
          }
        } else if (gpx_type_to_load == "points") {
          gpx_sf_wp <- tryCatch(sf::st_read(f, layer = "waypoints", quiet = TRUE), error = function(e) NULL)
          gpx_sf_tp <- NULL
          if (is.null(gpx_sf_wp) || nrow(gpx_sf_wp) == 0) {
            gpx_sf_tp <- tryCatch(sf::st_read(f, layer = "track_points", quiet = TRUE), error = function(e) NULL)
          }
          if (!is.null(gpx_sf_wp) && nrow(gpx_sf_wp) > 0) gpx_sf <- gpx_sf_wp
          else if (!is.null(gpx_sf_tp) && nrow(gpx_sf_tp) > 0) gpx_sf <- gpx_sf_tp
          if (!is.null(gpx_sf) && nrow(gpx_sf) > 0) {
            gpx_tracks$points[[length(gpx_tracks$points) + 1]] <- gpx_sf
            latest_feature_this_upload <- gpx_sf
          }
        }
        if (!is.null(gpx_sf) && nrow(gpx_sf) > 0) {
          current_crs <- st_crs(gpx_sf)
          transformed_sf <- gpx_sf 
          if (is.na(current_crs)) { st_crs(transformed_sf) <- 4326 }
          else if (current_crs$epsg != 4326) { transformed_sf <- st_transform(transformed_sf, crs=4326) }
          
          if(gpx_type_to_load == "lines" && length(gpx_tracks$lines) > 0 ) gpx_tracks$lines[[length(gpx_tracks$lines)]] <- transformed_sf
          if(gpx_type_to_load == "points" && length(gpx_tracks$points) > 0 ) gpx_tracks$points[[length(gpx_tracks$points)]] <- transformed_sf
          latest_feature_this_upload <- transformed_sf 
        } else if (is.null(gpx_sf) || nrow(gpx_sf) == 0) {
          message(paste("No valid", gpx_type_to_load, "data found in", fname))
        }
      }, error = function(e) { message(paste("Failed to read GPX file:", fname, "-", e$message)) })
    }
    if (!is.null(latest_feature_this_upload)) last_gpx_feature_to_focus(latest_feature_this_upload)
  })
  
  boat_launches <- reactive({ 
    if (input$toggle_boat_luanches == "On") return(casco_boat_ramps) else return(NULL)
  })
  most_recent_extent <- reactive({ 
    if (input$toggle_most_recent_extent == "On") return(most_recent_coverage) else return(NULL)
  })
  
  output$map <- renderLeaflet({ 
    b <- st_bbox(max_extent) |> as.numeric()
    casco_map <- leaflet() |> 
      addProviderTiles(provider = "Esri.WorldTopoMap", group = "Topographic") |> 
      addProviderTiles(provider = "Esri.WorldImagery", group = "Satellite") |> 
      fitBounds(b[1], b[2], b[3], b[4]) |> 
      addLegend(pal = loss_gain_pal, values = loss_gain_pal_domain, title = "Loss or Gain") |> 
      addLegend(colors = c("blue", "red"), labels = c("Boatramps", "Eelgrass"), title = "Toggles", position = "bottomright") |> 
      addLegend(colors = c("yellow", "orange"), labels = c("Line data", "Point Data"), title = "Uploaded Data") |> 
      addLayersControl(
        baseGroups = c("Topographic", "Satellite"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, autoCenter = TRUE, setView = TRUE))   
    return(casco_map)
  })
  
  observe({ 
    plot_extent_data <- plot_extent()
    loss_gain_ref_data <- loss_gain_reference()
    boat_launches_data_val <- boat_launches()
    most_recent_extent_data_val <- most_recent_extent()
    gpx_lines_to_plot <- gpx_tracks$lines
    gpx_points_to_plot <- gpx_tracks$points
    feature_to_focus <- last_gpx_feature_to_focus()
    
    proxy <- leafletProxy(mapId = "map") |> 
      clearShapes() |> 
      clearGroup("boat_launches")
    
    if (!is.null(plot_extent_data) && nrow(plot_extent_data) > 0) {
      proxy <- proxy |> addPolylines(data = plot_extent_data, color = "black", weight = 2)
    }
    if (!is.null(loss_gain_ref_data) && nrow(loss_gain_ref_data) > 0 && "type" %in% names(loss_gain_ref_data)) {
      proxy <- proxy |> addPolygons(data = loss_gain_ref_data, fillColor = ~loss_gain_pal(type), stroke = FALSE, fillOpacity = 1)
    } # Debug messages for empty loss_gain_ref_data can be added here if needed
    
    if (input$toggle_boat_luanches == "On" && !is.null(boat_launches_data_val) && nrow(boat_launches_data_val) > 0) {
      proxy <- proxy |> addCircleMarkers(data = boat_launches_data_val, color = "blue", stroke = TRUE, fillOpacity = 0.8, radius = 4, group = "boat_launches")
    }
    if (input$toggle_most_recent_extent == "On" && !is.null(most_recent_extent_data_val) && nrow(most_recent_extent_data_val) > 0) {
      proxy <- proxy |> addPolygons(data = most_recent_extent_data_val, fillColor = "red", stroke = FALSE, fillOpacity = 0.8)
    }
    if (length(gpx_lines_to_plot) > 0) {
      for (gpx_line_sf in gpx_lines_to_plot) {
        if (!is.null(gpx_line_sf) && nrow(gpx_line_sf) > 0) {
          proxy <- proxy |> addPolylines(data = gpx_line_sf, color = "yellow", weight = 4)
        }
      }
    }
    if (length(gpx_points_to_plot) > 0) {
      for (gpx_point_sf in gpx_points_to_plot) {
        if (!is.null(gpx_point_sf) && nrow(gpx_point_sf) > 0) {
          proxy <- proxy |> addCircleMarkers(data = gpx_point_sf, color = "orange", radius = 5, stroke = TRUE, weight = 1, fillOpacity = 0.9)
        }
      }
    }
    
    if (!is.null(feature_to_focus) && nrow(feature_to_focus) > 0 && inherits(feature_to_focus, "sf")) {
      geom_column <- st_geometry(feature_to_focus)
      if (length(geom_column) > 0 && !all(st_is_empty(geom_column))) {
        geom_types <- unique(st_geometry_type(feature_to_focus))
        if (any(geom_types %in% c("LINESTRING", "MULTILINESTRING", "POLYGON", "MULTIPOLYGON")) || 
            (all(geom_types %in% c("POINT", "MULTIPOINT")) && nrow(feature_to_focus) > 1)) { 
          bbox <- st_bbox(feature_to_focus) 
          if(all(is.finite(as.numeric(bbox)))) {
            proxy |> flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
          }
        } else if (all(geom_types %in% c("POINT", "MULTIPOINT")) && nrow(feature_to_focus) == 1) { 
          coords <- st_coordinates(feature_to_focus) 
          if (nrow(coords) > 0 && !any(is.na(coords[1,1:2]))) {
            proxy |> flyTo(lng = coords[1,1], lat = coords[1,2], zoom = 16) 
          }
        } else { 
          centroid_geom <- suppressWarnings(st_centroid(geom_column))
          if(length(centroid_geom) > 0 && !st_is_empty(centroid_geom[[1]])) {
            coords <- st_coordinates(centroid_geom)
            if (nrow(coords) > 0 && !any(is.na(coords[1,1:2]))) {
              proxy |> flyTo(lng = coords[1,1], lat = coords[1,2], zoom = 14) 
            }
          }
        }
      }
      last_gpx_feature_to_focus(NULL) 
    }
  }) 
} 

shinyApp(ui = ui, server = server)