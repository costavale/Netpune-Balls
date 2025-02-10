# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(shinythemes)
library(plotly)

# Define file path for saving data
data_file <- "data/marked_points.csv"

# Load existing data (if the file exists)
if (file.exists(data_file)) {
  marked_points_data <- read.csv(data_file, stringsAsFactors = FALSE) %>%
    mutate(lat = as.numeric(lat), lon = as.numeric(lon))
} else {
  marked_points_data <- data.frame(lat = numeric(0), lon = numeric(0), tag = character(0), stringsAsFactors = FALSE)
}


# Define UI for the app
ui <- navbarPage(
  
  # Title for the app
  title = "Plastic Sentinels",
  theme = shinytheme("sandstone"),
  
  # First page: Project description
  tabPanel("Project Description",
           fluidPage(
             titlePanel("Rolling in the deep: Neptune balls as plastic sentinels"),
             
             # Add the project description text here
             h5("by Valentina Costa and Cristina Pedà"),
             br(),
             p("Plastic pollution poses a significant threat to marine ecosystems. Seagrass beds have been recognized as natural sinks for plastics, while Neptune balls are efficient particle traps. This pilot study seeks to explore the potential of these spherical structures as sentinels for plastic pollution along the coast of the Mediterranean Sea by analyzing their entrapment capability. We aim to introduce an innovative approach to assessing plastic pollution in marine ecosystems."),
           )
  ),
  
  # Second page: Map, inputs, and table
  tabPanel("Map and Data",
           fluidPage(
             # Layout with map on the left and input on the right
             fluidRow(
               # Column for the map
               column(8, 
                      leafletOutput("map", height = 500)  # Map takes up 8/12 of the width
               ),
               
               # Column for user input (latitude, longitude, and tag)
               column(4, 
                      wellPanel(
                        numericInput("lat", "Latitude", value = 35, min = -90, max = 90),
                        numericInput("lon", "Longitude", value = 15, min = -180, max = 180),
                        textInput("tag", "Tag/Name", value = "Point"),
                        actionButton("mark_button", "Mark Point"),
                        br(), br(),
                        downloadButton("download_data", "Download Data")  # Button to download data
                        )
               )
             ),
             
             # Table section below the map
             fluidRow(
               column(12, 
                      br(),
                      h4("Neptune Balls - Sampled Points"),
                      DTOutput("points_table"),  # Display the dataframe with points
                      actionButton("delete_button", "Delete Selected Point", class = "btn-danger") # Delete button
               )
             )
           )
  ),

  # Summary page with statistics
  tabPanel("Summary",
           fluidPage(
             titlePanel("Sampling Activity Summary"),
             
             # Display summary information
             fluidRow(
               column(4,
                      h4("Sampling Statistics"),
                      p("Total number of points:"),
                      textOutput("num_points"),
                      ),
               column(8,
                      p("Geographical Spread:"),
                      plotlyOutput("geo_spread_lat"),
                      plotlyOutput("geo_spread_lon")
               )
               
             )
           )
  ),
  
  # About page
  tabPanel("About",
           fluidPage(
             titlePanel("Who we are?"),
             
             # Add the project description text here
             h5("Valentina Costa and Cristina Pedà"),
             br(),
           )
  )
  
  )



# Define server logic
server <- function(input, output, session) {
  
  # Reactive dataframe to store marked points
  marked_points <- reactiveVal(marked_points_data)
  
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 13.5, lat = 37.5, zoom = 4.5)
    
    # Check if the dataframe has valid numeric data before adding markers
    if (nrow(marked_points()) > 0 && all(!is.na(marked_points()$lat)) && all(!is.na(marked_points()$lon))) {
      map <- map %>%
        addMarkers(data = marked_points(), ~as.numeric(lon), ~as.numeric(lat), 
                   popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))
    }
    
    map
  })
  
  # Observe when the "Mark Point" button is clicked
  observeEvent(input$mark_button, {
    new_point <- data.frame(lat = input$lat, lon = input$lon, tag = input$tag)
    updated_points <- rbind(marked_points(), new_point)
    marked_points(updated_points)
    write.csv(updated_points, data_file, row.names = FALSE)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = marked_points(), ~lon, ~lat, popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))
  })
  
  # Observe clicks on the map to add a point
  observeEvent(input$map_click, {
    click <- input$map_click
    if (!is.null(click)) {
      new_point <- data.frame(lat = click$lat, lon = click$lng, tag = "Clicked Point")
      updated_points <- rbind(marked_points(), new_point)
      marked_points(updated_points)
      
      write.csv(updated_points, data_file, row.names = FALSE)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = marked_points(), ~lon, ~lat, popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))
    }
  })
  
  # Render the dataframe as a table below the map
  output$points_table <- renderDT({
    datatable(marked_points(), editable = "cell", selection = "single")
  }, server = FALSE)
  
  # Observe Table Cell edit
  observeEvent(input$points_table_cell_edit, {
    info <- input$points_table_cell_edit
    updated_data <- marked_points()
    
    # Correctly update lat, lon, or tag without introducing new columns
    if (info$col == 1) {
      updated_data[info$row, "lat"] <- as.numeric(info$value)
    } else if (info$col == 2) {
      updated_data[info$row, "lon"] <- as.numeric(info$value)
    } else if (info$col == 3) {
      updated_data[info$row, "tag"] <- info$value
    }
    
    # Remove rows with NA lat/lon
    updated_data <- updated_data %>% filter(!is.na(lat) & !is.na(lon))
    
    marked_points(updated_data)
    write.csv(updated_data, data_file, row.names = FALSE)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = marked_points(), ~lon, ~lat, popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))
  })
  
  # Observe delete button click
  observeEvent(input$delete_button, {
    selected_row <- input$points_table_rows_selected  # Get selected row index
    if (!is.null(selected_row) && length(selected_row) > 0) {
      updated_points <- marked_points()[-selected_row, ]  # Remove selected row
      marked_points(updated_points)
      
      write.csv(updated_points, data_file, row.names = FALSE)
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = updated_points, ~lon, ~lat, popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))
    }
  })
  
  # Allow users to download the data
  output$download_data <- downloadHandler(
    filename = function() { "Neptune_Balls.csv" },
    content = function(file) {
      write.csv(marked_points(), file, row.names = FALSE)
    }
  )
  
  # Render statistics in the summary page
  output$num_points <- renderText({
    nrow(marked_points())
  })
  
  # Function to create column graphs for geographical spread (latitude and longitude)
  create_geo_spread_plot <- function(data, variable, var_name) {
    breaks <- seq(min(data), max(data), by = 5)  # Define breaks (intervals)
    cut_data <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
    counts <- table(cut_data)
    
    plot_ly(
      x = names(counts),
      y = as.numeric(counts),
      type = "bar",
      name = var_name
    ) %>%
      layout(
        title = paste(var_name, "Distribution"),
        xaxis = list(title = var_name),
        yaxis = list(title = "Count")
      )
  }
  
  # Render geographical spread plots
  output$geo_spread_lat <- renderPlotly({
    if (nrow(marked_points()) > 0) {
      create_geo_spread_plot(marked_points()$lat, "Latitude", "Latitude")
    }
  })
  
  output$geo_spread_lon <- renderPlotly({
    if (nrow(marked_points()) > 0) {
      create_geo_spread_plot(marked_points()$lon, "Longitude", "Longitude")
    }
  })
}

shinyApp(ui = ui, server = server)