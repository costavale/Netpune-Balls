# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(shinythemes)

# Define file path for saving data
data_file <- "marked_points.csv"

# Load existing data (if the file exists)
if (file.exists(data_file)) {
  marked_points_data <- read.csv(data_file, stringsAsFactors = FALSE)
} else {
  marked_points_data <- data.frame(lat = numeric(0), lon = numeric(0), tag = character(0))
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
                      h4("Marked Points"),
                      DTOutput("points_table")  # Display the dataframe with points
               )
             )
           )
  )
)



# Define server logic
server <- function(input, output, session) {
  
  # Reactive dataframe to store marked points
  marked_points <- reactiveVal(marked_points_data)
  
  # Initial leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  # Using Stamen Toner basemap
      setView(lng = 13.5, lat = 37.5, zoom = 4.5) 
  })
  
  # Observe when the "Mark Point" button is clicked
  observeEvent(input$mark_button, {
    # Get the coordinates from the inputs
    lat <- input$lat
    lon <- input$lon
    tag <- input$tag
    
    # Update the dataframe with the new coordinates
    new_point <- data.frame(lat = lat, lon = lon, tag = tag)
    updated_points <- rbind(marked_points(), new_point)  # Add the new point to the existing dataframe
    marked_points(updated_points)  # Update the reactive dataframe
    
    # Save updated data to CSV
    write.csv(updated_points, data_file, row.names = FALSE)
    
    # Update the map with all marked points
    leafletProxy("map") %>%
      clearMarkers() %>%  # Clear existing markers
      addMarkers(data = marked_points(), ~lon, ~lat, popup = ~paste("Tag:", tag, "<br>Lat:", lat, "<br>Lon:", lon))  # Add all markers
  })
  
  # Render the dataframe as a table below the map
  output$points_table <- renderDT({
    marked_points()  # Display the dataframe with marked points
  })
  
  # Allow users to download the data
  output$download_data <- downloadHandler(
    filename = function() { "Neptune_Balls.csv" },
    content = function(file) {
      write.csv(marked_points(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)