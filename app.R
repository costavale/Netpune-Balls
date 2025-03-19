# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(shinythemes)
library(plotly)
library(googlesheets4)

# Define file path for saving data
data_file <- "data/marked_points.csv"

# Load existing data (if the file exists)
if (file.exists(data_file)) {
  marked_points_data <- read.csv(data_file, stringsAsFactors = FALSE) %>%
    mutate(lat = as.numeric(lat), lon = as.numeric(lon), date = as.Date(date))
} else {
  marked_points_data <- data.frame(
    lat = numeric(0), 
    lon = numeric(0),
    date = as.Date(character(0)),
    site = character(0), 
    name = character(0), 
    photo = character(0), stringsAsFactors = FALSE) 
}


# Define UI for the app
ui <- navbarPage(

  title = "Neptune Balls - Plastic Sentinels",
  theme = shinytheme("sandstone"),
  
  # First page: Project description
  tabPanel("Project Description",
           fluidPage(
             titlePanel("Rolling in the deep: Neptune balls as plastic sentinels"),
             h5("by Valentina Costa and Cristina Pedà"),
             br(),

             tags$div(
               style = "text-align: center;",
               if (file.exists("www/doll-detritus.jpg")) {
                 img(src = "doll-detritus.jpg", height = "300px")
               } else {
                 p("Image not found.")
               }
             ),
             br(),

             p("Plastic pollution poses a significant threat to marine ecosystems. 
               Seagrass beds have been recognized as natural sinks for plastics, 
               while Neptune balls are efficient particle traps. 
               This pilot study seeks to explore the potential of these spherical 
               structures as sentinels for plastic pollution along the coast 
               of the Mediterranean Sea by analyzing their entrapment capability. 
               We aim to introduce an innovative approach to assessing plastic 
               pollution in marine ecosystems."),
             br(),

             tags$p("Learn more about the project at the experiment.com website: ", 
                    tags$a(href = "https://doi.org/10.18258/68571", 
                           "Rolling in the deep: Neptune balls as plastic sentinels", 
                           target = "_blank"))
             )
  ),

  # Second page: Map, inputs, and table
  tabPanel("Map and Data",
           fluidPage(
            fluidRow(
               column(8, leafletOutput("map", height = 600)
               ),
               
               # Column for user input (latitude, longitude, and site)
               column(4, 
                      wellPanel(
                        numericInput("lat", "Latitude", value = 35, min = -90, max = 90),
                        numericInput("lon", "Longitude", value = 15, min = -180, max = 180),
                        dateInput("date", "Date", value = Sys.Date(), format = "yyyy-mm-dd"),
                        textInput("site", "Site", value = "Name of the Site"),
                        textInput("name", "Name", value = "Name of the collector"),
                        fileInput("photo", "Upload a photo", accept = c('image/png', 'image/jpeg')),
                        actionButton("mark_button", "Mark Point"),
                        br(), br()
                        )
               )
             ),
             
             # Table section below the map
             fluidRow(
               column(12, 
                      br(),
                      h4("Neptune Balls - Sampled Points"),
                      DTOutput("points_table"),
                      downloadButton("download_data", "Download Data")
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
               column(6,
                      h4("Sampling Statistics"),
                      p("Total number of sampling sites:"),
                      textOutput("num_points")
                      ),
               column(6,
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
                   popup = ~paste("<br>Site:", site, "<br>Date:", date, "<br>Lat:", lat, "<br>Lon:", lon))
    }
    
    map
  })
  
  # Observe when the "Mark Point" button is clicked
  observeEvent(input$mark_button, {
    
    # Debugging: Print input values
    print(paste("Latitude:", input$lat))
    print(paste("Longitude:", input$lon))
    print(paste("Site:", input$site))
    print(paste("Name:", input$name))
    print(paste("Photo input:", input$photo))  # Check if NULL or unexpected type
    
    # Handle file upload
  img_path <- NA_character_
  
  if (!is.null(input$photo) && !is.null(input$photo$datapath)) {
    img_path <- paste0("www/", input$photo$name)  # Store in the "www" folder
    file.copy(input$photo$datapath, img_path, overwrite = TRUE)
  }
  
  # Debugging: Print processed photo path
  print(paste("Stored Photo Path:", img_path))
    
    new_point <- data.frame(
        lat = as.numeric(input$lat), 
        lon = as.numeric(input$lon), 
        date = as.Date(input$date),
        site = as.character(input$site), 
        name = as.character(input$name), 
        photo = ifelse(is.na(img_path), "", img_path),  # Convert NA to ""
        stringsAsFactors = FALSE
    )
    
    # Debugging
    print("New point created:")
    print(new_point)
    
    updated_points <- bind_rows(marked_points(), new_point)
    
    # Debugging: Print updated dataset
    print("Updated dataset:")
    print(updated_points)
    
    marked_points(updated_points)

    write.csv(updated_points, data_file, row.names = FALSE)
    
    # Debugging: Print after saving
    print("Data saved successfully!")
    
    # Debugging: Ensure correct column names exist before updating the map
    print("Updated dataset columns: ")
    print(colnames(updated_points))
    
    print("Debug - marked_points() structure:")
    str(marked_points())
    
    # Update map with images in popups
    output$map <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%
    addMarkers(
      data = marked_points(),
      lng = ~lon,
      lat = ~lat,
      popup = ~paste0(
        "<b>Site:</b> ", site, "<br>",
        "<b>Date:</b> ", date, "<br>",        
        "<b>Collector:</b> ", name, "<br>",
        "<b>Lat:</b> ", lat, "<br>",
        "<b>Lon:</b> ", lon, "<br>"
      )
    )
})
  })

  # Render the dataframe as a table below the map
  output$points_table <- renderDT({
    datatable(marked_points(), editable = "cell", selection = "single")
  }, server = FALSE)
  
  # Observe Table Cell edit
  observeEvent(input$points_table_cell_edit, {
    info <- input$points_table_cell_edit
    updated_data <- marked_points()
    
    # Correctly update lat, lon, or site without introducing new columns
    if (info$col == 1) {
      updated_data[info$row, "lat"] <- as.numeric(info$value)
    } else if (info$col == 2) {
      updated_data[info$row, "lon"] <- as.numeric(info$value)
    } else if (info$col == 3) {
      updated_data[info$row, "site"] <- info$value
    }
    
    # Remove rows with NA lat/lon
    updated_data <- updated_data %>% filter(!is.na(lat) & !is.na(lon))
    
    marked_points(updated_data)
    write.csv(updated_data, data_file, row.names = FALSE)
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(data = marked_points(), ~lon, ~lat, popup = ~paste("Site:", site, "<br>Lat:", lat, "<br>Lon:", lon))
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