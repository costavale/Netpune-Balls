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

# Load existing data
marked_points_data <- read.csv(data_file, stringsAsFactors = FALSE) %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon), date = as.Date(date))



# Define UI for the app
ui <- navbarPage(

  title = "Neptune Balls - Plastic Sentinels",
  theme = shinytheme("sandstone"),
  
  # First page: Project description
  tabPanel("Project Description",
           fluidPage(
             titlePanel("Rolling in the deep: Neptune balls as plastic sentinels"),
             h5("by Valentina Costa and Cristina Pedà**"),
             br(),

             tags$div(
               style = "text-align: center;",
               img(src = "doll-detritus.jpg", height = "500px")
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
                        textInput("search_country", "Search by Country", value = ""),
                        textInput("search_site", "Search by Site", value = ""),
                        actionButton("search_button", "Search"),
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
  
  # Filtered points based on search criteria
  filtered_points <- reactive({
    data <- marked_points()
    
    if (input$search_country != "") {
      data <- data %>% filter(grepl(input$search_country, country, ignore.case = TRUE))
    }
    
    if (input$search_site != "") {
      data <- data %>% filter(grepl(input$search_site, site, ignore.case = TRUE))
    }
    
    return(data)
  })
  
  # # Render map with filtered points
  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addProviderTiles("Esri.WorldImagery") %>%
  #     setView(lng = 13.5, lat = 37.5, zoom = 4.5) %>%
  #     addMarkers(
  #       data = filtered_points(),
  #       lng = ~lon,
  #       lat = ~lat,
  #       popup = ~paste0(
  #         "<b>Site:</b> ", site, "<br>",
  #         "<b>Collector:</b> ", name, "<br>",
  #         "<b>Date:</b> ", date, "<br>",
  #         "<b>Lat:</b> ", lat, "<br>",
  #         "<b>Lon:</b> ", lon, "<br>",
  #         ifelse(photo != "", paste0('<img src="', photo, '" width="200px">'), "")
  #       )
  #     )
  # })
  
  output$map <- renderLeaflet({
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 13.5, lat = 37.5, zoom = 4.5)

    # Check if the dataframe has valid numeric data before adding markers
    if (nrow(filtered_points()) > 0) {
      map <- map %>%
        addMarkers(data = filtered_points(), ~as.numeric(lon), ~as.numeric(lat),
                   popup = ~paste0(
                    ifelse(photo != "", paste0('<img src="', photo, '" width="200px">'), ""),
                    "<b>Site:</b> ", site, "<br>",
                            "<b>Collector:</b> ", name, "<br>",
                            "<b>Date:</b> ", date, "<br>",
                            "<b>Lat:</b> ", lat, "<br>",
                            "<b>Lon:</b> ", lon, "<br>"))
    }

    map
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
    
    marked_points(updated_data)
   
     })

  
  # Allow users to download the data
  output$download_data <- downloadHandler(
    filename = function() { "Neptune_Balls.csv" },
    content = function(file) {
      write.csv(filtered_points(), file, row.names = FALSE)
    }
  )
  
  # Render statistics in the summary page
  output$num_points <- renderText({
    nrow(filtered_points())
  })
  
  # Function to create column graphs for geographical spread (latitude and longitude)
create_geo_spread_plot <- function(data, variable, var_name) {
  # Dynamic breaks
  data_range <- max(data) - min(data)
  if (data_range <= 10) {
    by_value <- 1
  } else if (data_range <= 20) {
    by_value <- 2
  } else if (data_range <= 50) {
    by_value <- 5
  } else if (data_range <= 100) {
    by_value <- 10
  } else {
    by_value <- 20
  }
  breaks <- seq(min(data), max(data), by = by_value)  # Define breaks (intervals)
  cut_data <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
  counts <- table(cut_data)

  # Define a color palette
  color_palette <- c("#2c7bb6", "#00a6ca", "#00ccbc", "#90eb9d", "#ffff8c")

  plot_ly(
    x = names(counts),
    y = as.numeric(counts),
    type = "bar",
    name = var_name,
    marker = list(color = color_palette[1]), # Use the first color of the palette
    hoverinfo = "text",
    text = paste("Range:", names(counts), "<br>Count:", as.numeric(counts))
  ) %>%
    layout(
      title = list(text = paste(var_name, "Distribution"), x = 0.5), # Centered title
      xaxis = list(title = var_name, showgrid = FALSE), # Remove gridlines
      yaxis = list(title = "Count", showgrid = FALSE), # Remove gridlines
      plot_bgcolor = "rgba(240, 240, 240, 0.8)", # Light gray background
      paper_bgcolor = "rgba(255, 255, 255, 0.8)", # White background
      margin = list(l = 50, r = 50, b = 50, t = 50) # Adjust margins
    )
}

# Render geographical spread plots
output$geo_spread_lat <- renderPlotly({
  if (nrow(filtered_points()) > 0) {
    create_geo_spread_plot(filtered_points()$lat, "Latitude", "Latitude")
  } else {
    plotly_empty()
  }
})

output$geo_spread_lon <- renderPlotly({
  if (nrow(filtered_points()) > 0) {
    create_geo_spread_plot(filtered_points()$lon, "Longitude", "Longitude")
  } else {
    plotly_empty()
  }
})

}

shinyApp(ui = ui, server = server)