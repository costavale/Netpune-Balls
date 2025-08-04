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
  mutate(lat = as.numeric(lat), lon = as.numeric(lon), date = as.Date(date, format = "%d/%m/%Y"))


# Define UI
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
                      wellPanel(
                        h4("Sampling Statistics"),
                        p("Total number of sampling sites:"),
                        textOutput("num_points")
                        )),
               column(6,
                      wellPanel(
                        h4("Geographical Spread:"),
                      plotlyOutput("geo_spread_lat"),
                      plotlyOutput("geo_spread_lon")
               )
               )
             )
           )
  ),
  
  # About page
  tabPanel("About",
           fluidPage(
             titlePanel("Who we are?"),
             
             fluidRow(
               column(6,
                      wellPanel(
                        tags$div(style = "overflow: hidden;",
                                 tags$img(src = "valentina-photo.jpg", 
                                          style = "float: left; margin-right: 15px; width: 150px;"),
                                 h4("Valentina Costa"),
                                 p("My research activity is focused on the study of the
                        ecology of seagrass marine ecosystems. The main purpose
                        of my research is to answer ecological questions by
                        applying an integrated approach based on three main
                        keywords: map, protect and restore.To map and protect
                        any ecosystem, knowledge represents the starting point,
                        and how different environmental variables can affect
                        the ecosystem itself is also necessary. During my PhD,
                        I worked on evaluating the effects of particular
                        environmental gradients (acidification and confinement)
                        on seagrass detritus decomposition dynamics and the
                        colonization by marine invertebrates of the detritus
                        itself. After my PhD, I focused my research on the
                        effects of ocean acidification on seagrasses also
                        using experimental approaches and mesocosm studies. More
                        recently, my interest has been more focused on the
                        keyword restore. One approach to ecosystem restoration
                        is the transplanting of organisms from non-impacted
                        areas to areas that need to be reforested. For this
                        reason, part of my research activity is focused on the
                        evaluation of transplantation techniques used for
                        marine seagrass restoration activities.I am also very
                          interested in new open-source technologies (e.g.
                          Arduino) and how these can be applied to the study of
                          the ecology of marine ecosystems.")
                        )
                      )
                      
                        
                      ),
               column(6,
                      wellPanel(
                        h4("Cristina Pedà"),
                        p("My research activity focuses on the study of 
                          different aspects of marine litter pollution with 
                          particular attention to the impacts of plastic on the 
                          marine ecosystems and biodiversity. During my PhD, I 
                          investigated the effects of microplastics ingestion 
                          in a commercially valuable species under controlled 
                          laboratory conditions. Then, I continued to study the 
                          plastic ingestion phenomenon in teleost and 
                          cartilaginous fish and in cephalopod molluscs as well 
                          as the potential transfer of plastics along the 
                          trophic web. To explore the issue of marine litter 
                          pollution, I also studied the presence of litter in 
                          marine different compartments and in particular its 
                          composition and abundance on the seafloor and on the 
                          sea surface, as well as the interaction between litter 
                          and marine organisms. In this view, part of my 
                          research activity is focused on the application and 
                          validation of methodologies for the microplastics 
                          extraction from different marine matrices (sediment, 
                          seawater, and biota) and polymer’s identification by 
                          Fourier transforms infrared spectroscopy (FT-IR). In 
                          the future, I would like to investigate the potential 
                          effects of plastics in sensitive habitats and also 
                          in underexplored organisms.")
                      )  
                      )
             ) 
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
  
  output$map <- renderLeaflet({
    filtered <- filtered_points()
    
    map <- leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      setView(lng = 13.5, lat = 37.5, zoom = 4.5)

    # Check if the dataframe has valid numeric data before adding markers
    if (nrow(filtered) > 0) {
      map <- map %>%
        addMarkers(
          data = filtered, 
          ~as.numeric(lon), ~as.numeric(lat),
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
    df <- marked_points()[, !(names(marked_points()) %in% c("id", "photo"))]
    df$date <- format(as.Date(df$date), "%d-%m-%Y")  # format date as string
    
    datatable(
      df,
      selection = "single"
    )
  }, server = FALSE)
  
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
  
  create_geo_spread_plot <- function(data, variable, var_name) {
    data <- na.omit(data)
    
    if (length(data) <= 1) return(plotly_empty())
    
    # Use a fixed number of bins (e.g., 8) and pretty breaks
    breaks <- pretty(data, n = 8)
    if (length(breaks) < 2) {
      breaks <- seq(min(data), max(data), length.out = 3)
    }
    
    cut_data <- cut(data, breaks = breaks, include.lowest = TRUE, right = FALSE)
    counts <- table(cut_data)
    
    color_palette <- c("#2c7bb6", "#00a6ca", "#00ccbc", "#90eb9d", "#ffff8c")
    
    plot_ly(
      x = names(counts),
      y = as.numeric(counts),
      type = "bar",
      name = var_name,
      marker = list(color = color_palette),
      hoverinfo = "text",
      text = paste("Range:", names(counts), "<br>Count:", as.numeric(counts))
    ) %>%
      layout(
        title = list(text = paste(var_name, "Distribution"), x = 0.5),
        xaxis = list(title = var_name, showgrid = FALSE),
        yaxis = list(title = "Count", showgrid = FALSE),
        plot_bgcolor = "rgba(240, 240, 240, 0.8)",
        paper_bgcolor = "rgba(255, 255, 255, 0.8)",
        margin = list(l = 50, r = 50, b = 50, t = 50)
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