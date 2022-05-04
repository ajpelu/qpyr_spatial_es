library(raster)
library(rgdal)
library(leaflet)
library(mapview)
library(shiny)


set.seed(1991)
x1 <- matrix(round(runif(min=1, max=8, n=100),0),10,10)
x2 <- matrix(round(runif(min=1, max=10, n=100),0),10,10)
x3 <- matrix(round(runif(min=1, max=12, n=100),0),10,10)
r1 <- raster(x1); names(r1) <- "K8"
r2 <- raster(x2); names(r2) <- "K10"
r3 <- raster(x3); names(r3) <- "K12"
extent(r1) <- c(144,148,-35,-31)
projection(r1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(r2) <- c(144,148,-35,-31)
projection(r2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
extent(r3) <- c(144,148,-35,-31)
projection(r3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# extent(s) <- c(144,148,-35,-31)
# projection(s) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 

ui <- fluidPage(
  
  # App title ----
  titlePanel("Some title"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h4("Which raster", align = "left"),
      br(),
      # Input: Box for the number of classes ----
      sliderInput(inputId = "K",
                  label = "Desired number of classes",
                  min=8, max=12, value = 10, step = 2),
      
      br()
    ),
    
    # Main panel ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Whole raster",
                           leafletOutput(outputId = "KMap",width = "100%",height=700)
                  ),
                  tabPanel("Selected pixels",
                           # For a given number of classes, which class do I want to examine more closely?
                           uiOutput("ClusterChoice"),
                           br(),
                           actionButton("go", "Update class"),
                           textOutput("selected_class"),
                           br(),
                           leafletOutput(outputId = "ClusterMap",width = "100%",height=700),
                           br()
                  )
      )
    )
  )
)


server <- function(input, output) {
  
  ### The max values of the numeric input depends on the value for the first input
  output$ClusterChoice <- renderUI({
    numericInput(inputId = "clusteri",
                 label = "Pixel value to examine",
                 min = 1, max = input$K, value = 1, step = 1)
  })
  
  ### Transform the selected value into another object to use in several places
  Geno.i <- reactive({as.numeric(input$clusteri)})
  Kchar <- reactive({as.character(input$K)})
  
  ### Remind the user his option
  output$selected_value <- renderText({paste("Locations of the selected pixel value ", Geno.i())})
  
  ### Select raster layer
  K.input <- reactive({
    switch(Kchar(),
           "8" = r1,
           "10" = r2,
           "12" = r3)
  })
  
  # # Its output type is a plot (map)
  # output$KMap <- renderPlot({
  #     ### Plot the selected raster with levelplot
  #     levelplot(K.input(), margin=FALSE, par.settings=plasmaTheme)
  # })
  
  output$KMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      ### using default raster palette, Spectral
      addRasterImage(K.input(), opacity = 1, #colors=pal()
                     layerId = "values", maxBytes = 300000000) %>%
      fitBounds(lng1=144, lat1=-35, lng2=148, lat2=-31) %>%
      leafem::addMouseCoordinates() %>%
      leafem::addImageQuery(K.input(), project = FALSE, type="mousemove", layerId = "values")
  })
  
  ### Mask all values different than our choice
  GenoRas <- eventReactive(input$go, {
    calc(K.input(), fun = function(x) {x[x != Geno.i()] <- NA; return(x) })
  })
  
  output$ClusterMap <- renderLeaflet({
    
    label.G <- as.character(Geno.i())
    
    # Default tiles. Just plotting presence, absence of that class
    leaflet() %>% 
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles("Esri.WorldImagery") %>% # , group = "World Imagery"
      addProviderTiles("OpenTopoMap", group = "Topo Map") %>%
      ### Set bounds
      fitBounds(lng1=144, lat1=-35, lng2=148, lat2=-31) %>%
      ### Genosoil layer
      addRasterImage(GenoRas(), colors = "red", maxBytes = 300000000, opacity = 0.5) %>%
      ### context
      addMiniMap() %>%
      addLegend("topright", colors="red",labels = label.G,
                title = "Genosoil class",
                opacity = 1)  %>%
      #Layers control
      addLayersControl(
        baseGroups = c("World Imagery", "Topo Map"), # "OSM (default)",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}


shinyApp(ui = ui, server = server)