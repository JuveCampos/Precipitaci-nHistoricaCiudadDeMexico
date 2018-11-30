
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(dygraphs)
library(xts)



# Global

root <- "Info Geografica/ShapeFiles/"

# Estado CDMX
CDMX_shape <- st_read(paste0("Info Geografica/ShapeFiles/mapa_division_politica/division_politica_estatal/dest_2010gw/dest_2010gw.shp")) %>%
  filter(ENTIDAD == "DISTRITO FEDERAL")

# MUNICIPIOS CDMX
CDMX <- st_read(paste0("Info Geografica/ShapeFiles/mapa_division_politica/division_politica_municipal/muni_2012gw/Muni_2012gw.shp")) %>%
  filter(CVE_ENT == "09")

# Estaciones Meteorologicas CDMX
est <- st_intersection(CDMX_shape, st_transform(st_read(paste0(root, "estclimgw_c/estclimcw.shp")), crs = st_crs(CDMX)))
estaciones <- as.factor(est$ID_ESTACIO) 
estaciones <- estaciones[order(estaciones)]


# Define UI for application that draws a histogram
ui <- fluidPage(
  h2("Tablero de trabajo"), 
  h3("Precipitación Historica en la Ciudad de México"),
  br(), br(),
    fluidRow(
      column(12, leafletOutput("Mapa"))
    ), br(), br(),
    fluidRow(
      column(12, dygraphOutput("TS"))
    ), br(), br()
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  # HACER QUE AL APRETAR CLICK SE COMUNIQUE CON OTROS OBJETOS #
  mark_of_click <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$Mapa_marker_click, 
               {
                 pol_of_click <- input$mark_of_click
                 p <- input$Mapa_marker_click$id
                 #print(p)
                 mark_of_click$clickedMarker <- input$Mapa_marker_click$id
                 print(as.numeric(mark_of_click$clickedMarker))
               })
  
  
  # MAPA #
  output$Mapa <- renderLeaflet({
    # Ploteamos
    leaflet(est) %>%
      addTiles() %>%
      addMarkers(label = paste0(est$ID_ESTACIO, " ", est$NOMBRE), layerId = est$ID_ESTACIO) 
  })
  
  # Serie de Tiempo #
  
  output$TS <- renderDygraph({
    
    print(mark_of_click$clickedMarker)
    print(is.null(mark_of_click$clickedMarker))
    
    if(is.null(mark_of_click$clickedMarker)){
      i <- "9026"
    } else {
      i <- mark_of_click$clickedMarker
    }
    
    tiempo <- read.csv(paste0("DatosEM", i , "TEST.csv"))
    #tiempo <- read.csv(paste0("DatosEM9005TEST.csv"))
    tiempo <- tiempo[,3:8]
    tiempo$fecha <- as.Date(tiempo$fecha, format = "%d/%m/%Y")
    
    xts <- xts(x = tiempo$precip, order.by = tiempo$fecha)
    
    tiempo$fecha[1]
    tiempo$fecha[nrow(tiempo)]
    
    dygraph(xts, main = paste0("Precipitación presente en la estacion ", 
                               i, "<br>", est$NOMBRE[est$ID_ESTACIO == as.numeric(i)])) %>%
      dyRangeSelector(dateWindow = c( tiempo$fecha[1], tiempo$fecha[nrow(tiempo)]))
    #lapply(tiempo[,1:7], class)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

