# Librerias
library(dplyr)
library(readr)
library(sf)
library(sp)
library(stringr)
library(rebus)
library(leaflet)
library(raster)
library(kriging)

# Importar datos
data <- read_csv("Datos_Normales_Climatologicas.csv") 
estgw <- st_read("estclimgw/estclimgw.shp")
zmvm <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/EdosZM.geojson")


  # Centroide
  cntrd <- st_centroid(st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/ZMVM_shell.geojson"))
  cntrd
  peri <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/ZMVM_shell.geojson") %>%
    as_Spatial() %>%
    as('SpatialLines') 
  class(peri)
  plot(peri)
  peri
  
  
  # Exploracion datos
head(estgw)
class(data$Id_Estacion)
data$Id_Estacion <-  str_remove(data$Id_Estacion, pattern = START %R% "0")

# VEr si estan las estaciones en los datos
data$Id_Estacion %in% estgw$ID_ESTACIO

# Creamos una base con los datos de interes
map <- merge(estgw, data, by.x = "ID_ESTACIO", by.y = "Id_Estacion")
class(map)

# FIltramos por el primer periodo
map_5110 <- map %>%
  filter(Periodo == "5110")

# Hacemos el Leaflet

leaflet(map_5110) %>%
  addTiles() %>%
   addMarkers(label = paste0(map_5110$ID_ESTACIO, " ",  map_5110$NOMBRE)) %>%
  addPolygons(data = zmvm, color = "gray", weight = 1)


xyz <- as.data.frame(cbind(st_coordinates(map_5110$geometry)[,1], st_coordinates(map_5110$geometry)[,2], map_5110$SEPTIEMBRE))

a <- raster::rasterFromXYZ(xyz = xyz)

