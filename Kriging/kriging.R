# librerias utilizadas
library(dplyr) 
library(rebus)
library(stringr)
library(sf)
library(kriging)
library(raster)
library(leaflet)


# Leemos datos (rutas absolutas!)
estaciones <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/Datos_Estaciones.geojson")
datos      <- read.csv("/Users/admin/Downloads/Datos_Normales_Climatológicas.csv")
zmvm       <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/ZMVM_shell.geojson")
estados       <- st_read("https://github.com/JuveCampos/MexicoSinIslas/raw/master/Sin_islas.geojson", 
                                 quiet = TRUE) 
CDMX <- estados %>%  mutate(estado_interes = str_detect(as.character(estados$ENTIDAD), 
                                     pattern = or1(c("DISTRITO FEDERAL")))) %>% filter(estado_interes == TRUE)

plot(CDMX)

# Cambiamos nombres a las variables para que tengan 
# un nombre comun a la hora de hacer el merge
names(estaciones)[1] <- "ID_Estacion"
names(datos)

##########################################
# Data.frames de Normales climatologicas #
##########################################

# PERIODO 1951 - 2010 #
#---------------------#

# Filtramos los datos para el periodo de interes
datos_5110 <-  filter(datos, Periodo2 == "1951 - 2010")

# Hacemos merge con la base que contiene la localizacion de las estaciones
# y removemos las observaciones con la misma informacion en el mismo punto x-y
datos_5110 <- left_join(estaciones, datos_5110) %>%
  filter(!is.na(Periodo))
datos_5110 <- datos_5110[!duplicated(datos_5110$geometry),]

# Extraemos las coordenadas x-y y las asociamos con la precipitacion promedio de septiembre (ejemplo para el n = 1)
xyz <- cbind(st_coordinates(datos_5110)[,1], 
             st_coordinates(datos_5110)[,2], datos_5110$SEPTIEMBRE) %>% 
  as_tibble()

xyz


# Corroboramos que los datos xyz sean numericos
lapply(xyz[1:3], class)


# Kriging #
#---------#

# Aplicamos el metodo de triangulacion de kriging para obtener una imagen raster.
kriged <- kriging(x = xyz$V1, y = xyz$V2, response = xyz$V3)
class(kriged)

# Extraemos la matriz de pixeles del objeto 'kriging' para generar un raster con 
# el paquete `raster`

rastered <- raster::rasterFromXYZ(xyz = kriged$map)
class(rastered)

# Observamos la figura 
plot(rastered)

# Obtenemos las curvas de nivel # 
a <- raster::rasterToContour(rastered)
head(a)
CDMX



plot(a)
plot(zmvm, add = T)

class(a)


e <- st_as_sf(a)
e <- st_polygonize(e)
plot(e)


b <- rasterToPolygons(rastered, fun=function(x){x > 150 & x<250}, dissolve = T)
plot(b)
class(b)

c <- st_as_sf(b)
class(c)

leaflet(a) %>%
  addTiles() %>%
  addPolylines(label = as.character(a$level))

# 
# # PENDIENTE #
# class(datos_5110)
# 
# plot(datos_5110)
# 
# datos_7100 <- filter(datos, Periodo2 == "1971 - 2000")
# datos_8110 <- filter(datos, Periodo2 == "1981 - 2010")
# 
# plot(estaciones)






