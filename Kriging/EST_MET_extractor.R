# Pr'acticas con datos de las Normales

library(dplyr)
library(rebus)
library(stringr)
library(sf)

# Leemos el archivo
a <- read.delim("http://smn.cna.gob.mx/tools/RESOURCES/Normales5110/NORMAL09007.TXT", 
                fileEncoding = "latin1", stringsAsFactors = FALSE) 
# Renombramos la variable
names(a) <- "V1"

# Generamos el patron 
pat <- or1(c("TEMPERATURA MAXIMA", 
             "TEMPERATURA MEDIA", 
             "TEMPERATURA MINIMA", 
             "PRECIPITACION", 
             "EVAPORACION TOTAL"))

meses <- c("null","Enero", "Febrero", "Marzo", "Abril", 
           "Mayo", "Junio", "Julio", "Agosto", 
           "Septiembre", "Octubre", "Noviembre", "Diciembre", "Total")

# Hacemos un vector de casillas
c <- which(str_detect(a$V1, pattern = pat))

# Generamos un vector de categorias de los datos
v <- rep("", nrow(a))
v[1:c[1]-1]     <- "datos"
v[c[1]:c[2]-1]  <- "tmax"
v[c[2]:c[3]-1]  <- "tmed"
v[c[3]:c[4]-1]  <- "tmin"
v[c[4]:c[5]-1]  <- "prec"
v[c[5]:nrow(a)] <- "evap"
length(v)

# Pegamos ese vector 
a <- cbind(a, v)

########################################### 
# Clasificamos los datos de precipitaci?n #
###########################################

# Definimos funcion captura para detectar todo el texto que haya entre dos espacios
captura <- function(x) capture(one_or_more(x))

# Generamos una matriz para capturar solo los datos de precipitaci?n
pp <- filter(a, a$v == "prec")

# Definimos nuestro patron de captura para remover las palabras al inicio de cada rengl?n
pat_pp <- START %R% capture(one_or_more(WRD)) %R% optional(SPC) %R% optional(captura(WRD)) %R% optional(SPC) %R% optional(captura(WRD)) %R% optional(SPC) 
str_view(pp$V1, pattern = pat_pp)

# Sacamos el texto a una nueva variable
pp$v3 <- str_extract(pp$V1, pat_pp)

# REmovemos el texto de nuestro renglon con numeros
pp$V1 <- str_remove_all(pp$V1, pat_pp)
pp$V1[3]

# Cortamos el rengl?n en 12 observaciones para cada mes
pnormal <- as.numeric(str_split(pp$V1[3], pattern = captura(SPC))[[1]])
pnormal

# Le ponemos un nombre todo meco a una variable temporal (BORRAR!!!!!)
xxx <- as.data.frame(t(c("9001", pnormal)))
names(xxx) <- c("ID_ESTACION", meses)

#######################################################
# Descarga masiva de datos de normales climatologicas #
######################################################

# Jalamos el numero de la estacion, lo homologamos con la pagina y lo convertimos en texto
estaciones <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/Datos_Estaciones.geojson")
estaciones$ID_ESTACIO <- as.character(estaciones$ID_ESTACIO)
estaciones$ID_ESTACIO
estaciones$ID_ESTACIO[nchar(estaciones$ID_ESTACIO) == 4] <- paste0("0", estaciones$ID_ESTACIO[nchar(estaciones$ID_ESTACIO) == 4])
class(estaciones$ID_ESTACIO)
estaciones$ID_ESTACIO

# Definimos los periodos
periodos <- c("5110", "7100", "8110")
meses <- c("null","ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE", "TOTAL")

# Leemos el archivo de interes
i <- estaciones$ID_ESTACIO[134] # poner el numero que sea... es ejemplo
j <- periodos[1]
a <- read.delim(paste0("http://smn.cna.gob.mx/tools/RESOURCES/Normales", j, "/NORMAL", i,".TXT"), 
                fileEncoding = "latin1", stringsAsFactors = FALSE) 

tryCatch(a <- read.delim(paste0("http://smn.cna.gob.mx/tools/RESOURCES/Normales", j, "/NORMAL", i,".TXT"), 
                         fileEncoding = "latin1", stringsAsFactors = FALSE), 
         error = function(e){
           print("Error en Estacion:", i, " y periodo: ", j)
         }
         )

write.csv(a, paste0("EST_", i, "_P_", j, ".csv"))


#######################################################################################################################################################
#######################
# Loop para esa madre #
#######################

# Definimos los periodos
periodos <- c("5110", "7100", "8110")
meses <- c("null","ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE", "TOTAL")

# Jalamos el numero de la estacion, lo homologamos con la pagina y lo convertimos en texto
estaciones <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/Datos_Estaciones.geojson")
estaciones$ID_ESTACIO <- as.character(estaciones$ID_ESTACIO)
estaciones$ID_ESTACIO
estaciones$ID_ESTACIO[nchar(estaciones$ID_ESTACIO) == 4] <- paste0("0", estaciones$ID_ESTACIO[nchar(estaciones$ID_ESTACIO) == 4])
class(estaciones$ID_ESTACIO)
estaciones$ID_ESTACIO

# Generamos el patron 
pat <- or1(c("TEMPERATURA MAXIMA", 
             "TEMPERATURA MEDIA", 
             "TEMPERATURA MINIMA", 
             "PRECIPITACION"
             #,"EVAPORACION TOTAL"
             ))

resultados <- as.data.frame(c())

for (s in estaciones$ID_ESTACIO){
  for (p in periodos){
    tryCatch({a <- read.delim(paste0("http://smn.cna.gob.mx/tools/RESOURCES/Normales", p, "/NORMAL", s,".TXT"), 
                             fileEncoding = "latin1", stringsAsFactors = FALSE)
              print(paste0("Exito en ", s, " para el periodo ", p))
              write.csv(a, paste0("/Users/admin/Desktop/Proyectos/Resiliencia\ CDMX/Scripts/Agua/Conagua/Normales/DATOS/", s, "_", p, "_", ".csv"))
    
    }, 
             error = function(e){
               message("Error en Estacion:", s, " y periodo: ", p)
             }
    ) # Fin del tryCatch
  }
}
####################################################################################################################################    
j <- 0       
# Definimos funcion captura para detectar todo el texto que haya entre dos espacios
captura <- function(x) capture(one_or_more(x))

resultados <- c()

for(i in list.files("C:/Users/lnpp/Desktop/ESTACIONES/DATOS")){
  a <-  read.csv(paste0("C:/Users/lnpp/Desktop/ESTACIONES/DATOS/", i), 
                   fileEncoding = "latin1", stringsAsFactors = FALSE) 
  a$X <- NULL
  # REnombramos la variable 1
  names(a) <- "V1"
  a$V1 <- str_remove(a$V1, pattern = START %R% captura(DGT) %R% ",")
  
  
  
    j <- j + 1
    
    s <- str_extract(i, captura(DGT))
    p <- str_extract(i, capture("_" %R% captura(DGT))) %>%
      str_remove(pattern = "_")
    
    # Hacemos un vector de casillas
    c <- which(str_detect(a$V1, pattern = pat))
    #print(c(c, i, j))
    
    # Generamos un vector de categorias de los datos
    v <- rep("", nrow(a))
    v[1:c[1]-1]     <- "datos"
    v[c[1]:c[2]-1]  <- "tmax"
    v[c[2]:c[3]-1]  <- "tmed"
    v[c[3]:c[4]-1]  <- "tmin"
    v[c[4]:nrow(a)]  <- "prec"
    #v[c[5]:nrow(a)] <- "evap"
    
    # Pegamos ese vector 
    a <- cbind(a, v)
    
    ########################################### 
    # Clasificamos los datos de precipitaci?n #
    ###########################################
    
    # Generamos una matriz para capturar solo los datos de precipitaci?n
    pp <- filter(a, a$v == "prec")
    
    
    # Definimos nuestro patron de captura para remover las palabras al inicio de cada rengl?n
    pat_pp <- capture(one_or_more(WRD)) %R% optional(SPC) %R% optional(captura(WRD)) %R% optional(SPC) %R% optional(captura(WRD)) %R% optional(SPC) 
    str_view(pp$V1, pattern = pat_pp)
    
    # Sacamos el texto a una nueva variable
    pp$v3 <- str_extract(pp$V1, pat_pp)
    
    # REmovemos el texto de nuestro renglon con numeros
    pp$V1 <- str_remove(pp$V1, pat_pp)
    
    # Cortamos el rengl?n en 12 observaciones para cada mes
    pnormal <- as.numeric(str_split(pp$V1[str_detect(pp$v3, "NORMAL")], pattern = captura(SPC))[[1]])
    pnormal
    #print(pp$V3[3])
    
    # Le ponemos un nombre todo meco a una variable temporal 
    resultados <- rbind(resultados, as.data.frame(t(c(s,p, pnormal))))
    #print(resultados)
    
  }

names(resultados) <- c("ID_Estacion", "Periodo", meses)
class(resultados$Periodo)

resultados <- resultados %>%
  mutate(Periodo2 = case_when(Periodo == "5110" ~ "1951 - 2010", 
                              Periodo == "7100" ~ "1971 - 2000", 
                              Periodo == "8110" ~ "1981 - 2010"))
  

write.csv(resultados, "Datos_Normales_Climatol?gicas.csv")

# Resultados (edicion sobre la base de resultados original)
res <- read.csv("Datos/Datos_Normales_Climatol?gicas.csv")
res$null <- NULL
res$sum <- rowSums(res[,4:15])


library(lubridate)



meses <- c("01-01", "01-02", "01-03", "01-04", "01-05", "01-06", "01-07", "01-08", "01-09", "01-10", "01-11", "01-12")
meses <- as.Date(meses, format = "%d-%m")
class(meses)
meses




x <- names(res[,4:15])
y <- as.numeric(res[1,4:15])
df <- as.data.frame(t(rbind(x,y)))
df$V2 <- as.numeric(df$V2)

time <- xts(y, order.by = meses)
plot(time)

#ggplot(df, aes(x = "V1", y = "V2")) + geom_line()


library(ggplot2)
 by_year <- gapminder %>% group_by(year) %>% summarize(medianGdpPercap = median(gdpPercap))
 ggplot(by_year, aes(x = year, y = medianGdpPercap)) + geom_line() + expand_limits(y = 0)


 
 
ggplot(aes(x = x , y = y)) + geom_line() 



