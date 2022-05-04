library(here)
library(tidyverse)
library(leaflet)
library(raster)
library(mapview)
library(leafem)
library(sf)
library(leaflet.opacity)


# install.packages("leaflet.opacity")
# leaflet.mapboxgl
# leaflet.extras 
# 
# 

# Read data 
soc <- raster::raster(here::here("data/socsn.tif"))
sn <- st_read(here::here("data/sn_enp.shp"))

sn <- st_transform(sn, 4326)




m <- leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  addMiniMap(tiles = providers$Esri.WorldTerrain, 
             toggleDisplay = TRUE) %>% 
  # http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addRasterImage(soc, project = TRUE, 
                 group = "soc", opacity = 0.6,
                 layerId = "soc") %>% 
  addMouseCoordinates() %>% # View coordinates 
  addOpacitySlider(layerId = "soc") %>%
  leafem::addImageQuery(soc, project = TRUE, 
                        type="mouse", 
                        layerId = "soc",
                        digits = 2, 
                        prefix = "SOC", 
                        className = "hjola") %>% 
  addPolygons(data = sn,
              group= 'Natural Park',
              fillOpacity = 0, 
              color = "black",
              stroke = TRUE) %>% 
  addLayersControl(position = 'bottomright',
                   baseGroups = c("Satellite"),
                   overlayGroups = c('Natural Park',
                                     'soc'),
                   options = layersControlOptions(collapsed = TRUE)) 


library(htmlwidgets)
saveWidget(m, "index.html")





  
  # addPolygons(data = qp,
  #             group= 'Quercus pyrenaica forests',
  #             fillColor = 'red', fillOpacity = 0, 
  #             stroke = FALSE) %>% 
# Base Maps 
# http://leaflet-extras.github.io/leaflet-providers/preview/index.html


soc_id <- expression(SOC^2)


addRasterImage(poppendorf[[1]], project = TRUE, group = "poppendorf",
               layerId = "poppendorf") %>%
  addImageQuery(poppendorf[[1]], project = TRUE,
                layerId = "poppendorf") %>%
  addLayersControl(overlayGroups = "poppendorf")


layerId = "socn")

# %>%
#   addLegend(pal = pal, values = values(r),
#             title = "Surface temp")

leaflet() %>% 
  addTiles() %>% 
  addRasterImage(soc) 

