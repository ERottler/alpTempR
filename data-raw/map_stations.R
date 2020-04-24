###

#Multi-variable analysis Switzerland
#Interactive map using R-package 'leaflet'

###

library("leaflet")
library("htmlwidgets")

baseDir    <- "u:/RhineFlow/Elevation/"
setwd(baseDir)

stationMeta <- read.table(paste0(baseDir,"/Data/rawData/IDAweb/stationMeta.csv"), sep=",", header=T)

###

map <- leaflet(stationMeta) %>%
  # Base groups
  #addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.TerrainBackground, group = "TerrainBackground") %>%
  addProviderTiles(providers$Esri.WorldImagery,        group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap,      group = "NatGeoWorldMap") %>%
  addProviderTiles(providers$Esri.WorldTopoMap,      group = "Esri.WorldTopoMap") %>%
  # Overlay groups
  addCircleMarkers(~lon, ~lat, label = as.character(stationMeta$name), popup = ~paste0(as.character(name)," (",stationMeta$stn,"): ",alt," m"),
                   labelOptions = labelOptions(noHide = T, textOnly = TRUE, direction = "bottom"), stroke = F, group = "Stations", col="red") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("TerrainBackground", "WorldImagery", "NatGeoWorldMap","Esri.WorldTopoMap"),
    overlayGroups = c("Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

saveWidget(map, file=paste0(baseDir, "map_stations.html"))

