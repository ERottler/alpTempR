library("rgdal")
library("gdalUtils")

all.dem <- list.files("u:/RhineFlow/Elevation/Data/srtmData", pattern = ".tif$", 
                      full.names = T, recursive = F, include.dirs = T)

mosaic_rasters(all.dem, dst_dataset = "u:/RhineFlow/Elevation/Data/srtmData/srtmMosaic.tif")
#gdalinfo("u:/RhineFlow/Elevation/Data/srtmData/srtmMosaic.tif")

gdalwarp("u:/RhineFlow/Elevation/Data/srtmData/srtmMosaic.tif",
         "u:/RhineFlow/Elevation/Data/srtmData/srtmMosaic200m.tif",
         s_srs = "+proj=longlat +ellps=WGS84",
         t_srs = "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ",
         tr = c(200, 200),
         r = "cubic")
