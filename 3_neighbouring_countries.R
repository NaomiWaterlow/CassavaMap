library(raster)
library(sf)

# fill in the old map for the neighbouring countries
# Read the old map
host_raster <- raster("CassavaMap_Prod_v1.tif")

# Import Nigeria
Shape_Adm <- st_read("geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp")
Shape_Adm <- Shape_Adm[Shape_Adm$shapeGroup == "NGA", ]

# bounding box
nigeria_bbox <- st_bbox(Shape_Adm)  # returns xmin, ymin, xmax, ymax

#Convert box to raster::extent
bbox_extent <- extent(nigeria_bbox["xmin"], nigeria_bbox["xmax"],
                      nigeria_bbox["ymin"], nigeria_bbox["ymax"])

#Crop the raster using that extent
host_raster_area<- crop(host_raster, bbox_extent)

# "host_to_use_NGA.tif"
new_raster <- raster("host_production_raw.tif")
new_raster <- resample(new_raster, host_raster_area, method = "bilinear")  # or "ngb" for categorical

combined_raster <- cover(new_raster, host_raster_area)
plot(combined_raster)
# svae combined_raster
writeRaster(combined_raster, "host_production_extended.tif", overwrite = TRUE)


