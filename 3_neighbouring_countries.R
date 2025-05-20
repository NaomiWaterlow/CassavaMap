library(raster)
library(sf)
library(viridis)

# fill in the old map for the neighbouring countries (and the missing states!)
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
new_raster <- projectRaster(
  from = new_raster,
  to = host_raster_area,
  method = "ngb"
)
# only cover Nan values outside of Nigeria! 
country_NGA <- Shape_Adm[Shape_Adm$shapeGroup == "NGA",]
# also need to cover borno
country_NGA <- country_NGA[country_NGA$shapeName != "Borno",]

# Convert sf to Spatial object for compatibility with raster package
country_NGA_f <- as(country_NGA, "Spatial")
mask_outside <- mask(new_raster, country_NGA_f, inverse = TRUE)
# This will keep only the values of host_raster_area *outside* Nigeria
host_outside <- mask(host_raster_area, country_NGA_f, inverse = TRUE)
#Cover new_raster with host_raster_area *only outside* Nigeria
combined_raster <- cover(new_raster, host_outside)
plot(combined_raster)
# svae combined_raster
writeRaster(combined_raster, "host_production_extended.tif", overwrite = TRUE)


##### Checking ####

# Load shapefile as sf object
state <- Shape_Adm[Shape_Adm$shapeGroup == "NGA",]

# Convert sf to Spatial object for compatibility with raster package
state_f <- as(state, "Spatial")

# Optional: crop and/or mask raster to shape
cropped_raster <- crop(combined_raster, state_f)
masked_raster <- mask(cropped_raster, state_f)

# Plot using viridis colors
plot(masked_raster, col = viridis(100), main = "2023 Production")

total_sum <- cellStats(masked_raster, stat='sum')
print(total_sum)
print(FAOSTAT23Prod)