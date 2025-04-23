library(gridExtra)
library(viridis)


# display the original version
host_raster <- raster("CassavaMap_Prod_v1.tif")

plot(host_raster)

# Import country shapefile:
Shape_Adm <- st_read("geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp")
Shape_Adm <- Shape_Adm[Shape_Adm$shapeGroup == "NGA", ]

orig_nigeria <- crop(host_raster, extent(Shape_Adm))
orig_nigeria <- mask(orig_nigeria, Shape_Adm)
p_original <- plot(orig_nigeria) + title("Originial 2014 production")


host_ha <- raster("CassavaMap_HarvArea_v1.tif")
orig_nigeria_ha <- crop(host_ha, extent(Shape_Adm))
orig_nigeria_ha <- mask(orig_nigeria_ha, Shape_Adm)
p_original_ha <- plot(orig_nigeria_ha) + title("Originial 2014 Harvest Area")

my_colors <- viridis(100)

# plot them out
par(mfrow = c(2, 2))  # 2x2 grid
plot(orig_nigeria_ha, main = "Original 2014 Harvest Area", col = my_colors)
plot(orig_nigeria, main = "Original 2014 Production", col = my_colors)

plot(HA_raster, main = "2023 Harvest area", col = my_colors)
plot(Prod_raster, main = "2023 Production", col = my_colors)


#or on the log scale




