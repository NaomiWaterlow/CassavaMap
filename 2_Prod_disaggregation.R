# CassavaMap v1 15 May 2020

# This shapeName illustrates the methodology of disaggregating cassava production
# data between rural population.
# Various sProdpefiles of different formats were used for the continental scale map.
# This example shows calculations for a chosen country (Uganda)
# and can be upscaled to the entire continent

options(stringsAsFactors=FALSE)

# Load required libraries
library(sp)
library(raster)
#library(rgdal)
library(sf)
library(data.table)
library(ggplot2)



# Import country shapefile:
Shape_Adm <- st_read("geoBoundariesCGAZ_ADM1/geoBoundariesCGAZ_ADM1.shp")
Shape_Adm <- Shape_Adm[Shape_Adm$shapeGroup == "NGA", ]

# Load 2023 LandScan population raster obtained from https://landscan.ornl.gov/
PopRaster <- raster("landscan-global-2023.tif")
#whole world
plot(PopRaster)
# subset to Nigeria only
PopRaster <- crop(PopRaster, extent(Shape_Adm))
# Here the chosen threshold is 5000 people per pixel and above:
ThresPop <- 5000 
PopRaster[PopRaster > ThresPop] <- NA


# Import the production csv data file:
Data_Adm_Prod <- read.csv("AgroMaps_data/IITA_Cassava_2023_edited.csv")

# Extract production for each country of interest
FAOSTAT23 <- read.csv("FAO/FAOSTAT_data_2023.csv")
FAOSTAT23 <- data.table(FAOSTAT23[FAOSTAT23$Element == "Production",])

# List countries that you want to work on - as example Uganda is given but can be a longer list
CountryList <- "Nigeria"

# Function to extract production for respective countries
# from the 2023 FAOSTAT dataset:
getval<-function(country,element) {
  as.numeric(FAOSTAT23$Value[FAOSTAT23$Area==country & 
                               FAOSTAT23$Element==element])
}

# Add Prod area values per country 
FAOSTAT23Prod <- vector("list",length(CountryList))
for (i in 1:length(CountryList)) {
  FAOSTAT23Prod <- getval(CountryList,"Production")
}
FAOSTAT23Prod <- data.frame(CountryList,FAOSTAT23Prod)
colnames(FAOSTAT23Prod) <- c("Country","FAOSTAT23Prod")

# Tidy up country sProdpefile
Data_Adm_Prod <- Data_Adm_Prod[!duplicated(Data_Adm_Prod), ] # Remove duplicates
colnames(Data_Adm_Prod)[1] = "shapeName"

Shape_Adm$shapeName[!(Shape_Adm$shapeName %in% Data_Adm_Prod$shapeName)]
Data_Adm_Prod$shapeName[!(Data_Adm_Prod$shapeName %in%Shape_Adm$shapeName)]


## Add Prod area info to admin units
Prod_SH <- merge(Shape_Adm, Data_Adm_Prod, by='shapeName')

# # Visualise the production data added to the polygon
# ggplot(Prod_SH) +
#   geom_sf(aes(fill = PRODUCTION)) +
#   scale_fill_viridis_c() +  # or scale_fill_viridis_d() for discrete values
#   theme_minimal()

# Function to calculate per capita Prod area values:
Prod_PerCapita <- function(pop_data,poly_data,fao_data){
  # Extract rural population totals per admin unit:
  AllAdmProdPopRural <- extract(pop_data, poly_data)
  AllAdmProdPopRural_ul <- sapply(AllAdmProdPopRural, 
                                 function (x) sum(unlist(x), na.rm=TRUE))
  
  # Convert to dataframe
  AllAdmProdPopRural_df <- data.frame(AllAdmProdPopRural_ul, 
                                     poly_data$PRODUCTION,
                                     "Nigeria",
                                     as.character(poly_data$shapeName))
  colnames(AllAdmProdPopRural_df) <- c("PopRural","AM_Prod","Country","shapeName")

  
  # Sum prod per country
  CountryProd = data.frame(Country = "Nigeria", 
                         CountryProd_AM = sum(poly_data$PRODUCTION))
  
  
  # Add country total column
  CountryProdTotal   <- merge(AllAdmProdPopRural_df,CountryProd,"Country")
  CountryProdTotal23 <- merge(CountryProdTotal,fao_data, by="Country")
  
  # New column with a ratio of country total in adm unit
  CountryProdTotal23$Ratio <- CountryProdTotal23$AM_Prod / 
    CountryProdTotal23$CountryProd_AM
  CountryProdTotal23$ProdFAO23adj <- CountryProdTotal23$Ratio *
    CountryProdTotal23$FAOSTAT23Prod
  
  # Calculate Prod per pixel
  CountryProdTotal23$ProdRate <- CountryProdTotal23$AM_Prod /
    CountryProdTotal23$PopRural
  CountryProdTotal23$ProdRateFAO23Adj <- CountryProdTotal23$ProdFAO23adj /
    CountryProdTotal23$PopRural
  
  FinalPoly <- merge(poly_data,CountryProdTotal23,by="shapeName")
  return(list(FinalPoly,CountryProdTotal23))
}

Output <- Prod_PerCapita(PopRaster, Prod_SH, FAOSTAT23Prod)
FinalPolygon <- Output[[1]]
CountryProdTotal23 <- Output[[2]]

# Set the maximum value of cassava production per pixel allowed
MPPPV <- 1000

# Calculate host density per pixel
CalcHost <- function (x_poly,x_data,x_LS){
  x_finalpoly <- merge(x_poly,x_data,by="shapeName")
  rp <- rasterize(x_finalpoly,x_LS,'ProdRateFAO23Adj')
  x <- rp*x_LS
  x
}


# Reallocate Prod area if it exceeds the threshold MPPPV
ReallocateProd <- function (x_poly,x_data,x_LS,x) {
  x_Exc <- reclassify(x, matrix(c(0,MPPPV,0),
                                ncol=3, byrow=TRUE))
  x_Exc <- x_Exc-MPPPV
  x_Exc <- reclassify(x_Exc, matrix(c(-1*MPPPV-1,0.00000001,0),
                                    ncol=3, byrow=TRUE))
  y_data <- x_data
  
  x_01 <- reclassify(x, matrix(c(0,MPPPV,0,
                                 MPPPV+0.00000001,10000000,1),
                               ncol=3, byrow=TRUE))
  x_01sum <- cellStats(x_01, sum)
  y_data$ProdFAO23adj[1] <- x_data$ProdFAO23adj[1]-cellStats(x_01,sum)*MPPPV
  x_LSExc <- x_01*x_LS
  y_data$PopRural[1] <- x_data$PopRural[1]-cellStats(x_LSExc, sum)
  y_data$ProdRateFAO23Adj[1] <- y_data$ProdFAO23adj[1]/y_data$PopRural[1]
  
  # Remove pixels with excess values
  x_Exc0 <- reclassify(x, matrix(c(0,MPPPV,1,
                                   MPPPV+0.00000001,10000000,0),
                                 ncol=3, byrow=TRUE))
  x_LSExc0 <- x_Exc0*x_LS # Create raster with LandScan population outside exceeded limits
  
  x_finalpoly2 <- merge(x_poly,y_data,by="shapeName")
  rp <- rasterize(x_finalpoly2,x_LSExc0,'ProdRateFAO23Adj')
  resultd <- rp*x_LSExc0
  result <- list(resultd,y_data,x_LSExc0,x_01*MPPPV,x_01sum)
  result
  
}

# Supplementary function to reallocate Prod area if it exceeds the threshold MPPPV
ReallocateProd2 <- function (x_poly,x_data,x_LS,x,oldx01) {
  x_Exc <- reclassify(x, matrix(c(0,MPPPV,0),
                                ncol=3, byrow=TRUE))
  x_Exc <- x_Exc-MPPPV
  x_Exc <- reclassify(x_Exc, matrix(c(-1*MPPPV-1,0.00000001,0),
                                    ncol=3, byrow=TRUE))
  y_data <- x_data
  x_01 <- reclassify(x, matrix(c(0,MPPPV,0,
                                 MPPPV+0.00000001,10000000,1),
                               ncol=3, byrow=TRUE))
  y_data$ProdFAO23adj[1] <- x_data$ProdFAO23adj[1]-cellStats(x_01,sum)*MPPPV
  x_01sum <- cellStats(x_01, sum) # sum of pixels with exceeded value
  x_LSExc <- x_01*x_LS # count population in pixels with exceeded values
  y_data$PopRural[1] <- x_data$PopRural[1]-cellStats(x_LSExc, sum)
  y_data$ProdRateFAO23Adj[1] <- y_data$ProdFAO23adj[1]/y_data$PopRural[1]
  
  # Remove pixels with excess values
  x_Exc0 <- reclassify(x, matrix(c(0,MPPPV,1,
                                   MPPPV+0.00000001,10000000,0),
                                 ncol=3, byrow=TRUE))
  x_LSExc0 <- x_Exc0*x_LS # Create raster with LandScan population outside exceeded limits
  
  x_finalpoly2 <- merge(x_poly,y_data,by="shapeName")
  rp <- rasterize(x_finalpoly2,x_LSExc0,'ProdRateFAO23Adj')
  resultd <- rp*x_LSExc0
  result <- list(resultd,y_data,x_LSExc0,oldx01+x_01*MPPPV,x_01sum)
  result
  
}

# Function redistributing Prod until result has no values exceeding MPPPV
HostCtd <- function (a,b,c,d,e){
  success <- FALSE
  atmp = a
  btmp = b
  ctmp = c
  dtmp = d
  etmp = e
  while (!success){
    resulttmp <- ReallocateProd2(atmp,btmp,ctmp,dtmp,etmp)
    atmp = x_poly
    btmp = resulttmp[[2]]
    ctmp = resulttmp[[3]]
    dtmp = resulttmp[[1]]
    etmp = resulttmp[[4]]
    success <- cellStats(resulttmp[[1]], max) <= MPPPV
    print(resulttmp[[5]])
  }
  return(resulttmp)
}


### Run host calculation and redistribution:
shapeNames <- Prod_SH$shapeName
resultFinal <- list() 
cnt = 0
for (i in 1:length(Prod_SH$shapeName)){
  cnt = cnt +1
  print(paste0("Iteration: ",cnt," in progress"))
  x_poly <- Prod_SH[Prod_SH$shapeName == shapeNames[i],]
  x_data <- CountryProdTotal23[CountryProdTotal23$shapeName == shapeNames[i],]
  x_LS <- crop(PopRaster,extent(x_poly))
  x_LS <- mask(x_LS, x_poly)
  x <- CalcHost(x_poly,x_data,x_LS)
  x
  
  x_LS01c <- reclassify(x_LS, matrix(c(0.000000001,8000000,1),
                                     ncol=3, byrow=TRUE))
  if(is.na(x_data$ProdRateFAO23Adj[1])){
    resultFinal[[i]] <- x
  }else{
    if(is.na(x_data$ProdFAO23adj)){
      resultFinal[[i]] <- reclassify(x,matrix(c(MPPPV+0.000001,100000000,MPPPV),
                                              ncol=3,byrow=TRUE))
    }else{
      if(cellStats(x_LS01c,sum)*MPPPV<x_data$ProdFAO23adj){
        resultFinal[[i]] <- reclassify(x, matrix(c(0.000000001,8000000,MPPPV),
                                                 ncol=3, byrow=TRUE))
      }else{
        if(cellStats(x, max) <= MPPPV){
          resultFinal[[i]] <- x
        }else{
          result <- ReallocateProd(x_poly,x_data,x_LS,x)
          if(cellStats(result[[1]],max) <= MPPPV){
            resultFinal[[i]] <- result[[1]]+result[[4]]
          }else{
            HostFinali <- HostCtd(x_poly,result[[2]],result[[3]],result[[1]],result[[4]])
            resultFinal[[i]] <- HostFinali[[4]]+HostFinali[[1]]  # Final host distribution result
          }
        }
      }
    }
    
  }
}

# Create the final raster with Prod area:
Prod_raster <- do.call(merge,resultFinal)
plot(Prod_raster) + title("Adjusted Cassava Production 2023")



