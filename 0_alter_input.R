# updating the input data to include all states and Ebonyi fix

library(data.table)

# Import the production csv data file:
Data_Adm_Prod <- read.csv("AgroMaps_data/IITA_Cassava_2023.csv")
Data_Adm_Prod$PRODUCTION <- gsub(",", "", Data_Adm_Prod$PRODUCTION)
Data_Adm_Prod$AREA_HARVESTED <- gsub(",", "", Data_Adm_Prod$AREA_HARVESTED)
Data_Adm_Prod$YIELD <- gsub(",", "", Data_Adm_Prod$YIELD)
Data_Adm_Prod <- data.table(Data_Adm_Prod)
Data_Adm_Prod[, AREA_HARVESTED := as.numeric(AREA_HARVESTED)]
Data_Adm_Prod[, PRODUCTION := as.numeric(PRODUCTION)]
Data_Adm_Prod[, YIELD := as.numeric(YIELD)]
Data_Adm_Prod$COUNTRY_NAME <- "Nigeria"

# CHange yield to the 2017 report 2017 value before the reporting goes strange
Data_Adm_Prod[AREA_NAME == "Ebonyi", "YIELD"] <- 5.68
# recalculate the production statistics
Data_Adm_Prod[AREA_NAME == "Ebonyi", PRODUCTION := AREA_HARVESTED*YIELD]

# Add the missing states
missing_states =       data.frame(c("Kano", "Borno","Adamawa"),
      c(0,0,0),
      c(0,0,0),
      c(0,0,0),
      c("NGA","NGA", "NGA"), 
      c("Nigeria", "Nigeria", "Nigeria"))
Data_Adm_Prod <- rbind(Data_Adm_Prod, missing_states, use.names = FALSE)


# Save the .csv back
write.csv(Data_Adm_Prod, "AgroMaps_data/IITA_Cassava_2023_edited.csv", row.names = F)


# Import the production csv data file:
Data_Adm_Prod <- read.csv("AgroMaps_data/IITA_Cassava_2022.csv")
Data_Adm_Prod$PRODUCTION <- gsub(",", "", Data_Adm_Prod$PRODUCTION)
Data_Adm_Prod$AREA_HARVESTED <- gsub(",", "", Data_Adm_Prod$AREA_HARVESTED)
Data_Adm_Prod$YIELD <- gsub(",", "", Data_Adm_Prod$YIELD)
Data_Adm_Prod <- data.table(Data_Adm_Prod)
Data_Adm_Prod[, AREA_HARVESTED := as.numeric(AREA_HARVESTED)]
Data_Adm_Prod[, PRODUCTION := as.numeric(PRODUCTION)]
Data_Adm_Prod[, YIELD := as.numeric(YIELD)]
Data_Adm_Prod$COUNTRY_NAME <- "Nigeria"

# CHange yield to the 2017 report 2017 value before the reporting goes strange
Data_Adm_Prod[AREA_NAME == "Ebonyi", "YIELD"] <- 5.68
# recalculate the production statistics
Data_Adm_Prod[AREA_NAME == "Ebonyi", PRODUCTION := AREA_HARVESTED*YIELD]

# Add the missing states
missing_states =data.frame(c("Kano", "Borno","Adamawa"),
                                  c(0,0,0),
                                  c(0,0,0),
                                  c(0,0,0),
                                  c("Nigeria", "Nigeria", "Nigeria"))
Data_Adm_Prod <- rbind(Data_Adm_Prod, missing_states, use.names = FALSE)


# Save the .csv back
write.csv(Data_Adm_Prod, "AgroMaps_data/IITA_Cassava_2022_edited.csv", row.names = F)

