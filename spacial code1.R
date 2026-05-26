# spatial analysis
# lecture 1: mappiing tecneques 
# 1. country, regions, districts
# requered package 
# 1. maptools
# 1. Maps for Countries
library(maptools)
# Load the wrld_simple dataset
data(wrld_simpl)
windows()
plot(wrld_simpl, col=4)

wrld_simpl$NAME
# Extract a specific country from the dataset (e.g., Somalia)
somalia <- wrld_simpl[wrld_simpl$NAME == "Somalia", ]
windows()
plot(somalia, border = "black", col = "pink", 
     main = "Areal Data: Administrative 
     Boundaries of Somalia")

Yemen <-wrld_simpl[wrld_simpl$NAME=="Yemen",]
windows()
plot(Yemen,border="blue",col=7, main="Areal Data: Yemen")

DJabouti <-wrld_simpl[wrld_simpl$NAME== "Djibouti",]
windows()
plot(DJabouti,border="red",col=100,main="Areal Data: Djibouti")
Ethiopia <-wrld_simpl[wrld_simpl$NAME== "Ethiopia",]
windows()
plot(Ethiopia,border="green",col=14,main="Areal Data: Ethiopia")
windows()

par(mfrow=c(1,4))
plot(Ethiopia,border="lightblue",col=14,main="Areal Data: Ethiopia")
plot(DJabouti,border=9,col=100,main="Areal Data: Djibouti")
plot(Yemen,border="blue",col=7,main="Areal Data: Yemen")
plot(somalia, border = "black", col = "pink",main = "Areal Data: Administrative 
     Boundaries of Somalia")
# another packages 
library(raster)
library(geodata)
library(terra)

somalia0 <- gadm(country = "SOM", level = 0, path = tempdir())
windows()
plot(somalia0)

Somalia1<-gadm(country='SOM', level=1,path = tempdir() )  ##Get the whole for Somalia
windows()
plot(Somalia1)

Somalia2<-gadm(country='SOM', level=2,path = tempdir() )  ##Get the Province Shapefile for Somalia
windows()
plot(Somalia2)



# Plot Somalia with region names
windows()
plot(Somalia1)
text(Somalia1, labels = Somalia1$NAME_1, col = "black", cex = 0.8)

# Plot Somalia with districts names
windows()
plot(Somalia2)
text(Somalia2, labels = Somalia2$NAME_2, col = "black", cex = 1)


# Define a color palette for the regions

region_colors <- rainbow(length(Somalia1$NAME_1))
windows()
plot(Somalia1, col = region_colors)
districts_colors <- rainbow(length(Somalia2$NAME_2))
windows()
plot(Somalia2, col = districts_colors)

# Get the Province Shapefile for Somalia
Somalia0 <- gadm(country = 'SO', level = 1,path = tempdir() )
windows()
plot(somalia0)
# Define the regions you want to select
selected_regions <- c("Awdal", "Woqooyi Galbeed", "Togdheer","Sool","Sanaag")  # Replace with the regions you want
# Subset the Somalia data to include only the selected regions
selected_Somalia <- Somalia0[Somalia0$NAME_1 %in% selected_regions, ]
# Define a color palette for the regions
region_colors <- rainbow(length(selected_Somalia$NAME_1))
windows()
plot(selected_Somalia, col = region_colors)
text(selected_Somalia, labels = selected_Somalia$NAME_1, col = "black", cex = 0.8)

# Get the District Shapefile for Somalia
Somalia <- gadm(country = 'SO', level = 2, path =tempdir() )
Somalia$NAME_2
# Define the districts you want to select
selected_districts <- c("", "Ceerigaabo", "Ceel-Afwein","Badhan")  # Replace with the districts you want
# Subset the Somalia data to include only the selected districts
selected_Somalia <- Somalia[Somalia$NAME_2 %in% selected_districts, ]
# Define a color palette for the districts
district_colors <- rainbow(length(selected_Somalia$NAME_2))
# Plot the selected districts of Somalia with colored regions
windows()
plot(selected_Somalia, col = district_colors)
text(selected_Somalia, labels = selected_Somalia$NAME_2, col = "black", cex = 0.8)
# Get the world countries shapefile
# AFRICA 
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
world <- ne_countries(scale = "medium", returnclass = "sf")
# Filter African countries
africa <- world %>% filter(continent == "Africa")
windows()
plot(africa["geometry"])










# 2. shape files 
library(sf)
library(mapview)
d <- st_read(system.file("shape/nc.shp", package = "sf"),
             quiet = TRUE)
mapview(d, zcol = "SID74")



# reading shapefile for somalia
library(terra)
rast <- rast("yourfile.tif")   # for raster data
vect <- vect("yourfile.shp")   # for shapefiles
library(sf)
shp <- st_read("yourfile.shp")

library(rgdal)
somalia5<-readOGR(dsn="shapefile", layer= "shapefile")
mapview(somalia5)
# 3. nasa datasets 