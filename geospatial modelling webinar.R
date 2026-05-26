library(haven)
library(sf)
library(spdep)
library(mapview)
library(leaflet)
library(tmap)
library(readxl)
library(tidyverse)
# by entire country 
somalia0_shp_0<-st_read("~/gadm41_SOM_0.shp")
windows()
plot(somalia0_shp_0)
somalia0_shp_0$COUNTRY
# by regions 
somalia1_shp_1<-st_read("~/gadm41_SOM_1.shp")
windows()
plot(somalia1_shp_1)
somalia1_shp_1$NAME_1
# by districts 
somalia2_shp_2<-st_read("~/gadm41_SOM_2.shp")
windows()
plot(somalia2_shp_2)
somalia2_shp_2$NAME_2
plot(somalia0_shp_0)
plot(st_geometry(somalia2_shp_2))
view(somalia2_shp_2)
view(somalia1_shp_1)

sanaag <- somalia2_shp_2[c(50:52),] 
sanaag$ID <- c (1, 2, 3)
View(sanaag)
plot(st_geometry(sanaag))

View(somalia2_shp_2)


ds <- data.frame(
  ID = c(1, 2, 3),
  Prevalence = c(0.6, 0.8, 0.9))
new <- left_join(sanaag, ds, by = "ID")
View(new)

g <- ggplot(new) + geom_sf(aes(fill = Prevalence))

tm_shape(new) +
  tm_bubbles("Prevalence", col = "blue") +
  tm_borders()


#Interactive map with leaflet
pal <- colorNumeric(palette = "YlOrRd", domain = ds$prevalence)

leaflet(new) %>%
  addTiles() %>%
  addPolygons(color = "purple", fillColor = ~pal(Prevalence), 
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = ~Prevalence, opacity = 0.8)


# Plot administrative levels with different line thickness
plot(st_geometry(somalia0_shp_0), lwd = 3)
plot(st_geometry(somalia1_shp_1), lwd = 2, add = TRUE)
plot(st_geometry(somalia2_shp_2), lwd = 1, add = TRUE)

# Thematic map layout with varying border widths
tm_shape(somalia0_shp_0) + tm_borders(lwd = 5) +
  tm_shape(somalia1_shp_1) + tm_borders(lwd = 2) +
  tm_shape(somalia2_shp_2) + tm_borders(lwd = 0.6)

library(sf)
library(ggplot2)
library(spdep)
library(tmap)
library(haven)
library(survey)
library(RColorBrewer)
library(dplyr)
library(psych)
# Load shapefile
# Specify the file path to the shapefile representing the administrative boundaries of Somalia.
filepath<-"~/gadm41_SOM_1.shp"
view(filepath)
# Read the shapefile using st_read() from the sf package.
somalia<-st_read(filepath)

# Read the DHS dataset
data <- read_dta("~/ever_atten.dta")
table(data$V024,data$V106A)
data$V024
data_new <- na.omit(data)
# Calculate the proportion unskilled birth attendance by region
# Apply the function to subsets of the survey data using svyby
# Create a survey design object
options(survey.lonely.psu = "remove")
design <- svydesign(id = ~V001, strata = ~STRU_CODE, data = data,nest=TRUE)
result <- svyby(~V106A, ~V024, FUN = svyciprop, design = design, vartype = NULL)
# Print the resulting data frame showing the mean fuel use per region.
print(result)
view(result)
# Add region names to the result data frame
# Define a character vector with the names of the regions, ordered to match the codes in the result.
labels <- c("Awdal", "Woqooyi Galbeed", "Togdheer", "Sool", "Sanaag", "Bari", "Nugaal", "Mudug",
            "Galguduud", "Hiiraan", "Shabeellaha Dhexe", "Banaadir",  "Bay",
            "Bakool", "Gedo",  "Jubbada Hoose")
# Add a 'Region' column to the result data frame, mapping region names to region codes.
result$Region <- labels
# Sort the result by the 'Region' column
result <- result[order(result$Region), ]
# Print the resulting dataframe that is now labeled and sorted.
print(result)
# Merge the shapefile and result data frame
# Merge the shapefile data (`somalia`) with the calculated mean fuel use data (`result`).
merged_data <- merge(somalia, result, by.x = "NAME_1", by.y = "Region")

# Calculate centroids for label placement
# Calculate the centroids (geometric center points) of each polygon in the merged dataset.
centroid <- st_coordinates(st_centroid(merged_data))
# Extract x-coordinates of centroids and add them as column named 'x_position'
merged_data$x_position <- centroid[, 1]
# Extract y-coordinates of centroids and add them as column named 'y_position'
merged_data$y_position <- centroid[, 2]
# Convert SpatialPolygonsDataFrame to sf object
# Convert merged_data which is a SpatialPolygonsDataFrame into an sf object for use in ggplot
merged_data_sf <- st_as_sf(merged_data)
# Define a custom color palette
colors <- brewer.pal(9, "YlOrRd")
# Plot the spatial data
p1 <- ggplot() +
  geom_sf(data = merged_data_sf, aes(fill = V106A)) +
  scale_fill_gradientn(colors = colors, na.value = "grey") +
  guides(fill = guide_legend(title = "prevalence of ever_attended school")) +
  geom_sf_text(data = merged_data_sf, aes(label = NAME_1),
               cex = 3, hjust = 0.5, vjust = 0.5, color = "black", fontface = "bold") +
  ylab(label = "") +
  xlab(label = "") +
  ggtitle("Spatial distribution of ever_attended school in Somalia") +
  theme(legend.text = element_text(size = 10),
        plot.title = element_text(color = "blue", size = 10, face = "bold"),
        strip.text.x = element_text(size = 10))
windows()
print(p1)





#Interactive map with leaflet
pal <- colorNumeric(palette = "YlOrRd", domain = d$prevalence)

leaflet(d) %>%
  addTiles() %>%
  addPolygons(color = "white", fillColor = ~pal(prevalence), 
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = ~prevalence, opacity = 0.8)


#Map with mapview
library(mapview)
mapview(d, zcol = "prevalence")
#Thematic map with Tmap
tmap_mode("plot")

# Simple polygon map
tm_shape(d) + 
  tm_polygons("prevalence")

# Bubble map
tm_shape(d) +
  tm_bubbles("prevalence", col = "blue") +
  tm_borders()

# Choropleth Maps 
tm_shape(d) +
  tm_polygons("prevalence", style = "cont", palette = "YlOrRd") +
  tm_layout(
    legend.outside = FALSE,
    legend.stack = "vertical",
    legend.outside.position = "top",
    legend.outside.size = 0.1,
    legend.text.size = 1,
    legend.title.size = 1.5
  ) +
  tm_scale_bar() +
  tm_compass()
tm_shape(d) +
  tm_polygons("prevalence", style = "quantile", palette = "YlOrRd", n = 5) +
  tm_layout(
    legend.outside = FALSE,
    legend.stack = "vertical",
    legend.outside.position = "top",
    legend.outside.size = 0.1,
    legend.text.size = 1,
    legend.title.size = 1.5
  ) +
  tm_scale_bar() +
  tm_compass()
