library(haven)
library(sf)
library(spdep)
library(mapview)
library(leaflet)
library(tmap)
library(readxl)
library(tidyverse)

nig <- st_read("")
# Load administrative boundaries: country (0), state (1), and LGA (2)
nigeria_shp0 <- st_read("C:/Users/Ezra Gayawan/Documents/DHS Data/GPS data/Boundary file WA/Nigeria/gadm41_NGA_0.shp")
View(nigeria_shp0)
nigeria_shp1 <- st_read("C:/Users/Ezra Gayawan/Documents/DHS Data/GPS data/Boundary file WA/Nigeria/gadm41_NGA_1.shp")
nigeria_shp2 <- st_read("C:/Users/Ezra Gayawan/Documents/DHS Data/GPS data/Boundary file WA/Nigeria/gadm41_NGA_2.shp")

plot(nigeria_shp0)
plot(st_geometry(nigeria_shp2))

View(nigeria_shp2)
fct <- nigeria_shp2[c(277:282),] 
fct$ID <- c (1, 2, 3, 4, 5, 6)
View(fct)
plot(st_geometry(fct))

View(nigeria_shp2)


ds <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  Prevalence = c(0.2, 0.4, 0.6, 0.8, 0.9, 0.4)
  
)
new <- left_join(fct, ds, by = "ID")
View(new)

g <- ggplot(new) + geom_sf(aes(fill = Prevalence))

g

tm_shape(new) +
  tm_bubbles("Prevalence", col = "blue") +
  tm_borders()


#Interactive map with leaflet
pal <- colorNumeric(palette = "YlOrRd", domain = ds$prevalence)

leaflet(new) %>%
  addTiles() %>%
  addPolygons(color = "white", fillColor = ~pal(Prevalence), 
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = ~Prevalence, opacity = 0.8)


# Plot administrative levels with different line thickness
plot(st_geometry(nigeria_shp0), lwd = 3)
plot(st_geometry(nigeria_shp1), lwd = 2, add = TRUE)
plot(st_geometry(nigeria_shp2), lwd = 1, add = TRUE)

# Thematic map layout with varying border widths
tm_shape(nigeria_shp0) + tm_borders(lwd = 5) +
  tm_shape(nigeria_shp1) + tm_borders(lwd = 3) +
  tm_shape(nigeria_shp2) + tm_borders(lwd = 0.6)

# Read the DHS dataset
data <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/DHS Children recode 2013_2018/Chilren R 2018/NGKR7ADT/NGKR7AFL.DTA")
View(data)
# Select only relevant variables: state code and bed net usage
data_new <- select(data, state = "sstate", bed = "v461")
View(data_new)
# Rename for clarity
data_new <- select(data_new, REGCODE = state, bed)

# Remove rows with missing values
data_new <- na.omit(data_new)

# Convert 'bed' to numeric and calculate mean prevalence by region
data_new <- data_new %>%
  mutate(bed = as.numeric(bed)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(bed))

View(data_new)
# Load shapefile for subnational boundaries
map <- st_read("C:/Users/Ezra Gayawan/Documents/DHS Data/GPS data
               /Nig2008/shps/sdr_subnational_boundaries2.shp")

# Match REGCODE to region names if needed
#reg_data <- data.frame(
 # REGCODE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
  #            210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370),
#  REGNAME = c('Sokoto', 'Zamfara', 'Katsina', 'Jigawa', 'Yobe', 'Borno', 'Adamawa', 'Gombe', 'Bauchi', 'Kano',
#              'Kaduna', 'Kebbi', 'Niger', 'FCT Abuja', 'Nasarawa', 'Plateau', 'Taraba', 'Benue', 'Kogi', 'Kwara',
 #             'Oyo', 'Osun', 'Ekiti', 'Ondo', 'Edo', 'Anambra', 'Enugu', 'Ebonyi', 'Cross River', 'Akwa Ibom',
#              'Abia', 'Imo', 'Rivers', 'Bayelsa', 'Delta', 'Lagos', 'Ogun')
#)

# Join DHS prevalence data with spatial map data
d <- left_join(map, data_new, by = "REGCODE")

#Add static map with ggplot 
ggplot(d) + 
  geom_sf(aes(fill = prevalence)) +
  scale_fill_viridis_c()+
  theme_bw()

#Interactive map with Plotly
library(plotly)
g <- ggplot(d) + geom_sf(aes(fill = prevalence))+
  scale_fill_viridis_c()+
  theme_bw()
ggplotly(g)

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

