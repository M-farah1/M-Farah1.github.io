library(haven)
library(sf)
library(spdep)
library(mapview)
library(leaflet)
library(tmap)
library(readxl)
library(tidyverse)

#nig <- st_read("")
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