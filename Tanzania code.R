library(haven)     
library(sf)        
library(dplyr)     
library(spdep) 
library(ggplot2)   
library(mapview)   
library(leaflet)  
library(tmap) 
#Load the packages

if(!require(pacman))install.packages("pacman")

pacman::p_load(
  haven,
  sf,
  spdep,
  mapview,
  leaflet,
  tmap,
  readxl,
  tidyverse,
  rio,
  here,
  plotly
)


# Load administrative boundaries: 
# Country (0), Regional (1), and District (2)

tan_shp0 <- st_read(here("gadm41_TZA_shp", "gadm41_TZA_0.shp"))
tan_shp1 <- st_read(here("gadm41_TZA_shp", "gadm41_TZA_1.shp"))
tan_shp2 <- st_read(here("gadm41_TZA_shp", "gadm41_TZA_2.shp"))
som_shp0<-st_read("~/gadm41_SOM_0.shp")
som_shp1<-st_read("~/gadm41_SOM_1.shp")
som_shp2<-st_read("~/gadm41_SOM_2.shp")
plot(som_shp2)
plot(tan_shp2)
plot(st_geometry(som_shp2))
plot(st_geometry(tan_shp2))#Extracts only the geometry column from the sf object i.e., the spatial features (polygons, points, lines) without the attribute data

View(tan_shp2)
View(som_shp2)
# Create a new column called ID
fct <- 
  som_shp2 %>% 
  mutate(ID = c(1:74))%>% 
  #alternative: 
  #mutate(ID = row_number())
  select(ID, everything())

View(fct)

# Filter the Region of interest
fct_Sanaag <- 
  fct %>% 
  filter(NAME_1 == "Sanaag")

View(fct_Sanaag)

plot(st_geometry(fct_Sanaag))

# Create a new dataset
ds <- data.frame(
  ID = c(1:8),
  Prevalence = c(0.2, 0.4, 0.6, 0.8, 
                 0.9, 0.4, 0.6, 0.8))
View(ds)
# Join data
new_data <- left_join(fct_Sanaag, ds, by = "ID")
View(new_data)

g <- ggplot(new_data) + geom_sf(aes(fill = Prevalence))
g

# Bubble plot
tm_shape(new_data) +
  tm_bubbles("Prevalence", 
             col = "blue") +
  tm_borders()


#Interactive map with leaflet
pal <- colorNumeric(palette = "YlOrRd", 
                    domain = ds$prevalence)

leaflet(new_data) %>%
  addTiles() %>%
  addPolygons(color = "white", 
              fillColor = ~pal(Prevalence), 
              fillOpacity = 0.5) %>%
  addLegend(pal = pal, 
            values = ~Prevalence, 
            opacity = 0.8)


# Plot administrative levels with different line thickness
plot(st_geometry(tan_shp0), lwd = 3)
plot(st_geometry(tan_shp1), lwd = 2, add = TRUE)
plot(st_geometry(tan_shp2), lwd = 1, add = TRUE)

# Thematic map layout with varying border widths
tm_shape(tan_shp0) + 
  tm_borders(lwd = 2.5) +
  tm_shape(tan_shp1) + 
  tm_borders(lwd = 1.5) +
  tm_shape(tan_shp2) + 
  tm_borders(lwd = 1)


# Import the DHS dataset
data <- haven::read_dta(here("Tanzania_IR7BDT", "TZIR7BFL.DTA"))

View(data)

# Select only variable on state code and bed net usage

data_reg_nets <- 
  data %>% 
  select(REGCODE = "v101", 
         Bednets = "v461" ) %>% 
  na.omit()

table(data_reg_nets)

reg_nets_edited <- 
  data_reg_nets %>% 
  mutate(Bednets = as.numeric(Bednets)) %>% 
  group_by(REGCODE) %>% 
  summarise(prevalence = round(mean(Bednets)*100, 1))

view(reg_nets_edited)


# Load shapefile for regions & district

tza_shape <- st_read(here("TZA_dhs_shp", "shps",
                          "sdr_subnational_boundaries2.shp"))
view(tza_shape)


# Create new dataset by join tza_shape & data_reg_nets
fig_data <- left_join(tza_shape, reg_nets_edited, by = "REGCODE")

fig_data <- 
  fig_data %>% 
  rename(Prevalence = prevalence)


view(fig_data)


#Add static map with ggplot 
ggplot(fig_data) + 
  geom_sf(aes(fill = Prevalence)) +
  scale_fill_viridis_c()+
  theme_bw()


#Interactive map with Plotly

fig_plot <- 
  ggplot(fig_data) + 
  geom_sf(aes(fill = Prevalence))+
  scale_fill_viridis_c()+
  theme_bw()

ggplotly(fig_plot)


#Interactive map with leaflet

pal <- colorNumeric(palette = "YlOrRd", 
                    domain = fig_data$Prevalence)

leaflet(fig_data) %>%
  addTiles() %>%
  addPolygons(color = "white", 
              fillColor = ~pal(Prevalence), 
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = 
              ~Prevalence, opacity = 0.8)

#Map with mapview

mapview(fig_data, 
        zcol = "Prevalence")

tm_shape(fig_data) +
  tm_polygons("Prevalence", 
              fill.scale = tm_scale_intervals(n = 5,
                                              values = "viridis"
              ))+
  tm_text("REGNAME", 
          size = 0.5,           # Adjust text size
          col = "black",        # Text color
          bg.color = "white",   # Background color for better readability
          bg.alpha = 0.5,       # Background transparency
          auto.placement = TRUE, # Automatically place labels to avoid overlap
          remove.overlap = TRUE) + # Remove overlapping labels
  tm_layout(
    main.title = "Figure 1. Bednet usage prevalence among Tanzanian regions",
    main.title.size = 0.9,
    frame = TRUE,
    legend.outside = TRUE,                    # Legend outside map
    legend.outside.position = "right",        # Position outside right
    legend.outside.size = 0.3,                # Width of outside legend
    legend.title.size = 0.8,
    legend.text.size = 0.6,
    inner.margins = c(0.05, 0.05, 0.05, 0.15))+ # Adjust for outside legend
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position = "right", size = 1) 


# Lets view Tanzania shapefile 

View(tza_shape)

# Create spatial neighbor list using Queen contiguity (shared point or border)
tza_neighbour <- 
  poly2nb(tza_shape, 
          queen = T)


# View the structure of the first few neighbor relationships
head(tza_shape)# Plot the map geometry
plot(st_geometry(tza_shape), border = "lightgray")

# Overlay neighbor relationships on the map
plot.nb(tza_neighbour, 
        st_geometry(tza_shape), 
        add = TRUE)


# Define an area to be used as the focus lets call it ID
id <- 19

# Initialize a new column to classify neighbor types
#tza_shape$neighbors <- "Other"

# Mark the selected area
#tza_shape$neighbors[id] <- "Area"

# Mark its neighbors
#tza_shape$neighbors[tza_neighbour[[id]]] <- "Neighbors"

# Creates a 3-category classification system perfect for mapping where you can visually distinguish between:

# Your main area of interest ("Area")
# Its immediate neighbors ("Neighbors")
# All other areas ("other")

tza_shape_cat <- 
  tza_shape %>%
  mutate(neighbors = case_when(
    row_number() == id ~ "Area",
    row_number() %in% tza_neighbour[[id]] ~ "Neighbors",
    TRUE ~ "Other"
  )) %>% 
  rename(Neighbors = neighbors)


# Plot the map, coloring by area, neighbors, and others
ggplot(tza_shape_cat) + 
  geom_sf(aes(fill = Neighbors)) + 
  theme_bw() +
  scale_fill_manual(values = c("green", 
                               "pink", 
                               "white"))


# Create centroids of each polygon: These calculates the geographic center point of each polygon. These centroids are often used for point-based spatial analysis

tza_shape1_centroids <- st_centroid(tza_shape)

# Convert neighbors list to spatial weights with row standardization (W). Identifies which polygons share boundaries (using queen contiguity). Creates a list where each element shows the neighbors of each area

neighbour_centroids <- poly2nb(tza_shape, 
                               queen = TRUE)

neighbour_weight <- spdep::nb2listw(neighbour_centroids, 
                                    style = "B")

# This is the key step! It converts the neighbor list into spatial weights with different standardization. Style can take values “W”, “B”, “C”, “U”, “minmax” and “S”

# Check weights for first 3 areas
neighbour_weight$weights[1:3]

# This shows the weights assigned to the first 3 areas. With style "B", you'll typically see:

# [[1]] 1 1 1 (three neighbors, each with weight 1)
# [[2]] 1 1 (two neighbors, each with weight 1)
# [[3]] 1 1 1 1 (four neighbors, each with weight 1)


# This spatial weights matrix (neighbour_weight) is used for:
# Moran's I (global spatial autocorrelation)
# LISA (Local Indicators of Spatial Association)
# Spatial regression models
# Spatial autocorrelation tests


# Convert weights to matrix format for visualization
m1 <- listw2mat(neighbour_weight)

# Get centroids again
coo <- st_centroid(tza_shape)

# Create neighbor list again
nb <- poly2nb(tza_shape, 
              queen = TRUE)

# Compute distances between neighbors
dists <- nbdists(nb, coo)

# Create inverse distance weights
ids <- lapply(dists, function(x){1/x})

# Generate spatial weights using inverse distances
nbw <- nb2listw(nb, glist = ids, style = "B")

# Check new weights
nbw$weights[1:3]

# Create matrix from the new weights
m2 <- listw2mat(nbw)

# Visualize the inverse-distance spatial weights matrix
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))


# Get centroids again
coo <- st_centroid(tza_shape)

# Create neighbor list again
nb <- poly2nb(tza_shape, 
              queen = TRUE)

# Compute distances between neighbors
dists <- nbdists(nb, coo)

# Create inverse distance weights
ids <- lapply(dists, function(x){1/x})

# Generate spatial weights using inverse distances
nbw <- nb2listw(nb, glist = ids, style = "B")

# Check new weights
nbw$weights[1:3]

# Create matrix from the new weights
m2 <- listw2mat(nbw)

# Visualize the inverse-distance spatial weights matrix
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))


# Load DHS data for 2004, 2007, 2015 and 2022

tza_2004 <- haven::read_dta(here("TZKR4IDT", "TZKR4IFL.DTA"))
tza_2010 <- haven::read_dta(here("TZKR7BDT", "TZKR7BFL.DTA"))
tza_2017 <- haven::read_dta(here("TZKR63DT", "TZKR63FL.DTA"))
tza_2022 <- haven::read_dta(here("Tanzania_IR7BDT", "TZIR7BFL.DTA"))



# Extract only relevant columns: region and variable of interest (v461)
data_2004 <- tza_2004 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()

data_2010 <- tza_2010 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()

data_2017 <- tza_2017 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()


data_2022 <- tza_2022 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()


# Group each dataset by region (REGCODE), 
# Convert v461 to numeric, and compute mean prevalence


# Group each dataset by region (REGCODE), 
# Convert v461 to numeric,
# Compute mean prevalence


data2004_prev <- data_2004 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))


data2010_prev <- data_2010 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))


data2017_prev <- data_2017 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))



data2022_prev <- data_2022 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))

# Combine all datasets into a single dataset

merged_data <- bind_rows(data2004_prev, 
                         data2010_prev,
                         data2017_prev,
                         data2022_prev)

view(merged_data)

# Read the shapefile

map1 <- st_read(here("TZA_dhs_shp", "shps", "sdr_subnational_boundaries2.shp"))

plot(map1)


# Join prevalence data to the spatial object (shapefile)
new_data <- map1 %>% 
  left_join(merged_data, by = "REGCODE")


# View the spatial data using interactive map, colored by prevalence
mapview(new_data, 
        zcol = "prevalence")

plot(tan_shp2)
plot(st_geometry(tan_shp2))#Extracts only the geometry column from the sf object i.e., the spatial features (polygons, points, lines) without the attribute data

View(tan_shp2)

# Create a new column called ID
fct <- 
  tan_shp2 %>% 
  mutate(ID = c(1:186))%>% 
  #alternative: 
  #mutate(ID = row_number())
  select(ID, everything())

View(fct)

# Filter the Region of interest
fct_Mwanza <- 
  fct %>% 
  filter(NAME_1 == "Mwanza")

View(fct_Mwanza)

plot(st_geometry(fct_Mwanza))

# Create a new dataset
ds <- data.frame(
  ID = c(113:120),
  Prevalence = c(0.2, 0.4, 0.6, 0.8, 
                 0.9, 0.4, 0.6, 0.8))

# Join data
new_data <- left_join(fct_Mwanza, ds, by = "ID")
View(new_data)

g <- ggplot(new_data) + geom_sf(aes(fill = Prevalence))
g

# Bubble plot
tm_shape(new_data) +
  tm_bubbles("Prevalence", 
             col = "blue") +
  tm_borders()


#Interactive map with leaflet
pal <- colorNumeric(palette = "YlOrRd", 
                    domain = ds$prevalence)

leaflet(new_data) %>%
  addTiles() %>%
  addPolygons(color = "white", 
              fillColor = ~pal(Prevalence), 
              fillOpacity = 0.5) %>%
  addLegend(pal = pal, 
            values = ~Prevalence, 
            opacity = 0.8)


# Plot administrative levels with different line thickness
plot(st_geometry(tan_shp0), lwd = 3)
plot(st_geometry(tan_shp1), lwd = 2, add = TRUE)
plot(st_geometry(tan_shp2), lwd = 1, add = TRUE)

# Thematic map layout with varying border widths
tm_shape(tan_shp0) + 
  tm_borders(lwd = 2.5) +
  tm_shape(tan_shp1) + 
  tm_borders(lwd = 1.5) +
  tm_shape(tan_shp2) + 
  tm_borders(lwd = 1)


# Import the DHS dataset
data <- haven::read_dta(here("Tanzania_IR7BDT", "TZIR7BFL.DTA"))

View(data)

# Select only variable on state code and bed net usage

data_reg_nets <- 
  data %>% 
  select(REGCODE = "v101", 
         Bednets = "v461" ) %>% 
  na.omit()

table(data_reg_nets)

reg_nets_edited <- 
  data_reg_nets %>% 
  mutate(Bednets = as.numeric(Bednets)) %>% 
  group_by(REGCODE) %>% 
  summarise(prevalence = round(mean(Bednets)*100, 1))

view(reg_nets_edited)


# Load shapefile for regions & district

tza_shape <- st_read(here("TZA_dhs_shp", "shps",
                          "sdr_subnational_boundaries2.shp"))
view(tza_shape)


# Create new dataset by join tza_shape & data_reg_nets
fig_data <- left_join(tza_shape, reg_nets_edited, by = "REGCODE")

fig_data <- 
  fig_data %>% 
  rename(Prevalence = prevalence)


view(fig_data)


#Add static map with ggplot 
ggplot(fig_data) + 
  geom_sf(aes(fill = Prevalence)) +
  scale_fill_viridis_c()+
  theme_bw()


#Interactive map with Plotly

fig_plot <- 
  ggplot(fig_data) + 
  geom_sf(aes(fill = Prevalence))+
  scale_fill_viridis_c()+
  theme_bw()

ggplotly(fig_plot)


#Interactive map with leaflet

pal <- colorNumeric(palette = "YlOrRd", 
                    domain = fig_data$Prevalence)

leaflet(fig_data) %>%
  addTiles() %>%
  addPolygons(color = "white", 
              fillColor = ~pal(Prevalence), 
              fillOpacity = 0.8) %>%
  addLegend(pal = pal, values = 
              ~Prevalence, opacity = 0.8)

#Map with mapview

mapview(fig_data, 
        zcol = "Prevalence")

tm_shape(fig_data) +
  tm_polygons("Prevalence", 
              fill.scale = tm_scale_intervals(n = 5,
                                              values = "viridis"
              ))+
  tm_text("REGNAME", 
          size = 0.5,           # Adjust text size
          col = "black",        # Text color
          bg.color = "white",   # Background color for better readability
          bg.alpha = 0.5,       # Background transparency
          auto.placement = TRUE, # Automatically place labels to avoid overlap
          remove.overlap = TRUE) + # Remove overlapping labels
  tm_layout(
    main.title = "Figure 1. Bednet usage prevalence among Tanzanian regions",
    main.title.size = 0.9,
    frame = TRUE,
    legend.outside = TRUE,                    # Legend outside map
    legend.outside.position = "right",        # Position outside right
    legend.outside.size = 0.3,                # Width of outside legend
    legend.title.size = 0.8,
    legend.text.size = 0.6,
    inner.margins = c(0.05, 0.05, 0.05, 0.15))+ # Adjust for outside legend
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position = "right", size = 1) 


# Lets view Tanzania shapefile 

View(tza_shape)

# Create spatial neighbor list using Queen contiguity (shared point or border)
tza_neighbour <- 
  poly2nb(tza_shape, 
          queen = T)


# View the structure of the first few neighbor relationships
head(tza_shape)# Plot the map geometry
plot(st_geometry(tza_shape), border = "lightgray")

# Overlay neighbor relationships on the map
plot.nb(tza_neighbour, 
        st_geometry(tza_shape), 
        add = TRUE)


# Define an area to be used as the focus lets call it ID
id <- 19

# Initialize a new column to classify neighbor types
#tza_shape$neighbors <- "Other"

# Mark the selected area
#tza_shape$neighbors[id] <- "Area"

# Mark its neighbors
#tza_shape$neighbors[tza_neighbour[[id]]] <- "Neighbors"

# Creates a 3-category classification system perfect for mapping where you can visually distinguish between:

# Your main area of interest ("Area")
# Its immediate neighbors ("Neighbors")
# All other areas ("other")

tza_shape_cat <- 
  tza_shape %>%
  mutate(neighbors = case_when(
    row_number() == id ~ "Area",
    row_number() %in% tza_neighbour[[id]] ~ "Neighbors",
    TRUE ~ "Other"
  )) %>% 
  rename(Neighbors = neighbors)


# Plot the map, coloring by area, neighbors, and others
ggplot(tza_shape_cat) + 
  geom_sf(aes(fill = Neighbors)) + 
  theme_bw() +
  scale_fill_manual(values = c("green", 
                               "pink", 
                               "white"))


# Create centroids of each polygon: These calculates the geographic center point of each polygon. These centroids are often used for point-based spatial analysis

tza_shape1_centroids <- st_centroid(tza_shape)

# Convert neighbors list to spatial weights with row standardization (W). Identifies which polygons share boundaries (using queen contiguity). Creates a list where each element shows the neighbors of each area

neighbour_centroids <- poly2nb(tza_shape, 
                               queen = TRUE)

neighbour_weight <- spdep::nb2listw(neighbour_centroids, 
                                    style = "B")

# This is the key step! It converts the neighbor list into spatial weights with different standardization. Style can take values “W”, “B”, “C”, “U”, “minmax” and “S”

# Check weights for first 3 areas
neighbour_weight$weights[1:3]

# This shows the weights assigned to the first 3 areas. With style "B", you'll typically see:

# [[1]] 1 1 1 (three neighbors, each with weight 1)
# [[2]] 1 1 (two neighbors, each with weight 1)
# [[3]] 1 1 1 1 (four neighbors, each with weight 1)


# This spatial weights matrix (neighbour_weight) is used for:
# Moran's I (global spatial autocorrelation)
# LISA (Local Indicators of Spatial Association)
# Spatial regression models
# Spatial autocorrelation tests


# Convert weights to matrix format for visualization
m1 <- listw2mat(neighbour_weight)

# Get centroids again
coo <- st_centroid(tza_shape)

# Create neighbor list again
nb <- poly2nb(tza_shape, 
              queen = TRUE)

# Compute distances between neighbors
dists <- nbdists(nb, coo)

# Create inverse distance weights
ids <- lapply(dists, function(x){1/x})

# Generate spatial weights using inverse distances
nbw <- nb2listw(nb, glist = ids, style = "B")

# Check new weights
nbw$weights[1:3]

# Create matrix from the new weights
m2 <- listw2mat(nbw)

# Visualize the inverse-distance spatial weights matrix
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))


# Get centroids again
coo <- st_centroid(tza_shape)

# Create neighbor list again
nb <- poly2nb(tza_shape, 
              queen = TRUE)

# Compute distances between neighbors
dists <- nbdists(nb, coo)

# Create inverse distance weights
ids <- lapply(dists, function(x){1/x})

# Generate spatial weights using inverse distances
nbw <- nb2listw(nb, glist = ids, style = "B")

# Check new weights
nbw$weights[1:3]

# Create matrix from the new weights
m2 <- listw2mat(nbw)

# Visualize the inverse-distance spatial weights matrix
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))


# Load DHS data for 2004, 2007, 2015 and 2022

tza_2004 <- haven::read_dta(here("TZKR4IDT", "TZKR4IFL.DTA"))
tza_2010 <- haven::read_dta(here("TZKR7BDT", "TZKR7BFL.DTA"))
tza_2017 <- haven::read_dta(here("TZKR63DT", "TZKR63FL.DTA"))
tza_2022 <- haven::read_dta(here("Tanzania_IR7BDT", "TZIR7BFL.DTA"))



# Extract only relevant columns: region and variable of interest (v461)
data_2004 <- tza_2004 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()

data_2010 <- tza_2010 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()

data_2017 <- tza_2017 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()


data_2022 <- tza_2022 %>% 
  select("v024", "v461") %>% 
  rename(REGCODE = v024) %>% 
  na.omit()


# Group each dataset by region (REGCODE), 
# Convert v461 to numeric, and compute mean prevalence


# Group each dataset by region (REGCODE), 
# Convert v461 to numeric,
# Compute mean prevalence


data2004_prev <- data_2004 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))


data2010_prev <- data_2010 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))


data2017_prev <- data_2017 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))



data2022_prev <- data_2022 %>%
  mutate(v461 = as.numeric(v461)) %>%
  group_by(REGCODE) %>%
  summarise(prevalence = mean(v461))

# Combine all datasets into a single dataset

merged_data <- bind_rows(data2004_prev, 
                         data2010_prev,
                         data2017_prev,
                         data2022_prev)

view(merged_data)

# Read the shapefile

map1 <- st_read(here("TZA_dhs_shp", "shps", "sdr_subnational_boundaries2.shp"))

plot(map1)


# Join prevalence data to the spatial object (shapefile)
new_data <- map1 %>% 
  left_join(merged_data, by = "REGCODE")


# View the spatial data using interactive map, colored by prevalence
mapview(new_data, 
        zcol = "prevalence")