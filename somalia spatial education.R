#DAY 3
library(haven)     
library(sf)        
library(dplyr)     
library(spdep) 
library(ggplot2)   
library(mapview)   
library(leaflet)  
library(tmap)    

map0<-st_read("~/gadm41_SOM_0.shp")
map1<-st_read("~/gadm41_SOM_1.shp")
map2<-st_read("~/gadm41_SOM_2.shp")
data1<-read_dta("~/ever_atten.dta")
View(map2)
# Create spatial neighbor list using Queen contiguity (shared point or border)
nb <- spdep::poly2nb(map2, queen = TRUE)

# View the structure of the first few neighbor relationships
head(nb)# Plot the map geometry
plot(st_geometry(map2), border = "lightgray")

# Overlay neighbor relationships on the map
plot.nb(nb, st_geometry(map2), add = TRUE)

# Define an area ID to highlight
id <- 51

# Initialize a new column to classify neighbor types
map2$neighbors <- "other"

# Mark the selected area
map2$neighbors[id] <- "area"

# Mark its neighbors
map2$neighbors[nb[[id]]] <- "neighbors"

# Plot the map, coloring by area, neighbors, and others
ggplot(map2) + geom_sf(aes(fill = neighbors)) + theme_bw() +
  scale_fill_manual(values = c("gray30", "gray", "white"))

# Create centroids of each polygon
coo <- st_centroid(map2)
# Convert neighbors list to spatial weights with row standardization (W)
nb <- poly2nb(map2, queen = TRUE)
nbw <- spdep::nb2listw(nb, style = "B")
#style can take values “W”, “B”, “C”, “U”, “minmax” and “S”

# Check weights for first 3 areas
nbw$weights[1:3]

#m2 <- nb2mat(nb)
# Convert weights to matrix format for visualization
m1 <- listw2mat(nbw)

# Get centroids again
coo <- st_centroid(map2)

# Create neighbor list again
nb <- poly2nb(map2, queen = TRUE)

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
coo <- st_centroid(map2)

# Create neighbor list again
nb <- poly2nb(map2, queen = TRUE)

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

# Load 2003 DHS data
data1 <- read_dta("~/ever_atten.dta")
map0<-st_read("~/gadm41_SOM_0.shp")
map1<-st_read("~/gadm41_SOM_1.shp")
map2<-st_read("~/gadm41_SOM_2.shp")
# Load 2020 DHS data
data1<-read_dta("~/ever_atten.dta")
data1$V024
# Map raw DHS codes to new state codes
state_mapping <- data.frame(V024 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 
                                     110, 120, 130, 140,  150, 160),
        shapeName = c("Awdal", "Woqooyi Galbeed",
                   "Togdheer", "Sool", "Sanaag", "Bari",
                    "Nugaal", "Mudug",
                   "Galguduud", "Hiiraan", "Shabeellaha Dhexe", "Banaadir",  "Bay",
                     "Bakool", "Gedo",  "Jubbada Hoose"))
View(state_mapping)
data1_new <- left_join(data1, state_mapping, by = "V024")
View(data_new)
merged_map <- left_join(map1, data1_new, by = c("NAME_1" = "shapeName"))
names(map1)
View(merged_map)
# Merge state codes into data1
View(map2)
# Load other years of DHS data
data1<- read_dta("~/ever_atten.dta")
View(data1)
View(map2)
#data3 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/MIS/Child reode/NGKR61DT/NGKR61FL.DTA")
#data4 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/DHS Children recode 2013_2018/NGKR6AFL.DTA")
#data5 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/MIS/Child reode/NGKR71DT/NGKR71FL.DTA")
#data6 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/DHS Children recode 2013_2018/Chilren R 2018/NGKR7ADT/NGKR7AFL.DTA")
#data7 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/MIS/Child reode/NGKR81DT/NGKR81FL.DTA")
# Extract only relevant columns: region and variable of interest (v461)
data1_new <- select(data1, "V024", "V106A")
#data2_new <- select(data2, "sstate", "v461")
#data3_new <- select(data3, "sstate", "v461")
#data4_new <- select(data4, "sstate", "v461")
#data5_new <- select(data5, "sstate", "v461")
#data6_new <- select(data6, "sstate", "v461")
#data7_new <- select(data7, "v101", "v461")

# Rename region column to REGCODE for consistency
data1_new <- select(data1, shapeName = V024, V106A)
View(data1_new)
#data2_new <- select(data2, REGCODE = sstate, v461)
#data3_new <- select(data3, REGCODE = sstate, v461)
#data4_new <- select(data4, REGCODE = sstate, v461)
#data5_new <- select(data5, REGCODE = sstate, v461)
#data6_new <- select(data6, REGCODE = sstate, v461)
#data7_new <- select(data7, REGCODE = v101, v461)

# Convert 2021 codes to match older format
#data7_new$REGCODE <- data7_new$REGCODE * 10
# Drop rows with missing values
#data1_new <- na.omit(data1_new)
data1_new <- na.omit(data1_new)
#data3_new <- na.omit(data3_new)
#data4_new <- na.omit(data4_new)
#data5_new <- na.omit(data5_new)
#data6_new <- na.omit(data6_new)
#data7_new <- na.omit(data7_new)


# Group each dataset by region (REGCODE), convert v461 to numeric, and compute mean prevalence
#data1_new <- data1_new %>%
# mutate(v461 = as.numeric(v461)) %>%
#  group_by(REGCODE) %>%
#  summarise(prevalence = mean(v461))
data1_new <- data1_new %>%
  mutate(V106A = as.numeric(V106A)) %>%
  group_by(shapeName) %>%
  summarise(prevalence = mean(V106A))
View(data1_new)
View(map1)
#data3_new <- data3_new %>%
#mutate(v461 = as.numeric(v461)) %>%
#group_by(REGCODE) %>%
# summarise(prevalence = mean(v461))

#data4_new <- data4_new %>%
#  mutate(v461 = as.numeric(v461)) %>%
# group_by(REGCODE) %>%
# summarise(prevalence = mean(v461))

#data5_new <- data5_new %>%
#mutate(v461 = as.numeric(v461)) %>%
#group_by(REGCODE) %>%
# summarise(prevalence = mean(v461))

#data6_new <- data6_new %>%
#mutate(v461 = as.numeric(v461)) %>%
# group_by(REGCODE) %>%
# summarise(prevalence = mean(v461))

#data7_new <- data7_new %>%
# mutate(v461 = as.numeric(v461)) %>%
#group_by(REGCODE) %>%
#summarise(prevalence = mean(v461))
# Combine all datasets into a single dataset
#merged_data <- rbind(data2_new, data3_new, data4_new, data5_new, 
#                    data6_new, data7_new)

# Read the shapefile
map1 <- st_read("~/sdr_subnational_boundaries2.shp")
merged_data <- map1 %>%
  left_join(data1_new, by = c("V024" = "V024"))
View(data1_new)
View(map2)
# Join the prevalence data to the shapefile using REGCODE
new_data <- left_join(map1, data1_new, by = "ShapeName")
View(map1)
View(data1_new)
# View the spatial data using interactive map, colored by prevalence
mapview(new_data, zcol = "prevalence")

# Create neighbors list (Queen contiguity)
nb <- poly2nb(new_data, queen = TRUE)

# Convert neighbor list to spatial weights
nbw <- nb2listw(nb, style = "W")
# Run Global Moran’s I test for spatial autocorrelation
gmoran <- moran.test(new_data$prevalence, nbw, alternative = "greater")


# View test result
gmoran

# Extract the Moran’s I statistic value
gmoran[["estimate"]][["Moran I statistic"]]

# Extract the z-score of the test
gmoran[["statistic"]]

# Run a permutation test (Monte Carlo simulation) for Moran’s I
gmoranMC <- moran.mc(new_data$prevalence, nbw, nsim = 999)

# View Monte Carlo test result
gmoranMC

# Extract p-value from Monte Carlo test
gmoran[["p.value"]]

# Plot histogram of simulated Moran’s I values
hist(gmoranMC$res)
# Add a vertical line for the observed Moran’s I value
abline(v = gmoranMC$statistic, col = "red")

# Create a Moran scatterplot
moran.plot(new_data$prevalence, nbw)

# Run Local Moran’s I (LISA) to detect local clusters
lmoran <- localmoran(new_data$prevalence, nbw, alternative = "greater")

# View first few results
head(lmoran)

# Store Local Moran’s I values in the spatial dataset
new_data$lmI <- lmoran[, "Ii"]   # Local Moran’s I
new_data$lmZ <- lmoran[, "Z.Ii"] # Z-scores
new_data$lmp <- lmoran[, "Pr(z > E(Ii))"] # p-values

# Prevalence map
p1 <- tm_shape(new_data) +
  tm_polygons(
    fill = "prevalence",
    fill.scale = tm_scale(style = "quantile"),
    fill.legend = tm_legend(title = "Prevalence")
  ) +
  tm_layout(legend.outside = TRUE)
# Local Moran's I (lmI)
p2 <- tm_shape(new_data) +
  tm_polygons(
    fill = "lmI",
    fill.scale = tm_scale(style = "quantile"),
    fill.legend = tm_legend(title = "Local Moran's I")
  ) +
  tm_layout(legend.outside = TRUE)

# Z-scores
p3 <- tm_shape(new_data) +
  tm_polygons(
    fill = "lmZ",
    fill.scale = tm_scale(
      breaks = c(-Inf, 1.65, Inf),
      values = c("gray", "red"),
      labels = c("Not significant", "Significant")
    ),
    fill.legend = tm_legend(title = "Z-score")
  ) +
  tm_layout(legend.outside = TRUE)

# p-values
p4 <- tm_shape(new_data) +
  tm_polygons(
    fill = "lmp",
    fill.scale = tm_scale(
      breaks = c(-Inf, 0.05, Inf),
      values = c("red", "white"),
      labels = c("Significant", "Not significant")
    ),
    fill.legend = tm_legend(title = "p-value")
  ) +
  tm_layout(legend.outside = TRUE)
# Arrange all four thematic maps
tmap_arrange(p1, p2, p3, p4)

# Switch to interactive view mode
tmap_mode("view")

# Create a cluster map showing areas with significant local autocorrelation
tm_shape(new_data) + tm_polygons(col = "lmZ",
                                 title = "Local Moran's I", style = "fixed",
                                 breaks = c(-Inf, -1.96, 1.96, Inf),
                                 labels = c("Negative SAC", "No SAC", "Positive SAC"),
                                 palette =  c("blue", "white", "red")) +
  tm_layout(legend.outside = TRUE)

# Re-run Local Moran’s I test using two-sided p-values
lmoran <- localmoran(new_data$prevalence, nbw, alternative = "two.sided")

# Store p-values in the spatial dataset
new_data$lmp <- lmoran[, 5]

mp <- moran.plot(as.vector(scale(new_data$prevalence)), nbw)

# View first few plot values
head(mp)

# Create a new column for cluster type
new_data$quadrant <- NA
# High-High cluster (positive autocorrelation, significant)
new_data[(mp$x >= 0 & mp$wx >= 0) & (new_data$lmp <= 0.05), "quadrant"] <- 1

# Low-Low cluster (negative autocorrelation, significant)
new_data[(mp$x <= 0 & mp$wx <= 0) & (new_data$lmp <= 0.05), "quadrant"] <- 2

# High-Low outlier (negative spatial lag)
new_data[(mp$x >= 0 & mp$wx <= 0) & (new_data$lmp <= 0.05), "quadrant"] <- 3

# Low-High outlier (positive spatial lag)
new_data[(mp$x <= 0 & mp$wx >= 0) & (new_data$lmp <= 0.05), "quadrant"] <- 4
# Non-significant locations
new_data[(new_data$lmp > 0.05), "quadrant"] <- 5

# Visualize the LISA cluster map with legend
tm_shape(new_data) +
  tm_polygons(
    fill = "quadrant",
    fill.scale = tm_scale(
      breaks = c(1, 2, 3, 4, 5, 6),
      values = c("red", "blue", "lightpink", "skyblue2", "white"),
      labels = c("High-High", "Low-Low", "High-Low", "Low-High", "Non-significant")
    ),
    fill.legend = tm_legend(title = "")
  ) +
  tm_borders(fill_alpha = 0.5) +
  tm_title("Clusters") +
  tm_layout(legend.outside = TRUE)