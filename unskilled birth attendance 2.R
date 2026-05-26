# Mapping the Prevalence of Solid Fuel Use in Somalia
# Load libraries
library(sf)
library(ggplot2)
library(spdep)
library(tmap)
library(haven)
library(survey)
library(RColorBrewer)
library(dplyr)
library(mapview)
library(leaflet)

# Load shapefile
# Specify the file path to the shapefile representing the administrative boundaries of Somalia.
filepath<-"~/gadm41_SOM_1.shp"
# Read the shapefile using st_read() from the sf package.
somalia<-st_read(filepath)
# Print the names of the first-level administrative units (regions) in Somalia.
somalia$NAME_1
# Load DHS data
# Specify the file path to the DHS dataset (.dta file).
usba <- read_dta("~/SBA.dta")
# Print the structure of the 'HV024' column, which likely contains region codes.
table(usba$V024,usba$dv)
usba$V024

# Calculate the proportion unskilled birth attendance by region
# Apply the function to subsets of the survey data using svyby
# Create a survey design object
options(survey.lonely.psu = "remove")
design <- svydesign(id = ~V001, strata = ~STRU_CODE, weights = ~V005, data = usba,nest=TRUE)
result <- svyby(~dv, ~V024, FUN = svyciprop, design = design, vartype = NULL)
# Print the resulting data frame showing the mean fuel use per region.
print(result)

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
    geom_sf(data = merged_data_sf, aes(fill = dv)) +
    scale_fill_gradientn(colors = colors, na.value = "grey") +
    guides(fill = guide_legend(title = "unskilled birth attendance")) +
    geom_sf_text(data = merged_data_sf, aes(label = NAME_1),
                 cex = 3, hjust = 0.5, vjust = 0.5, color = "black", fontface = "bold") +
    ylab(label = "") +
    xlab(label = "") +
    ggtitle("Spatial distribution of unskilled birth attendance in Somalia") +
    theme(legend.text = element_text(size = 10),
          plot.title = element_text(color = "blue", size = 10, face = "bold"),
          strip.text.x = element_text(size = 10))
windows()
print(p1)

# Exploratory Spatial Data Analysis
# Spatial Autocorrelation Analysis

# 1. Create spatial weights matrix: Queen contiguity
# Remove rows with NA values in the `dv` column (mean fuel use)
merged_data <- merged_data[!is.na(merged_data$dv), ]
# Create a neighborhood object based on queen contiguity between regions.
nb <- poly2nb(merged_data, queen = TRUE)
# Create a spatial weights list from the neighborhood object using the 'W' style (row-standardized weights).
nbw <- nb2listw(nb, style = "W")
# Calculate Global Moran's I
set.seed(123)
# Calculate Global Moran's I statistic for the fuel use data.
gmoran <- moran.test(merged_data$dv, nbw, alternative = "greater")
# Print the output of the Moran's I test.
print(gmoran)
# Extract and print the Moran's I statistic.
print(gmoran[["estimate"]][["Moran I statistic"]])
# Extract and print the z-score of the test.
print(gmoran[["statistic"]])
# Extract and print the p-value of the test.
print(gmoran[["p.value"]])

# Interpretation:
# - If p-value < 0.05 (significance level), reject the null hypothesis of no spatial autocorrelation.
# - Conclusion: There is evidence of spatial autocorrelation in fuel use.

# Moran's for MC
# Conduct a Monte Carlo simulation for Moran's I to assess significance.
gmoranMC <- moran.mc(merged_data$dv, nbw, nsim = 999)
# Print the output of the Monte Carlo simulation
print(gmoranMC)
# Open a new graphics window.
windows()
# Create a histogram of simulated Moran's I values.
hist(gmoranMC$res)
# Add a vertical line at the observed Moran's I statistic.
abline(v = gmoranMC$statistic, col = "red")

# 4. Calculate Local Moran's I
# Calculate Local Moran's I for each region.
lmoran <- localmoran(merged_data$dv, nbw,
                     alternative = "greater")
# Print the first few rows of the local Moran's I results
print(head(lmoran))

# 5. Add Local Moran's results to merged_data
# Add the local Moran's I statistic to the merged dataset.
merged_data$lmI <- lmoran[, "Ii"]
# Add the z-scores of local Moran's I to the merged dataset.
merged_data$lmZ <- lmoran[, "Z.Ii"]
# Add p-values of local Moran's I to the merged dataset.
merged_data$lmp <- lmoran[, "Pr(z > E(Ii))"]

# 6. Calculate Getis-Ord Gi*
# Calculate Getis-Ord Gi* statistic for each region.
local_gi <- localG(merged_data$dv, nbw)
# Add Getis-Ord Gi* statistic to the merged dataset
merged_data$gi <- local_gi

# Convert Getis-Ord Gi* results into z-score to use for plotting
# Calculate z-scores for the Getis-Ord Gi* statistic.
z_gi <- (local_gi - mean(local_gi)) / sd(local_gi)
# Add the z-scores of Gi* to the merged dataset.
merged_data$zgi <- z_gi

# Calculate P-values for Getis-Ord statistic:
# The original localG function doesn't directly give p-values, we need to calculate them based on the z-scores:

merged_data$p_gi <- pnorm(abs(merged_data$zgi), lower.tail = FALSE)*2
# Create maps using tmap
# Set tmap mode to "view" for interactive map viewing.
tmap_mode("view")
# Figure 1 : Spatial Map of dv and its Spatial Lag Map
# (Visualizing Data & Spatial Context)
#################################################

# Figure 1 : Spatial Map of dv and its Spatial Lag Map
# (Visualizing Data & Spatial Context)
#################################################

# Map 1: Unskilled Birth Attendance by region
# create maps using tmap
tmap_mode("view") # ineractive map viewer

# Map 1: Unskilled Birth Attendance by region
p1 <- tm_shape(merged_data) +
    # Plot polygons filled based on `dv` (uncleaned fueluse) using quantile breaks.
    tm_polygons(col = "dv", title = "unskilled birth attendance", style = "quantile") +
    # Adjust layout so that the legend is outside of the plot.
    tm_layout(legend.outside = TRUE)

p0<-tm_shape(merged_data) +
    tm_polygons(
        fill = "dv",
        fill.scale = tm_scale_intervals(style = "quantile"),
        fill.legend = tm_legend(title = "unskilled birth attendance")
    ) +
    tm_layout(legend.outside = TRUE)

# Calculate spatial lag variable
merged_data$dv_lag <- lag.listw(nbw, merged_data$dv)
# Create the spatial lag map using tmap
p2 <- tm_shape(merged_data) +
    tm_polygons("dv_lag", fill.legend = "Spatial Lag of Unskilled Birth Attendance", style = "quantile") +
    tm_layout(legend.outside = TRUE)
tmap_arrange(p1, p2, ncol = 1)

# Create the Normal Distribution Curve Plot
# Generate data for the normal distribution curve
set.seed(123)
x <- seq(-4, 4, length = 200)
y <- dnorm(x)
df <- data.frame(x, y)

# Define critical z-scores
critical_z <- c(-2.58, -1.96, -1.65, 1.65, 1.96, 2.58)

# Define colors for the legend
legend_colors <- c("blue", "lightblue", "yellow", "lightcoral", "red")
legend_labels <- c("< -2.58", "-2.58 - -1.96", "-1.96 - -1.65", "1.65 - 1.96", "1.96 - 2.58", "> 2.58")

# Create the plot
p3 <- ggplot(df, aes(x, y)) +
    geom_line(color = "black") +
    # Add shaded areas for different significance levels
    geom_area(aes(x = ifelse(x < critical_z[1], x, NA)), fill = "blue", alpha = 0.5) +
    geom_area(aes(x = ifelse(x > critical_z[1] & x < critical_z[2], x, NA)), fill = "lightblue", alpha = 0.5) +
    geom_area(aes(x = ifelse(x > critical_z[2] & x < critical_z[3], x, NA)), fill = "yellow", alpha = 0.5) +
    geom_area(aes(x = ifelse(x > critical_z[3] & x < critical_z[4], x, NA)), fill = "yellow", alpha = 0.5) +
    geom_area(aes(x = ifelse(x > critical_z[4] & x < critical_z[5], x, NA)), fill = "lightcoral", alpha = 0.5) +
    geom_area(aes(x = ifelse(x > critical_z[5], x, NA)), fill = "red", alpha = 0.5) +
    geom_vline(xintercept = gmoran[["statistic"]], color = "red", linetype = "dashed", linewidth = 1) +
    # Add critical value lines and labels
    geom_vline(xintercept = critical_z, color = "gray", linetype = "dotted") +
    annotate("text", x = critical_z, y = 0, label = critical_z, vjust = 1.5, size = 3, color = "black") +
    # Add text annotations for z-score and p-value
    annotate("text", x = gmoran[["statistic"]], y = max(y)/2,
             label = paste("z-score:", round(gmoran[["statistic"]], 2)),
             size = 3, color = "red", hjust = 0, vjust = -1) +
    annotate("text", x = 0, y = max(y) * 0.3,
             label = "Random",
             size = 3, color = "black", hjust = 0.5, vjust = -1) +
    annotate("text", x = -3, y = max(y) * 0.3,
             label = "Significant",
             size = 3, color = "black", hjust = 0.5, vjust = -1) +
    annotate("text", x = 3, y = max(y) * 0.3,
             label = "Significant",
             size = 3, color = "black", hjust = 0.5, vjust = -1) +
    # Add text annotations for Moran's I, z-score, and p-value
    annotate("text", x = -3.5, y = max(y) * 0.8,
             label = paste("Moran's Index:", round(gmoran[["estimate"]][["Moran I statistic"]], 5)),
             size = 2.5, color = "black", hjust = 0) +
    annotate("text", x = -3.5, y = max(y) * 0.7,
             label = paste("z-score:", round(gmoran[["statistic"]], 5)),
             size = 2.5, color = "black", hjust = 0) +
    annotate("text", x = -3.5, y = max(y) * 0.6,
             label = paste("p-value:", format(gmoran[["p.value"]], scientific = FALSE, digits = 5)),
             size = 2.5, color = "black", hjust = 0) +
    labs(title = "Normal Distribution Curve", x = "Z-score", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size=9,face = "bold", color = "blue"),
          legend.position = "right",
          panel.spacing = unit(0.05, "lines"),
          plot.margin = margin(t = 0.2, r = 0.2, b = 0.2, l = 0.2, "cm")
    ) +
    # Add legend using annotate
    annotate("rect", xmin = 4.1, xmax = 4.3, ymin = seq(0.05, 0.45, length.out = length(legend_colors)),
             ymax = seq(0.1, 0.5, length.out = length(legend_colors)), fill = legend_colors, alpha = 0.5) +
    annotate("text", x = 4.4, y = seq(0.075, 0.475, length.out = length(legend_labels)),
             label = legend_labels, hjust = 0, size = 2.5) +
    annotate("text", x = 4.2, y = 0.55, label = "Critical Value (z-score)", hjust = 0.5, size = 2.5, fontface = "bold")+
    coord_cartesian(xlim = c(-4,5), ylim = c(0,0.6))


# Display the plot
windows()
print(p3)

#################################################
# Figure 3 Local Moran's I and its p-values
# (Pinpointing Local Clusters)
#################################################


# Map 4: Local Moran's I statistic
p4 <- tm_shape(merged_data) +
    # Plot polygons filled based on the `lmI` variable using quantile breaks.
    tm_polygons(col = "lmI", title = "Local Moran's I",
                style = "quantile") +
    # Adjust layout so that the legend is outside of the plot.
    tm_layout(legend.outside = TRUE)



# Map 5: P-value for Local Moran's I
p5 <- tm_shape(merged_data) +
    # Plot polygons filled based on `lmp` (p-values) with specified breaks
    tm_polygons(col = "lmp", title = "p-value",
                breaks = c(-Inf, 0.05, Inf)) +
    # Adjust layout so that the legend is outside of the plot.
    tm_layout(legend.outside = TRUE)

tmap_arrange(p4,p5, ncol = 2)
#####################################################
# Figure 4: Getis-Ord Gi* and its p-values
# (Hot and Cold Spots, and Significance)
#####################################################

# Map 5: Getis-Ord Gi* statistic
p6 <- tm_shape(merged_data) +
    # Plot polygons filled based on `gi` (Getis-Ord Gi*) using quantile breaks
    tm_polygons(col = "gi", title = "Getis-Ord Gi*",
                style = "quantile") +
    # Adjust layout so that the legend is outside of the plot.
    tm_layout(legend.outside = TRUE)


p7 <- tm_shape(merged_data) +
    # Plot polygons filled based on `p_gi` (p-values of Getis-Ord Gi*) with specified breaks
    tm_polygons(col = "p_gi", title = "P-value of Getis-Ord Gi*",
                breaks = c(-Inf, 0.05, Inf))+
    # Adjust layout so that the legend is outside of the plot.
    tm_layout(legend.outside = TRUE)
tmap_arrange(p6,p7, ncol = 1)
tmap_arrange(p1, p2, p4,  p5, p6, p7, ncol = 2)

window()
tmap_arrange(p1, p4,   p6,  ncol = 1)
# View the spatial data using interactive map, colored by prevalence

###########
# Create a cluster map showing areas with significant local autocorrelation
tm_shape(merged_data) + tm_polygons(col = "lmZ",
                                    title = "Local Moran's I", style = "fixed",
                                    breaks = c(-Inf, -1.96, 1.96, Inf),
                                    labels = c("Negative SAC", "No SAC", "Positive SAC"),
                                    palette =  c("blue", "white", "red")) +
    tm_layout(legend.outside = TRUE)

# Re-run Local Moranâ€™s I test using two-sided p-values
lmoran <- localmoran(merged_data$dv, nbw, alternative = "two.sided")

# Store p-values in the spatial dataset
merged_data$lmp <- lmoran[, 5]

mp <- moran.plot(as.vector(scale(merged_data$dv)), nbw)

# View first few plot values
head(mp)

# Create a new column for cluster type
merged_data$quadrant <- NA
# High-High cluster (positive autocorrelation, significant)
merged_data[(mp$x >= 0 & mp$wx >= 0) & (merged_data$lmp <= 0.05), "quadrant"] <- 1

# Low-Low cluster (negative autocorrelation, significant)
merged_data[(mp$x <= 0 & mp$wx <= 0) & (merged_data$lmp <= 0.05), "quadrant"] <- 2

# High-Low outlier (negative spatial lag)
merged_data[(mp$x >= 0 & mp$wx <= 0) & (merged_data$lmp <= 0.05), "quadrant"] <- 3

# Low-High outlier (positive spatial lag)
merged_data[(mp$x <= 0 & mp$wx >= 0) & (merged_data$lmp <= 0.05), "quadrant"] <- 4
# Non-significant locations
merged_data[(merged_data$lmp > 0.05), "quadrant"] <- 5

# Visualize the LISA cluster map with legend
tm_shape(merged_data) +
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
