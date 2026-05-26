library(haven)
library(ggplot2)

data<-read_sav("adoledata.sav")
library(vioplot)
vioplot(data$Weight)
library(beeswarm)
beeswarm(data$Weight)
library(beanplot)
beanplot(data$Weight)
# vioplot for a single distribution
vioplot(data$Weight, 
        col = "gold",                 # Sets the fill color of the violin
        xlab = "Weight Distribution", # Sets the x-axis labe
        main = "Violin Plot of Weight") # Sets the plot title

# beeswarm for a single distribution when you want to show outliers
beeswarm(data$Weight, 
         col = "darkgreen",           # Sets the color of the points
         pch = 16,                    # Sets the point type (16 is a solid circle)
         method = "swarm",            # Defines the non-overlapping layout
         xlab = "Weight Distribution", # Sets the x-axis label
         ylab = "Weight Value",        # Sets the y-axis label
         main = "Beeswarm Plot of Weight") # Sets the plot title
# beanplot for a single distribution when you want to disappear outliers.
beanplot(data$Weight, 
         col = "firebrick",           # Sets the color of the bean/density shape
         border = "green",            # Sets the color of the border lines
         names = "All Weights",        # Sets the label for the single group on the x-axis
         xlab = "Weight Distribution", # Sets the x-axis label
         ylab = "Weight Value",        # Sets the y-axis label
         main = "Bean Plot of Weight") # Sets the plot title
