# data visualization using histogram 
library(haven)
library(ggplot2)
library(gcookbook)
data<-read_sav("adoledata.sav")
sum(is.na(data)) 
data<-na.omit(data)
sum(is.na(data))
View(data)
# combine mean difference according to sex
aggregate(Height ~ Sex, data = data, FUN = mean)
aggregate(Weight ~ place, data = data, FUN = mean)
ggplot(data, aes(x=Weight)) + geom_histogram()
shapiro.test(data$Weight)
ggplot(data, aes(x = Weight)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,        # you can change the number of bins
                 colour = "black",
                 fill = "green") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data$Weight, na.rm = TRUE),
                            sd   = sd(data$Weight,   na.rm = TRUE)),
                linewidth = 1,
                colour = "red")

# Store the values in a simple vector
w <- data$Weight

ggplot(NULL, aes(x = w)) + geom_histogram()
# Set the width of each bin to 5
ggplot(data, aes(x = Weight)) +
  geom_histogram(binwidth = 5, fill = "pink", colour = "black")
# Divide the x range into 15 bins
binsize <- diff(range(data$Weight))/15
ggplot(data, aes(x = Weight)) +
  geom_histogram(binwidth = binsize, fill = "30", colour = "black")

h <- ggplot(data, aes(x = Weight))  # Save the base object for reuse
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 35)
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 35)
# Making Multiple Histograms from grouped data
library(MASS)
# Use gender as the faceting variable
windows()
ggplot(data, aes(x = Weight)) + geom_histogram(fill = "white", colour = "black") +
  facet_grid(Sex ~ .)
data1<- data # Make a copy of the data
View(data)
data1$Sex
# Convert Sex to a factor
data1$Sex<- factor(data1$Sex)
levels(data1$Sex)
# For the recode() function
library(dplyr) 
data1$Sex <- recode(data1$Sex, "0" = "female", "1" = "male")
ggplot(data1, aes(x = Height)) + geom_histogram(fill = "pink", colour = "black") +
  facet_grid(Sex ~ .)
#  Use place as the faceting variable
ggplot(data, aes(x = Weight)) + geom_histogram(fill = "white", colour = "black") +
  facet_grid(place ~ .)
data2<- data # Make a copy of the data
View(data)
data2$place
# Convert place to a factor
data2$place<- factor(data2$place)
levels(data2$place)
# For the recode() function
library(dplyr) 
data2$place <- recode(data2$place, "1" = "urban", "2" = "semi-urban", "3" = "rural")
ggplot(data2, aes(x = Weight)) + geom_histogram(fill = "pink", colour = "black") +
  facet_grid(place ~ .)
# Map place to fill, make the bars NOT stacked, and make them semitransparent
ggplot(data2, aes(x = Weight, fill = place)) +
  geom_histogram(position = "identity", alpha = 0.4)
# Convert sex to a factor
data1$Sex <- factor(data$Sex)
# Map sex to fill, make the bars NOT stacked, and make them semitransparent
ggplot(data1, aes(x = Height, fill = Sex)) +
  geom_histogram(position = "identity", alpha = 0.4)
# Making a Density Curve
ggplot(data, aes(x = Weight)) + geom_density()
# The expand_limits() increases the y range to include the value 0
ggplot(data, aes(x = Weight)) + geom_line(stat = "density") +
  expand_limits(y = 0)

# Store the values in a simple vector
w <- data$Weight
ggplot(NULL, aes(x = w)) + geom_density()

ggplot(data, aes(x = Weight)) +
  geom_line(stat = "density", adjust = .25, colour = "red") +
  geom_line(stat = "density") +
  geom_line(stat = "density", adjust = 2, colour = "blue")
ggplot(data, aes(x = Weight)) +
  geom_density(fill = "blue", alpha = .2) +
  xlim(20, 75)
# This draws a blue polygon with geom_density(), then adds a line on top
ggplot(data, aes(x = Weight)) +
  geom_density(fill = "blue", colour = NA, alpha = .2) +
  geom_line(stat = "density") +
  xlim(20, 75)
ggplot(data, aes(x = Weight, y = ..density..)) +
  geom_histogram(fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() +
  xlim(20, 75)
#Making Multiple Density Curves from  Grouped Data (145)
library(MASS) # For the data set
# Make a copy of the data
data1 <- data
# Convert smoke to a factor
data1$Sex <- factor(data1$Sex)
# Map smoke to colour
ggplot(data1, aes(x = Weight, colour = Sex)) + geom_density()
# Map smoke to fill and make the fill semitransparent by setting alpha
ggplot(data1, aes(x = Weight, fill = Sex)) + geom_density(alpha = .3)
ggplot(data1, aes(x = Weight)) + geom_density() + facet_grid(Sex ~ .)
levels(data1$Sex)
library(dplyr) # For the recode() function
data1$Sex <- recode(data1$Sex, "0" = "female", "1" = "male")
ggplot(data1, aes(x = Weight)) + geom_density() + facet_grid(Sex ~ .)
ggplot(data1, aes(x = Weight, y = ..density..)) +
  geom_histogram(binwidth = 2, fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() + xlim(20, 70) + 
  facet_grid(Sex ~ .)
#  Making a Frequency Polygon
ggplot(data1, aes(x=Weight)) + geom_freqpoly()
ggplot(data1, aes(x = Weight)) + geom_freqpoly(binwidth = 4)
# Use 15 bins
binsize <- diff(range(data1$Weight))/15
ggplot(data1, aes(x = Weight)) + geom_freqpoly(binwidth = binsize)

# Making a Basic Box Plot
library(MASS) # For the data set
ggplot(data1, aes(x = factor(place), y = Weight)) + geom_boxplot()
ggplot(data1, aes(x = factor(place), y = Weight)) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21)
# combine all the three weight from place
ggplot(data1, aes(x = 1, y = Weight)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) +
  theme(axis.title.x = element_blank())
 
# Adding Notches to a Box Plot
library(MASS) # For the data set #> notch went outside hinges. Try setting notch=FALSE.
ggplot(data1, aes(x = factor(place), y= Weight)) + geom_boxplot(notch = TRUE)
#Adding Means to a Box Plot
library(MASS) # For the data set
ggplot(data1, aes(x = factor(place), y = Weight)) + geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "black")
# Making a Violin Plot
library(gcookbook) # For the data set
# Base plot
p <- ggplot(data1, aes(x=Sex, y=Height))
p + geom_violin()
p + geom_violin() + geom_boxplot(width = .1, fill = "black", outlier.colour = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)
p + geom_violin(trim = FALSE)
# Scaled area proportional to number of observations
p + geom_violin(scale = "count")

#  Making Multiple Dot Plots for Grouped Data
library(gcookbook) # For the data set
ggplot(data1, aes(x = Sex, y = Height)) +
  geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center")
ggplot(data1, aes(x = Sex, y = Height)) +
  geom_boxplot(outlier.colour = NA, width = .4) +
  geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center", fill = NA)

ggplot(data1, aes(x = Sex, y = Height)) +
  geom_boxplot(aes(x = as.numeric(Sex) + .2, group = Sex), width = .25) +
  geom_dotplot(aes(x = as.numeric(Sex) - .2, group = Sex), binaxis = "y", 
                binwidth = .5, stackdir = "center") +
  scale_x_continuous(breaks = 1:nlevels(data1$Sex),
                     labels = levels(data1$Sex))

# Making a Density Plot of Two Dimensional Data
# The base plot
p <- ggplot(data1, aes(x=Height, y=Weight))
p + geom_point() + stat_density2d()
# Contour lines, with "height" mapped to color
p + stat_density2d(aes(colour=..level..))

# Map density estimate to fill color
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)
# With points, and map density estimate to alpha
p + geom_point() +
  stat_density2d
p + stat_density2d(aes(fill = ..density..), geom = "raster",
                   contour = FALSE, h = c(.5,5))