library(haven)
library(ggplot2)
library(gcookbook)
data<-read_sav("diet.sav")
data$gender
table(data$gender)
View(data)
data$Weightloss
sum(is.na(data)) 
data<-na.omit(data)
sum(is.na(data))
View(data)
# Common ggplot point shapes
#15	Square (filled)#16	Circle (filled)#17	Triangle (filled)#18	Diamond#0	Square (hollow)
#1	Circle (hollow)#2	Triangle (hollow)#3	Plus#	X
ggplot(data, aes(x = preweight, y =Weightloss)) + geom_point()
ggplot(data, aes(x = preweight, y = Weightloss)) + geom_point(size = 1.5)

ggplot(data, aes(x = Age,  y = Weightloss,   shape = factor(gender), 
                 colour = factor(gender))) +geom_point() +
  scale_shape_manual(values = c(0,1)) +
  scale_colour_brewer(palette = "Set1")
# add labels smoker and non smoker 
ggplot(data, aes(x = Age,  
                 y = Weightloss,   
                 shape = factor(gender), 
                 colour = factor(gender))) +
  geom_point(size= 3) +
  scale_shape_manual(values = c(10,19),
                     labels = c("female", "male")) +
  scale_colour_brewer(palette = "Set1",
                      labels = c("female", "male")) +
  labs(shape = "Gender Status",
       colour = "Gender Status")
Weightgroup <- cut(data$Weightloss, 
                   breaks = c(-Inf, 5, Inf), 
                   labels = c("<5", ">=5"))
data$Weightloss

# Use shapes with fill and color, and use colors that are empty (NA) and
# filled
ggplot(data, aes(x = preweight, y = Weightloss, shape = factor(gender), fill = Weightgroup)) +
  geom_point(size = 2.5) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values = c(NA, "black"),
                    guide = guide_legend(override.aes = list(shape = 21)))

# combine all 
ggplot(data, aes(x = preweight, 
                 y = Weightloss,
                 shape = factor(gender),
                 colour = factor(gender),
                 fill = Weightgroup)) +
  geom_point(size = 3, stroke = 0.8) +  # stroke for outline
  scale_shape_manual(values = c(21, 24), labels = c("Female", "Male")) +
  scale_colour_manual(values = c("Female" = "red", "Male" = "blue"), 
                      labels = c("Female", "Male")) +
  scale_fill_manual(values = c("<5" = NA, ">=5" = "black"),
                    labels = c("<5", ">=5"),
                    guide = guide_legend(override.aes = list(shape = 15))) +
  labs(shape = "Gender",
       colour = "Gender",
       fill = "Weight Group") +
  theme_minimal()

# List the four columns we'll use
data[, c("gender", "Age", "preweight", "Weightloss")]
ggplot(data, aes(x = Age, y = preweight, colour = Weightloss)) +
  geom_point()
range(data$Weightloss)

size_range <- range(data$Weightloss) / max(data$Weightloss) *  6
size_range
#> [1] 1.766764 6.000000
ggplot(data, aes(x = Age, y = preweight, size = Weightloss)) +
  geom_point() +
  scale_size_continuous(range = size_range)
ggplot(data, aes(x = Age, y = preweight, size = Weightloss)) +
  geom_point() +
  scale_size_area()
# overplotting 
sp <- ggplot(data, aes(x = Age, y = Weightloss))
sp + geom_point()
sp + geom_point(alpha = 1)
sp + geom_point(alpha = .5)

sp + stat_bin2d()
sp + stat_bin2d(bins = 50) +
  scale_fill_gradient(low = "lightblue", high = "red", limits = c(-2.1, 22.8))
library(hexbin)
sp + stat_binhex() +
  scale_fill_gradient(low = "black", high = "red",
                      limits = c(-2.1, 22.8))
sp + stat_binhex() +
  scale_fill_gradient(low = "black", high = "red",
                      breaks = c( 0, 5, 10, 15, 20, 25, 30),
                      limits = c(-2.1, 22.8))
