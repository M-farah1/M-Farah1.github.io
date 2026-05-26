# Load required libraries
library(haven)  
library(sf)        
library(dplyr)     
library(spdep)     
library(ggplot2)   
library(mapview)   
library(leaflet)   
library(tmap)      
library(INLAspacetime)      


# Load shapefile containing subnational boundaries of Nigeria
map <- st_read("C:/Users/Ezra Gayawan/Documents/DHS Data/GPS data/Nig2008/shps/sdr_subnational_boundaries2.shp")

# Load DHS datasets for 2008, 2010, and 2013
data1 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/DHS Children recode 2013_2018/Children 2008 with spatial covariates/NGKR53DT/NGKR53FL.DTA")
data2 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/MIS/Child reode/NGKR61DT/NGKR61FL.DTA")
data3 <- read_dta("C:/Users/Ezra Gayawan/Documents/DHS Data/DHS Children recode 2013_2018/NGKR6AFL.DTA")

# Select relevant columns: state, bed net use, survey year
data1_new <- select(data1, "sstate", "v461", "v007", "v190", "v106", "v012")
data2_new <- select(data2, "sstate", "v461", "v007", "v190", "v106", "v012")
data3_new <- select(data3, "sstate", "v461", "v007", "v190", "v106", "v012")

# Rename columns for clarity: REGCODE = state, bed = bed net use, year = survey year
data1_new <- select(data1, REGCODE = sstate, bed = v461, year = v007, wealth = v190, edu = v106, age=v012)
data2_new <- select(data2, REGCODE = sstate, bed = v461, year = v007, wealth = v190, edu = v106, age=v012)
data3_new <- select(data3, REGCODE = sstate, bed = v461, year = v007, wealth = v190, edu = v106, age=v012)

# Combine all years into one dataset and remove rows with missing values
merged_data <- rbind(data1_new, data2_new, data3_new)
merged_data <- na.omit(merged_data)
View(merged_data)

library(fastDummies)
#Create dummies for variables wealth_index and educational_level
merged_data_new <- dummy_cols(merged_data, select_columns = 
                                c("wealth", "edu"), 
                              remove_first_dummy = TRUE, 
                              remove_selected_columns = FALSE)


# Create a mapping between numeric region codes and region names
reg_data <- data.frame(
  REGCODE = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240, 250, 260, 270, 280, 290, 300, 310, 320, 330, 340, 350, 360, 370),
  REGNAME = c('Sokoto', 'Zamfara', 'Katsina', 'Jigawa', 'Yobe', 
              'Borno', 'Adamawa', 'Gombe', 'Bauchi', 'Kano', 'Kaduna', 'Kebbi', 'Niger', 
              'FCT Abuja', 'Nasarawa', 'Plateau', 'Taraba', 'Benue', 'Kogi', 'Kwara', 'Oyo',
              'Osun', 'Ekiti', 'Ondo', 'Edo', 'Anambra', 'Enugu', 'Ebonyi', 'Cross River', 
              'Akwa Ibom', 'Abia', 'Imo', 'Rivers', 'Bayelsa', 'Delta', 'Lagos', 'Ogun')
)

# Merge region names with the survey data based on region code
merged_data_new  <- left_join(merged_data_new, reg_data, by = c("REGCODE" = "REGCODE"))  

# Create a spatial neighbors object based on the map (defines adjacency between regions)
nb <- poly2nb(map)  
#matn <- nb2mat(nb)
# Convert neighborhood structure to INLA graph format and save it
nb2INLA("NGR.graph", nb)  

# Read the INLA graph back into R
NGR.adj <- inla.read.graph(filename="NGR.graph")  

merged_data_new$state <- merged_data_new$REGCODE/10
# Convert region names into numeric ID for modeling
#merged_data_new$state <- as.numeric(as.factor(merged_data_new$REGNAME))  

# Create a time variable
merged_data_new$time <- 1 + merged_data_new$year - min(merged_data_new$year)
View(merged_data_new)
#
# Model with linear terms alone
# formula1 <- bed ~ factor(wealth) + factor(edu) + age
# glm(formula1, family = "binomial", data= merged_data_new)

# Define a spatial model formula using Besag model for structured spatial effects
formula <- bed ~ wealth_2 + wealth_3 + wealth_4 + wealth_5 + 
  edu_1 + edu_2 + edu_3 +
  f(state, model = "besag", graph = NGR.adj)  

#### Run the formula in INLA
model1 <- inla(formula, data = merged_data_new, family = "binomial",
               control.compute = list(dic = TRUE, waic = TRUE))

summary(model1)
plot(model1)

sp <- model1$summary.random$state
View(sp)

map$ID <- 1:37

mapsp <- inner_join(map, sp)

##### Plotting the spatial effects
# Create faceted maps to visualize changes in prevalence over survey years
ggplot(data = mapsp) +
  geom_sf(aes(fill = exp(mean))) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")

#### Duplicate state variable because a single var cannot be used twice in INLA
merged_data_new$state1 <- merged_data_new$state

# Extend model to include temporal effects using random walk (RW1) model
formula <- bed ~ wealth_2 + wealth_3 + wealth_4 + wealth_5 + 
  edu_1 + edu_2 + edu_3 + f(age, model = "rw1")+
  f(state, model = "besag", graph = NGR.adj)  + f(time, model="ar1") +
  f(state1, model="iid") 

# Fit the model using INLA (binomial family since bed net use is binary)
model2 <- inla(formula, data = merged_data_new, family = "binomial", 
               control.compute = list(dic = TRUE, waic = TRUE), verbose = TRUE)  

summary(model2)
plot(model2)

sp1 <- model2$summary.random$state1

mapsp1 <- inner_join(map, sp1)
###
# Create faceted maps to visualize changes in prevalence over survey years
ggplot(data = mapsp1) +
  geom_sf(aes(fill = exp(mean))) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")

model$summary.fixed

fitv <- model2$summary.fitted.values
View(fitv)
# Extract posterior mean estimates of bed net use prevalence
merged_data_new$prevalence <- model2$summary.fitted.values[, "mean"]  

prev <- merged_data_new %>% 
  group_by(REGCODE) %>% 
  summarise(prevalence = mean(prevalence))
# Merge the modeled prevalence data with the spatial map data for visualization

mapm2 <- inner_join(map, prev)

ggplot(data = mapm2) +
  geom_sf(aes(fill = prevalence)) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")


############## Plot the space-time prevalence
View(merged_data_new)
prevyr <- merged_data_new %>% 
  group_by(REGCODE, year) %>% 
  summarise(prevalence = mean(prevalence))

mapprevyr <- inner_join(map, prevyr)

# Create faceted maps to visualize changes in prevalence over survey years
ggplot(data = mapprevyr) +
  geom_sf(aes(fill = prevalence)) + 
  facet_wrap(~year) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")


######## Run a model with space-time interaction
merged_data_new$time1 <- ifelse(merged_data_new$time==1,1,0)
merged_data_new$time2 <- ifelse(merged_data_new$time==3,1,0)
merged_data_new$time3 <- ifelse(merged_data_new$time==6,1,0)
merged_data_new$state2 <- merged_data_new$state
merged_data_new$state3 <- merged_data_new$state
merged_data_new$state4 <- merged_data_new$state


formula3 <- bed ~ wealth_2 + wealth_3 + wealth_4 + wealth_5 + 
  edu_1 + edu_2 + edu_3 + f(age, model = "rw1")+
  f(state, model = "besag", graph = NGR.adj)  + f(time, model="ar1") +
  f(state4, model="iid") +
  f(state1, time1, model = "besag", graph = NGR.adj)+
  f(state2, time2, model = "besag", graph = NGR.adj)+
  f(state3, time3, model = "besag", graph = NGR.adj)


model3 <- inla(formula3, data = merged_data_new, family = "binomial", 
               control.compute = list(dic = TRUE, waic = TRUE), verbose = TRUE)  

######## plotting the maps
spt1 <- model3$summary.random$state1
spt2 <- model3$summary.random$state2
spt3 <- model3$summary.random$state3


mapspt3 <- inner_join(map, spt3)
###
# Create faceted maps to visualize changes in prevalence over survey years
ggplot(data = mapspt3) +
  geom_sf(aes(fill = exp(mean))) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")

####### GWR for education
formula4 <- bed ~ wealth_2 + wealth_3 + wealth_4 + wealth_5 + 
  f(age, model = "rw1")+
  f(state, model = "besag", graph = NGR.adj)  + f(time, model="ar1") +
  f(state4, model="iid") +
  f(state1, edu_1, model = "besag", graph = NGR.adj)+
  f(state2, edu_2, model = "besag", graph = NGR.adj)+
  f(state3, edu_3, model = "besag", graph = NGR.adj)

model4 <- inla(formula4, data = merged_data_new, family = "binomial", 
               control.compute = list(dic = TRUE, waic = TRUE), verbose = TRUE)  


######## plotting the maps
spe1 <- model4$summary.random$state1
spe2 <- model4$summary.random$state2
spe3 <- model4$summary.random$state3


mapspe3 <- inner_join(map, spe3)
###
# Create faceted maps to visualize changes in prevalence over survey years
ggplot(data = mapspe3) +
  geom_sf(aes(fill = exp(mean))) + 
  scale_fill_distiller(palette = "RdYlBu", name = NULL) +
  labs(title = NULL) + 
  theme_minimal() + 
  theme(legend.position = "right")

