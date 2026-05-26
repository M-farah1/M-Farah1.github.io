# Let's make this nice bar Chart in R📈

#A 100% stacked bar chart is perfect for showing how different categories contribute proportionally to a total. For example, physical activity levels across schools, gender, or treatment groups.

#In this tutorial, you can fully customize:
#✅ Bar colors
#✅ Font size & style
#✅ Axis names
#✅ Legend style
#✅ Bar width
#✅ Layout 
#✅ High-quality export

#🔴Step 1: Install & load packages

#We mainly use ggplot2 for plotting, dplyr for data handling, and scales for percentage formatting.

🔴Step 2: Prepare your data

#Your data should be in this format:

#Group (e.g., School, Gender, Treatment)

#Category (e.g., Sedentary, Light PA, Moderate PA, etc.)

#Value or Percentage

#Each group should sum to 100%.

#🔴Step 3: Convert values into percentages (if needed)

#If your data are raw counts, convert them into proportions so each bar represents 100%.

#🔴Step 4: Customize colors 🎨

#You can manually define colors for each category (Sedentary, Standing, Light PA, etc.) to match your theme or publication style.

#🔴Step 5: Plot using ggplot2

#Use:

#geom_bar() for stacked bars

#scale_y_continuous() for percentage format

#scale_fill_manual() for custom colors

#theme() to customize font size, legend, and axis labels

#🔴Step 6: Split into panels (facets)

#You can divide your plot into multiple panels (e.g., by school, gender, or treatment) using facet_wrap().

#Step 7: Add labels inside bars (optional)

#Display percentage values directly inside each stacked section.

#🔴Step 8: Export high-quality images

#Save your figure in 300 DPI for publications or presentations.

#Great, this is a 100% stacked bar chart (proportional stacked bar plot) with multiple groups. I’ll give you fully customizable R code, step by step, using ggplot2. You can change:

#✅ Colors
#✅ Font size
#✅ Axis titles
#✅ Legend
#✅ Facets / grouping
#✅ Bar width
#✅ Labels
#✅ Theme

#📌Step 1: Install & load required packages


library(ggplot2)
library(dplyr)
library(scales)

# 📌Step 2: Create example data (you can replace with your own)

#This mimics your structure:
#School × Gender × Activity × Percentage

data <- data.frame(
  School = rep(c("Nur", "HRC", "BA", "IT"), each = 10),
  Gender = rep(rep(c("Girl", "Boy"), each = 5), 4),
  Activity = rep(c("Sedentary", "Standing", "Light PA", "Moderate PA", "Vigorous PA"), 8),
  Percentage = c(
    58, 10, 22, 7, 3,
    60, 10, 20, 7, 3,
    
    59, 12, 20, 6, 3,
    62, 11, 18, 6, 3,
    
    60, 12, 20, 5, 3,
    63, 10, 18, 6, 3,
    
    70, 10, 15, 3, 2,
    72, 8, 14, 4, 2))

#📌Step 3: Convert to percentages (if needed)

#If your raw data are counts, do this:

data <- data %>%
  group_by(School, Gender) %>%
  mutate(Percentage = Percentage / sum(Percentage))

#Skip this if already in %.

#📌Step 4: Define custom colors (Fully Editable 🎨)

custom_colors <- c(
  "Sedentary"   = "#1b0c41",
  "Standing"    = "#4b116f",
  "Light PA"    = "#9c2963",
  "Moderate PA" = "#e4572e",
  "Vigorous PA" = "#f4a261")

# 📌Step 5: Create the stacked 100% bar plot

p <- ggplot(data, aes(x = interaction(School, Gender),
                      y = Percentage,
                      fill = Activity)) +
  
  geom_bar(stat = "identity", width = 0.7) +
  
  scale_y_continuous(labels = percent_format()) +
  
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = "Physical Activity Distribution",
    x = "School and Gender",
    y = "Percentage (%)",
    fill = "Activity Type"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14, face = "bold"),
    plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title= element_text(size = 13, face = "bold")
  )

plot(p)

#📌Step 6: Separate panels like your image (Facet)

#To split by School:

p + facet_wrap(~School, nrow = 1)

#📌Step 7: Change Font Family

#If you want Arial, Times, etc:

p + theme(text = element_text(family = "Arial"))

#📌Step 8: Change Bar Width

geom_bar(stat = "identity", width = 0.5)

#📌Step 9: Add Percentage Labels on Bars

p + geom_text(aes(label = paste0(round(Percentage*100), "%")),
              position = position_stack(vjust = 0.5),
              size = 3,
              color = "white")

#Step 10: Save High-Quality Image

ggsave("activity_plot.png", width = 10, height = 6, dpi = 300)
ggsave("C:/Users/THINKPAD/Desktop/activity_plot.png", width = 10, height = 6, dpi = 300)

