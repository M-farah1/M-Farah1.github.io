#Perform Kruskal–Wallis Test in R

#The Kruskal–Wallis test is a non-parametric alternative to one-way ANOVA.
#Use it when: ✔ Data is not normality distributed
#✔ You compare 3 or more groups
#✔ Your variable is continuous/ordinal

#✅ Step-by-Step in R

# 📊 1️⃣ Load your data
library(haven)
df <- read_sav("dietdata.sav")
shapiro.test(df$Weightloss)
hist(df$Weightloss, 
     main = "Histogram of weightloss",
     xlab = "weightloss",
     freq = FALSE,
     col = "pink")
# Add normal distribution curve
windows()
curve(dnorm(x,
            mean = mean(df$Weightloss),
            sd = sd(df$Weightloss)),
      add = TRUE,
      col = "red",
      lwd = 2)

#📊 2️⃣ Run the Kruskal–Wallis test

kruskal.test(Weightloss ~ Diet, data = df)
df$Diet
df$Diet <- as_factor(df$Diet)
table(df$Diet)
#👉 This tells whether at least one group is different.

# 📊 3️⃣ Post-hoc test (Dunn test)

#If the Kruskal–Wallis test is significant:
  
  install.packages("FSA")
library(FSA)

dunnTest(Weightloss ~ Diet, data = df, method = "holm")

#👉 Shows which groups differ after correction.

#📊 4️⃣ Effect size (optional but useful)

library(rstatix)
kruskal_effsize(df, Weightloss ~ Diet)

#👉 Gives epsilon-squared (ε²) to show strength of difference.

#📊 5️⃣ Simple visualization

boxplot(Weightloss ~ Diet, data = df)
boxplot(Weightloss ~ Diet, data = df,
        col = c("lightblue", "lightgreen", "salmon"),
        main = "Weight Loss by Diet Type",
        xlab = "Diet",
        ylab = "Weight Loss",
        border = "darkgray")
#📊 Interpretation

#p < 0.05 → There is a significant difference between groups

#Dunn test → Shows the specific groups that differ

#ε² → Effect size (small, medium, large)