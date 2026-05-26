# 1. Load necessary libraries
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
library(lubridate) # For date handling

# ==========================================
# STEP 0: Create a "Dirty" Dataset for Demonstration
# ==========================================
raw_data <- data.frame(
  Employee_ID = c(101, 102, 102, 103, 104, 105, 106, 107),
  Name = c("John Doe", "Jane Smith", "Jane Smith", " bob brown ", "Alice White", "Charlie Black", "Dave Green", "Eve Blue"),
  Age = c(28, 34, 34, NA, 150, 42, -5, 30), # Contains NA, an outlier (150), and an error (-5)
  Salary = c(50000, 60000, 60000, 55000, 45000, NA, 70000, 52000),
  Join_Date = c("2023-01-01", "2023-02-15", "2023-02-15", "03/10/2023", "2023-05-20", "2023-06-01", NA, "2023-08-12"),
  Department = c("IT", "marketing", "marketing", "IT", "Sales", "it", "Sales ", "HR") # Inconsistent casing/spacing
)

print("Original Dirty Data:")
print(raw_data)

# ==========================================
# STEP 1: Remove Duplicates
# ==========================================
# Identifies and removes identical rows
clean_data <- raw_data %>%
  distinct()

# ==========================================
# STEP 2: Fix Structural & Formatting Errors
# ==========================================
clean_data <- clean_data %>%
  mutate(
    # Fix inconsistent casing and whitespace in Department
    Department = str_trim(Department),
    Department = str_to_upper(Department),
    
    # Clean up Name strings (remove leading/trailing spaces)
    Name = str_trim(Name),
    
    # Standardize Date Formats using lubridate
    # parse_date_time handles multiple formats (YYYY-MM-DD and MM/DD/YYYY)
    Join_Date = parse_date_time(Join_Date, orders = c("ymd", "mdy"))
  )
print(clean_data)
# ==========================================
# STEP 3: Detect and Evaluate Outliers
# ==========================================
# We define Age < 0 or Age > 110 as "impossible" data for an employee
clean_data <- clean_data %>%
  mutate(Age = ifelse(Age < 0 | Age > 110, NA, Age)) 
print(clean_data)

# ==========================================
# STEP 4: Handle Missing Values (NAs)
# ==========================================
# Strategy A: Impute (fill) Age with the median
# Strategy B: Drop rows where Join_Date is missing (if it's critical)
clean_data <- clean_data %>%
  mutate(Age = replace_na(Age, median(Age, na.rm = TRUE))) %>%
  drop_na(Join_Date) # Removing rows where date is unknown
print(clean_data)
# ==========================================
# STEP 5: Validate the Cleaned Data
# ==========================================

# Check 1: Ensure Salary is numeric and handle the remaining NA
clean_data <- clean_data %>%
  mutate(Salary = replace_na(Salary, mean(Salary, na.rm = TRUE)))
print(clean_data)

# Final Check: Summary of the clean dataset
print("Final Cleaned Data:")
print(clean_data)

# Check characteristics of clean data:
summary(clean_data)

# ==========================================
# SUMMARY OF ACTIONS TAKEN:
# ==========================================
# 1. Duplicates: Removed the duplicate entry for Jane Smith.
# 2. Structural: Converted 'it' and 'IT' to 'IT'; trimmed spaces in 'Sales '.
# 3. Formatting: Converted '03/10/2023' string into a proper Date object.
# 4. Outliers: Removed Age 150 and -5, replacing them with the median.
# 5. Missing Data: Filled missing Age and Salary; dropped record with no Join_Date.