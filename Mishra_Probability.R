#Student Name: NIKITA MISHRA
#Date:24-02-2026.
#Class:Probability Theory and Introductory Statistics Section: 18.
#Module 1 – R Practice Assignment (Week 1)


# Clear all objects from the Workspace
rm(list = ls())
# Clear the console
cat("\014")

#---------------------------------------------------------------------
# Load Required Libraries (Install if Missing)
#---------------------------------------------------------------------

required_packages <- c("gmodels", "lubridate", "tidyverse")

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

invisible(lapply(required_packages, install_if_missing))


#Explanation-
#These libraries provide tools for data manipulation, cleaning, and date handling.
#They make the analysis process more efficient and organized.


#---------------------------------------------------------------------
#Import Dataset-New file name "Sales"
#---------------------------------------------------------------------
Balaji <- read.csv("Balaji Fast Food Sales.csv")


#Explanation:
#This imports the Balaji Fast Food Sales dataset into R.
#The dataset is stored in an object named Balaji for further analysis.


######################################################################
#PART-1
######################################################################

#---------------------------------------------------------------------
#A- Data SetReview and Initial Exploration
#---------------------------------------------------------------------

str(Balaji)
summary(Balaji)
head(Balaji)


#---------------------------------------------------------------------
# B- Rename variables.
#---------------------------------------------------------------------

Balaji <- Balaji %>%
  rename(
    invoice_id      = order_id,
    sales_date      = date,
    item_desc       = item_name,
    category_name   = item_type,
    price_per_unit  = item_price,
    units_sold      = quantity,
    sales_value     = transaction_amount,
    payment_type    = transaction_type,
    handled_by     = received_by,
    sales_time      = time_of_sale
  )


#Explanation:
# This renames columns to more meaningful and analysis-friendly names.


#---------------------------------------------------------------------
# C-Remove blank/NA spaces from Payment_type
#---------------------------------------------------------------------

Balaji$payment_type <- as.character(Balaji$payment_type)

Balaji$payment_type[is.na(Balaji$payment_type) |
                      Balaji$payment_type == ""] <- "Unknown"


#Explanation:
# This converts payment_type to character format.
# Empty values are replaced with "Unknown" for consistency.


#---------------------------------------------------------------------
# D - Handle invalid values using conditional statement (ifelse)
#---------------------------------------------------------------------

Balaji <- Balaji %>%
  mutate(units_sold = ifelse(units_sold < 0, 0, units_sold))


# Explanation:
# This replaces negative values in units_sold with 0.
# It ensures no invalid sales quantities exist in the dataset.


#---------------------------------------------------------------------
# E-Apply Appropriate Data Structure.
#---------------------------------------------------------------------

Balaji <- Balaji %>%
  mutate(
    sales_date  = mdy(sales_date),
    category_name = as.factor(category_name),
    item_desc = as.factor(item_desc),
    payment_type = as.factor(payment_type),
    sales_time = as.factor(sales_time),
    handled_by = as.character(handled_by),
    price_per_unit = as.numeric(price_per_unit),
    units_sold = as.numeric(units_sold),
    sales_value = as.numeric(sales_value)
  )


#Explanation:
#Variables are converted into appropriate data types such as Date, factor, and numeric.
#This ensures accurate statistical analysis and proper visualization.


#---------------------------------------------------------------------
# F: Remove unnecessary column.
#---------------------------------------------------------------------

Balaji <- Balaji %>% select(-handled_by)

#Explanation:
#The handled_by column is removed because it is not needed for analysis.
#This simplifies the dataset and focuses only on relevant variables


#---------------------------------------------------------------------
# G- Manipulate the string columns..
#---------------------------------------------------------------------


Balaji <- Balaji %>%
  mutate(
    payment_type  = str_trim(payment_type),
    payment_type  = str_to_upper(payment_type),
    category_name = str_to_title(category_name)
  )


#Explanantion:
#Extra spaces are removed and text formatting is standardized.
#This ensures consistent category names and avoids duplication errors.


#---------------------------------------------------------------------
# H- Remove duplicate rows.
#---------------------------------------------------------------------

Balaji <- Balaji %>%
  distinct()


#Explanation:
#Duplicate records are removed using distinct().
#This ensures the dataset contains only unique transactions.

######################################################################
#PART-2
######################################################################


#---------------------------------------------------------------------
# A:Frequency Tables
#---------------------------------------------------------------------

# Frequency of Category
category_table <- as.data.frame(table(Balaji$category_name))
colnames(category_table) <- c("Category_Name", "Frequency")

# Frequency of Payment Type
payment_table <- as.data.frame(table(Balaji$payment_type))
colnames(payment_table) <- c("Payment_Type", "Frequency")

# Frequency of Sales Time
sales_time_table <- as.data.frame(table(Balaji$sales_time))
colnames(sales_time_table) <- c("Sales_Time", "Frequency")

# Category vs Payment Type
cross_table <- as.data.frame(table(Balaji$category_name, 
                                   Balaji$payment_type))
colnames(cross_table) <- c("Category_Name", "Payment_Type", "Frequency")

# Sales Time vs Category
sales_category_table <- as.data.frame(
  table(Balaji$sales_time, Balaji$category_name)
)
colnames(sales_category_table) <- c("Sales_Time", "Category_Name", "Frequency")


#Explanation:
#These tables display the number of transactions by category, payment type, and time period.
#They help identify the most common sales patterns.


#---------------------------------------------------------------------
# B-Cross Tabulation
#---------------------------------------------------------------------

Balaji_clean <- subset(Balaji, payment_type != "Unknown")

product_category <- Balaji_clean$category_name
mode_of_payment  <- Balaji_clean$payment_type


# Cross Tabulation
table(product_category, mode_of_payment)

# Flat Table (ftable)
ftable(product_category, mode_of_payment)

# CrossTable (gmodels)
CrossTable(
  product_category,
  mode_of_payment,
  prop.chisq = FALSE,
  prop.t = FALSE,
  prop.r = TRUE,
  prop.c = TRUE
)

#Explanation:
#This script performs a cross-tabulation between product category and payment type.
#It analyze their frequency distribution and row/column percentages.


#---------------------------------------------------------------------
#C-Histograms.
#---------------------------------------------------------------------

# Histogram of Units Sold

hist(
  Balaji$units_sold,
  breaks = 15,
  col = "skyblue",
  border = "white",
  main = "Distribution of Units Sold",
  xlab = "Units Sold",
  ylab = "Number of Transactions",
  cex.main = 1,
  cex.lab = 1.1
)

grid(col = "gray80", lty = "dotted")

# Histogram of Price per Unit
hist(
  Balaji$price_per_unit,
  breaks = 15,
  col = "lightgreen",
  border = "white",
  main = "Distribution of Price per Unit",
  xlab = "Price per Unit",
  ylab = "Number of Transactions",
  cex.main = 1,
  cex.lab = 1.1
)

grid(col = "gray80", lty = "dotted")

# Histogram of Sales Value
hist(
  Balaji$sales_value,
  breaks = 20,
  col = "orange",
  border = "white",
  main = "Distribution of Sales Value",
  xlab = "Sales Value",
  ylab = "Number of Transactions",
  cex.main = 1,
  cex.lab = 1.1
)

grid(col = "gray80", lty = "dotted")


#Explanation:
#Histograms show how numerical values like units sold and sales value are distributed.
#They help understand the spread and pattern of the data.

######################################################################
#PART-3
######################################################################

#---------------------------------------------------------------------
#A-Compare Results Across Multiple Classes using Boxplot
#---------------------------------------------------------------------


# Compare mean sales value by category
aggregate(sales_value ~ category_name, data = Balaji, mean)

# Compare mean sales value by payment type
aggregate(sales_value ~ payment_type, data = Balaji, mean)


# Bar Chart - Category Frequency

category_counts <- table(Balaji$category_name)

barplot(
  category_counts,
  col = "steelblue",
  main = "Number of Transactions by Category",
  xlab = "Category",
  ylab = "Frequency"
)


# Boxplot comparison
boxplot(
  sales_value ~ category_name,
  data = Balaji,
  col = c("blue", "red", "green", "purple"),
  main = "Sales Value by Category",
  xlab = "Product Category",
  ylab = "Sales Value"
)


#Explanation:
#The aggregate() function calculates average sales across different groups.
#The boxplot visually compares sales distribution across categories.

#---------------------------------------------------------------------
#B- Scatter Diagram and Comparison Analysis
#---------------------------------------------------------------------

#Scatter Plot: Units Sold vs Sales Value
plot(
  Balaji$units_sold,
  Balaji$sales_value,
  col = "darkblue",
  pch = 19,
  main = "Units Sold vs Sales Value",
  xlab = "Units Sold",
  ylab = "Sales Value"
)

abline(lm(sales_value ~ units_sold, data = Balaji),
       col = "red",
       lwd = 2)

grid(col = "gray80", lty = "dotted")

# Correlation 1
cor_units_sales <- cor(Balaji$units_sold, Balaji$sales_value, 
                       use = "complete.obs")
cor_units_sales


# 2 Scatter Plot: Price per Unit vs Sales Value
plot(
  Balaji$price_per_unit,
  Balaji$sales_value,
  col = "purple",
  pch = 19,
  main = "Price per Unit vs Sales Value",
  xlab = "Price per Unit",
  ylab = "Sales Value"
)

abline(lm(sales_value ~ price_per_unit, data = Balaji),
       col = "red",
       lwd = 2)

grid(col = "gray80", lty = "dotted")

# Correlation 2
cor_price_sales <- cor(Balaji$price_per_unit, Balaji$sales_value,
                       use = "complete.obs")
cor_price_sales


#Explanation:
#Shows relationship between units sold and sales value.
#Shows relationship between price per unit and sales value.
#Correlation values measure the strength of these relationships.


#---------------------------------------------------------------------
#C-Probability Analysis
#---------------------------------------------------------------------


# Assume Sales Value follows Normal Distribution
mean_sales <- mean(Balaji$sales_value)
sd_sales <- sd(Balaji$sales_value)

# Probability Sales Value > 500
1 - pnorm(500, mean = mean_sales, sd = sd_sales)

# Probability Sales Value between 200 and 600
pnorm(600, mean = mean_sales, sd = sd_sales) -
  pnorm(200, mean = mean_sales, sd = sd_sales)


#Explanation:
#Mean and standard deviation are used to model sales value under a Normal distribution.
#Probabilities estimate the chance of high-value transactions.


#---------------------------------------------------------------------
#D- Probability Visualization
#---------------------------------------------------------------------


x <- seq(min(Balaji$sales_value), max(Balaji$sales_value), length = 100)
y <- dnorm(x, mean = mean_sales, sd = sd_sales)

plot(
  x,
  y,
  type = "l",
  col = "darkblue",
  main = "Normal Distribution of Sales Value",
  xlab = "Sales Value",
  ylab = "Density"
)

abline(v = 500, col = "red", lwd = 2)


#Explanation:
#A Normal distribution curve is plotted to visualize probability density.
#A vertical reference line highlights the probability threshold value.
