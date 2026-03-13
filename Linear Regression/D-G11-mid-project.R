# ===================================================================
#                  MID-TERM DATA SCIENCE PROJECT 

# Section & Group:[{SECTION-D & GROUP NUMBER: 11]

# Dataset_Link: https://www.kaggle.com/datasets/mirichoi0218/insurance
              
#             Dataset_Name: insurance.csv
# ===================================================================

# --------------------- Install & Load Libraries ---------------------
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")

library(dplyr)
library(ggplot2)
library(corrplot)

cat("All ibraries loaded successfully!\n")

# --------------------- A. DATA UNDERSTANDING ---------------------

# mydata <- read.csv("https://drive.google.com/uc?id=YOUR_ACTUAL_FILE_ID_HERE&export=download",header = TRUE, stringsAsFactors = FALSE)

mydata <- read.csv("C:/Users/My PC/Downloads/Rafi/insurance.csv",header = TRUE, stringsAsFactors = FALSE)

# A2. First few rows
head(mydata, 10)

# Original Dataset doesn't any missing values & outliers.
# Intentionally create some missing values & outliers.

set.seed(123)
mydata$age[sample(1:nrow(mydata), 8)] <- NA
mydata$bmi[sample(1:nrow(mydata), 5)] <- NA
mydata$charges[sample(1:nrow(mydata), 3)] <- NA
mydata$bmi[sample(1:nrow(mydata), 4)] <- c(60, 70, 55, 80)  # unrealistic high BMI
cat("Intentionally added missing values and outliers for preprocessing demonstration.\n")

# A3. Shape
cat("Dataset Shape:", nrow(mydata), "rows ×", ncol(mydata), "columns\n")

# A4. Data types
str(mydata)
cat("\nCategorical: sex, smoker, region\n")
cat("Numerical: age, bmi, children, charges\n")

# A5. Descriptive statistics + skewness (manual, no extra package)
summary(mydata)
num_cols <- c("age", "bmi", "children", "charges")

# Manual skewness function (base R)
manual_skewness <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  sum((x - m)^3) / (n * s^3)
}

skew_values <- sapply(mydata[num_cols], manual_skewness)
cat("\nSkewness of Numerical Columns:\n")
print(skew_values)

# --------------------- B. DATA EXPLORATION & VISUALIZATION ---------------------

# Univariate
ggplot(mydata, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "steelblue") +
  ggtitle("Distribution of Age") + theme_minimal()

ggplot(mydata, aes(x = bmi)) + 
  geom_histogram(binwidth = 2, fill = "orange") +
  ggtitle("Distribution of BMI") + theme_minimal()

ggplot(mydata, aes(x = charges)) + 
  geom_histogram(binwidth = 2000, fill = "purple") +
  ggtitle("Distribution of Charges") + theme_minimal()

boxplot(mydata$age, main = "Boxplot - Age", col = "lightblue")
boxplot(mydata$bmi, main = "Boxplot - BMI", col = "lightgreen")
boxplot(mydata$charges, main = "Boxplot - Charges", col = "red")

# Categorical frequency
ggplot(mydata, aes(x = sex)) + geom_bar(fill = "pink") + ggtitle("Gender")
ggplot(mydata, aes(x = smoker)) + geom_bar(fill = "red") + ggtitle("Smoker")
ggplot(mydata, aes(x = region)) + geom_bar(fill = "darkgreen") + ggtitle("Region")

# Bivariate
num_data <- mydata[, num_cols]
cor_matrix <- cor(num_data, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.8, addCoef.col = "black")

ggplot(mydata, aes(x = age, y = charges, color = smoker)) + 
  geom_point(alpha = 0.7) + ggtitle("Age vs Charges")

ggplot(mydata, aes(x = bmi, y = charges, color = smoker)) + 
  geom_point(alpha = 0.7) + ggtitle("BMI vs Charges")

ggplot(mydata, aes(x = smoker, y = charges, fill = smoker)) + 
  geom_boxplot() + ggtitle("Smoker vs Charges")

ggplot(mydata, aes(x = region, y = charges, fill = region)) + 
  geom_boxplot() + ggtitle("Region vs Charges")

# --------------------- C. DATA PREPROCESSING ---------------------

# C1. Missing Values
print(colSums(is.na(mydata)))

mydata$age[is.na(mydata$age)] <- median(mydata$age, na.rm = TRUE)
mydata$bmi[is.na(mydata$bmi)] <- median(mydata$bmi, na.rm = TRUE)
mydata$children[is.na(mydata$children)] <- median(mydata$children, na.rm = TRUE)
mydata$charges[is.na(mydata$charges)] <- median(mydata$charges, na.rm = TRUE)

get_mode <- function(x) {
  uniqx <- unique(x[!is.na(x)])
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
mydata$sex[is.na(mydata$sex)] <- get_mode(mydata$sex)
mydata$smoker[is.na(mydata$smoker)] <- get_mode(mydata$smoker)
mydata$region[is.na(mydata$region)] <- get_mode(mydata$region)

print(colSums(is.na(mydata)))

# C2. Outliers (IQR capping)
cap_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- IQR(x, na.rm = TRUE)
  x[x < Q1 - 1.5*IQR_val] <- Q1 - 1.5*IQR_val
  x[x > Q3 + 1.5*IQR_val] <- Q3 + 1.5*IQR_val
  x
}

mydata$age <- cap_outliers(mydata$age)
mydata$bmi <- cap_outliers(mydata$bmi)
mydata$charges <- cap_outliers(mydata$charges)

mydata$bmi[mydata$bmi > 50] <- 50
mydata$charges[mydata$charges < 100] <- median(mydata$charges)

# C3. Encoding
mydata$sex <- as.factor(mydata$sex)
mydata$smoker <- as.factor(mydata$smoker)
mydata$region <- as.factor(mydata$region)

# C4. Transformation
mydata_scaled <- mydata
mydata_scaled[, num_cols] <- scale(mydata[, num_cols])
mydata$charges_log <- log(mydata$charges + 1)

# C5. Feature Selection
cor_with_charges <- cor(mydata[, num_cols], use = "complete.obs")["charges", ]
print(cor_with_charges)

# --------------------- Save & Finish ---------------------
write.csv(mydata, "cleaned_insurance.csv", row.names = FALSE)
