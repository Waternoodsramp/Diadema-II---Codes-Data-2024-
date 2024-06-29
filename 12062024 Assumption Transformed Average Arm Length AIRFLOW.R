# Load necessary libraries
library(readxl)
library(car)

# Load the data from the Excel file
file_path <- "C:/Users/Administrator/Documents/Airflow speeldoos STRIP.xlsx"
data <- read_excel(file_path, sheet = 1)

# Calculate the average arm length for each row
data$Average_Arm_Length <- rowMeans(data[, grep("^Arm length", names(data))], na.rm = TRUE)

# Ensure Treatment is a factor and DPF is numeric
data$Treatment <- as.factor(data$Treatment)
data$DPF <- as.numeric(data$DPF)

# Transform Average Arm Length
data$Transformed_Arm_Length <- sqrt(data$Average_Arm_Length)

# Fit linear model for transformed variable
lm_transformed_arm_length <- lm(Transformed_Arm_Length ~ Treatment * DPF, data = data)

# Levene's test for homogeneity of variances
levene_transformed_arm_length <- leveneTest(Transformed_Arm_Length ~ Treatment, data = data)
print(levene_transformed_arm_length)
