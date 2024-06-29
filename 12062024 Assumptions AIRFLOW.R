# Load necessary libraries
library(readxl)
library(car)

# Load the data from the Excel file
file_path <- "C:/Users/Administrator/Documents/Airflow speeldoos STRIP.xlsx"
data <- read_excel(file_path, sheet = 1)

# Calculate the average arm length for each row
data$Average_Arm_Length <- rowMeans(data[, grep("^Arm length", names(data))], na.rm = TRUE)

# Calculate the average body length and width for each row
data$Average_Body_Length <- rowMeans(data[, grep("^Body length", names(data))], na.rm = TRUE)
data$Average_Body_Width <- rowMeans(data[, grep("^Body width", names(data))], na.rm = TRUE)

# Calculate the body surface area using the formula: pi * (Average_Body_Length / 2) * (Average_Body_Width / 2)
data$Average_Body_Surface <- pi * (data$Average_Body_Length / 2) * (data$Average_Body_Width / 2)

# Ensure Treatment is a factor
data$Treatment <- as.factor(data$Treatment)
data$DPF <- as.numeric(data$DPF)

# Fit linear models for Average arm length and body surface
lm_arm_length <- lm(Average_Arm_Length ~ Treatment * DPF, data = data)
lm_body_surface <- lm(Average_Body_Surface ~ Treatment * DPF, data = data)

# Shapiro-Wilk test for normality of residuals
shapiro_arm_length <- shapiro.test(residuals(lm_arm_length))
print(shapiro_arm_length)

shapiro_body_surface <- shapiro.test(residuals(lm_body_surface))
print(shapiro_body_surface)

# Levene's test for homogeneity of variances
levene_arm_length <- leveneTest(Average_Arm_Length ~ Treatment, data = data)
print(levene_arm_length)

levene_body_surface <- leveneTest(Average_Body_Surface ~ Treatment, data = data)
print(levene_body_surface)

# Fit linear models for Damage signs and Competency signs
lm_damage_signs <- lm(`Damage signs` ~ Treatment * DPF, data = data)
lm_competency_signs <- lm(`Competency signs` ~ Treatment * DPF, data = data)

# Perform Shapiro-Wilk test for normality on residuals for Damage signs
shapiro_damage <- shapiro.test(residuals(lm_damage_signs))
print(shapiro_damage)

# Perform Shapiro-Wilk test for normality on residuals for Competency signs
shapiro_competency <- shapiro.test(residuals(lm_competency_signs))
print(shapiro_competency)

# Perform Levene's test for homogeneity of variance for Damage signs
levene_damage <- leveneTest(`Damage signs` ~ Treatment, data = data)
print(levene_damage)

# Perform Levene's test for homogeneity of variance for Competency signs
levene_competency <- leveneTest(`Competency signs` ~ Treatment, data = data)
print(levene_competency)
