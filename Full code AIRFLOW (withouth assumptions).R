# Load necessary libraries
library(lme4)     # For fitting mixed models
library(MASS)     # For glmer.nb
library(DHARMa)   # For residual diagnostics
library(emmeans)  # For post hoc tests
library(readxl)   # For reading Excel files
library(lmerTest) # For enhanced summaries and ANOVA with Kenward-Roger
library(ggplot2)  # For plotting

# Load the data from the Excel file
file_path <- "C:/Users/Administrator/Documents/Sample text"
data <- readxl::read_excel(file_path, sheet = 1)

# Ensure Treatment is a factor and DPF is numeric
data$Treatment <- as.factor(data$Treatment)
data$DPF <- as.numeric(data$DPF)

# Calculate the average arm length from the relevant columns in the dataset
data$Average_Arm_Length <- rowMeans(data[, grep("^Arm length", names(data))], na.rm = TRUE)

# Apply a square root transformation to meet the homogeneity of variances assumption
data$Transformed_Arm_Length <- sqrt(data$Average_Arm_Length)

# Fit a Linear Mixed Model (LMM) for Transformed Arm Length
lmm_arm_length <- lmer(Transformed_Arm_Length ~ Treatment * DPF + (1 | Vessel), data = data)

# Output the summary of the model using Kenward-Roger for degrees of freedom
summary(lmm_arm_length, ddf = "Kenward-Roger")

# Diagnostic plots for the LMM to check for any issues in model fitting
par(mfrow=c(2,2))
plot(residuals(lmm_arm_length) ~ fitted(lmm_arm_length), main="Residuals vs Fitted")
qqnorm(residuals(lmm_arm_length), main="Normal Q-Q")
qqline(residuals(lmm_arm_length))
hist(residuals(lmm_arm_length), breaks=30, main="Histogram of Residuals")

# Estimate marginal means for treatments, adjust for multiple comparisons using Tukey's method
em_results_arm <- emmeans(lmm_arm_length, pairwise ~ Treatment, adjust = "tukey", ddf = "Kenward-Roger")
print(summary(em_results_arm))

# Estimate pairwise comparisons of treatment effects, providing detailed output
pairwise_comparisons_arm <- emmeans(lmm_arm_length, ~ Treatment, adjust = "tukey", ddf = "Kenward-Roger")
print(pairwise_comparisons_arm)
print(summary(pairwise_comparisons_arm, infer = c(TRUE, TRUE), adjust = "tukey"))

# Output summary including ANOVA to get F-statistics using Kenward-Roger
anova(lmm_arm_length, ddf = "Kenward-Roger")  # This will provide an F-test for each fixed effect using Kenward-Roger

# Predict transformed arm length from the model
data$predicted_arm_length <- predict(lmm_arm_length, type = "response")

# Interaction plot
ggplot(data, aes(x = DPF, y = predicted_arm_length, group = Treatment, color = Treatment)) +
  geom_line(size = 1) +
  labs(title = "Interaction of DPF and Treatment on Transformed Arm Length",
       x = "Days Post Fertilization (DPF)",
       y = "Predicted Transformed Arm Length") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("3000" = "#1b9e77", "4000" = "#d95f02", "5000" = "#7570b3", "Shaker" = "#e7298a"))


# Fit GLMM for Damage Signs using Negative Binomial distribution
glmm_damage_signs <- glmer.nb(`Damage signs` ~ Treatment * DPF + (1 | Vessel), 
                              data = data, 
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Output the summary of the model using Kenward-Roger
summary(glmm_damage_signs, ddf = "Kenward-Roger")

# Diagnostic plots
par(mfrow=c(2,2))
plot(fitted(glmm_damage_signs), residuals(glmm_damage_signs), main="Fitted vs. Residuals")
qqnorm(residuals(glmm_damage_signs))
qqline(residuals(glmm_damage_signs))
hist(residuals(glmm_damage_signs), breaks=30)

# Estimate marginal means for treatments, adjust for multiple comparisons using Tukey's method
em_results_damage <- emmeans(glmm_damage_signs, pairwise ~ Treatment, adjust = "tukey", ddf = "Kenward-Roger")
print(summary(em_results_damage))

# Output summary including ANOVA to get F-statistics
anova(glmm_damage_signs, ddf = "Kenward-Roger")

# Predict damage signs
data$predicted_damage_signs <- predict(glmm_damage_signs, type = "response")

# Interaction plot
ggplot(data, aes(x = DPF, y = predicted_damage_signs, group = Treatment, color = Treatment)) +
  geom_line(size = 1) +
  labs(title = "Interaction of DPF and Treatment on Damage Signs",
       x = "Days Post Fertilization (DPF)",
       y = "Predicted Damage Signs") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("3000" = "#1b9e77", "4000" = "#d95f02", "5000" = "#7570b3", "Shaker" = "#e7298a"))

# Fit GLMM for Competency Signs using Negative Binomial distribution
glmm_competency_signs <- glmer.nb(`Competency signs` ~ Treatment * DPF + (1 | Vessel), 
                                  data = data, 
                                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))

# Output the summary of the model using Kenward-Roger
summary(glmm_competency_signs, ddf = "Kenward-Roger")

# Diagnostic plots
par(mfrow=c(2,2))
plot(fitted(glmm_competency_signs), residuals(glmm_competency_signs), main="Fitted vs. Residuals")
qqnorm(residuals(glmm_competency_signs))
qqline(residuals(glmm_competency_signs))
hist(residuals(glmm_competency_signs), breaks=30)

# Estimate marginal means for treatments, adjust for multiple comparisons using Tukey's method
em_results_competency <- emmeans(glmm_competency_signs, pairwise ~ Treatment, adjust = "tukey", ddf = "Kenward-Roger")
print(summary(em_results_competency))

# Output summary including ANOVA to get F-statistics
anova(glmm_competency_signs, ddf = "Kenward-Roger")

# Predict competency signs
data$predicted_competency <- predict(glmm_competency_signs, type = "response")

# Interaction plot 
ggplot(data, aes(x = DPF, y = predicted_competency, group = Treatment, color = Treatment)) +
  geom_line(size = 1) +
  labs(title = "Interaction of DPF and Treatment on Competency Signs",
       x = "Days Post Fertilization (DPF)",
       y = "Predicted Competency Signs") +
  theme_minimal() +
  theme(legend.position = "right") +  # Shows the legend on the right
  scale_color_manual(values = c("3000" = "#1b9e77", "4000" = "#d95f02", "5000" = "#7570b3", "Shaker" = "#e7298a"))


# Calculate the average body length and width from the relevant columns in the dataset
data$Average_Body_Length <- rowMeans(data[, grep("^Body length", names(data))], na.rm = TRUE)
data$Average_Body_Width <- rowMeans(data[, grep("^Body width", names(data))], na.rm = TRUE)

# Calculate the average body surface area using the formula for the area of an ellipse
data$Average_Body_Surface <- pi * (data$Average_Body_Length / 2) * (data$Average_Body_Width / 2)

# Fit a Gamma GLMM for Average Body Surface with a log link, including random effects for Vessel
glmm_body_surface <- glmer(Average_Body_Surface ~ Treatment * DPF + (1 | Vessel), 
                           family = Gamma(link = "log"), data = data)

# Output the summary of the model to review fixed and random effects, and overall model fit
summary(glmm_body_surface)

# Plot the residuals to diagnose potential issues with model fit such as non-linearity or heteroscedasticity
residuals_body_surface <- residuals(glmm_body_surface)
plot(residuals_body_surface, main="Residuals of Gamma GLMM for Average Body Surface")

# Check if the model converged successfully by examining the optimizer's return code
if (summary(glmm_body_surface)$optinfo$conv$opt == 0) {
  cat("Model converged successfully.\n")
} else {
  cat("Model did not converge. Check the optimization details.\n")
}

# Create additional diagnostic plots for a deeper investigation into the model's performance
par(mfrow=c(2,2))
plot(fitted(glmm_body_surface), residuals_body_surface, main="Fitted vs Residuals")
qqnorm(residuals_body_surface)
qqline(residuals_body_surface)
hist(residuals_body_surface, breaks=30, main="Histogram of Residuals")

# Estimate marginal means for treatments, adjust for multiple comparisons using Tukey's method
em_results <- emmeans(glmm_body_surface, pairwise ~ Treatment, adjust = "tukey")
print(summary(em_results))

# Estimate pairwise comparisons of treatment effects, providing detailed output
pairwise_comparisons <- emmeans(glmm_body_surface, ~ Treatment, adjust = "tukey")
print(pairwise_comparisons)
print(summary(pairwise_comparisons, infer = c(TRUE, TRUE), adjust = "tukey"))

# Output summary including ANOVA to get F-statistics
anova(glmm_body_surface, ddf = "Kenward-Roger")

# Predict body surface
data$predicted_body_surface <- predict(glmm_body_surface, type = "response")

# Interaction plot 
ggplot(data, aes(x = DPF, y = predicted_body_surface, group = Treatment, color = Treatment)) +
  geom_line(size = 1) +
  labs(title = "Interaction of DPF and treatment on average body surface",
       x = "Days post fertilization",
       y = "Predicted average body surface") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("3000" = "#1b9e77", "4000" = "#d95f02", "5000" = "#7570b3", "Shaker" = "#e7298a"))
