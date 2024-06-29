library(readxl)
library(car)
library(reshape2)
library(ggplot2)

# Load data
data <- read_excel("C:/Users/Administrator/Desktop/Sample text")

# Convert 'Height' to numeric if necessary (replace commas with dots)
data$Height <- as.numeric(gsub(",", ".", data$Height))

# Convert count columns to numeric
data$`Count 1` <- as.numeric(data$`Count 1`)
data$`Count 2` <- as.numeric(data$`Count 2`)
data$`Count 3` <- as.numeric(data$`Count 3`)
data$`Count 4` <- as.numeric(data$`Count 4`)

# Normality check using Shapiro-Wilk test
shapiro_test_results <- apply(data[, c("Count 1", "Count 2", "Count 3", "Count 4")], 2, shapiro.test)

# Check if the grouping variable needs to be a factor for Levene's Test
data$Height <- as.factor(data$Height)

# Combine counts into a single column for Levene's Test
data_long <- melt(data, id.vars = "Height", measure.vars = c("Count 1", "Count 2", "Count 3", "Count 4"))

# Simplify the Levene's Test model to focus only on heights
levene_test_result <- leveneTest(value ~ Height, data = data_long, center=mean)

# Output the results
print(shapiro_test_results)
print(levene_test_result)

# Conducting ANOVA
anova_results <- aov(value ~ Height, data = data_long)
summary(anova_results)

# Calculate means and standard errors
group_stats <- data_long %>%
  group_by(Height) %>%
  summarise(Mean = mean(value), Std_Error = sd(value)/sqrt(n()))

# Create a bar graph with error bars
ggplot(group_stats, aes(x = as.factor(Height), y = Mean, fill = as.factor(Height))) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - Std_Error, ymax = Mean + Std_Error), width = 0.2, size = 0.7, position = position_dodge(0.7)) +
  labs(title = "Mean Microsphere Counts by Height with Standard Errors",
       x = "Height in centimeters",
       y = "Average microsphere count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")  # For aesthetic coloring

# Print the plot
print(ggplot_object)
