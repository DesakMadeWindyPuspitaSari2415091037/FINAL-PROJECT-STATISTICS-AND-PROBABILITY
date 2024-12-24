# Name: Desak Made Windy Puspita Sari / Student ID: 2415091037 / Class: 1IKI

# Install Packages if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readxl")) install.packages("readxl")
if (!require("car")) install.packages("car")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

# Load Libraries
library(ggplot2) 
library(readxl)
library(car)
library(dplyr)
library(tidyr)

# Import Excel Data
crime_data <- read_excel("D:/MATKUL STATPROB/Banyaknya Tindak Pidana Menonjol Menurut Jenisnya di Provinsi Bali, 2021-2023.xlsx", skip = 2)

# Rename columns and clean up data
data_clean <- crime_data |> 
  rename(`Type of Crime` = `...1`) |> 
  select(`Type of Crime`, `2021`, `2022`, `2023`)

# Reshape data to long format
data_long <- pivot_longer(data_clean, 
                          cols = c(`2021`, `2022`, `2023`), 
                          names_to = "Year", 
                          values_to = "Count") |> 
  filter(!is.na(Count))

# Convert columns to appropriate data types
data_long$Year <- as.factor(data_long$Year)
data_long$Count <- as.numeric(data_long$Count)

# Normality Test (Shapiro-Wilk)
normality <- shapiro.test(residuals(aov(Count ~ Year, data = data_long)))
print("Normality Test Result:")
print(normality)

# Homogeneity Test (Levene's Test)
homogeneity <- leveneTest(Count ~ Year, data = data_long)
print("Homogeneity Test Result:")
print(homogeneity)

# Outlier Detection with Boxplot
outlier_detection <- boxplot(data_long$Count, plot = FALSE)
outlier_indices <- which(data_long$Count %in% outlier_detection$out)
if (length(outlier_indices) > 0) {
  print("Outliers detected at the following indices:")
  print(outlier_indices)
} else {
  print("No outliers detected.")
}

# Kruskal-Wallis Test (Alternative to ANOVA for non-normal data)
kruskal_test <- kruskal.test(Count ~ Year, data = data_long)
print("Kruskal-Wallis Test Result:")
print(kruskal_test)

# Post-Hoc Test (If Kruskal-Wallis is significant, we can use pairwise Wilcoxon test)
if(kruskal_test$p.value < 0.05) {
  posthoc <- pairwise.wilcox.test(data_long$Count, data_long$Year, p.adjust.method = "bonferroni")
  print("Post-Hoc Wilcoxon Test Result:")
  print(posthoc)
} else {
  print("No need for post-hoc testing as Kruskal-Wallis is not significant.")
}

# **Bar Chart** (Visualization)
plot <- ggplot(data_long, aes(x = `Type of Crime`, y = Count, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Number of Crimes in Bali (2021-2023)",
       x = "Type of Crime",
       y = "Number of Cases") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.text.y = element_text(size = 12)
  ) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) + 
  scale_fill_brewer(palette = "Blues")

# Display plot in RStudio
print(plot)

# Save plot as a PNG file
ggsave("Crime_Bali_BarChart.png", plot = plot, width = 10, height = 7)

# Final Output
print("Kruskal-Wallis Analysis and Bar Chart Visualization Completed.")



