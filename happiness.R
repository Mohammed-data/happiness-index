# 📦 Load Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tseries)
library(urca)
library(vars)
library(corrplot)

# 📂 Step 1: Load Data
happiness_data <- read_excel("~/Downloads/happiness RStudio.xlsx",
                             col_types = c("date", rep("numeric", 8)))
colnames(happiness_data) <- c("Date", "Spiritual", "Mental", "Emotional",
                              "Social", "Physical", "Productivity",
                              "Internal_Battle", "Happiness")

# 🧼 Step 2: Remove Date and Prepare Data
data <- dplyr::select(happiness_RStudio, -Date)


# 📊 Step 3: Visualize Trends (Optional but Insightful)
data_long <- tidyr::pivot_longer(data, cols = everything(), names_to = "Variable", values_to = "Value")
ggplot(data_long, aes(x = rep(1:(nrow(data)), length(unique(data_long$Variable))),
                      y = Value, color = Variable)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time Series Plots of Each Variable")

# 🔍 Step 4: Correlation Matrix
corrplot(cor(data), method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# 🧪 Step 5: Check for Stationarity (ADF test)
adf_results <- lapply(data, function(x) adf.test(x))
names(adf_results) <- colnames(data)
adf_results  # You’ll get p-values. If p < 0.05 → stationary.

# ⛓️ Step 6: Make Stationary (If needed, apply differencing)
data_diff <- data.frame(apply(data, 2, diff))
data_diff <- na.omit(data_diff)  # Remove first NA row

# 📈 Step 7: Fit VAR Model (Lag = 3)
var_model <- VAR(data_diff, p = 3, type = "const")

# 📣 Step 8: Summary of Happiness Equation
summary(var_model)$varresult$Happiness
