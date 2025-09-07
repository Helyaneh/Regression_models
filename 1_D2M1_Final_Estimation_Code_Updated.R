# Install and load required libraries
library(readxl)
library(ggplot2)
library(patchwork)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(lmtest)
library(stats)
library(tidyverse)
library(tidyr)
library(stargazer)
library(pscl)
library(MASS)
library(officer)
library(flextable)
library(knitr)
library(VGAM)

# Load dataset
df <- read_excel("AUT_dataset_FE.xlsx")
df <- na.omit(df)

# Label variables
var_label(df) <- list(
  Q1 = "NUTS1 group of states",
  Q2 = "NUTS2 federal states (Bundesland)",
  Q3 = "NUTS3 group of districts",
  Q4 = "LAU name (national)",
  Q5 = "CWS in LAU (yes/no)",
  Q6 = "Number of CWS in LAU",
  Q7 = "Rural area dummy (DEGURBA level 1, GHS DUC)",
  Q8 = "Towns and suburbs dummy (DEGURBA level 1, GHS DUC)",
  Q9 = "Population density of municipalities 2020 (per km², Rural Observatory, log)",
  Q10 = "Population density of municipalities 2020 (per km², Rural Observatory)",
  Q11 = "Average download speed LAU fixed network 2022 (Mbps, Rural Observatory, log)",
  Q12 = "Average download speed LAU fixed networkhane 2022 (Mbps, Rural Observatory)",
  Q13 = "Average download speed of mobile network in LAU 2022 (Mbps, Rural Observatory, log)",
  Q14 = "Average download speed of mobile network in LAU 2022 (Mbps, Rural Observatory)",
  Q15 = "Average distance to train stations in LAU 2018 (meters, Rural Observatory, log)",
  Q16 = "Average distance to train stations in LAU 2018 (meters, Rural Observatory)",
  Q17 = "Municipalities in FUA (OECD 2022, dummy)",
  Q18 = "Employment rate 15-64 years in LAU 2021 (Statistics Austria, log)",
  Q19 = "Employment rate 15-64 years in LAU 2021 (Statistics Austria)",
  Q20 = "Tourism capacity, number of rooms (Rural Observatory 2021, log)",
  Q21 = "Tourism capacity, number of rooms (Rural Observatory 2021)",
  Q22 = "Number of libraries in LAU (log)",
  Q23 = "Number of libraries in LAU",
  Q24 = "Number of cafe/bar/pubs in LAU (log)",
  Q25 = "Number of cafe/bar/pubs in LAU",
  Q26 = "Tertiary education of 25-64 years in LAU (percentage, 2022, Statistics Austria, log)",
  Q27 = "Tertiary education of 25-64 years in LAU (percentage, 2022, Statistics Austria)",
  Q28 = "LAU inside Innovation Leader region (2022-2023, RIS)",
  Q29 = "LAU inside Strong Innovation region (2022-2023, RIS)",
  Q30 = "Employed persons in tertiary sectors in districts 2022 (Statistics Austria, log)",
  Q31 = "Employed persons in tertiary sectors in districts 2022 (Statistics Austria)",
  Q32 = "Inner Alpine dummy",
  Q33 = "Pre-Alpine dummy",
  Q34 = "Pre-Alpine dummy"
)

# Function to create Spearman correlation matrix heatmap
create_correlation_heatmap <- function(data, vars, model_name) {
  corr_matrix <- cor(data[, vars], method = "spearman", use = "complete.obs")
  p <- ggcorrplot(corr_matrix, method = "square", type = "upper", lab = TRUE, 
                  lab_size = 3, colors = c("red", "white", "blue"),
                  title = paste("Spearman Correlation Heatmap -", model_name))
  ggsave(paste0("corr_heatmap_", tolower(gsub(" ", "_", model_name)), ".png"), p, width = 8, height = 6)
  
  high_corr_pairs <- which(abs(corr_matrix) > 0.7 & lower.tri(corr_matrix), arr.ind = TRUE)
  pairs_list <- if (nrow(high_corr_pairs) > 0) {
    paste0(rownames(corr_matrix)[high_corr_pairs[, 1]], " - ",
           colnames(corr_matrix)[high_corr_pairs[, 2]], ": ",
           round(corr_matrix[high_corr_pairs], 3))
  } else {
    "No pairs with |ρ| > 0.7"
  }
  return(list(model = model_name, pairs = pairs_list))
}

# Function to create density heatmap
create_density_heatmap <- function(data, x_var, y_var, model_name) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_bin2d(bins = 30) +
    scale_fill_continuous(type = "viridis") +
    labs(title = paste("Density Heatmap -", model_name, "(", x_var, "vs", y_var, ")"),
         x = x_var, y = y_var) +
    theme_minimal()
  ggsave(paste0("density_heatmap_", tolower(gsub(" ", "_", model_name)), "_", x_var, "_", y_var, ".png"), p, width = 6, height = 5)
}

# Function to create contour plot
create_contour_plot <- function(data, x_var, y_var, model_name) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_density_2d() +
    labs(title = paste("Contour Plot -", model_name, "(", x_var, "vs", y_var, ")"),
         x = x_var, y = y_var) +
    theme_minimal()
  ggsave(paste0("contour_", tolower(gsub(" ", "_", model_name)), "_", x_var, "_", y_var, ".png"), p, width = 6, height = 5)
}

# Function to create density plots
create_density_plots <- function(data, vars, model_name) {
  df_long <- pivot_longer(data, cols = vars, names_to = "Variable", values_to = "Value")
  p <- ggplot(df_long, aes(x = Value, fill = Variable)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ Variable, scales = "free", ncol = 2) +
    labs(title = paste("Density Plots -", model_name),
         x = "Value", y = "Density") +
    theme_minimal() +
    theme(legend.position = "none")
  ggsave(paste0("density_", tolower(gsub(" ", "_", model_name)), ".png"), p, width = 10, height = 8)
}

# Define model variables (including Q34)
model1_vars <- c("Q5", "Q7", "Q15", "Q18", "Q20", "Q26", "Q29", "Q30", "Q34")
model2_vars <- c("Q5", "Q9", "Q13", "Q17", "Q11", "Q18", "Q26", "Q30", "Q34")
model3_vars <- c("Q5", "Q15", "Q17", "Q11", "Q18", "Q24", "Q26", "Q29", "Q30", "Q32", "Q34")

# Create plots for each model
result1 <- create_correlation_heatmap(df, model1_vars, "Model 1")
create_density_plots(df, model1_vars, "Model 1")
create_density_heatmap(df, "Q15", "Q18", "Model 1")
create_contour_plot(df, "Q15", "Q18", "Model 1")

result2 <- create_correlation_heatmap(df, model2_vars, "Model 2")
create_density_plots(df, model2_vars, "Model 2")
create_density_heatmap(df, "Q9", "Q18", "Model 2")
create_contour_plot(df, "Q9", "Q18", "Model 2")

result3 <- create_correlation_heatmap(df, model3_vars, "Model 3")
create_density_plots(df, model3_vars, "Model 3")
create_density_heatmap(df, "Q15", "Q18", "Model 3")
create_contour_plot(df, "Q15", "Q18", "Model 3")

# Save correlation pairs
sink("correlation_pairs.txt")
cat("Highly Correlated Pairs (|ρ| > 0.7):\n\n")
cat("Model 1:\n", paste(result1$pairs, collapse = "\n"), "\n\n")
cat("Model 2:\n", paste(result2$pairs, collapse = "\n"), "\n\n")
cat("Model 3:\n", paste(result3$pairs, collapse = "\n"), "\n\n")
sink()

# Regression Models
# Model 1: ZIP, ZINB, Logistic
zip_model1 <- zeroinfl(Q6 ~ Q7 + Q15 + Q18 + Q20 + Q26 + Q29 + Q30 + Q34 | 1, data = df, dist = "poisson")
zinb_model1 <- zeroinfl(Q6 ~ Q7 + Q15 + Q18 + Q20 + Q26 + Q29 + Q30 + Q34 | 1, data = df, dist = "negbin")
logistic_model1 <- glm(Q5 ~ Q7 + Q15 + Q18 + Q20 + Q26 + Q29 + Q30 + Q34, family = binomial, data = df)

# Model 2: ZIP, ZINB, Logistic
zip_model2 <- zeroinfl(Q6 ~ Q9 + Q13 + Q17 + Q11 + Q18 + Q26 + Q30 + Q34 | 1, data = df, dist = "poisson")
zinb_model2 <- zeroinfl(Q6 ~ Q9 + Q13 + Q17 + Q11 + Q18 + Q26 + Q30 + Q34 | 1, data = df, dist = "negbin")
logistic_model2 <- glm(Q5 ~ Q9 + Q13 + Q17 + Q11 + Q18 + Q26 + Q30 + Q34, family = binomial, data = df)

# Model 3: ZIP, ZINB, Logistic
zip_model3 <- zeroinfl(Q6 ~ Q15 + Q17 + Q11 + Q18 + Q24 + Q26 + Q29 + Q30 + Q32 + Q34 | 1, data = df, dist = "poisson")
zinb_model3 <- zeroinfl(Q6 ~ Q15 + Q17 + Q11 + Q18 + Q24 + Q26 + Q29 + Q30 + Q32 + Q34 | 1, data = df, dist = "negbin")
logistic_model3 <- glm(Q5 ~ Q15 + Q17 + Q11 + Q18 + Q24 + Q26 + Q29 + Q30 + Q32 + Q34, family = binomial, data = df)

# Extract AIC and BIC
model_comparison <- data.frame(
  Model = c("ZIP Model 1", "ZINB Model 1", "Logistic Model 1",
            "ZIP Model 2", "ZINB Model 2", "Logistic Model 2",
            "ZIP Model 3", "ZINB Model 3", "Logistic Model 3"),
  AIC = c(AIC(zip_model1), AIC(zinb_model1), AIC(logistic_model1),
          AIC(zip_model2), AIC(zinb_model2), AIC(logistic_model2),
          AIC(zip_model3), AIC(zinb_model3), AIC(logistic_model3)),
  BIC = c(BIC(zip_model1), BIC(zinb_model1), BIC(logistic_model1),
          BIC(zip_model2), BIC(zinb_model2), BIC(logistic_model2),
          BIC(zip_model3), BIC(zinb_model3), BIC(logistic_model3))
)

# Vuong Tests
vuong_zip_zinb1 <- vuong(zip_model1, zinb_model1)
vuong_zip_logistic1 <- vuong(zip_model1, logistic_model1)
vuong_zinb_logistic1 <- vuong(zinb_model1, logistic_model1)

vuong_zip_zinb2 <- vuong(zip_model2, zinb_model2)
vuong_zip_logistic2 <- vuong(zip_model2, logistic_model2)
vuong_zinb_logistic2 <- vuong(zinb_model2, logistic_model2)

vuong_zip_zinb3 <- vuong(zip_model3, zinb_model3)
vuong_zip_logistic3 <- vuong(zip_model3, logistic_model3)
vuong_zinb_logistic3 <- vuong(zinb_model3, logistic_model3)

# Output regression results
stargazer(zip_model1, zinb_model1, logistic_model1,
          type = "html",
          title = "Regression Models - Model 1",
          dep.var.labels = c("Number of CWS (Q6)", "CWS Yes/No (Q5)"),
          covariate.labels = c("Rural Area Dummy (Q7)", "Log Distance to Train Stations (Q15)",
                               "Log Employment Rate (Q18)", "Log Tourism Capacity (Q20)",
                               "Log Tertiary Education (Q26)", "Strong Innovation Region (Q29)",
                               "Log Employed Persons (Q30)", "Pre-Alpine Dummy (Q34)"),
          out = "model1_results.html")

stargazer(zip_model2, zinb_model2, logistic_model2,
          type = "html",
          title = "Regression Models - Model 2",
          dep.var.labels = c("Number of CWS (Q6)", "CWS Yes/No (Q5)"),
          covariate.labels = c("Log Population Density (Q9)", "Log Mobile Network Speed (Q13)",
                               "Municipalities in FUA (Q17)", "Log Fixed Network Speed (Q11)",
                               "Log Employment Rate (Q18)", "Log Tertiary Education (Q26)",
                               "Log Employed Persons (Q30)", "Pre-Alpine Dummy (Q34)"),
          out = "model2_results.html")

stargazer(zip_model3, zinb_model3, logistic_model3,
          type = "html",
          title = "Regression Models - Model 3",
          dep.var.labels = c("Number of CWS (Q6)", "CWS Yes/No (Q5)"),
          covariate.labels = c("Log Distance to Train Stations (Q15)", "Municipalities in FUA (Q17)",
                               "Log Fixed Network Speed (Q11)", "Log Employment Rate (Q18)",
                               "Log Number of Cafes/Pubs (Q24)", "Log Tertiary Education (Q26)",
                               "Strong Innovation Region (Q29)", "Log Employed Persons (Q30)",
                               "Inner Alpine Dummy (Q32)", "Pre-Alpine Dummy (Q34)"),
          out = "model3_results.html")

# Output AIC/BIC comparison
sink("model_comparison.html")
cat("<h2>Model Comparison: AIC and BIC</h2>\n")
kable(model_comparison, format = "html", caption = "AIC and BIC for All Models")
sink()

# Output Vuong test results
sink("vuong_tests.html")
cat("<h2>Vuong Test Results</h2>\n")
cat("<h3>Model 1</h3>\n")
cat("ZIP vs ZINB:\n"); print(vuong_zip_zinb1)
cat("ZIP vs Logistic:\n"); print(vuong_zip_logistic1)
cat("ZINB vs Logistic:\n"); print(vuong_zinb_logistic1)
cat("<h3>Model 2</h3>\n")
cat("ZIP vs ZINB:\n"); print(vuong_zip_zinb2)
cat("ZIP vs Logistic:\n"); print(vuong_zip_logistic2)
cat("ZINB vs Logistic:\n"); print(vuong_zinb_logistic2)
cat("<h3>Model 3</h3>\n")
cat("ZIP vs ZINB:\n"); print(vuong_zip_zinb3)
cat("ZIP vs Logistic:\n"); print(vuong_zip_logistic3)
cat("ZINB vs Logistic:\n"); print(vuong_zinb_logistic3)
sink()

# Print output instructions
cat("Outputs:
- Correlation heatmaps saved as: 'corr_heatmap_model_*.png'
- Density plots saved as: 'density_model_*.png'
- Density heatmaps saved as: 'density_heatmap_model_*_*.png'
- Contour plots saved as: 'contour_model_*_*.png'
- Correlation pairs saved to: 'correlation_pairs.txt'
- Regression results saved as: 'model*_results.html'
- Model comparison saved as: 'model_comparison.html'
- Vuong test results saved as: 'vuong_tests.html'\n")