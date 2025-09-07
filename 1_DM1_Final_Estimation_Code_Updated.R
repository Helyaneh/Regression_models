# Install and load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(pscl)
library(MASS)
library(knitr)
library(stargazer)
library(gridExtra)
library(nonnest2)

# Load dataset
df <- read_excel("ITA_dataset_FE.xlsx")
df <- na.omit(df)

# Ensure Q41 is a factor and aggregate rare levels
df$Q41 <- as.factor(df$Q41)
if (length(unique(df$Q41)) < 2) {
  stop("Q41 (NUTS2 Regions) has insufficient variation. Please check the data.")
}
if (length(unique(df$Q41)) > 10) {
  # Aggregate rare levels into "Other"
  freq <- table(df$Q41)
  rare_levels <- names(freq[freq < 10])
  if (length(rare_levels) > 0) {
    df$Q41 <- as.character(df$Q41)
    df$Q41[df$Q41 %in% rare_levels] <- "Other"
    df$Q41 <- as.factor(df$Q41)
    message("Aggregated ", length(rare_levels), " rare Q41 levels into 'Other'.")
  }
}

# Label variables
library(labelled)
var_label(df) <- list(
  Q1 = "Number of CWS in LAU",
  Q2 = "CWS in LAU (Yes/No)",
  Q3 = "Rural area dummy (DEGURBA level 1, GHS DUC)",
  Q5 = "Population density of municipalities 2020 per km² (Rural Observatory, log)",
  Q7 = "Average download speed LAU fixed network 2022 Mbps (Rural Observatory, log)",
  Q9 = "Average download speed of mobile network in LAU 2022 Mbps (Rural Observatory, log)",
  Q13 = "Municipalities in FUA OECD 2022 dummy",
  Q20 = "Tourism capacity number of rooms (Rural Observatory 2021, log)",
  Q21 = "Tourism capacity number of rooms (Rural Observatory 2021)",
  Q24 = "Number of cafe/bar/pubs in LAU (log)",
  Q26 = "Tertiary education in LAU percentage (Openpolis 2021, log)",
  Q28 = "Strong Innovation region 2022-2023 (RIS)",
  Q30 = "Employed persons in selected enterprises from Ateco in NUTS3 2022 (log)",
  Q32 = "Inner Alpine dummy",
  Q41 = "NUTS2 Regions"
)

# Correlation matrix and density heatmap with Q41
create_correlation_heatmap <- function(data, vars, model_name) {
  numeric_vars <- vars[sapply(data[, vars], is.numeric)]
  if (length(numeric_vars) < 2) {
    warning("Not enough numeric variables for correlation in ", model_name)
    return(list(model = model_name, pairs = "Insufficient numeric variables"))
  }
  corr_matrix <- cor(data[, numeric_vars], method = "spearman", use = "complete.obs")
  png(paste0("correlation_", tolower(gsub(" ", "_", model_name)), ".png"), width = 800, height = 800)
  corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
           addCoef.col = "black", tl.col = "black", tl.srt = 45, diag = FALSE,
           title = paste("Spearman Correlation Heatmap -", model_name),
           col = colorRampPalette(c("red", "white", "blue"))(200))
  dev.off()
  
  high_corr_pairs <- which(abs(corr_matrix) > 0.7 & lower.tri(corr_matrix), arr.ind = TRUE)
  pairs_list <- if (nrow(high_corr_pairs) > 0) {
    paste0(rownames(corr_matrix)[high_corr_pairs[, 1]], " - ",
           colnames(corr_matrix)[high_corr_pairs[, 2]], ": ",
           round(corr_matrix[high_corr_pairs], 3))
  } else {
    "No pairs with |ρ| > 0.7"
  }
  
  list(model = model_name, pairs = pairs_list)
}

create_density_heatmap <- function(data, vars, model_name) {
  numeric_vars <- vars[sapply(data[, vars], is.numeric)]
  if (length(numeric_vars) < 2) {
    warning("Not enough numeric variables for density heatmap in ", model_name)
    return(NULL)
  }
  data_subset <- data[, numeric_vars]
  pairs <- combn(numeric_vars, 2, simplify = FALSE)
  plots <- lapply(pairs, function(pair) {
    ggplot(data_subset, aes_string(x = pair[1], y = pair[2])) +
      geom_bin2d(bins = 30) +
      scale_fill_gradientn(colors = c("blue", "white", "red")) +
      labs(title = paste(pair[1], "vs", pair[2]), x = pair[1], y = pair[2]) +
      theme_minimal()
  })
  png(paste0("density_heatmap_", tolower(gsub(" ", "_", model_name)), ".png"), width = 1200, height = 800)
  do.call(grid.arrange, c(plots, ncol = min(length(plots), 3)))
  dev.off()
}

# Define variable sets for models (including Q41)
vars_model1 <- c("Q1", "Q3", "Q20", "Q28", "Q26", "Q30", "Q41")
vars_model2 <- c("Q1", "Q5", "Q9", "Q7", "Q13", "Q28", "Q26", "Q30", "Q41")
vars_model3 <- c("Q1", "Q13", "Q21", "Q7", "Q28", "Q24", "Q26", "Q30", "Q32", "Q41")

# Generate correlation and density heatmaps
result1 <- create_correlation_heatmap(df, vars_model1, "Model_1")
result2 <- create_correlation_heatmap(df, vars_model2, "Model_2")
result3 <- create_correlation_heatmap(df, vars_model3, "Model_3")
create_density_heatmap(df, vars_model1, "Model_1")
create_density_heatmap(df, vars_model2, "Model_2")
create_density_heatmap(df, vars_model3, "Model_3")

# Print and save correlation results
sink("correlation_pairs.txt")
cat("Highly Correlated Pairs (|ρ| > 0.7):\n\n")
cat("Model 1:\n", paste(result1$pairs, collapse = "\n"), "\n\n")
cat("Model 2:\n", paste(result2$pairs, collapse = "\n"), "\n\n")
cat("Model 3:\n", paste(result3$pairs, collapse = "\n"), "\n\n")
sink()

# Function to check model validity
check_model <- function(model, model_name) {
  if (inherits(model, "zeroinfl") || inherits(model, "glm")) {
    if (!model$converged) {
      warning("Model ", model_name, " did not converge.")
      return(FALSE)
    }
    vcov_mat <- tryCatch(vcov(model), error = function(e) NULL)
    if (is.null(vcov_mat) || any(is.na(vcov_mat))) {
      warning("Model ", model_name, " has invalid variance-covariance matrix.")
      return(FALSE)
    }
  }
  return(TRUE)
}

# Regression Models
# Model 1: ZIP, ZINB, Logistic
zip_model1 <- zeroinfl(Q1 ~ Q3 + Q20 + Q28 + Q26 + Q30 + Q41 | Q3, data = df, dist = "poisson")
zinb_model1 <- zeroinfl(Q1 ~ Q3 + Q20 + Q28 + Q26 + Q30 + Q41 | Q3, data = df, dist = "negbin")
logistic_model1 <- glm(Q2 ~ Q3 + Q20 + Q28 + Q26 + Q30 + Q41, family = binomial, data = df)

# Model 2: ZIP, ZINB, Logistic
zip_model2 <- zeroinfl(Q1 ~ Q5 + Q9 + Q7 + Q13 + Q28 + Q26 + Q30 + Q41 | Q13, data = df, dist = "poisson")
zinb_model2 <- zeroinfl(Q1 ~ Q5 + Q9 + Q7 + Q13 + Q28 + Q26 + Q30 + Q41 | Q13, data = df, dist = "negbin")
logistic_model2 <- glm(Q2 ~ Q5 + Q9 + Q7 + Q13 + Q28 + Q26 + Q30 + Q41, family = binomial, data = df)

# Model 3: ZIP, ZINB, Logistic
zip_model3 <- zeroinfl(Q1 ~ Q13 + Q21 + Q7 + Q28 + Q24 + Q26 + Q30 + Q32 + Q41 | Q13, data = df, dist = "poisson")
zinb_model3 <- zeroinfl(Q1 ~ Q13 + Q21 + Q7 + Q28 + Q24 + Q26 + Q30 + Q32 + Q41 | Q13, data = df, dist = "negbin")
logistic_model3 <- glm(Q2 ~ Q13 + Q21 + Q7 + Q28 + Q24 + Q26 + Q30 + Q32 + Q41, family = binomial, data = df)

# Check model validity
models <- list(zip_model1 = zip_model1, zinb_model1 = zinb_model1, logistic_model1 = logistic_model1,
               zip_model2 = zip_model2, zinb_model2 = zinb_model2, logistic_model2 = logistic_model2,
               zip_model3 = zip_model3, zinb_model3 = zinb_model3, logistic_model3 = logistic_model3)
for (name in names(models)) {
  check_model(models[[name]], name)
}

# Vuong Tests
vuong_zip1 <- vuong(zip_model1, glm(Q1 ~ Q3 + Q20 + Q28 + Q26 + Q30 + Q41, family = poisson, data = df))
vuong_zinb1 <- vuong(zinb_model1, glm(Q1 ~ Q3 + Q20 + Q28 + Q26 + Q30 + Q41, family = poisson, data = df))
vuong_zip2 <- vuong(zip_model2, glm(Q1 ~ Q5 + Q9 + Q7 + Q13 + Q28 + Q26 + Q30 + Q41, family = poisson, data = df))
vuong_zinb2 <- vuong(zinb_model2, glm(Q1 ~ Q5 + Q9 + Q7 + Q13 + Q28 + Q26 + Q30 + Q41, family = poisson, data = df))
vuong_zip3 <- vuong(zip_model3, glm(Q1 ~ Q13 + Q21 + Q7 + Q28 + Q24 + Q26 + Q30 + Q32 + Q41, family = poisson, data = df))
vuong_zinb3 <- vuong(zinb_model3, glm(Q1 ~ Q13 + Q21 + Q7 + Q28 + Q24 + Q26 + Q30 + Q32 + Q41, family = poisson, data = df))

# Extract AIC and BIC
comparison <- data.frame(
  Model = c("Model 1 ZIP", "Model 1 ZINB", "Model 1 Logistic",
            "Model 2 ZIP", "Model 2 ZINB", "Model 2 Logistic",
            "Model 3 ZIP", "Model 3 ZINB", "Model 3 Logistic"),
  AIC = round(c(AIC(zip_model1), AIC(zinb_model1), AIC(logistic_model1),
                AIC(zip_model2), AIC(zinb_model2), AIC(logistic_model2),
                AIC(zip_model3), AIC(zinb_model3), AIC(logistic_model3)), 2),
  BIC = round(c(BIC(zip_model1), BIC(zinb_model1), BIC(logistic_model1),
                BIC(zip_model2), BIC(zinb_model2), BIC(logistic_model2),
                BIC(zip_model3), BIC(zinb_model3), BIC(logistic_model3)), 2)
)

# Save model comparison table
sink("model_comparison.html")
cat("<h2>Model Comparison: AIC and BIC</h2>\n")
kable(comparison, format = "html", caption = "AIC and BIC for Regression Models")
sink()

# Save Vuong test results
sink("vuong_tests.html")
cat("<h2>Vuong Test Results</h2>\n")
cat("<h3>Model 1: ZIP vs Poisson</h3>\n"); print(vuong_zip1)
cat("<h3>Model 1: ZINB vs Poisson</h3>\n"); print(vuong_zinb1)
cat("<h3>Model 2: ZIP vs Poisson</h3>\n"); print(vuong_zip2)
cat("<h3>Model 2: ZINB vs Poisson</h3>\n"); print(vuong_zinb2)
cat("<h3>Model 3: ZIP vs Poisson</h3>\n"); print(vuong_zip3)
cat("<h3>Model 3: ZINB vs Poisson</h3>\n"); print(vuong_zinb3)
sink()

# Contour Plots
create_contour_plot <- function(model, data, x_var, y_var, model_name) {
  x_range <- seq(min(data[[x_var]], na.rm = TRUE), max(data[[x_var]], na.rm = TRUE), length.out = 100)
  y_range <- seq(min(data[[y_var]], na.rm = TRUE), max(data[[y_var]], na.rm = TRUE), length.out = 100)
  grid <- expand.grid(x = x_range, y = y_range)
  names(grid) <- c(x_var, y_var)
  
  # Set other variables to mean or mode
  for (var in setdiff(names(data), c(x_var, y_var, "Q1", "Q2"))) {
    if (is.numeric(data[[var]]) && !is.factor(data[[var]])) {
      grid[[var]] <- mean(data[[var]], na.rm = TRUE)
    } else {
      grid[[var]] <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
    }
  }
  
  pred <- tryCatch({
    if (inherits(model, "zeroinfl")) {
      predict(model, newdata = grid, type = "response")
    } else {
      predict(model, newdata = grid, type = "response")
    }
  }, warning = function(w) {
    message("Warning in prediction for ", model_name, ": ", w)
    rep(NA, nrow(grid))
  }, error = function(e) {
    message("Error in prediction for ", model_name, ": ", e)
    rep(NA, nrow(grid))
  })
  
  grid$pred <- pred
  p <- ggplot(grid, aes_string(x = x_var, y = y_var, z = "pred")) +
    geom_contour(aes(color = after_stat(level))) +
    geom_tile(aes(fill = pred), alpha = 0.5) +
    scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
    labs(title = paste("Contour Plot -", model_name), x = x_var, y = y_var) +
    theme_minimal()
  
  ggsave(paste0("contour_", tolower(gsub(" ", "_", model_name)), ".png"), p, width = 8, height = 6)
}

# Generate contour plots for continuous predictors
create_contour_plot(zip_model1, df, "Q20", "Q26", "model_1_zip")
create_contour_plot(zinb_model1, df, "Q20", "Q26", "model_1_zinb")
create_contour_plot(logistic_model1, df, "Q20", "Q26", "model_1_logistic")
create_contour_plot(zip_model2, df, "Q5", "Q9", "model_2_zip")
create_contour_plot(zinb_model2, df, "Q5", "Q9", "model_2_zinb")
create_contour_plot(logistic_model2, df, "Q5", "Q9", "model_2_logistic")
create_contour_plot(zip_model3, df, "Q21", "Q24", "model_3_zip")
create_contour_plot(zinb_model3, df, "Q21", "Q24", "model_3_zinb")
create_contour_plot(logistic_model3, df, "Q21", "Q24", "model_3_logistic")

# Save individual model summaries to ensure all outputs are generated
model_list <- list(
  zip_model1 = list(model = zip_model1, name = "zip_model1", type = "zeroinfl"),
  zinb_model1 = list(model = zinb_model1, name = "zinb_model1", type = "zeroinfl"),
  logistic_model1 = list(model = logistic_model1, name = "logistic_model1", type = "logistic"),
  zip_model2 = list(model = zip_model2, name = "zip_model2", type = "zeroinfl"),
  zinb_model2 = list(model = zinb_model2, name = "zinb_model2", type = "zeroinfl"),
  logistic_model2 = list(model = logistic_model2, name = "logistic_model2", type = "logistic"),
  zip_model3 = list(model = zip_model3, name = "zip_model3", type = "zeroinfl"),
  zinb_model3 = list(model = zinb_model3, name = "zinb_model3", type = "zeroinfl"),
  logistic_model3 = list(model = logistic_model3, name = "logistic_model3", type = "logistic")
)

for (m in model_list) {
  tryCatch({
    stargazer(m$model,
              type = "html",
              title = paste("Model Summary:", m$name),
              dep.var.labels = if (m$type == "zeroinfl") "Number of CWS (Q1)" else "CWS Presence (Q2)",
              covariate.labels = c("Rural Area Dummy (Q3)", "Log Tourism Capacity (Q20)",
                                   "Strong Innovation Region (Q28)", "Log Tertiary Education (Q26)",
                                   "Log Employed Persons (Q30)", "Log Population Density (Q5)",
                                   "Log Mobile Network Speed (Q9)", "Log Fixed Network Speed (Q7)",
                                   "Municipalities in FUA (Q13)", "Tourism Capacity (Q21)",
                                   "Log Number of Cafes/Pubs (Q24)", "Inner Alpine Dummy (Q32)",
                                   paste("NUTS2 Region:", levels(df$Q41)[-1])),
              out = paste0(m$type, "_", m$name, ".html"))
    message("Successfully generated summary for ", m$name)
  }, error = function(e) {
    message("Error in stargazer for ", m$name, ": ", e)
  })
}

# Attempt combined model summaries
tryCatch({
  stargazer(zip_model1, zinb_model1, zip_model2, zinb_model2, zip_model3, zinb_model3,
            type = "html",
            title = "Zero-Inflated Models Comparison",
            dep.var.labels = c("Number of CWS (Q1)", "Number of CWS (Q1)",
                               "Number of CWS (Q1)", "Number of CWS (Q1)",
                               "Number of CWS (Q1)", "Number of CWS (Q1)"),
            covariate.labels = c("Rural Area Dummy (Q3)", "Log Tourism Capacity (Q20)",
                                 "Strong Innovation Region (Q28)", "Log Tertiary Education (Q26)",
                                 "Log Employed Persons (Q30)", "Log Population Density (Q5)",
                                 "Log Mobile Network Speed (Q9)", "Log Fixed Network Speed (Q7)",
                                 "Municipalities in FUA (Q13)", "Tourism Capacity (Q21)",
                                 "Log Number of Cafes/Pubs (Q24)", "Inner Alpine Dummy (Q32)",
                                 paste("NUTS2 Region:", levels(df$Q41)[-1])),
            out = "zeroinfl_models.html")
  message("Successfully generated combined zero-inflated model summary")
}, error = function(e) {
  message("Error in combined stargazer for zero-inflated models: ", e)
})

tryCatch({
  stargazer(logistic_model1, logistic_model2, logistic_model3,
            type = "html",
            title = "Logistic Models Comparison",
            dep.var.labels = c("CWS Presence (Q2)", "CWS Presence (Q2)", "CWS Presence (Q2)"),
            covariate.labels = c("Rural Area Dummy (Q3)", "Log Tourism Capacity (Q20)",
                                 "Strong Innovation Region (Q28)", "Log Tertiary Education (Q26)",
                                 "Log Employed Persons (Q30)", "Log Population Density (Q5)",
                                 "Log Mobile Network Speed (Q9)", "Log Fixed Network Speed (Q7)",
                                 "Municipalities in FUA (Q13)", "Tourism Capacity (Q21)",
                                 "Log Number of Cafes/Pubs (Q24)", "Inner Alpine Dummy (Q32)",
                                 paste("NUTS2 Region:", levels(df$Q41)[-1])),
            out = "logistic_models.html")
  message("Successfully generated combined logistic model summary")
}, error = function(e) {
  message("Error in combined stargazer for logistic models: ", e)
})

# Print output instructions
cat("Outputs:
- Correlation heatmaps saved as: correlation_model_1.png, correlation_model_2.png, correlation_model_3.png
- Density heatmaps saved as: density_heatmap_model_1.png, density_heatmap_model_2.png, density_heatmap_model_3.png
- Highly correlated pairs saved to: correlation_pairs.txt
- Model comparison table saved to: model_comparison.html
- Vuong test results saved to: vuong_tests.html
- Individual zero-inflated model summaries saved as: zeroinfl_zip_model1.html, zeroinfl_zinb_model1.html, zeroinfl_zip_model2.html, zeroinfl_zinb_model2.html, zeroinfl_zip_model3.html, zeroinfl_zinb_model3.html
- Individual logistic model summaries saved as: logistic_logistic_model1.html, logistic_logistic_model2.html, logistic_logistic_model3.html
- Combined zero-inflated model summaries (if successful) saved to:

 zeroinfl_models.html
- Combined logistic model summaries (if successful) saved to: logistic_models.html
- Contour plots saved as: contour_model_1_zip.png, contour_model_1_zinb.png, contour_model_1_logistic.png,
  contour_model_2_zip.png, contour_model_2_zinb.png, contour_model_2_logistic.png,
  contour_model_3_zip.png, contour_model_3_zinb.png, contour_model_3_logistic.png\n")