# 2_fit_gams.R
# =================================================================================
# GAM Analysis: Incidence, Temperature, Year, and Season
# This script fits multiple GAMs, compares them, performs diagnostics, and outputs
# results to a file. The output files summarising the model are saved to out/gams/.
# Model diagnostic plots are saved to the same directory.
# =================================================================================

# =================================================================================
# --- Set up ---
# =================================================================================
source("R/utility/helper.R")


# =================================================================================
# --- Load data ---
# =================================================================================
pathogen_data = read.csv("data/foodborne_pathogen.csv")
pathogen_long = long_pathogen_data(pathogen_data)

# Add time index to pathogen_long for 'continuous' time 
pathogen_long$time = as.numeric(as.factor(pathogen_long$year * 4 + pathogen_long$quarter))


# =================================================================================
# --- Define GAM models ---
# =================================================================================
models = list(
  M1 = cbind(cases, population - cases) ~ s(temperature),
  M2 = cbind(cases, population - cases) ~ s(temperature) + s(quarter, bs = "cc", k = 4) +
    te(temperature, quarter, bs = c("tp", "cc"), k = c(5, 4)),
  M3 = cbind(cases, population - cases) ~ s(temperature) + s(time, k = 5) +
    te(temperature, time),
  M4 = cbind(cases, population - cases) ~ s(temperature) + s(time) +
    s(quarter, bs = "cc", k = 4) + te(temperature, time) +
    te(temperature, quarter, bs = c("tp", "cc"), k = c(5, 4)),
  M5 = cbind(cases, population - cases) ~ s(temperature) +
    s(quarter, bs = "cc", k = 4) +
    te(temperature, time) 
)

# Fit and evaluate all models: logit link
fitted_models_logit = lapply(names(models), function(name) {
  fit_and_evaluate_gam(models[[name]], name, pathogen_long, "logit")
})

# Fit and evaluate all models: probit link
fitted_models_probit = lapply(names(models), function(name) {
  fit_and_evaluate_gam(models[[name]], name, pathogen_long, "probit")
})

# Fit and evaluate all models: cloglog link
fitted_models_cloglog = lapply(names(models), function(name) {
  fit_and_evaluate_gam(models[[name]], name, pathogen_long, "cloglog")
})

# Fit and evaluate all models: cauchit link
fitted_models_cauchit = lapply(names(models), function(name) {
  fit_and_evaluate_gam(models[[name]], name, pathogen_long, "cauchit")
})


# =================================================================================
# --- Model Performance and Comparison ---
# =================================================================================

# Collect all fitted models in a named list
all_models = list(
  logit = fitted_models_logit,
  probit = fitted_models_probit,
  cloglog = fitted_models_cloglog,
  cauchit = fitted_models_cauchit
)

# Extract AIC and BIC for each model within each link function
aic_results = lapply(all_models, function(models) sapply(models, AIC))
bic_results = lapply(all_models, function(models) sapply(models, BIC))

# Convert results to data frames for easier comparison
aic_df = do.call(cbind, aic_results)
bic_df = do.call(cbind, bic_results)

# Print results
print(aic_df)
print(bic_df)


# =================================================================================
# --- Visualization of best model: M2
# =================================================================================

# Best Model (M2)
best_model = fitted_models_logit[[2]]

# Create data grid for predicted values
fitted_data = expand.grid(
  temperature = seq(min(pathogen_long$temperature), max(pathogen_long$temperature), length.out = 100),
  quarter = seq(min(pathogen_long$quarter), max(pathogen_long$quarter), length.out = 100)
)

# Predict fitted values from the model
fitted_data$p_pred = predict(best_model, newdata = fitted_data, type = "response")

# Plot heat map of fitted values
p_gam_best_model = ggplot(fitted_data, aes(x = temperature, y = quarter, fill = p_pred)) +
  geom_tile() +
  scale_fill_verian_continuous(palette = "verian_ot", direction = -1) +
  labs(title = "",
       x = "Temperature",
       y = "Quarter",
       fill = "Predicted Case Incidence") +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.8)) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# =================================================================================
# --- Visualization of best model with time: M5
# =================================================================================

# Best Model with Time (M5)
best_model_with_time = fitted_models_logit[[5]]

# Define temperatures
temperatures = c(20, 24, 28, 32, 36)

# Create fitted data frame with time
fitted_data_time = do.call(rbind, lapply(temperatures, function(temp) {
  data.frame(time = 1:20, quarter = rep(1:4, 5), temperature = temp)
}))

# Add a dummy population to ensure binomial predictions work
fitted_data_time$population = 200  # Arbitrary value, cancels out in rate calculation
fitted_data_time$cases = NA  # Predict cases

# Predict case probabilities with confidence intervals
preds = predict(best_model_with_time, newdata = fitted_data_time, type = "link", se.fit = TRUE)
fitted_data_time$p_pred = plogis(preds$fit)  # Convert from logit scale
fitted_data_time$lower = plogis(preds$fit - 1.96 * preds$se.fit)
fitted_data_time$upper = plogis(preds$fit + 1.96 * preds$se.fit)
fitted_data_time$temperature = paste0("Temp: ", fitted_data_time$temperature, " deg")

# Plot predictions with uncertainty in a 1x5 grid
p_gam_best_model_with_time = ggplot(fitted_data_time, aes(x = time, y = p_pred)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(temperature)), alpha = 0.2) +
  geom_line(aes(color = factor(temperature))) +
  facet_wrap(~temperature, nrow = 1) +
  scale_color_manual(values = rep("#ff7d04", 5)) +
  scale_fill_manual(values = rep("#ff7d04", 5)) +
  labs(title = "", 
       x = "Quarters since Q1 2020", 
       y = "Predicted Case Incidence", 
       color = "Temperature (°C)",
       fill = "Temperature (°C)") +
  theme_minimal() + 
  theme(
    legend.position = "none"
  )

p_blank = ggplot() + 
  theme_minimal()


# Combine plots into a single figure with labels 
g1 = ggarrange(p_blank, p_gam_best_model, p_blank, nrow = 1, labels = c("", "A", ""), 
          font.label = list(size = 20), widths = c(1.3, 3, 1.3))
g2 = ggarrange(p_gam_best_model_with_time, ncol = 1, labels = c("B"), 
               font.label = list(size = 20))
ggarrange(g1, g2, ncol = 1)

# =================================================================================
# --- Save visualisations to file
# =================================================================================
ggsave("out/plot_gam_predictions.jpg", width = 7.5, height = 7, dpi = 250)
