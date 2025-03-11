# 2_fit_gmrf.R
# =================================================================================
# GMRF Analysis: Incidence, Temperature, Year, and Season
# This script fits a Gaussian Markov Random Field using INLA.
# =================================================================================
source("R/utility/helper.R")


# =================================================================================
# --- Load and prepare data ---
# =================================================================================
# Load the patogen dataset
pathogen_data = read.csv("data/foodborne_pathogen.csv")

# Convert pathogen dataset into long format
pathogen_long = long_pathogen_data(pathogen_data)

# Create a time index for the spatio-temporal model (quarterly from 2020-2024)
pathogen_long$time_index = as.numeric(as.factor(pathogen_long$year * 4 + pathogen_long$quarter))

# Arrange pathogen_long by time
pathogen_long = pathogen_long %>% arrange(time_index)

# Boundary for Nigeria
boundary_nigeria = geoboundaries("Nigeria", "adm0") %>% sf::st_simplify(dTolerance = 100)


# =================================================================================
# --- Fit spatio-temporal GMRF ---
# =================================================================================

fit_alpha_1 = fit_latent_binomial_gmrf(
  y = pathogen_long$cases, 
  n = pathogen_long$population, 
  longitude = pathogen_long$longitude, 
  latitude = pathogen_long$latitude, 
  time_index = pathogen_long$time_index,
  boundary = boundary_nigeria,
  mesh_max_edge = c(0.8, 2), 
  mesh_cutoff = 0.2,
  mesh_offset = c(0.5, 1),
  matern_alpha = 1
)

fit_alpha_2 = fit_latent_binomial_gmrf(
  y = pathogen_long$cases, 
  n = pathogen_long$population, 
  longitude = pathogen_long$longitude, 
  latitude = pathogen_long$latitude, 
  time_index = pathogen_long$time_index,
  boundary = boundary_nigeria,
  mesh_max_edge = c(0.8, 2), 
  mesh_cutoff = 0.2,
  mesh_offset = c(0.5, 1),
  matern_alpha = 2
)


# =================================================================================
# --- Model selection  ---
# =================================================================================
print(fit_alpha_1$model$waic$waic)
print(fit_alpha_2$model$waic$waic)


# =================================================================================
# --- Plot GMRF  ---
# =================================================================================
data_pred = fit_alpha_1$predictions
data_pred$year = 2020 + (data_pred$time - 1) %/% 4
data_pred$quarter = paste0("Q", (data_pred$time - 1) %% 4 + 1)

# map predictions
p_gmrf_case_incidence = map_country("Nigeria", data_pred$loc_x, data_pred$loc_y, 
                                    value = data_pred$p_pred, interpolate = TRUE, 
                                    group_col = data_pred$quarter, 
                                    group_row = data_pred$year, 
                                    value_limits = NULL, 
                                    title_legend = "Predicted case incidence", 
                                    palette = "verian_otw", palette_dir = -1)
# map uncertainties
p_gmrf_case_incidence_unc = map_country("Nigeria", data_pred$loc_x, data_pred$loc_y, 
                                        value = data_pred$p_upper-data_pred$p_lower, 
                                        interpolate = TRUE, 
                                        group_col = data_pred$quarter, 
                                        group_row = data_pred$year, 
                                        value_limits = NULL, 
                                        title_legend = "Predicted case incidence (95% uncertainty)", 
                                        palette = "verian_purple", palette_dir = -1)

# =================================================================================
# --- Save plots to file ---
# =================================================================================
ggsave("out/plot_gmrf_case_incidence.jpg", plot = p_gmrf_case_incidence, 
       width = 6, height = 7, dpi = 250)
ggsave("out/plot_gmrf_case_incidence_unc.jpg", plot = p_gmrf_case_incidence_unc, 
       width = 6, height = 7, dpi = 250)




  
  
  
  
  

