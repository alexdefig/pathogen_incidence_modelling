# 1_data_visualisation.R
# =================================================================================
# Data Visualisation: Location, Incidence and Temperature
# This script maps the pathogen data
# =================================================================================

# =================================================================================
# --- Setup ---
# =================================================================================
source("R/utility/helper.R")  # Helper functions

# =================================================================================
# --- Load data ---
# =================================================================================
pathogen_data = read.csv("data/foodborne_pathogen.csv")

# Convert pathogen dataset into long format
pathogen_long = long_pathogen_data(pathogen_data)

# =================================================================================
# --- Plot site locations ---
# =================================================================================
p_site_locs = map_country("Nigeria", pathogen_data$longitude, pathogen_data$latitude, 
                          title_plot = "", show_adm_labels = FALSE)
p_adm_labels = map_country("Nigeria", 10, 10, title_plot = "", show_adm_labels = TRUE)


# =================================================================================
# --- Plot case incidence ---
# =================================================================================
p_case_incidence = map_country("Nigeria", pathogen_long$longitude, pathogen_long$latitude, 
                               pathogen_long$incidence, 
                               group_col = paste0("Q", pathogen_long$quarter), 
                               group_row = pathogen_long$year, 
                               value_limits =  c(0, 0.20), 
                               title_plot = NULL, title_legend = "Case incidence", 
                               palette = "verian_ot", palette_dir = -1)
  
  
# =================================================================================
# --- Plot temperature ---
# =================================================================================
p_temperature = map_country("Nigeria", pathogen_long$longitude, pathogen_long$latitude, 
                            pathogen_long$temperature, 
                            group_col = paste0("Q", pathogen_long$quarter), 
                            group_row = pathogen_long$year, 
                            value_limits =  c(18, 38), 
                            title_plot = NULL, title_legend = "Temperature", 
                            palette = "verian_ob", palette_dir = -1)

# =================================================================================
# --- Save figures to /out/ ---
# =================================================================================
p_sites_and_labs = ggarrange(p_adm_labels, p_site_locs, nrow = 1, labels = c("A", "B"), 
                             font.label = list(size = 30))

ggsave("out/plot_site_locations.png", plot = p_sites_and_labs, width = 13, height = 7, dpi = 200)
ggsave("out/plot_case_incidence_raw.jpg", plot = p_case_incidence, width = 6, height = 7, dpi = 250)
ggsave("out/plot_site_temperatures.jpg", plot = p_temperature, width = 6, height = 7, dpi = 250)




