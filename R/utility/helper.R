# helper.R
#
# Helper functions to improve readability and reusability of main scripts.

# ==== Fit a spatio-temporal binomial model using a latent Gaussian Markov Random Field (GMRF)
fit_latent_binomial_gmrf = function(y, n, longitude, latitude, time_index,
                                    boundary, 
                                    mesh_max_edge = c(0.8, 2), mesh_cutoff = 0.2, 
                                    mesh_offset = c(0.5, 1),
                                    matern_alpha = 2, 
                                    n_pred = 2000) {
  
  #' Fit a spatial-temporal INLA model and make predictions
  #'
  #' This function fits a spatial-temporal model using INLA on the given data and makes 
  #' predictions on a regular grid within a specified boundary.
  #'
  #' @param y Numeric vector. Count data (e.g., number of cases).
  #' @param n Numeric vector. Population.
  #' @param longitude Numeric vector. Longitude coordinates of observations.
  #' @param latitude Numeric vector. Latitude coordinates of observations.
  #' @param time_index Numeric vector. Time indices for observations.
  #' @param boundary sf object. Geographical boundary for study region.
  #' @param mesh_max_edge Numeric vector. The max edge length of the mesh for spatial modelling.
  #' @param mesh_offset Numeric vector. How far to extend domain for inner and outer mesh boundary.
  #' @param mesh_cutoff Numeric. The minimum distance between mesh points to avoid redundancy.
  #' @param matern_alpha Numeric. The smoothness parameter for the Matérn covariance function.
  #' @param n_pred Integer. Number of prediction points (default = 2000).
  #'
  #' @return List containing the fitted model (`result`) and predictions (`data_pred`).
  #' @export
  
  # Ensure required packages are installed
  if (!requireNamespace("INLA", quietly = TRUE)) stop("Package 'INLA' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  
  # Create a spatial mesh
  coords_rep = data.frame(longitude, latitude)
  mesh = inla.mesh.2d(loc = coords_rep, boundary = boundary, max.edge = mesh_max_edge, 
                      offset = mesh_offset, cutoff = mesh_cutoff)
  
  # Prediction grid points
  n_time = length(unique(time_index))
  grid_coords = st_coordinates(st_sample(boundary, size = n_pred, type = "regular"))
  grid_coords_rep = as.data.frame(grid_coords[rep(seq_len(nrow(grid_coords)), times = n_time), ])
  colnames(grid_coords_rep) = c("longitude", "latitude")
  time_index_pred = rep(1:n_time, each = nrow(grid_coords))
  
  # SPDE Model for spatial dependency
  spde = inla.spde2.matern(mesh = mesh, alpha = matern_alpha)
  spatial_index <- inla.spde.make.index("spatial_field", n.spde = spde$n.spde, n.group = n_time)
  
  # Construct A-matrices for observations and predictions
  A = inla.spde.make.A(mesh = mesh, loc = as.matrix(coords_rep), group = time_index)
  A_pred = inla.spde.make.A(mesh = mesh, loc = as.matrix(grid_coords_rep), group = time_index_pred)
  
  # Data frames for estimation and prediction
  data = data.frame(y = y, n = n, loc_x = longitude, loc_y = latitude, time = time_index)
  data_pred = data.frame(y = rep(NA, length(time_index_pred)), n = rep(NA, length(time_index_pred)),
                         loc_x = grid_coords_rep$longitude, loc_y = grid_coords_rep$latitude, 
                         time = time_index_pred)
  
  # INLA Data Stacks
  stack = inla.stack(tag = "est", data = list(y = data$y), A = list(A, 1),
                     effects = list(spatial_index, data.frame(beta0 = 1, time = time_index)))
  stack_pred = inla.stack(tag = "pred", data = list(y = NA), A = list(A_pred, 1),
                          effects = list(spatial_index, data.frame(beta0 = 1, time = data_pred$time)))
  stack_full = inla.stack(stack, stack_pred)
  
  # Model formula
  formula = y ~ 0 + beta0 + 
    f(spatial_field, model = spde, group = spatial_field.group, control.group = list(model = "ar1"))
  
  # Fit model
  result = inla(formula, data = inla.stack.data(stack_full, spde = spde),
                control.predictor = list(link = 1, A = inla.stack.A(stack_full), compute = TRUE),
                family = "binomial", quantiles = c(0.025, 0.975), Ntrials = c(data$n, data_pred$n),
                control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
                control.inla(strategy = 'simplified.laplace', huge = TRUE))
  
  # Extract predictions and confidence intervals
  index_pred = inla.stack.index(stack_full, "pred")$data
  post_mean_pred_logit = result$summary.linear.predictor[index_pred, "mean"]
  post_lower_pred_logit = result$summary.linear.predictor[index_pred, "0.025quant"]
  post_upper_pred_logit = result$summary.linear.predictor[index_pred, "0.975quant"]
  
  # Transform logit predictions to probability scale
  data_pred$p_pred = invlogit(post_mean_pred_logit)
  data_pred$p_lower = invlogit(post_lower_pred_logit)
  data_pred$p_upper = invlogit(post_upper_pred_logit)

  return(list(model = result, predictions = data_pred))
}

# ==== Inverse logit 
invlogit = function(p){
  #' Calculate inverse logit
  #'
  #' @param p Numeric. 
  #'
  #' @return Inverse logit
  #' @export
  
  return(exp(p)/(1+exp(p)))
}

# ==== Function to fit, check, and summarise GAM models
fit_and_evaluate_gam = function(gam_model_formula, model_name, data, link_function = "logit", 
                                output_file = "gam_results.txt") {
  #' Wrapper to fit and evaluate GAM models with custom link function
  #'
  #' @param gam_model_formula Formula. GAM model formula
  #' @param model_name Character. Model name. 
  #' @param data Dataframe. Long-format data frame with latitude, longitude, year,  
  #' quarter, and population.
  #' @param link_function Character. Link function for the binomial family (default = "logit").
  #' @param output_file Character. Name of the output text file (default = "gam_results.txt").
  #'
  #' @return 
  #' Fitted GAM model.
  #' @export
  
  # Define the diagnostic plots filename
  pdf_filename = paste0("out/gams/", model_name, "_", link_function, "_gam_checks.pdf")
  
  # Update output file with link function 
  output_file = paste0("out/gams/", link_function, "_", output_file)
  
  # Log model fitting start
  cat("\n========================================\n", file = output_file, append = TRUE)
  cat("Fitting model:", model_name, "with link:", link_function, "\n", file = output_file, append = TRUE)
  cat("========================================\n", file = output_file, append = TRUE)
  
  # Fit the model with the specified link function
  fit = gam(gam_model_formula, family = binomial(link = link_function), data = data)
  
  # Capture summary output
  summary_text = capture.output(summary(fit))
  
  # Save model summary to the text file
  cat("\nModel Summary:\n", file = output_file, append = TRUE)
  cat(summary_text, file = output_file, append = TRUE, sep = "\n")
  
  # Capture and save gam.check() output
  cat("\nDiagnostic Check:\n", file = output_file, append = TRUE)
  check_text = capture.output(invisible(gam.check(fit))); dev.off()
  cat(check_text, file = output_file, append = TRUE, sep = "\n")
  
  # Save gam.check plots as a single-page PDF
  pdf(pdf_filename)
  par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
  
  # Suppress gam.check() output while still generating plots
  sink(tempfile())  # Redirect console output to a temporary file
  invisible(capture.output(gam.check(fit)))  # Capture and discard any text output
  sink()  # Restore normal console output
  
  dev.off()  
  
  
  return(fit)
}
  
  


# ==== Convert pathogen data from wide format to long format
long_pathogen_data = function(pathogen_data) {
  #' Convert Pathogen Data to Long Format
  #'
  #' @param pathogen_data Data frame. Pathogen data with temperature and case counts across time
  #' @return A long-format data frame with latitude, longitude, year, quarter, population, 
  #' temperature, cases, and incidence.
  #' @export
  
  if (!requireNamespace("tidyverse", quietly = TRUE)) stop("Package 'tidyverse' is required.")
  
  pathogen_data %>%
    pivot_longer(cols = starts_with("temp_q"), names_to = "quarter", values_to = "temperature") %>%
    mutate(quarter = gsub("temp_q", "", quarter)) %>%
    pivot_longer(cols = starts_with("cases_q"), names_to = "case_quarter", values_to = "cases") %>%
    mutate(case_quarter = gsub("cases_q", "", case_quarter)) %>%
    filter(quarter == case_quarter) %>%
    mutate(
      incidence = cases / population,
      year = as.numeric(year),
      quarter = as.numeric(quarter)
    ) %>%
    dplyr::select(-case_quarter)
}


# ==== Plotting helper for country values 
map_country = function(country, longitude, latitude, value = NULL, interpolate = FALSE, 
                       group_col = NULL, group_row = NULL, value_limits = NULL, 
                       title_plot = NULL, title_legend = NULL, 
                       palette = NULL, palette_dir = 1, show_adm_labels = FALSE){
  #' Plot coordinates and values on an administrative level map
  #'
  #' This function visualises latitude-longitude coordinates and associated values (optional)
  #' on a backdrop of the administrative map of a specified country.
  #'
  #' @param country Character. Name of the country to plot.
  #' @param longitude Numeric vector. Contains longitude values.
  #' @param latitude Numeric vector. Contains latitude values.
  #' @param value Numeric vector. Values associated with the coordinates.
  #' @param value_limits Numeric two-vector. Lower and upper limits of values.
  #' @param interpolate Boolean. Interpolate between values.
  #' @param group_col Character vector. Grouping variable to be displayed columnwise. 
  #' @param group_row Character vector. Grouping variable to be displayed rowwise. 
  #' @param title_plot Character. Title for the plot.
  #' @param title_legend Character. Title for the legend.
  #' @param palette Character. Color palette for visualizing values.
  #' @param palette_dir Numeric. Direction of color palette (1 or -1 to reverse).
  #' @param show_adm_labels Boolean. Whether to show labels for administrative units.
  #'
  #' @return A ggplot object displaying the map with plotted values.
  #' @export
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.")
  if (!requireNamespace("scico", quietly = TRUE)) stop("Package 'scico' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  
  # Check input arguments
  if (is.null(palette)) palette = "verian_ot"
  
  # Flag to wrap plot
  flag_wrap = FALSE
  flag_null_value = is.null(value)
  
  # Country administrative boundary 
  boundary_adm0 = geoboundaries(country, "adm0") %>% sf::st_simplify(dTolerance = 100)
  boundary_adm1 = geoboundaries(country, "adm1") %>% sf::st_simplify(dTolerance = 100)
  
  # Extract centroids for labels
  if (show_adm_labels) {
    adm1_centroids = boundary_adm1 %>% 
      dplyr::mutate(centroid = sf::st_centroid(geometry)) %>%
      dplyr::mutate(long = sf::st_coordinates(centroid)[, 1],
                    lat = sf::st_coordinates(centroid)[, 2]) %>%
      dplyr::select(shapeName, long, lat)  # Keep only necessary columns
  }
  
  # Dataframe of values to plot
  data = data.frame(long = longitude, lat = latitude)
  if (flag_null_value) data$value = rep(1, length(longitude))
  
  if (!is.null(group_col) & !is.null(group_row)){
    data$group_col = group_col
    data$group_row = group_row
    flag_wrap = TRUE
  }
  
  # Create base map with country boundaries
  p = ggplot() +
    labs(title = title_plot) 
  
  if (flag_null_value){
    legend.position = "none"
    p = p + 
      geom_point(data = data, aes(x = long, y = lat))
  } else{
    if (interpolate){
      legend.position = "bottom"
      p = p +
        geom_raster(data = data, aes(x = long, y = lat, fill = value), interpolate = interpolate) +
        scale_fill_verian_continuous(
          palette = palette, limits = value_limits, name = title_legend, direction = palette_dir
        ) +
        guides(fill = guide_colorbar(title.position = "top", barwidth = 10, barheight = 1))
    } else{
      legend.position = "bottom"
      p = p +
        geom_point(data = data, aes(x = long, y = lat, color = value), size = 2) +
        scale_color_verian_continuous(
          palette = palette, limits = value_limits, name = title_legend, direction = palette_dir
        ) +
        guides(color = guide_colorbar(title.position = "top", barwidth = 10, barheight = 1))
    }
  }
  
  # Facet if group_col, group_row specified
  if (flag_wrap){
    p = p + facet_grid(group_row ~ group_col)
  }
  
  # Boundary and theme elements
  p =  p +
    geom_sf(data = boundary_adm1, fill = NA, color = "black", linewidth = .2) +
    geom_sf(data = boundary_adm0, fill = NA, color = "black", linewidth = .7) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = legend.position,
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  # Add labels if requested
  if (show_adm_labels) {
    p = p +
      ggrepel::geom_label_repel(
        data = adm1_centroids, 
        aes(x = long, y = lat, label = shapeName),
        size = 4, 
        fontface = "bold", 
        box.padding = 0.5, 
        force = 10,
        min.segment.length = 0,
        point.padding = 0.2, 
        segment.color = "black",
        segment.size = 0.5, 
        segment.ncp = 3,  # Controls number of control points for smoothing
        arrow = arrow(length = unit(0.01, "npc")) # Adds an arrowhead
      )
  }
  
  
  return(p)
}