# verian_palette.R
#
# Verian Colour Palettes

# Define the discrete palettes
verian_palettes = list(
  verian_orange = c("#ff7d04", "#ff9847", "#ffb175", "#ffcba2", "#ffe5d0"),
  verian_teal = c("#20baaf", "#64c8bf", "#8fd6ce", "#b6e4de", "#dbf2ef"),
  verian_purple = c("#6f018b", "#8e44a2", "#ac72b9", "#c8a0d0", "#e4cfe8"),
  verian_blue = c("#0060ad", "#557dbe", "#849cce", "#aebcde", "#d6ddef"),
  verian_ot = c("#ff7d04", "#20baaf"),
  verian_otw = c("#ffe5d0", "#ff7d04", "#20baaf"),
  verian_ob = c("#ff7d04", "#0060ad"),
  verian_pt = c("#6f018b", "#20baaf"),
  verian_op = c("#ff7d04", "#6f018b"),
  verian_pb = c("#6f018b", "#0060ad")
)

# Functions to create discrete scales
scale_fill_verian_discrete = function(palette, direction = 1, ...) {
  scale_fill_manual(values = adjust_palette(palette, direction), ...)
}

scale_color_verian_discrete = function(palette, direction = 1, ...) {
  scale_color_manual(values = adjust_palette(palette, direction), ...)
}

# Functions to create continuous color scales
scale_fill_verian_continuous = function(palette, direction = 1, ...) {
  scale_fill_gradientn(colors = adjust_palette(palette, direction), ...)
}

scale_color_verian_continuous = function(palette, direction = 1, ...) {
  scale_color_gradientn(colors = adjust_palette(palette, direction), ...)
}

# Function to handle flipping the palette if direction is -1
adjust_palette = function(palette_name, direction) {
  if (!palette_name %in% names(verian_palettes)) {
    stop("Invalid palette name. Choose from: ", paste(names(verian_palettes), collapse=", "))
  }
  
  palette = verian_palettes[[palette_name]]
  
  if (!is.null(direction) && direction == -1) {
    palette = rev(palette)  # Reverse colors if direction = -1
  }
  
  return(palette)
}

