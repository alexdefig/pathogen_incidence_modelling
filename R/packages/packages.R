# List of required CRAN packages
cran_packages = c("ggplot2", "tidyverse", "sf", "scico", "roxygen2", "purrr", "patchwork", 
                  "mgcv", "INLA", "ggpubr", "scales", "ggrepel")

# List of local packages (name = path)
local_packages = list(
  "rgeoboundaries" = "R/lib/rgeoboundaries_1.3.tar"
)

# Anonymous function to test whether packages defined properly above
(function() {
  if (!exists("cran_packages") || !is.vector(cran_packages)) {
    stop("Error: 'cran_packages' is not defined or not a vector.")
  }
  if (!exists("local_packages") || !is.list(local_packages)) {
    stop("Error: 'local_packages' is not defined or not a list.")
  }
})()