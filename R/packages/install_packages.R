# install_packages.R

# Load the package list
if (file.exists("R/packages/packages.R")) {
  source("R/packages/packages.R")
  source("R/utility/verian_palette.R")
} else stop("packages.R does not exist.")

# Function to check and install missing packages
install_if_missing = function(pkg, local_path = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!is.null(local_path)) {
      message(paste("Installing local package:", pkg))
      install.packages(local_path, repos = NULL, type = "source")
    } else {
      message(paste("Installing from CRAN:", pkg))
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

# Install CRAN packages
invisible(lapply(cran_packages, install_if_missing))

# Install local packages
invisible(lapply(names(local_packages), function(pkg) {
  install_if_missing(pkg, local_packages[[pkg]])
}))

message("All required packages installed.")

# Load libraries
invisible(lapply(cran_packages, library, character.only = TRUE))
invisible(lapply(names(local_packages), library, character.only = TRUE))

message("All required packages loaded.")

# Remove variables and functions from namespace
rm(install_if_missing, local_packages, cran_packages)
