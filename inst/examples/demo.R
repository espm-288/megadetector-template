
# MegaDetector Demo Script

# If testing in rocker/ml container, prefer the conda env
# MUST be set before reticulate is loaded
if (file.exists("/opt/conda/bin/python")) {
  Sys.setenv(RETICULATE_PYTHON = "/opt/conda/bin/python")
}

# Ensure reticulate is installed
if (!require("reticulate")) install.packages("reticulate")

# Install megadetector if not present (assumes package is installed in library)
library(megadetector)

# Ensure python environment is prepared
if (dir.exists("/opt/conda")) {
  reticulate::use_condaenv("/opt/conda", required = TRUE) # Try to use base conda
}

# --- 1. Setup Model ---
# Check if megadetector module is available (it installs as 'detection', 'md_utils', etc), if not install it
if (!reticulate::py_module_available("detection")) {
  cat("MegaDetector python module not found. Installing...\n")
  method <- "auto"
  conda_path <- "auto"
  pip_arg <- TRUE
  env_name <- NULL
  
  if (dir.exists("/opt/conda")) {
    method <- "conda"
    conda_path <- "/opt/conda/bin/conda"
    env_name <- "base"
  }
  
  install_megadetector(method = method, conda = conda_path, pip = pip_arg, envname = env_name)
}





cat("Loading Model...\n")
# Download model using R to avoid Python SSL/HTTPS issues in some containers
model_url <- "https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt"
model_file <- file.path(tempdir(), "md_v5a.0.0.pt")

if (!file.exists(model_file)) {
    cat("Downloading model checkpoint (MDV5A) to:", model_file, "...\n")
    # Increase timeout for large file download
    options(timeout = 300) 
    tryCatch({
        download.file(model_url, model_file, mode = "wb")
    }, error = function(e) {
        stop("Failed to download model checkpoint: ", e$message)
    })
}

model <- load_model(model_file)

# --- 2. Setup Test Images ---
# Using stable Wikimedia Commons images
images <- list(
  "Red_Fox.jpg" = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/30/Red_Fox_-_British_Wildlife_Centre_%2816656268152%29.jpg/640px-Red_Fox_-_British_Wildlife_Centre_%2816656268152%29.jpg",
  "Police_Car.jpg" = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/Police_car_in_Geneva.jpg/640px-Police_car_in_Geneva.jpg",
  "Forest_Path.jpg" = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5b/Forest_path_in_autumn_%282463372250%29.jpg/640px-Forest_path_in_autumn_%282463372250%29.jpg"
)

temp_dir <- tempdir()
cat("Downloading sample images to:", temp_dir, "\n")

local_images <- c()
for (name in names(images)) {
  dest <- file.path(temp_dir, name)
  download.file(images[[name]], dest, mode = "wb", quiet = TRUE)
  local_images[[name]] <- dest
}

# --- 3. Run Detection ---
cat("\nRunning Detections:\n")
cat("------------------------------------------------\n")

for (name in names(local_images)) {
  img_path <- local_images[[name]]
  
  # Detect
  detections <- detect_image(model, img_path)
  
  # Classify
  class_label <- get_classification(detections)
  
  # Print Result
  cat(sprintf("Image: %-15s | Class: %s\n", name, class_label))
  
  # Detailed debug of first detection if exists
  if (length(detections) > 0) {
    top_score <- detections[[1]]$conf
    cat(sprintf("  -> Top Confidence: %.4f\n", top_score))
  }
  cat("------------------------------------------------\n")
}

cat("\nDemo Complete!\n")
