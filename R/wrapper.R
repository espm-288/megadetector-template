#' Download MegaDetector Model
#'
#' Downloads a MegaDetector model to the user's cache directory.
#'
#' @param model Model identifier (default "MDV5A").
#' @param force If TRUE, force download even if the file exists.
#' @return Path to the downloaded model file.
#' @export
download_model <- function(model = "MDV5A", force = FALSE) {
  
  # Define available models
  models <- list(
    "MDV5A" = "https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt",
    "MDV5B" = "https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5b.0.0.pt"
  )
  
  if (!model %in% names(models)) {
    cli::cli_abort("Unknown model: {.val {model}}. Available models: {.val {names(models)}}")
  }
  
  url <- models[[model]]
  cache_dir <- tools::R_user_dir("megadetector", which = "cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  dest <- file.path(cache_dir, paste0(tolower(model), ".pt"))
  
  if (file.exists(dest) && !force) {
    cli::cli_alert_success("Model {.val {model}} found in cache: {.path {dest}}")
    return(dest)
  }
  
  cli::cli_alert_info("Downloading {.val {model}} to cache...")
  tryCatch({
    options(timeout = 300) # Ensure enough time for large download
    download.file(url, dest, mode = "wb", quiet = FALSE)
    cli::cli_alert_success("Downloaded {.val {model}} successfully.")
  }, error = function(e) {
    cli::cli_abort("Failed to download model: {e$message}")
  })
  
  return(dest)
}

#' Load MegaDetector Model
#'
#' Loads the MegaDetector model.
#'
#' @param model_file Path to the checkpoint file or a model identifier (e.g., "MDV5A", "MDV5B").
#'   If NULL or a specific identifier, the model will be downloaded to the cache if needed.
#' @return A loaded model object (Python object).
#' @export
load_model <- function(model_file = NULL) {
  if (is.null(md)) cli::cli_abort("MegaDetector module not loaded.")

  # Handle defaults
  if (is.null(model_file)) {
    model_file <- download_model("MDV5A")
  } else if (model_file %in% c("MDV5A", "MDV5B")) {
    model_file <- download_model(model_file)
  }
  
  # run_detector might not be exposed in detection module, import explicitly
  rd <- tryCatch({
    reticulate::import("megadetector.detection.run_detector")
  }, error = function(e) {
    cli::cli_abort("Could not import megadetector.detection.run_detector: {e$message}")
  })

  cli::cli_alert_info("Loading model from {.path {model_file}}...")
  return(rd$load_detector(model_file))
}

#' Detect Objects in Image
#'
#' Runs detection on a single image.
#'
#' @param model A loaded model object.
#' @param image_file Path to the image file.
#' @return A list containing detection results.
#' @export
detect_image <- function(model, image_file) {
  # if (is.null(md)) stop("MegaDetector module not loaded.") # Disabled check as 'megadetector' module doesn't exist
  
  # Use cli for errors
  # Import visualization utils to load image
  vis_utils <- tryCatch({
    reticulate::import("megadetector.visualization.visualization_utils")
  }, error = function(e) {
    cli::cli_abort("Could not import megadetector.visualization.visualization_utils: {e$message}")
  })

  # Load image using python utility
  image <- tryCatch({
    vis_utils$load_image(image_file)
  }, error = function(e) {
    cli::cli_abort("Failed to load image: {e$message}")
  })
  
  # Run detection
  # generate_detections_one_image(image, image_id, detection_threshold, image_size=None, augment=False)
  result <- model$generate_detections_one_image(image, image_file)
  
  detections <- result$detections
  
  # Sort by confidence descending if detections exist
  if (length(detections) > 1) {
    confs <- sapply(detections, function(x) x$conf)
    detections <- detections[order(confs, decreasing = TRUE)]
  }
  
  # Return just the detections list
  return(detections)
}

#' Get Classification from Detections
#'
#' Processes the raw detection results and returns the most likely class.
#'
#' @param detections A list of detection results from \code{detect_image}.
#' @param threshold Confidence threshold for considering a detection valid.
#' @return A character string: "animal", "person", "vehicle", or "blank".
#' @export
get_classification <- function(detections, threshold = 0.1) {
  if (length(detections) == 0) return("blank")
  
  # MegaDetector v5 classes: 1=animal, 2=person, 3=vehicle
  class_map <- c("1" = "animal", "2" = "person", "3" = "vehicle")
  
  max_conf <- 0
  best_class <- "blank"
  
  for (det in detections) {
    conf <- det$conf
    cat_id <- as.character(det$category)
    
    if (conf > threshold && conf > max_conf) {
      max_conf <- conf
      if (cat_id %in% names(class_map)) {
        best_class <- class_map[[cat_id]]
      } else {
        best_class <- paste0("unknown_", cat_id)
      }
    }
  }
  
  return(best_class)
}
