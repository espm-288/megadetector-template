#' Load MegaDetector Model
#'
#' Loads the MegaDetector model.
#'
#' @param model_file Path to the checkpoint file or a model identifier (e.g., "MDV5A", "MDV5B").
#'   If a reserved name like "MDV5A" is used, the package may download the weights automatically.
#' @return A loaded model object (Python object).
#' @export
load_model <- function(model_file = "MDV5A") {
  if (is.null(md)) stop("MegaDetector module not loaded.")

  # run_detector might not be exposed in detection module, import explicitly
  rd <- tryCatch({
    reticulate::import("megadetector.detection.run_detector")
  }, error = function(e) {
    stop("Could not import megadetector.detection.run_detector: ", e$message)
  })

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
