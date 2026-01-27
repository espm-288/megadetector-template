
md <- NULL

.onLoad <- function(libname, pkgname) {
  # If running in a container with /opt/conda, try to use it
  if (file.exists("/opt/conda/bin/python")) {
    tryCatch({
      reticulate::use_python("/opt/conda/bin/python", required = FALSE)
    }, error = function(e) warning("Failed to set /opt/conda python: ", e$message))
  }
  
  # Delay load the module
  md <<- reticulate::import("detection", delay_load = TRUE)
}
