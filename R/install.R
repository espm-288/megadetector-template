#' Install MegaDetector Python Package
#'
#' Installs the megadetector python package and its dependencies.
#'
#' @param method Installation method ("auto", "virtualenv", "conda").
#' @param conda Path to conda executable (or "auto").
#' @param version Version of the package to install.
#' @param pip If TRUE, use pip for installation.
#' @param force If TRUE, force reinstallation even if already present.
#' @param ... Additional arguments passed to \code{reticulate::py_install}.
#'
#' @export
install_megadetector <- function(
  method = "auto",
  conda = "auto",
  version = "default",
  pip = TRUE,
  force = FALSE,
  ...
) {
  if (!force && reticulate::py_module_available("megadetector")) {
    cli::cli_alert_success("MegaDetector python package is already installed.")
    return(invisible(TRUE))
  }

  cli::cli_progress_step("Installing MegaDetector python package...")

  # Core dependencies that are best installed via conda if possible
  conda_deps <- c(
    "pandas",
    "requests",
    "matplotlib",
    "seaborn",
    "scikit-learn",
    "numpy",
    "pillow",
    "pyyaml",
    "tqdm",
    "humanfriendly"
  )

  # If we are using conda, try to install these first
  # This helps leverage pre-compiled binaries and avoid pip compiling from source
  if (
    method == "conda" ||
      (method == "auto" &&
        !is.null(tryCatch(reticulate::conda_binary(), error = function(e) {
          NULL
        })))
  ) {
    # Check if we are in an environment that already has torch (like rocker/ml)
    # If so, we do NOT want to reinstall/upgrade it via pip or conda if possible
    has_torch <- reticulate::py_module_available("torch")

    tryCatch(
      {
        reticulate::conda_install(
          packages = conda_deps,
          conda = conda,
          pip = FALSE,
          ...
        )
      },
      error = function(e) {
        cli::cli_alert_warning(
          "Failed to install conda dependencies: {e$message}"
        )
      }
    )
  }

  package <- "megadetector"
  if (version != "default") {
    package <- paste0("megadetector==", version)
  }

  # Install megadetector via pip
  # We use pip = TRUE explicitly.
  reticulate::py_install(
    package,
    method = method,
    conda = conda,
    pip = pip,
    ...
  )

  if (reticulate::py_module_available("megadetector")) {
    cli::cli_progress_done()
    cli::cli_alert_success("MegaDetector installed successfully!")
  } else {
    cli::cli_alert_danger(
      "Installation failed. Please check your python environment."
    )
  }
}
