test_that("MegaDetector loads and detects objects", {
  skip_if_not_installed("reticulate")

  # Ensure python dependency is present (install in temp path if feasible, or skip)
  if (!reticulate::py_module_available("megadetector")) {
    skip("MegaDetector python package not available")
  }

  # Load model (mocks download if needed or uses cache)
  model <- load_model()
  expect_true(!is.null(model))

  # Use bundled image
  img_path <- system.file("img", "Caltech_Animal.jpg", package = "megadetector")
  expect_true(file.exists(img_path))

  # Run detection
  detections <- detect_image(model, img_path)
  expect_true(is.list(detections))

  # Check classification
  # Animal image should be classified as animal
  class_label <- get_classification(detections)
  expect_equal(class_label, "animal")

  # Check confidence score logic
  if (length(detections) > 0) {
    expect_true(detections[[1]]$conf > 0.5)
  }
})

test_that("MegaDetector classifies vehicle images correctly", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("megadetector")) {
    skip("MegaDetector python package not available")
  }

  model <- load_model()
  img_path <- system.file(
    "img",
    "Caltech_Vehicle.jpg",
    package = "megadetector"
  )
  expect_true(file.exists(img_path))

  detections <- detect_image(model, img_path)
  class_label <- get_classification(detections)
  expect_equal(class_label, "vehicle")

  # Vehicles should have reasonable confidence
  if (length(detections) > 0) {
    expect_true(detections[[1]]$conf > 0)
  }
})

test_that("MegaDetector classifies empty images correctly", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("megadetector")) {
    skip("MegaDetector python package not available")
  }

  model <- load_model()
  img_path <- system.file("img", "Caltech_Empty.jpg", package = "megadetector")
  expect_true(file.exists(img_path))

  detections <- detect_image(model, img_path)
  class_label <- get_classification(detections)
  expect_equal(class_label, "empty")

  # Empty images should have no detections or very low confidence
  expect_true(length(detections) == 0 || detections[[1]]$conf < 0.5)
})

test_that("MegaDetector handles all demo images like demo.R", {
  skip_if_not_installed("reticulate")

  if (!reticulate::py_module_available("megadetector")) {
    skip("MegaDetector python package not available")
  }

  model <- load_model()

  # Test all three images from demo.R
  image_files <- c(
    "Caltech_Animal.jpg",
    "Caltech_Vehicle.jpg",
    "Caltech_Empty.jpg"
  )
  expected_classes <- c("animal", "vehicle", "empty")

  for (i in seq_along(image_files)) {
    img_path <- system.file("img", image_files[i], package = "megadetector")
    expect_true(
      file.exists(img_path),
      info = sprintf("Image file %s exists", image_files[i])
    )

    detections <- detect_image(model, img_path)
    expect_true(
      is.list(detections),
      info = sprintf("Detections for %s is a list", image_files[i])
    )

    class_label <- get_classification(detections)
    expect_equal(
      class_label,
      expected_classes[i],
      info = sprintf("Classification for %s", image_files[i])
    )
  }
})
