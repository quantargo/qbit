get_packages_loaded <- function(x) {
  pkg <- .packages()
  pkg[!pkg %in% c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")]
}
