#' @importFrom utils zip
zip_dir <- function(zip_file, files_to_zip, within_dir = NULL, verbose = FALSE, ...) {
  if (!is.null(within_dir)) {
    mydir <- getwd()
    setwd(within_dir)
    on.exit(setwd(mydir))
    files_to_zip <- sub("^/", "", sub(within_dir, "", files_to_zip))
  }
  zip_out <- zip(zip_file, files_to_zip)
  if (verbose) message(sprintf("Zip file '%s' successfully generated.", zip_file))
  invisible(zip_file)
}
