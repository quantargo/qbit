#' Create Empty Main File
#' 
#' Create an empty file named main.<extension> to be used as default for 
#' qbit deployments.
#' @param extension character; File extension to be used for main file
#' @export 
empty_main_file <- function(extension = "R") {
  fname <- sprintf("main.%s", extension)
  create_out <- file.create(fname)
  stopifnot(!create_out)
  fname
}
