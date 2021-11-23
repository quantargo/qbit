#' @export
extract_result <- function(console_output, idx = length(console_output)) {
  # Extract table results
  tab <- jsonlite::read_json(console_output[[idx]]$content$content)
  df <- data.frame(do.call(rbind, tab$data), row.names = 1)
  colnames(df) <- unlist(tab$colnames)[-1]
  df <- data.frame(lapply(df, unlist))
  suppressWarnings(df$value <-as.numeric(df$value))
  df
}
