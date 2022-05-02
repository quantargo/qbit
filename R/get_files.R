get_json_files <- function(path = ".") {
  out <- list.files(path, pattern = "[^(renv)]*.json", recursive = TRUE, full.names = TRUE)
  grep("./renv", out, fixed = TRUE, value = TRUE, invert = TRUE)
}

get_asset_files <- function(path = ".") {
  out <- list.files(path, pattern = "[^(html)|(Rmd)|(json)|(.DS_Store)]$", recursive = TRUE, full.names = TRUE)
  out <- grep("_cache", out, fixed = TRUE, value = TRUE, invert = TRUE)
  out <- grep("./renv", out, fixed = TRUE, value = TRUE, invert = TRUE)
  out <- grep("./qbit.*", out, value = TRUE, invert = TRUE)
  out <- grep("./script_.*\\.py", out, value = TRUE, invert = TRUE)
  out
}
