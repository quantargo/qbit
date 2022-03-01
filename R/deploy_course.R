#' Deploy Quantargo Course
#'
#' After the course build process is finished the generated json files can be
#' uploaded through this function.
#'
#' @param course_id character; Name of the QBit function.
#' @param path character; Root path of course.
#' @param index list; Index meta data for qbit.
#' @param json_files character; list of json files to be uploaded through endpoint
#' @param asset_files character; files to be uploaded as course assets.
#' @param apikey character; API key from QBit platform.
#' @param tmpdir character; Temporary directory to be used for qbit/zip creation.
#' @param url character; Upload url to be used
#' @importFrom httr POST PUT add_headers content
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @export
deploy_course <- function(
  course_id,
  path = ".",
  index = read_yaml(file.path(path, "index.yml")),
  json_files = get_json_files(path),
  asset_files = get_asset_files(path),
  apikey = getOption("QKEY"),
  url = getOption("QBITURL", "https://api.quantargo.com/v2"),
  tmpdir = file.path(tempdir(), course_id)) {

  index$moduleId <- course_id
  index$contentId <- course_id

  h <- list(`x-api-key` = apikey)
  url_upload <- paste0(url, "/courses/", course_id, "/upload")

  stopifnot(length(json_files) > 0)
  contents <- do.call(c, lapply(json_files, function(x) read_json(x)))
  contents <- c(contents,
                read_yaml(file.path(path, "badge.yml")),
                read_yaml(file.path(path, "contents.yml"))
  )
  contents <- contents[sapply(contents, function(x) x[["moduleId"]]) == course_id]

  body_upload <- list(index = index,
                      files = contents)

  body_upload_json <- jsonlite::toJSON(body_upload, auto_unbox = TRUE)
  message("*** Upload File")
  resp_upload <- POST(url_upload, do.call(add_headers, h), body = body_upload_json, encode = "raw")

  resp_upload_content <- content(resp_upload, "parsed")
  upload_url <- resp_upload_content$uploadUrl

  zip_file <- NULL
  files_to_zip <- sub("^\\./", "", asset_files)

  if (length(files_to_zip) > 0) {
    on.exit(unlink(tmpdir))
    zip_file <- file.path(tmpdir, sprintf("%s.zip", course_id))
    target_files <- file.path(tmpdir, files_to_zip)
    out_dir_create <- sapply(unique(dirname(target_files)), dir.create, recursive = TRUE)
    file.copy(files_to_zip, target_files)
    zip_dir(zip_file, files_to_zip, within_dir=tmpdir)
  }

  if (!is.null(upload_url) && !is.null(zip_file)) {
    resp_upload_file <- PUT(upload_url,
                            body = httr::upload_file(zip_file))
    stopifnot(resp_upload_file$status == 200)
  }

  message("*** Start Deploy")
  url_deploy <- paste0(url, "/courses/", course_id, "/deploy")

  resp_deploy <- POST(url_deploy, do.call(add_headers, h),
                      body = list(async = 1),
                      encode = "json")

  event_id <- content(resp_deploy)$eventId

  if (resp_deploy$status_code == 200) {
    wait_for_event(course_id, event_id, url, h)
    message(sprintf("Course '%s' successfully deployed!", course_id))
  } else {
    message(resp_deploy)
    stop("An error occured deploying the course")
  }

  invisible(TRUE)
}
