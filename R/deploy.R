#' Deploy QBit Workspace
#'
#' QBit workspaces can be deployed with a (pre-existing) \code{qbit_id} and a
#' specified \code{main_file} containing the source code of a qbit. Additional files
#' containing, e.g. data are included through the \code{files} parameter.
#'
#' @param qbit_id character; Name of the QBit function.
#' @param main_file character; Main file (either main.R or main.Rmd) to be used
#' for QBit.
#' @param index list; Index meta data for qbit
#' @param files character; Files to be part of Qbit
#' @param meta list; Meta data of the QBit object.
#' @param source_main_file character; Specify if file shall be sourced before uploading.
#' This setting can be helpful to create the required environment.
#' @param packagesLoaded character; Packages namespaces to be loaded (through library)
#' @param apikey character; API Key used to invoke QBit API endpoint.
#' After creating an account at \url{https://www.quantargo.com} the API key is available
#' in the user settings \url{https://www.quantargo.com/dashboard}.
#' @param url character; Upload url to be used
#' @param obj character; Names of objects to be included into qbit environment
#' @param eval_env environment; Environment from which R objects shall be taken
#' @param package_lock character; Package lock file
#' @param verbose logical; Specify if output shall be shown
#' @param tmpdir character; Temporary directory to be used for qbit/zip creation.
#' @param timeout numeric; QBit timeout in seconds.
#' @param usagePlan character; Usage plan of Qbit, can be either either public or private.
#' @param secrets list; List of provided secrets to be used for workspace.
#' @examples
#' \dontrun{
#'   deploy('qbit-example-landing-page', main_file = 'main.R')
#'   deploy("qbit-tidymodels-model-selection-used-cars",
#'          main_file = "main.Rmd",
#'          files = c("README.md",
#'                    "dt_tuned.rds",
#'                    "rf_tuned.rds",
#'                    "audi.csv"),
#'          index = yaml::read_yaml("index.yml"),
#'          meta = list(type="qbit"),
#'          timeout = 300)
#' }
#' @importFrom httr POST PUT add_headers content
#' @importFrom rmarkdown render
#' @importFrom renv dependencies
#' @importFrom utils capture.output str
#' @importFrom jsonlite unbox
#' @export
deploy <- function(qbit_id,
                   main_file = empty_main_file(extension = "R"),
                   files = NULL,
                   index = NULL,
                   meta = NULL,
                   source_main_file = TRUE,
                   eval_env = .GlobalEnv,
                   obj = ls(envir = eval_env),
                   timeout = 60,
                   package_lock = "renv.lock",
                   packagesLoaded = dependencies()$Package,
                   apikey = getOption("QKEY"),
                   usagePlan = "public",
                   url = getOption("QBITURL", "https://api.quantargo.com/v2"),
                   verbose = getOption("verbose"),
                   tmpdir = tempdir(),
                   secrets = NULL) {

  stopifnot(!is.null(apikey))
  stopifnot(file.exists(main_file))
  stopifnot(main_file %in% c("main.R", "main.Rmd", "main.py"))
  files <- unique(c(files, main_file))
  stopifnot(usagePlan %in% c("public", "private"))
  packagesLoaded <- unique(packagesLoaded[!packagesLoaded %in% "renv"])

  if (!is.null(index$timeout)) {
    timeout = index$timeout
  }

  if(source_main_file) {
    fext <- tolower(tools::file_ext(main_file))
    if(fext == "r") {
      source(main_file)
    } else if (fext == "rmd") {
      rmarkdown::render(main_file)
    } else if (fext == "py") {
      # Do nothing
    } else {
      stop(sprintf("File extension '%s' not supported.", fext))
    }
  }

  env_file <- NULL
  env <- NULL
  if(!is.null(obj) && length(obj) > 0) {
    env_fname <- ".RData"
    env_file <- env_fname
    dir.create(dirname(env_file), recursive = TRUE, showWarnings = FALSE)
    save(list = obj, envir = eval_env, file = env_file, compression_level=9)
    env <- lapply(obj, function(x) capture.output(str(get(x,
                                                               envir = eval_env))))
    names(env) <- obj
  }

  if (!is.null(index$image)) {
    files <- unique(c(files, index$image))
  }

  files_to_zip <- c(files, env_file)

  zip_file <- NULL
  if (length(files_to_zip) > 0) {
    zip_file <- file.path(tmpdir, sprintf("%s.zip", qbit_id))
    sapply(files_to_zip, function(f) file.copy(f, tmpdir, recursive = TRUE))
    zip_dir(zip_file, basename(files_to_zip), within_dir=tmpdir)
  }

  # Upload zip file to landing zone
  message("*** Starting Upload")
  h <- list(`x-api-key` = apikey)
  url_upload <- paste0(url, "/qbits/", qbit_id, "/upload")

  pkg <- jsonlite::read_json(package_lock)

  qbitFiles <- NULL
  if (length(files) > 0) {
    qbitFiles <- lapply(files, function(x) {
      ### Generate QBit Main file
      out <- list(
        contentId = unbox(paste0(qbit_id, sprintf("#files#%s", x))),
        moduleId = unbox(qbit_id),
        contentType = unbox("file"),
        name = unbox(x)
      )
      if (tools::file_ext(x) %in% c("R", "Rmd", "md", "py")) {
        out$content <- paste(readLines(x), collapse = "\n")
      }
      out
    })
  }

  body_upload <- list(meta = meta,
                      packages = pkg,
                      timeout = timeout)

  body_upload$files = qbitFiles
  body_upload$env = env
  body_upload$packagesLoaded = packagesLoaded
  index$usagePlan = usagePlan
  body_upload$index = index
  body_upload$secrets = secrets

  body_upload_json <- jsonlite::toJSON(body_upload, auto_unbox = TRUE)
  message("*** Upload File")
  resp_upload <- POST(url_upload, do.call(add_headers, h), body = body_upload_json, encode = "raw")

  resp_upload_content <- content(resp_upload, "parsed")
  upload_url <- resp_upload_content$uploadUrl
  if (!is.null(upload_url) && !is.null(zip_file)) {
      resp_upload_file <- PUT(upload_url,
                              body = httr::upload_file(zip_file))
      stopifnot(resp_upload_file$status == 200)
  }

  message("*** Start Deploy")
  url_deploy <- paste0(url, "/qbits/", qbit_id, "/deploy")
  body_deploy <- list(qbitId = qbit_id)
  resp_deploy <- POST(url_deploy, do.call(add_headers, h),
                      body = body_deploy,
                      encode = "json")


  if (resp_deploy$status_code == 200) {
    message(sprintf("QBit '%s' successfully deployed!", qbit_id))
    invisible(TRUE)
    #return(resp_upload_content$id)
  } else {
    message(resp_deploy)
    stop("An error occured uploading the function.")
  }
}


get_json_files <- function(path = ".") {
  out <- list.files(path, pattern = "[^(renv)]*.json", recursive = TRUE, full.names = TRUE)
  grep("^[^renv]", out, perl = TRUE, value = TRUE)
}

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
#' @importFrom httr POST PUT add_headers content
#' @importFrom yaml read_yaml
#' @importFrom jsonlite read_json
#' @export
deploy_course <- function(
  course_id,
  path = ".",
  index = read_yaml(file.path(path, "index.yml")),
  json_files = get_json_files(path),
  asset_files = list.files(path, pattern = "[^(html)|(Rmd)|(json)|(.DS_Store)]$", full.names = TRUE),
  apikey = getOption("QKEY"),
  tmpdir = tempdir()) {

  index$moduleId <- course_id
  index$contentId <- course_id

  h <- list(`x-api-key` = apikey)
  url_upload <- paste0(url, "/courses/", course_id, "/upload")

  stopifnot(length(contents) < 1)
  contents <- lapply(contents, read_json())
  body_upload <- list(index = index,
                      files = contents)

  body_upload_json <- jsonlite::toJSON(body_upload, auto_unbox = TRUE)
  message("*** Upload File")
  resp_upload <- POST(url_upload, do.call(add_headers, h), body = body_upload_json, encode = "raw")

  resp_upload_content <- content(resp_upload, "parsed")
  upload_url <- resp_upload_content$uploadUrl

  zip_file <- NULL
  files_to_zip <- asset_files
  if (length(files_to_zip) > 0) {
    zip_file <- file.path(tmpdir, sprintf("%s.zip", course_id))
    sapply(files_to_zip, function(f) file.copy(f, tmpdir, recursive = TRUE))
    zip_dir(zip_file, basename(files_to_zip), within_dir=tmpdir)
  }


  if (!is.null(upload_url) && !is.null(zip_file)) {
    resp_upload_file <- PUT(upload_url,
                            body = httr::upload_file(zip_file))
    stopifnot(resp_upload_file$status == 200)
  }

  message("*** Start Deploy")
  url_deploy <- paste0(url, "/courses/", course_id, "/deploy")
  resp_deploy <- POST(url_deploy, do.call(add_headers, h),
                      encode = "json")


  if (resp_deploy$status_code == 200) {
    message(sprintf("Course '%s' successfully deployed!", course_id))
    invisible(TRUE)
    #return(resp_upload_content$id)
  } else {
    message(resp_deploy)
    stop("An error occured uploading the function.")
  }


}
