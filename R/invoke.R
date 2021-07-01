# Invoke QBit
#
# QBit workspaces can be completely controlled via API including not only code
# executions but also tasks like file upload/download, workspace export, etc.
# Note, that this function is intended for internal use only.
#
# @param qbit_id character; Name of the QBit function.
# @param event_type character; Event type to be used. Can be one of the following:
# - qbit-run: Run selected code in qbit.
# - qbit-fork: Create a new QBit from template
# - qbit-remove: Remove QBit
# - qbit-render: Render markdown document.
# - qbit-packages-install: Install packages in qbit
# - qbit-packages-remove: Remove packages from QBit
# - qbit-fetch-result: Fetch result from QBit
# - state-reset: Remove console outputs from QBit
# - qbit-update-state
# - qbit-download-workspace
# - upload-request
# - upload-finished
# - file-delete
# @param event_input list;
# @param stateful logical;
# @param apikey character; API Key used to invoke QBit API endpoint.
# After creating an account at \url{https://www.quantargo.com} the API key is available
# in the user settings \url{https://www.quantargo.com/dashboard}.
# @examples
# \dontrun{
#   options(QKEY = "<YOUR-API-KEY>")
#   qbit:::invoke("qbit-template-r-base", "qbit-run", list(code = "1+1"))
#   qbitnew <- qbit:::invoke("qbit-template-r-base", "qbit-fork", list(qbitId = "qbit-template-r-base", qbitName = "TEST QBit"))
#   qbit:::invoke(qbitnew$newQbitId, "qbit-run", list(code = "1+1")) # TODO: Check if there is console output
#   qbit:::invoke(qbitnew$newQbitId, "qbit-remove", list(qbitId = qbitnew$newQbitId))
#   qbitnew$newQbitId
# }
#' @importFrom httr POST PUT add_headers content
#' @importFrom jsonlite unbox
invoke <- function(qbit_id,
                   event_type,
                   event_input,
                   stateful = FALSE,
                   apikey = getOption("QKEY"),
                   verbose = getOption("verbose")) {

  events_available <- c(
    'qbit-run',
    'qbit-render',
    'qbit-packages-install',
    'qbit-packages-remove',
    'qbit-fetch-result',
    'state-reset',
    'qbit-fork',
    'qbit-remove',
    'qbit-update-state',
    'qbit-download-workspace',
    'upload-request',
    'upload-finished',
    'file-delete'
  )

  stopifnot(event_type %in% events_available)
  stopifnot(!is.null(apikey))

  qbit_url <- getOption("QBITURL", "https://api.quantargo.com/lambda")
  url_invoke <- sprintf("%s/invoke", qbit_url)
  h <- list(`x-api-key` = apikey)

  body_invoke <- list(eventType = event_type,
                      eventInput = event_input,
                      stateful = stateful)

  body_invoke$qbitId <- qbit_id

  body_invoke_json <- jsonlite::toJSON(body_invoke, auto_unbox = TRUE)

  resp_invoke <- POST(url_invoke, do.call(add_headers, h), body = body_invoke_json, encode = "raw")
  resp_invoke_content <- content(resp_invoke, "parsed")
  if (!is.null(resp_invoke_content$error)) {
    stop(sprintf("ERROR: %s", resp_invoke_content$error))
  }
  jsonlite::fromJSON(resp_invoke_content$body)
}

#' Create a New QBit based on template
#'
#' @param qbit_name character; Title to be used for workspace.
#' @param template character; Id of template to be copied/forked from. Can be any id accessible
#' at quantargo, see also \url{https://www.quantargo.com/qbits/explore}
#' @param verbose logical; Show intermediary console outputs.
#' @return Id of created workspace
#' @export
create <- function(qbit_name, template = get_qbit_template("r"), verbose = getOption("verbose")) {
  qbitnew <- invoke(template, "qbit-fork", list(qbitId = template, qbitName = qbit_name))
  qbit_id <- qbitnew$newQbitId
  if(verbose) message(sprintf("New QBit '%s' successfully created.\nVisit https://www.quantargo.com/qbits/%s to view your workspace.", qbit_id, qbit_id))
  qbit_id
}

#' Create a New QBit based on template
#'
#' @param qbit_id character; Workspace Id to be removed.
#' @param verbose logical; Show intermediary console outputs.
#' @return New QBit id
#' @export
remove <- function(qbit_id, verbose = getOption("verbose")) {
  out <- invoke(qbit_id, "qbit-fork", list(qbitId = qbit_id))
  if(verbose) message(sprintf("QBit '%s' successfully removed.", qbit_id))
  invisible(TRUE)
}


#' Get QBit Template
#'
#' Currently the following templates are available via the \code{short_name}:
#'
#' \describe{
#'   \item{r}{Basic R template mapping to \href{https://www.quantargo.com/qbits/qbit-template-r-base}{qbit-template-r-base}}
#'   \item{rmd}{Basic RMarkdown template template mapping to \href{https://www.quantargo.com/qbits/qbit-template-r-rmarkdown}{qbit-template-r-rmarkdown}}
#   \item{python}{Basic Python script template mapping to \href{https://www.quantargo.com/qbits/qbit-template-py-base}{qbit-template-py-base}}
#' }
#'
#' @param short_name character; Short name of the template.
#' @export
get_qbit_template <- function(short_name) {
  named_templates <- c(r = "qbit-template-r-base",
                 rmd = "qbit-template-rmarkdown"
                 #python = "qbit-template-python"
                 )

  named_templates[match(short_name, names(named_templates))]
}



