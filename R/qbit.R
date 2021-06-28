# Functions to interact with qbits
#
# @param fun_unserial function; Function to be used to de-serialize object from QBit.
# @param url character; URL to be used for QBit call.
# @rdname qbit
# @importFrom httr POST
# @importFrom jsonlite serializeJSON unserializeJSON
# qbit.call <- function(fun,
#                       ...,
#                       fun_unserial = unserializeJSON,
#                       apikey = getOption("QKEY"),
#                       url = getOption("QBITURL", "https://api.quantargo.com/lambda")) {
#   payload <- serializeJSON(list(...))
#   h <- list(`x-api-key` = apikey)
#   url <- paste0(url, "/", fun)
#   #body_upload <- list(name = name, packages = packages)
#   resp_post <- POST(url, do.call(add_headers, h), body = payload, encode = "raw")
#   res <- resp_post$status_code
#   if (resp_post$status_code == 200) {
#     res <- fun_unserial(rawToChar(resp_post$content))
#   } else {
#     msg <- sprintf("STATUS %d: %s", resp_post$status_code, content(resp_post))
#     stop(msg)
#   }
#   res
# }

# @rdname qbit
# @importFrom jsonlite fromJSON
# qbit.run <- function(fun, code) {
#   fromJSON(qbit.call(fun,
#             QBIT_RUN_CODE = code,
#             fun_unserial = identity))
# }
