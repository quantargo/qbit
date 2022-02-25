wait_for_event <- function(user_id, event_id, h) {
  while (TRUE) {
    url_invoke <- sprintf("%s/qbits/%s/invoke/%s", url, user_id, "qbit-fetch-result")
    resp <- POST(url_invoke, do.call(add_headers, h),
                 body = list(eventId = event_id,
                             stateful = 0,
                             waitSync = 0),
                 encode = "json")

    stopifnot(resp$status_code == 200)

    resp_state <- content(resp)$state

    if (resp_state == "Ready") {
      break
    } else if (resp_state == "Pending") {
      sleep_time <- 10
      message(sprintf("Deployment state 'Pending'. Sleeping for %d seconds", sleep_time))
      Sys.sleep(sleep_time)
      next
    } else if (resp_state == "Timeout") {
      stop("Deployment timed out.")
    } else {
      stop(sprintf("Unknown response state '%s'", resp_state))
    }
  }
}
