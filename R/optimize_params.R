# @export
optimize_params <- function(qbit_id, params,
                             max_lambda = 10L,
                             sleep_time = 10L,
                             verbose = getOption("verbose")) {

  out <- rep(list(list(state = "Queued")), nrow(params))
  queue_idx <- 1:min(max_lambda, nrow(params))

  repeat {
    for (i in queue_idx) {
      if (verbose) cat(sprintf("Processing idx %d...\n", i))
      res <- invoke(qbit_id,
                    event_type = "qbit-run",
                    event_input = list(file = "main.R",
                                       async = TRUE,
                                       convertTablesJSON = TRUE,
                                       params = params[i, ]))
      out[[i]] <- list(
        state = "Pending",
        response = res
      )
    }

    if (sleep_time > 0) {
      if (verbose) cat(sprintf("Sleeping %d seconds...\n", sleep_time))
      Sys.sleep(sleep_time)
    }

    # Try to resolve Pending items
    states <- sapply(out, function(x) x$state)
    states_pending_idx <- which(states == "Pending")
    states_queued_idx <- which(states == "Queued")
    if (length(states_pending_idx) < 1 &&
        length(states_queued_idx) < 1) {
      break
    }
    for (pidx in states_pending_idx) {
      if (verbose) cat(sprintf("Retrieving results for idx %d...\n", pidx))
      res_fetch <- invoke( qbit_id,
                           event_type = "qbit-fetch-result",
                           event_input = list(eventId = out[[pidx]]$response$eventId, waitSync = 0))

      out[[pidx]]$state <- res_fetch$state
      if (res_fetch$state == "Ready") {
        out[[pidx]]$result <- extract_result(res_fetch$consoleOutput)
      }
    }
    states <- sapply(out, function(x) x$state)
    states_empty_idx <- which(states == "Queued")
    states_pending_idx <- which(states == "Pending")
    queue_idx <- integer(0)
    if (length(states_empty_idx) > 0 && length(states_pending_idx) < max_lambda) {
      queue_idx <- states_empty_idx[1:min(max_lambda, length(states_empty_idx))]
    }
  }
  out
}
