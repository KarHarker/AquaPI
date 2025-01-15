#' Retrieve list of unique time-series ids
#'
#' @param site A character site number (format "11AA1111")
#' @param username API username required
#' @param password API password required
#' @param output Preferred format of the output ('list' or 'dataframe').
#' Dataframe provides site number with timeseries id, list does not.
#'
#' @returns A list or dataframe of timeseries ID by site number
#' @export
#'
#' @examples extract_list <- ts_unique_ids(site = "08MH0041", "username", "password", output = "list")
#'
retrieve_ts_ids <- function(site, username, password, output = c("list", "dataframe")) {
  ts_url <- paste0("GetTimeSeriesUniqueIdList?LocationIdentifier=", site)

  req_ts <- api_req(ts_url, username, password)

  resp_ts <- req_ts |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    tibble::as_tibble()

  ts_id <- resp_ts |>
    purrr::pluck("TimeSeriesUniqueIds") |>
    dplyr::mutate(site = site)

  if (output == "list") {
    ts_id <- as.list(ts_id$UniqueId)
    ts_id
  } else if (output == "dataframe") {
    ts_id
  }
}



#' Retrieve the approval status and the change in approvals over time by timeseries
#'
#' @param sample_ts The unique ID of a timeseries (several within each site)
#' @param username API username required
#' @param password API password required
#'
#' @returns A dataframe with approval levels and date of approval level change by unique timeseries id
#' @export
#'
#' @examples extract_appr <- retrieve_approvals(
#'   sample_ts = "8a0ed03b7fad4a40904122d3b6d0086f",
#'   "username", "password"
#' )
#'
retrieve_approvals <- function(sample_ts, username, password) {
  approval_url <- paste0("GetApprovalsTransactionList?TimeSeriesUniqueId=", sample_ts)

  approval_data <- httr2::api_req(approval_url, username, password)

  appr_ts_data <- approval_data |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("ApprovalsTransactions") |>
    tibble::as_tibble() |>
    dplyr::mutate(UniqueId = sample_ts, .before = 1)

  appr_ts_data
}
