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
