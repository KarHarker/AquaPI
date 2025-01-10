#' Retrieves field visit dataset that provides information on length of time for user upload
#'
#' @param site The site name for retrieving field visit information
#' @param username API username required
#' @param password API password required
#'
#' @returns A dataframe that provides the number of days from field visit to upload
#' and number of site visits conducted each year.
#' @export
#'
#' @examples field_data = extract_fv_info("08MH0041", "username", "password")
#'
extract_fv_info <- function(site, username, password) {
  fv_url <- "GetFieldVisitDataByLocation"

  site_specific_url <- paste0(fv_url, "?LocationIdentifier=", site)

  req_fv <- api_req(url = site_specific_url, username = username, password = password)

  resp_fv <- req_fv |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("FieldVisitData") |>
    tibble::as_tibble() |>
    # unnest(col = Attachments) |>
    tidyr::unnest(
      cols = c(
        Attachments, ControlConditionActivity, Approval,
        InspectionActivity, LevelSurveyActivity, CompletedWork
      ),
      names_repair = "universal"
    )
  # pluck('Approval') |>
  # glimpse()

  fv_db <- resp_fv |>
    dplyr::select(
      Identifier, LocationIdentifier, ApprovalLevel, LevelDescription,
      StartTime, EndTime, DateUploaded, UploadedByUser
    ) |>
    dplyr::mutate(
      StartTime = lubridate::as_date(StartTime),
      EndTime = lubridate::as_date(EndTime),
      DateUploaded = lubridate::as_date(DateUploaded),
      Year = as.numeric(lubridate::year(StartTime))
    ) |>
    dplyr::group_by(Identifier, LocationIdentifier, StartTime, EndTime) |>
    dplyr::slice_head() |>
    dplyr::mutate(
      uploadtime = DateUploaded - EndTime,
      days = round(as.numeric(uploadtime, units = "days"), digits = 2)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(Year) |>
    dplyr::mutate(n_per_year = n())
}
