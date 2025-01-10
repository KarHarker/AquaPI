# api_req_site <- function(dataset, site, username, password) {
#   request(paste0("https://bcmoe-prod.aquaticinformatics.net/AQUARIUS/Publish/v2/",
#                  dataset, "?LocationIdentifier=", site)) |>
#     req_auth_basic(username=username, password=password)
# }

#' Basic Aquarius API request
#'
#' @param url the api endpoint url of interest, character type
#' @param username API username required
#' @param password API password required
#'
#' @returns a BC gov aquarius specific api request
#' @export
#'
#' @examples req <- api_req("ts_url", "username", "password")
api_req <- function(url, username, password) {
  httr2::request(paste0(
    "https://bcmoe-prod.aquaticinformatics.net/AQUARIUS/Publish/v2/",
    url
  )) |>
    httr2::req_auth_basic(username = username, password = password)
}
