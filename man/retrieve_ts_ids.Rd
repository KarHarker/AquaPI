% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{retrieve_ts_ids}
\alias{retrieve_ts_ids}
\title{Retrieve list of unique time-series ids}
\usage{
retrieve_ts_ids(site, username, password, output = c("list", "dataframe"))
}
\arguments{
\item{site}{A character site number (format "11AA1111")}

\item{username}{API username required}

\item{password}{API password required}

\item{output}{Preferred format of the output ('list' or 'dataframe').
Dataframe provides site number with timeseries id, list does not.}
}
\value{
A list or dataframe of timeseries ID by site number
}
\description{
Retrieve list of unique time-series ids
}
\examples{
extract_list <- ts_unique_ids(site = "08MH0041", "username", "password", output = "list")

}
