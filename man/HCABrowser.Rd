% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HCABrowser.R
\docType{class}
\name{HCABrowser-class}
\alias{HCABrowser-class}
\alias{.HCABrowser}
\alias{HCABrowser}
\alias{getEsQuery}
\alias{undoEsQuery}
\alias{resetEsQuery}
\alias{searchBundles}
\alias{show,HCABrowser-method}
\title{Browse the HCA}
\usage{
HCABrowser(
  host = "dss.data.humancellatlas.org",
  api_url = "https://dss.data.humancellatlas.org/v1/swagger.json",
  per_page = 10,
  authenticate = FALSE
)

getEsQuery(x)

undoEsQuery(x, n = 1L)

resetEsQuery(x)

searchBundles(
  x,
  output_format = c("summary", "raw"),
  replica = c("aws", "gcp"),
  per_page = 100L
)

\S4method{show}{HCABrowser}(object)
}
\arguments{
\item{host}{character(1) path to hca-dcp server}

\item{api_url}{character(1) path to hca-dcp api file}

\item{per_page}{numeric(1). Max number of results to return per page.}

\item{authenticate}{logical(1) authenticate? required for actions
that update the HCA data repostiory (rare).}

\item{x}{An HCABrowser object}

\item{n}{integer(1) the number of filter queries to undo}

\item{output_format}{character(1). Specifies the output
format. Either "summary" or "raw". The default format,
"summary", is a list of UUIDs for bundles that match the
query. Set this parameter to "raw" to get the verbatim JSON
metadata for bundles that match the query.}

\item{replica}{character(1). A replica to fetch form. Can either be
set to "aws" or "gcp".  DEFAULT is "aws".}

\item{object}{An HCAbrowser object to show}
}
\value{
\code{HCABrowser()} returns an object representing the
connection to the HCA DCP DSS.

\verb{getEsQuery() returns a JSON object representing the elastic search query formed by application of }filter()\code{and}select()` to an HCABrowser object.

\code{undoEsQuery()} returns an HCABrowser object with n fewer
queries.

`resetEsQuery()` returns an HCABrowser object with no
    queries.
}
\description{
Use \code{HCABrowser()} to create an object to connect with
the Human Cell Atlas Data Coordination Platform Data Storage
System (HCA DCP DSS).

\code{searchBundles()} searches the HCA DCP DSS for bundles
matching the filters applied to the HCA object.
}
\details{
\code{getEsQuery()}, \code{undoEsQuery()}, and \code{resetEsQuery()}
retrieve, remove the most recent, or remove all filters added
to an \code{HCABrowser} object via \code{filter()}.

\code{searchBundles()} can return \code{summary} or \code{raw}
results. \code{summary} results include the \code{bundle_fqid} (unique
identifier) and \code{bundle_url} (location of a JSON file with
complete bundle information). Up to 500 \code{summary} or 10 \code{raw}
results can be returned per query; use \code{nextResults()} to
'page' through subsequent bundles.
}
\examples{
HCABrowser()

hca <-
    HCABrowser() \%>\%
    filter(organ.text == brain) \%>\%
    filter(organ.text == heart)

hca

str(getEsQuery(hca))

hca \%>\%
    undoEsQuery()

hca \%>\%
    resetEsQuery()

searchBundles(HCABrowser(), per_page = 10L) # All records!
HCABrowser() \%>\%
    filter(files.specimen_from_organism_json.organ.text == "brain") \%>\%
    searchBundles() \%>\%
    as_tibble()

}
\author{
Daniel Van Twisk
}
