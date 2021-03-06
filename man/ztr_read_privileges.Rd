% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_read.R
\name{ztr_read_privileges}
\alias{ztr_read_privileges}
\title{Read The User Id And Privileges Of The Given Api Key}
\usage{
ztr_read_privileges(zotero_object, timeout = 20)
}
\arguments{
\item{zotero_object}{\code{Zotero} object.}

\item{timeout}{Seconds for the timeout of request (default: \code{20}).}
}
\value{
\code{httr::response} object
}
\description{
Read The User Id And Privileges Of The Given Api Key
}
\examples{
\dontrun{
ztr <- Zotero$new("ztr4r")
ztr \%>\%
  ztr_read_privileges()
}
}
