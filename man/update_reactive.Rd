% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GUI-Utils.R
\name{update_reactive}
\alias{update_reactive}
\title{Force a reactive value to update (useful for \code{\link[shiny]{reactivePoll}} if the
check function indicates the data hasn't changed, but the cached data needs updating anyway)}
\usage{
update_reactive(r)
}
\arguments{
\item{r}{The reactive}
}
\description{
Force a reactive value to update (useful for \code{\link[shiny]{reactivePoll}} if the
check function indicates the data hasn't changed, but the cached data needs updating anyway)
}
