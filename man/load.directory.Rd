% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DO-load.R
\name{load.directory}
\alias{load.directory}
\title{Read every file in a directory}
\usage{
\method{load}{directory}(dir, func, ext = NULL, pattern = NULL,
  sort = FALSE, ...)
}
\arguments{
\item{dir}{The path to the directory}

\item{func}{The loader function}

\item{ext}{The file extension to look for}

\item{pattern}{The pattern to look for}

\item{sort}{If TRUE then use a "natural" numerical sort (see \code{\link{list.files.sorted}}), else use the default alphabetical sort}
}
\value{
A list containing the data
}
\description{
This function reads every .ext file in a directory
}
\details{
One of ext or pattern must be specified
}
\examples{
load.directory("/path/to/directory", load_function, ext="ext")
}
\seealso{
\code{\link{list.files.sorted}}
}
\keyword{internal}
