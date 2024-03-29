% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GUI-MODULE-checkbox_legend.R
\name{checkbox_legendUI}
\alias{checkbox_legendUI}
\alias{checkbox_legend}
\title{Module: Display a legend with checkboxes}
\usage{
checkbox_legendUI(id, header = "Legend")

checkbox_legend(input, output, session, rowNames, colours)
}
\arguments{
\item{id}{Namespace id parameter (must be unique)}

\item{header}{Header for the legend}

\item{input, output, session}{Shiny server parameters}

\item{rowNames}{\code{\link[shiny]{reactive}} vector of legend names}

\item{colours}{\code{\link[shiny]{reactive}} vector of colours}
}
\value{
logical vector corresponding to which of \code{rowNames} are selected
}
\description{
Module: Display a legend with checkboxes
}
\examples{
ui <- checkbox_legendUI("myID")
server <- function(input, output, session) {
  callModule(checkbox_legend, "myID", rowNames=reactive(expr), colours=reactive(expr))
}
}
