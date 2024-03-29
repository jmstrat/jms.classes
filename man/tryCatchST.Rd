% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UTILS-tryCatchST.R
\name{tryCatchST}
\alias{tryCatchST}
\alias{formatST}
\title{TryCatch with a stack trace}
\usage{
tryCatchST(expr, error)

formatST(st)
}
\arguments{
\item{expr}{expression to execute}

\item{error}{function of form error(e, stack_trace)}
}
\description{
TryCatch with a stack trace
}
