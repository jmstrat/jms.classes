% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UTILS-Type-Validation.R
\name{assert_file}
\alias{assert_file}
\title{Check whether or not a file exists}
\usage{
assert_file(value, error_msg = "Value must be a file",
  errorifempty = FALSE)
}
\arguments{
\item{value}{The file to test}

\item{error_msg}{The error message if the file does not exist}
}
\value{
The file, error if it doesn't exist
}
\description{
Check whether or not a file exists
}
