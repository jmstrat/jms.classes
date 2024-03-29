% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UTILS-Type-Validation.R
\name{assert_character}
\alias{assert_character}
\title{Check whether or not a value is or can be coerced to a character string}
\usage{
assert_character(value, error_msg = "Value must be a character string",
  allow_na = FALSE)
}
\arguments{
\item{value}{The value to test}

\item{error_msg}{The error message if the value is not positive}
}
\value{
The value if character, error otherwise
}
\description{
Check whether or not a value is or can be coerced to a character string
}
