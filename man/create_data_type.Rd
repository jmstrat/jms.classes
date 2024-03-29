% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DO-extend-class.R
\name{create_data_type}
\alias{create_data_type}
\title{Extend jms.data.object for a custom class}
\usage{
create_data_type(name, xlab, ylab, inherits = c(),
  envir = parent.frame(), y2lab = NA)
}
\arguments{
\item{name}{The name of the new data type}

\item{xlab}{The x-axis label for the new data type}

\item{ylab}{The y-axis label for the new data type}

\item{inherits}{Name of any additional data types from which this should inherit}

\item{envir}{The environment within which to define the new functions}
}
\description{
Creates the following functions:
\itemize{
\item <name>.data.object
\item as.<name>.data.object
\item is.<name>.data.object
\item [.<name>.data.object
\item read.table.<name>
}
}
\details{
Intended usage is within a package's .onLoad function.
If any function already exists within the environment at load time,
the function <function>.super will be created instead. This allows
the functions created here to be extended (simply call the super
method within the new method)
}
