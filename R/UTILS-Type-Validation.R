#' Check whether or not a value is numeric or can be coerced to numeric
#' @param value The value to test
#' @param if_empty The value to return if length(value)==0
#' @return \code{TRUE} or \code{FALSE}
#' @export
verify_numeric <- function(value,if_empty=FALSE) {
  if(any(is.null(value))) return(FALSE)
  if(is.numeric(value)&&length(value)==0) return(if_empty)
  tryCatch({value=suppressWarnings(as.numeric(value))
  if(any(is.na(value))) {
    return(FALSE)
  }} , error = function(e) {
    return(FALSE)
  })
  return(value)
}

#' Check whether or not a value is numeric or can be coerced to numeric
#' and is greater than or equal to 0
#' @param value The value to test
#' @param if_empty The value to return if length(value)==0
#' @return \code{TRUE} or \code{FALSE}
#' @export
verify_positive <- function(value,if_empty=TRUE) {
  value=verify_numeric(value,numeric())
  if(length(value)==0) return(if_empty)
  is.numeric(value) && value>=0
}


#' Check whether or not a value is numeric or can be coerced to numeric
#' and is greater than or equal to 0
#' @param value The value to test
#' @param error_msg The error message if the value is not positive
#' @return The value if positive, error otherwise
#' @export
assert_positive <- function(value,error_msg) {
  value=verify_numeric(value,numeric())
  if(!verify_positive(value,TRUE)) stop(error_msg,call.=F)
  value
}

#' Check whether or not a value is numeric or can be coerced to numeric
#' @param value The value to test
#' @param error_msg The error message if the value is not numeric
#' @return The value if numeric, error otherwise
#' @export
assert_numeric <- function(value,error_msg) {
  value=verify_numeric(value,numeric())
  if(!is.numeric(value)) stop(error_msg,call.=F)
  value
}

#' Check whether or not a value is or can be coerced to a character string
#' @param value The value to test
#' @param error_msg The error message if the value is not positive
#' @return The value if character, error otherwise
#' @export
assert_character <- function(value,error_msg) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.character(value)) return(value)
  tryCatch({value=suppressWarnings(as.character(value))
  if(any(is.na(value))) {
    stop(error_msg,call.=F)
  }} , error = function(e) {
    stop(error_msg,call.=F)
  })
  value
}

#' Check whether or not a value is or can be coerced to a list
#' @param value The value to test
#' @param error_msg The error message if the value is not a list
#' @return The value if it's a list, error otherwise
#' @export
assert_list <- function(value,error_msg) {
  if(is.list(value)) return(value)
  if(is.null(value)) return(list())
  if(all(is.na(value))&&length(value)==1) return(list())
  tryCatch({value=suppressWarnings(as.list(value))
  if(any(is.na(value))) {
    stop(error_msg,call.=F)
  }} , error = function(e) {
    stop(error_msg,call.=F)
  })
  value
}

#' Check whether or not a value is a date
#' @param x The value to test
#' @return \code{TRUE} or \code{FALSE}
#' @export
is.Date <- function(x) inherits(x, 'Date')

#' Check whether or not a value is or can be coerced to a data
#' @param value The value to test
#' @param error_msg The error message if the value is not a ate
#' @return The value if it's a date, error otherwise
#' @export
assert_date <- function(value,error_msg) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.Date(value)) return(value)
  tryCatch({value=suppressWarnings(as.Date(value))
  if(any(is.na(value))) {
    stop(error_msg,call.=F)
  }} , error = function(e) {
    stop(error_msg,call.=F)
  })
  value
}

#' Check whether or not a file exists
#' @param file The file to test
#' @param error The error message if the file does not exist
#' @return The file, error if it doesn't exist
#' @export
assert_file <- function(file,error) {
  if(file.exists(file)) return(file)
  stop(error,call.=F)
}

#' Check whether or not a directory exists
#' @param directory The directory to test
#' @param error The error message if the directory does not exist
#' @return The directory, error if it doesn't exist
#' @export
assert_directory <- function(directory,error) {
  if(dir.exists(file)) return(directory)
  stop(error,call.=F)
}

#' Check whether or not a value is or can be coerced to a logial
#' @param value The value to test
#' @param error The error message if the value is not a logical
#' @return The value if it's a logical, error otherwise
#' @export
assert_logical <- function(value,error) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.logical(value)) return(value)
  tryCatch({value=suppressWarnings(as.logical(value))
  if(any(is.na(value))) {
    stop(error_msg,call.=F)
  }} , error = function(e) {
    stop(error_msg,call.=F)
  })
  value
}

#' Check whether or not a value is or can be coerced to a colour
#' @param value The value to test
#' @param error The error message if the value is not a colour
#' @return The value if it's a colour, error otherwise
#' @export
assert_colour <- function(colour,error) {
  #Vectorized
  iscol=sapply(colour, function(X) {tryCatch(is.matrix(col2rgb(X)), error = function(e) FALSE)})
  if(all(iscol)) return(colour)
  stop(error,call.=F)
}

#' Check whether or not a value is or can be coerced to a function
#' @param value The value to test
#' @param error The error message if the value is not a function
#' @return The value if it's a function, error otherwise
#' @export
assert_function <- function(func,error) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.function(func)) return(func)
  if(exists(func, mode='function')) return(call(func))
  stop(error,call.=F)
}
