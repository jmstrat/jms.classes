force_numeric <- function(value,if_empty=numeric()) {
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
#' @param value The value to test
#' @param if_empty The value to return if length(value)==0
#' @return \code{TRUE} or \code{FALSE}
#' @export
verify_numeric <- function(value) {
  value=force_numeric(value,FALSE)
  if(!identical(value,FALSE)) return(TRUE)
  FALSE
}

#' Check whether or not a value is numeric or can be coerced to numeric
#' @param value The value to test
#' @param error_msg The error message if the value is not numeric
#' @return The value if numeric, error otherwise
#' @export
assert_numeric <- function(value,error_msg='Value must be a number') {
  value=force_numeric(value,numeric())
  if(identical(value,FALSE)) stop(error_msg,call.=F)
  value
}

#' Check whether or not a value is numeric or can be coerced to numeric
#' and is greater than or equal to 0
#' @param value The value to test
#' @param if_empty The value to return if length(value)==0
#' @return \code{TRUE} or \code{FALSE}
#' @export
verify_positive <- function(value,if_empty=TRUE) {
  value=force_numeric(value,numeric())
  if(length(value)==0) return(if_empty)
  is.numeric(value) && value>=0
}


#' Check whether or not a value is numeric or can be coerced to numeric
#' and is greater than or equal to 0
#' @param value The value to test
#' @param error_msg The error message if the value is not positive
#' @return The value if positive, error otherwise
#' @export
assert_positive <- function(value,error_msg='Value must be a positive number') {
  value=assert_numeric(value,error_msg)
  if(!verify_positive(value,TRUE)) stop(error_msg,call.=F)
  value
}

#' Check whether or not a value is or can be coerced to a character string
#' @param value The value to test
#' @param error_msg The error message if the value is not positive
#' @return The value if character, error otherwise
#' @export
assert_character <- function(value,error_msg='Value must be a character string',allow_na=FALSE) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.character(value)) return(value)
  tryCatch({value=suppressWarnings(as.character(value))
  if(any(is.na(value)) && ! allow_na) {
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
assert_list <- function(value,error_msg='Value must be a list') {
  if(is.list(value)) return(value)
  if(is.null(value)) stop(error_msg,call.=F)
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
assert_date <- function(value,error_msg='Value must be a Date') {
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
#' @param value The file to test
#' @param error_msg The error message if the file does not exist
#' @return The file, error if it doesn't exist
#' @export
assert_file <- function(value,error_msg='Value must be a file',errorifempty=FALSE) {
  value=assert_character(value,error_msg,allow_na=TRUE)
  if(!length(value)) {
    if(errorifempty) stop(error_msg,call.=F)
    return(character())
  }
  nas=is.na(value)
  exists <- sapply(value[!nas], file.exists)
  if(all(exists)) {
    value[!nas]<-cannonicalPath(value[!nas])
    return(value)
  }
  for(file in value[!exists]) warning(file,' is not a valid file path',.call=FALSE)
  stop(error_msg,call.=F)
}

#' Check whether or not a directory exists
#' @param value The directory to test
#' @param error_msg The error message if the directory does not exist
#' @return The directory, error if it doesn't exist
#' @export
assert_directory <- function(value,error_msg='Value must be a directory') {
  if(dir.exists(value)) return(value)
  stop(error_msg,call.=F)
}

#' Check whether or not a value is or can be coerced to a logial
#' @param value The value to test
#' @param error_msg The error message if the value is not a logical
#' @return The value if it's a logical, error otherwise
#' @export
assert_logical <- function(value,error_msg='Value must be logical') {
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
#' @param error_msg The error message if the value is not a colour
#' @return The value if it's a colour, error otherwise
#' @export
assert_colour <- function(value,error_msg='Value must be a colour') {
  #Vectorized
  iscol=sapply(value, function(X) {tryCatch(is.matrix(grDevices::col2rgb(X)), error = function(e) FALSE)})
  if(all(iscol)) return(value)
  stop(error_msg,call.=F)
}

#' Check whether or not a value is or can be coerced to a function
#' @param value The value to test
#' @param error_msg The error message if the value is not a function
#' @return The value if it's a function, error otherwise
#' @export
assert_function <- function(value,error_msg) {
  if(any(is.null(value))) stop(error_msg,call.=F)
  if(is.function(value)) return(value)
  if(exists(value, mode='function')) return(call(value))
  stop(error_msg,call.=F)
}
