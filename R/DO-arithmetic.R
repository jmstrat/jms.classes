#' Add a number to a data objects y values
#'
#' @param e1 The data object
#' @param e2 The number to add
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`+.jms.data.object` <- function(e1, e2){
  if(length(e2)>1) {
    if(length(e2)!=length(ycol(e1))) {
      warning("Unsupported length for addition vector, using defaults -- do not expect sensible results!")
    } else {
      e2=matrix(rep(e2,each=nrow(e1)),nrow(e1),length(ycol(e1)))
    }
  }
  e1[,ycol(e1)]<-.Primitive("+")(as.data.frame(e1)[,ycol(e1)],e2)
  e1
}

#' Subtract a number to a data objects y values
#'
#' @param e1 The data object
#' @param e2 The number to subtract
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`-.jms.data.object` <- function(e1, e2){
  if(length(e2)>1) {
    if(length(e2)!=length(ycol(e1))) {
      warning("Unsupported length for subtraction vector, using defaults -- do not expect sensible results!")
    } else {
      e2=matrix(rep(e2,each=nrow(e1)),nrow(e1),length(ycol(e1)))
    }
  }
  e1[,ycol(e1)]<-.Primitive("-")(as.data.frame(e1)[,ycol(e1)],e2)
  e1
}

#' Multiply a data objects y values by a number
#'
#' @param e1 The data object
#' @param e2 The number by which to multiply
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`*.jms.data.object` <- function(e1, e2){
  if(length(e2)>1) {
    if(length(e2)!=length(ycol(e1))) {
      warning("Unsupported length for multiplication vector, using defaults -- do not expect sensible results!")
    } else {
      e2=matrix(rep(e2,each=nrow(e1)),nrow(e1),length(ycol(e1)))
    }
  }
  e1[,ycol(e1)]<-.Primitive("*")(as.data.frame(e1)[,ycol(e1)],e2)
  e1
}

#' Divide a data objects y values by a number
#'
#' @param e1 The data object
#' @param e2 The number by which to divide
#' @return The data object
#' @rdname arithmetic.jms
#' @export
`/.jms.data.object` <- function(e1, e2){
  if(length(e2)>1) {
    if(length(e2)!=length(ycol(e1))) {
      warning("Unsupported length for division vector, using defaults -- do not expect sensible results!")
    } else {
      e2=matrix(rep(e2,each=nrow(e1)),nrow(e1),length(ycol(e1)))
    }
  }
  e1[,ycol(e1)]<-.Primitive("/")(as.data.frame(e1)[,ycol(e1)],e2)
  e1
}
