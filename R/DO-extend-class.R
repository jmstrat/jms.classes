#' Extend jms.data.object for a custom class
#'
#' Creates the following functions:
#' \itemize{
#' \item <name>.data.object
#' \item as.<name>.data.object
#' \item is.<name>.data.object
#' \item [.<name>.data.object
#' \item read.table.<name>
#' }
#'
#' @details
#' Intended usage is within a package's .onLoad function.
#' If any function already exists within the environment at load time,
#' the function <function>.super will be created instead. This allows
#' the functions created here to be extended (simply call the super
#' method within the new method)
#'
#' @param name The name of the new data type
#' @param xlab The x-axis label for the new data type
#' @param ylab The y-axis label for the new data type
#' @param inherits Name of any additional data types from which this should inherit
#' @param envir The environment within which to define the new functions
#' @export
create_data_type <- function(name,xlab,ylab,inherits=c(),envir=parent.frame(),y2lab=NA) {
  dataObjName=paste0(name,'.data.object')
  inheritsNames=if(length(inherits)) paste0(inherits,'.data.object') else NULL
  asDataObjFun <- function(x) {
    x=as.jms.data.object(x)
    class(x) <- c(dataObjName,inheritsNames,class(x))
    ylab(x)<-ylab
    y2lab(x)<-y2lab
    xlab(x)<-xlab
    x
  }
  asName=paste0('as.',dataObjName)
  if(exists(asName,envir=envir)) asName=paste0(asName,'.super')
  assign(asName,asDataObjFun,envir=envir)

  isDataObjFun <- function(x) {
    return(inherits(x,dataObjName))
  }
  isName=paste0('is.',dataObjName)
  if(exists(isName,envir=envir)) isName=paste0(isName,'.super')
  assign(isName,isDataObjFun,envir=envir)

  dataObjFun <- function(...) {
    return(asDataObjFun(jms.data.object(...)))
  }
  dataObjFunName=dataObjName
  if(exists(dataObjFunName,envir=envir)) dataObjFunName=paste0(dataObjFunName,'.super')
  assign(dataObjFunName,dataObjFun,envir=envir)

  readTableFun <- function(...) {
    return(asDataObjFun(read.table.jms(...)))
  }
  readName=paste0('read.table.',name)
  if(exists(readName,envir=envir)) readName=paste0(readName,'.super')
  assign(readName,readTableFun,envir=envir)

  subsetFun <- function(x,...) {
    #Get the function call, and extract the class name from it (TODO: is there a better way???)
    me=sub('^`\\[\\.(.*?)(\\.super)?`.*$','\\1',deparse(sys.call()))
    #Save the current class
    myclass=class(x)
    i=which(myclass==me)
    #Remove everything preceeding and including the class for which this function was called
    if(length(i)<length(myclass)) class(x)<-myclass[i+1:length(myclass)]
    #Rerun the subset function for the remaining classess
    #(DO NOT USE UseMethod or NextMethod: the former doesn't return to this
    #function, the latter won't work if the "super" version of this function was called)
    r <- `[`(x,...)
    #Restore the original class (unless the subset resulted in e.g. a numeric)
    if(inherits(r,'jms.data.object')) class(r)<-myclass
    return(r)
  }
  subsetName=paste0('[.',dataObjName)
  if(exists(subsetName,envir=envir)) subsetName=paste0(subsetName,'.super')
  assign(subsetName,subsetFun,envir=envir)
}
