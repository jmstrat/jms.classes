#' Read data files
#'
#' This function reads one or more data files in a directory
#' @param path The path to the file / directory. If missing the contents of the clipboard are used.
#' @param func The function to read the file
#' @inheritParams load.directory
#' @param ... Additional parameters are passed to func
#' @return A jms.data.object containing the data
#' @examples
#' load.jms('/path/to/directory', load_function, ext='ext')
#' load.jms('/path/to/file.ext', load_function)
#' load.jms(c('/path/to/file.ext','/path/to/file2.ext'))
#' @export load.jms
load.jms <- function(path,func,ext=NULL,pattern=NULL, sort=FALSE,...) {
  if(missing(path)) path = clipboard_to_path()
  dat=c()
  for(p in path) {
    if(dir.exists(p)) dat=c(dat,load.directory(p,func,ext,pattern, sort,...))
    else if(file.exists(p)) dat=c(dat,list(load.file(p,func,...)))
    else stop(p, ' not found')
  }
  combine(as.list(dat))
}

#' Read a data file
#'
#' This function reads one data file
#' @param path The path to the file / directory
#' @param func The function to read the file
#' @return A jms.data.object containing the data
#' @examples
#' load.file('/path/to/file.ext', load_function)
#' @keywords internal
load.file <- function(path,func,...) {
  if(!file.exists(path)) stop(path,' not found')
  data = func(path,...)
  attr(data,'filepath')<-path
  attr(data,'filename')<-basename(path)
  data
}

#' Read every file in a directory
#'
#' This function reads every .ext file in a directory
#' @param dir The path to the directory
#' @param func The loader function
#' @param ext The file extension to look for
#' @param pattern The pattern to look for
#' @param sort If TRUE then use a "natural" numerical sort (see \code{\link[Plotting.Utils]{list.files.sorted}}), else use the default alphabetical sort
#' @details One of ext or pattern must be specified
#' @return A list containing the data
#' @examples
#' load_dir_as_list('/path/to/directory',<function>,ext='ext')
#' @keywords internal
#' @seealso \code{\link{load_directory}} \code{\link{list.files.sorted}}
load.directory <- function(dir,func, ext=NULL,pattern=NULL, sort=FALSE,...) {
  if(is.null(pattern)) {
    if(is.null(ext)) stop('One of ext or pattern must be specified')
    pattern=paste0('.*\\.',ext,'$')
  }
  sorter = if(sort) list.files.sorted else list.files
  file_list <- sorter(path=dir,full.names=TRUE,pattern=pattern)
  data=lapply(file_list,function(x) func(x,...))
}

