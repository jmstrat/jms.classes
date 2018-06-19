#' Module: File Chooser
#'
#' Modified version of \code{\link[shinyFiles]{shinyFileChoose}} adding support for
#' working and home directories, and disabling refresh when the dialogue is closed.
#'
#' @inherit shinyFiles::shinyFileChoose
#' @return The value of the input
#' @examples
#' ui <- fileChooserUI('myID', label='File select', title='Please select a file', multiple=FALSE)
#' server <- function(input, output, session) {
#'     callModule(fileChooser, "myID")
#' }
#' @param input,output,session Shiny server parameters
#' @param state \code{\link[shiny]{reactive}} value to determine the state
#' @inheritParams shinyFiles::fileGetter
#' @export
#' @rdname fileChooser
fileChooser <- function (input, output, session, state, filetypes=NULL, updateFreq = 2000) {
  ###Modified to save working directory
  working_path <- shiny::reactiveValues(path = NULL)
  wp<- function() {return(shiny::isolate(working_path$path))}
  path<-shiny::reactive({
    out=input$button
    path<-as.character(shinyFiles::parseFilePaths(getVolumes(wp),out)$datapath)
    if(length(path)) {
      working_path$path<-path
      return(path)
    } else {
      return(NULL)
    }
  })

  shiny::observe({
    s=state()
    if(s==0) {
      enableInput("button",session)
    } else if(s==1) {
      enableInput("button",session)
    } else {
      disableInput("button",session)
    }
  })

  fileGet <- shinyFiles:::fileGetter(root=getVolumes(path),filetypes=filetypes)
  currentDir <- list()
  shiny::observe({
    dir <- input$`button-modal`
    if (is.null(dir) || is.na(dir)) {
      dir <- list(dir = "")
    } else {
      dir <- list(dir = dir$path, root = dir$root)
    }
    dir$dir <- do.call(file.path, as.list(dir$dir))
    newDir <- do.call("fileGet", dir)
    newDir$files<-newDir$files[!is.na(newDir$files[,'mtime']),] ### WORKAROUND FOR BUG ###
    if (!identical(currentDir, newDir)) {
      currentDir <- newDir
      session$sendCustomMessage("shinyFiles", list(id = session$ns('button'), dir = newDir)) ###add ns
    }
    state=input$`button-state`
    if(!is.null(state)&&state=='open') shiny::invalidateLater(updateFreq, session) ### CHANGED THIS TO ONLY RUN IF OPEN
  })
  return(path)
}

#' @inheritParams shinyFiles::shinyFilesButton
#' @export
#' @rdname fileChooser
fileChooserUI <- function (id, label, title, multiple, ..., buttonType = "default", class = NULL)
{
  ns<-shiny::NS(id)
  ###Changed js file
  shiny::tagList(htmltools::singleton(
    shiny::tags$head(
      shiny::tags$script(src = "www/shinyFiles-mod.js"),### CHANGED THIS TO REPORT OPEN / CLOSED
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "sF/styles.css"),
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "sF/fileIcons.css")
    )
  ),
  shiny::tags$button(id=ns('button'), type = "button", class = paste(c("shinyFiles btn", paste0("btn-", buttonType), class), collapse = " "),
                     `data-title` = title, `data-selecttype` = ifelse(multiple,"multiple", "single"), ..., as.character(label))
  )
}

getVolumes <- function (current_path=NULL,exclude=NULL) {
  ###Modified to add home and working directory
  if (missing(exclude))
    exclude <- NULL
  function() {
    osSystem <- Sys.info()["sysname"]
    if (osSystem == "Darwin") {
      volumes <- list.files("/Volumes/", full.names = T)
      names(volumes) <- basename(volumes)
      volumes=c(home=path.expand("~"),volumes)
    }
    else if (osSystem == "Linux") {
      volumes <- c(Computer = "/")
      media <- list.files("/media/", full.names = T)
      names(media) <- basename(media)
      volumes <- c(volumes, media)
      volumes=c(home=path.expand("~"),volumes)
    }
    else if (osSystem == "Windows") {
      volumes <- system("wmic logicaldisk get Caption",
                        intern = T)
      volumes <- sub(" *\\r$", "", volumes)
      keep <- !tolower(volumes) %in% c("caption", "")
      volumes <- volumes[keep]
      volNames <- system("wmic logicaldisk get VolumeName",
                         intern = T)
      volNames <- sub(" *\\r$", "", volNames)
      volNames <- volNames[keep]
      volNames <- paste0(volNames, ifelse(volNames == "",
                                          "", " "))
      volNames <- paste0(volNames, "(", volumes, ")")
      names(volumes) <- volNames
    }
    else {
      stop("unsupported OS")
    }
    if (!is.null(exclude)) {
      volumes <- volumes[!names(volumes) %in% exclude]
    }
    if(!is.null(current_path)) {
      path=current_path()
      if(!is.null(path)) {
        name=basename(dirname(path))
        dir=dirname(path)
        volumes=c(dir,volumes)
        names(volumes)[[1]]<-name
      }
    }
    volumes
  }
}
