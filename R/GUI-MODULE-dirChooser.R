# N.B. https://github.com/rstudio/shiny/issues/2101

#' Module: JS Dir Chooser
#' @export
#' @rdname JS_dirChooser
JS_dirChooser <- function(input, output, session, state, updateFreq=2000) {
  stop("NOT IMPLEMENTED")
}

#' @inheritParams shinyFiles::shinyFilesButton
#' @export
#' @rdname JS_fileChooser
JS_dirChooserUI <- function(id, label, title, multiple, ..., buttonType="default", class=NULL) {
  ns <- shiny::NS(id)
  ### Changed js file
  shiny::tagList(
    htmltools::singleton(
      shiny::tags$head(
        shiny::tags$script(src="www/shinyFiles-mod.js"), ### CHANGED THIS TO REPORT OPEN / CLOSED
        shiny::tags$link(rel="stylesheet", type="text/css", href="sF/styles.css"),
        shiny::tags$link(rel="stylesheet", type="text/css", href="sF/fileIcons.css")
      )
    ),
    shiny::tags$button(
      id=ns("button"), type="button",
      class=paste(c("shinyDirectories btn", paste0("btn-", buttonType), class), collapse=" "),
      `data-title`=title, as.character(label)
    )
  )
}


#' Module: RStudio Dir Chooser
#'
#' @return The value of the input
#' @examples
#' ui <- rstudio_dirChooserUI("myID", label="Select Directory")
#' server <- function(input, output, session) {
#'   callModule(rstudio_dirChooser, "myID")
#' }
#' @param input,output,session Shiny server parameters
#' @param state \code{\link[shiny]{reactive}} value to determine the state
#' @export
#' @rdname rstudio_dirChooser
rstudio_dirChooser <- function(input, output, session, state) {
  shiny::observe({
    s <- state()
    if (s == 0) {
      enableInput("button", session)
    } else if (s == 1) {
      enableInput("button", session)
    } else {
      disableInput("button", session)
    }
  })
  chosenDir <- reactiveValues()
  observeEvent(input$button, {
    chosenDir$dir <- rstudioapi::selectDirectory("Select Directory", "Select", path=path.expand("~"))
  })
  reactive({
    chosenDir$dir
  })
}

#' @export
#' @rdname rstudio_fileChooser
rstudio_dirChooserUI <- function(id, label, ..., buttonType="default", class=NULL) {
  ns <- shiny::NS(id)
  shiny::actionButton(ns("button"), as.character(label), class=paste(c(paste0("btn btn-", buttonType), class), collapse=" "), ...)
}

#' Module: Dir Chooser
#'
#' Calls either \code{\link{JS_dirChooser}} or \code{\link[rstudioapi]{selectDir}}
#' depending on whether it is used in an Rstudio instance
#' @return The value of the input
#' @examples
#' ui <- dirChooserUI("myID", label="Select directory", title="Please select a directory")
#' server <- function(input, output, session) {
#'   callModule(dirChooser, "myID")
#' }
#' @param input,output,session Shiny server parameters
#' @param state \code{\link[shiny]{reactive}} value to determine the state
#' @export
#' @rdname dirChooser
dirChooser <- function(input, output, session, state, updateFreq=2000) {
  shiny::setBookmarkExclude(c("button"))
  if (rstudioapi::isAvailable()) {
    # Use RStudio's file chooser
    rstudio_dirChooser(input, output, session, state)
  } else {
    # Fallback to js
    JS_dirChooser(input, output, session, state, updateFreq)
  }
}

#' @export
#' @rdname dirChooser
dirChooserUI <- function(id, label, title, multiple, ..., buttonType="default", class=NULL) {
  if (rstudioapi::isAvailable()) {
    rstudio_dirChooserUI(id, label, ..., buttonType=buttonType, class=class)
  } else {
    # Fallback to js
    JS_dirChooserUI(id, label, title, multiple, ..., buttonType=buttonType, class=class)
  }
}
