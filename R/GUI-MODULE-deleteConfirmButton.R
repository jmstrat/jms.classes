#' Delete button with confirmation
#' @export
deleteConfirmButton <- function(id, ...) {
  shiny::addResourcePath(prefix = 'www', directoryPath = system.file('www', package='jms.classes'))

  shiny::tagList(
    htmltools::singleton(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/deleteConfirmButton.css"),
        shiny::tags$script(src = "www/deleteConfirmButton.js")
      )
    ),
    shiny::tags$button(id = id, class = "delete-confirm-button", `data-val` = 0,
                       shiny::div(class="delete-confirm-icon",
                                  shiny::icon('trash-o'),
                                  shiny::icon('question'),
                                  shiny::icon('check')
                       ),
                       shiny::div(class='delete-confirm-text', shiny::span('Delete')),
                       ...)
  )
}
