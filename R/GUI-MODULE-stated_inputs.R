#' Module: stated input controls
#'
#' Create an input control that is enabled and disabled automatically
#' based on the state parameter.
#'
#' @param id Namespace id parameter (must be unique)
#' @param input,output,session Shiny server parameters
#' @param state \code{\link[shiny]{reactive}} value to determine the state
#' @param value \code{\link[shiny]{reactive}} value to determine the value
#' @param ... Additional parameters passed to
#' \code{\link[shiny]{textInput}} or \code{\link[shiny]{selectInput}}
#' @return The value of the input
#' @examples
#' ui <- stated_textUI("myID", label="My text input")
#' server <- function(input, output, session) {
#'   callModule(stated_text, "myID", state=reactive(expr))
#' }
#' @rdname stated_inputs
#' @export
stated_textUI <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::textInput(ns("managed_input"), ...)
}
#' @rdname stated_inputs
#' @export
stated_text <- function(input, output, session, state, value) {
  shiny::observe({
    s <- state()
    if (s == 0) {
      enableInput("managed_input", session)
      shiny::updateTextInput(session, "managed_input", value="")
    } else if (s == 1) {
      enableInput("managed_input", session)
      shiny::updateTextInput(session, "managed_input", value=value())
    } else {
      disableInput("managed_input", session)
      shiny::updateTextInput(session, "managed_input", value="multiple")
    }
  })
  shiny::reactive(input$managed_input)
}
#' @rdname stated_inputs
#' @export
stated_selectUI <- function(id, ...) {
  ns <- shiny::NS(id)
  shiny::selectInput(ns("managed_input"), ...)
}
#' @rdname stated_inputs
#' @export
stated_select <- function(input, output, session, state, value) {
  shiny::observe({
    s <- state()
    if (s == 0) {
      enableSelect("managed_input", session)
    } else if (s == 1) {
      enableSelect("managed_input", session)
      shiny::updateSelectInput(session, "managed_input", selected=value())
    } else {
      disableSelect("managed_input", session)
    }
  })
  shiny::reactive(input$managed_input)
}

#' @rdname stated_inputs
#' @export
stated_buttonUI <- function(id, ...) {
  ns <- shiny::NS(id)
  normal_button(ns("managed_input"), ...)
}
#' @rdname stated_inputs
#' @export
stated_button <- function(input, output, session, state) {
  shiny::observe({
    s <- state()
    if (s == 0) {
      enableInput("managed_input", session)
    } else if (s == 1) {
      enableInput("managed_input", session)
    } else {
      disableInput("managed_input", session)
    }
  })
  shiny::reactive(input$managed_input)
}
