#' Module: Add / Update / Delete button control
#'
#' Create an input button whose type depends upon a reactive value
#'
#' @param id Namespace id parameter (must be unique)
#' @param input,output,session Shiny server parameters
#' @param type \code{\link[shiny]{reactive}} value to determine the state. 0 == add button, 1 == update and delete buttons, >1 == disabled update and enabled delete buttons
#' @param callback Function to call when a button is pressed
#' @examples
#' ui <- add_update_delete_buttonUI("myID")
#' server <- function(input, output, session) {
#'   callModule(add_update_delete_button, "myID", type=reactive(expr), callback=my_function)
#' }
#' @rdname add_update_delete_button
#' @export
add_update_delete_buttonUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("button_UI"))
}

#' @rdname add_update_delete_button
#' @export
add_update_delete_button <- function(input, output, session, type, callback) {
  output$button_UI <- shiny::renderUI({
    ns <- session$ns
    if (type() == 0) {
      normal_button(ns("add"), label="Add", style="width:100%; margin-top: 25px;")
    } else if (type() == 1) {
      list(
        normal_button(ns("update"), label="Update", style="width:61%; margin-top: 25px;box-sizing: border-box;"),
        delete_button(ns("delete"), style="width:35%; margin-top: 25px;box-sizing: border-box;")
      )
    }
    else {
      list(
        normal_button(ns("update"), label="Update", disabled="true", style="width:61%; margin-top: 25px;box-sizing: border-box;"),
        delete_button(ns("delete"), style="width:35%; margin-top: 25px;box-sizing: border-box;")
      )
    }
  })

  shiny::observeEvent(input$add, callback("add"))
  shiny::observeEvent(input$update, callback("update"))
  shiny::observeEvent(input$delete, callback("delete"))

  shiny::setBookmarkExclude(c("add", "update", "delete"))
}
