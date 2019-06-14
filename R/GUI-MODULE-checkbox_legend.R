#' Module: Display a legend with checkboxes
#'
#' @param id Namespace id parameter (must be unique)
#' @param input,output,session Shiny server parameters
#' @param header Header for the legend
#' @param rowNames \code{\link[shiny]{reactive}} vector of legend names
#' @param colours \code{\link[shiny]{reactive}} vector of colours
#' @return logical vector corresponding to which of \code{rowNames} are selected
#' @examples
#' ui <- checkbox_legendUI("myID")
#' server <- function(input, output, session) {
#'   callModule(checkbox_legend, "myID", rowNames=reactive(expr), colours=reactive(expr))
#' }
#' @rdname checkbox_legend
#' @export
checkbox_legendUI <- function(id, header="Legend") {
  ns <- shiny::NS(id)
  shiny::div(shiny::h4(header), shiny::tableOutput(ns("table")), style="overflow-y:auto; max-height: 600px")
}

#' @rdname checkbox_legend
#' @export
checkbox_legend <- function(input, output, session, rowNames, colours) {
  ids_value <- shiny::reactiveValues(ids=c())
  output$table <- shiny::renderTable({
    inputs <- list(
      list(
        name="checkbox",
        id="checkbox",
        ids=1:length(rowNames()),
        template=checkbox,
        column=1
      ),
      list(
        name="hr",
        id="",
        ids=grDevices::rgb(t(grDevices::col2rgb(colours()) / 255)),
        template=hr,
        column=3
      )
    )
    if (!length(rowNames())) {
      return(data.frame())
    }
    shiny::isolate({
      ids_value$ids <- rep_len(TRUE, length(rowNames()))
    })
    addInputsToData(data.frame(id=rowNames()), inputs, session)$data
  },
  spacing=c("xs"), width="100%", align="rcl",
  rownames=FALSE, colnames=FALSE, sanitize.text.function=function(x) x
  )

  shiny::observeEvent(input$checkbox, {
    components <- strsplit(input$checkbox, "_")[[1]]
    changedID <- as.numeric(components[[length(components) - 1]])
    ids_value$ids[[changedID]] <- !ids_value$ids[[changedID]]
  })

  ids <- shiny::reactive(ids_value$ids)
  return(ids)
}
