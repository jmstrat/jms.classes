#' Module: layout input
#'
#' Create an input control that allows a user to determine a plot layout. \cr
#' Returns a list of nPlots: <int>, matrix: <matrix>, input: <input for default_layout>\cr
#' The matrix can be used with the \code{\link{layout}} command.
#'
#'
#' @rdname layout_module
#' @export
layoutInput <- function(id, default_layout, plot_names, can_add_plots, min_plots) {
  shiny::addResourcePath(prefix="www", directoryPath=system.file("www", package="jms.classes"))

  buttons <- shiny::tagList()
  if (can_add_plots) {
    buttons <- shiny::tags$div(
      class="controls",
      shiny::tags$a(id="add-button", href="#", "Add Plot"),
      shiny::tags$span("|"),
      shiny::tags$a(id="remove-button", href="#", "Remove Plot")
    )
  }

  shiny::tagList(
    htmltools::singleton(
      shiny::tags$head(
        shiny::tags$link(rel="stylesheet", type="text/css", href="www/gridstack.min.css"),
        shiny::tags$link(rel="stylesheet", type="text/css", href="www/layoutInput.css"),
        shiny::tags$script(src="www/jquery-ui.min.js"),
        shiny::tags$script(src="www/lodash.min.js"),
        shiny::tags$script(src="www/gridstack.min.js"),
        shiny::tags$script(src="www/gridstack.jQueryUI.min.js"),
        shiny::tags$script(src="www/layoutInput.js")
      )
    ),
    shiny::tags$div(
      id=id, class="layout-input",
      buttons,
      shiny::tags$div(
        class="grid-stack",
        "data-min-plots"=min_plots,
        "data-plot-names"=shiny:::toJSON(plot_names),
        "data-default-layout"=shiny:::toJSON(default_layout)
      )
    )
  )
}
