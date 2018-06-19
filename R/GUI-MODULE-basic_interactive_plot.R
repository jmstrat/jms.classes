#' Module: Display a zoomable plot
#'
#' @param id Namespace id parameter (must be unique)
#' @param input,output,session Shiny server parameters
#' @param header Header for the legend
#' @param plotFunction Function to produce a plot
#' @param ... Extra parameters are passed to \code{plotFunction}
#' @examples
#' ui <- basicInteractivePlotUI('myID')
#' server <- function(input, output, session) {
#'     callModule(basicInteractivePlot, "myID",plotFunction=myFunction)
#' }
#' @rdname interactivePlot
#' @export
basicInteractivePlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(shinysky::shinyalert(ns("error"),auto.close.after = 5),
             shiny::plotOutput(ns('plot'), height = 400,
                               dblclick = ns('dblclick'),
                               brush = shiny::brushOpts(
                                 id = ns('brush'),
                                 resetOnNew = TRUE
                               )
             ))
}

#' @rdname interactivePlot
#' @export
basicInteractivePlot <- function(input,output,session,plotFunction,...) {
  hide_error('error',session)
  plot_range <- shiny::reactiveValues(x = NULL, y = NULL)
  output$plot <- shiny::renderPlot({
    tryCatch({
      graphics::par(mar=c(3,4.1,0.5,2.1))
      xlim=plot_range$x
      ylim=plot_range$y
      plotFunction(...,xlim=xlim,ylim=ylim)
    }, error = function(e) {
      show_error(e,'error',session)
    })
  })
  # zoom to the brush bounds; double click, reset the zoom.
  shiny::observeEvent(input$dblclick,{
    plot_range$x<-NULL
    plot_range$y<-NULL
  })
  shiny::observe({
    brush <- input$brush
    if (!is.null(brush)) {
      plot_range$x <- c(brush$xmin, brush$xmax)
      plot_range$y <- c(brush$ymin, brush$ymax)
    }
  })
}
