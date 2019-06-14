#' Module: Display a dataframe table
#'
#' @param id Namespace id parameter (must be unique)
#' @param input,output,session Shiny server parameters
#' @param dataframe A \code{\link[shiny]{reactive}} data.frame
#' @param display_filter Function to call to filter the data for display
#' @param buttons See \code{\link{addInputsToData}}
#' @return \code{list} with a reactive element \code{count} giving the number of rows selected
#' a reactive \code{ids} to get the ids selected, a reactive \code{row} to get the row selected, and a vector \code{buttonIDs} of reactives to get the button ids pressed
#' @examples
#' ui <- dataframe_table_UI("myID")
#' server <- function(input, output, session) {
#'   callModule(dataframe_table, "myID", data_function=reactive(expr), display_filter=my_function)
#' }
#' @rdname dataframe_table
#' @export
dataframe_table_UI <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("table"))
}

#' @rdname dataframe_table
#' @export
dataframe_table <- function(input, output, session, dataframe, display_filter, buttons=NULL) {
  # To remember which columns shouldn't be escaped
  positions <- c()
  # The output
  output$table <- DT::renderDataTable(DT::datatable({
    data <- display_filter(dataframe())
    positions <- c()
    if (!is.null(buttons)) {
      ret <- addInputsToData(data, buttons, session)
      data <- ret$data
      positions <- ret$htmlCols
    }
    # Ugly, but works...
    assign("positions", positions, parent.frame())
    data
  },
  rownames=TRUE,
  escape=if (length(positions)) -(positions - 1) else TRUE, # 1==row.names
  extensions="Select", # active Select extension
  selection="none", # disable custom implemented selection of DT
  options=list(
    select=list(
      style="os", # set 'os' select style so that ctrl/shift + click is enabled
      items="row"
    ), # items can be cell, row or column
    fixedHeader=TRUE,
    columnDefs=list(list(visible=FALSE, targets=c(0))) # Hide row.names
  ),
  callback=DT::JS(
    "table.on( 'select.dt deselect.dt xhr.dt', function ( e, dt, type, indexes ) {", # react on select, deselect, update events
    "var type = table.select.items();", # get the items setting of Select extension
    "var trows = table[type + 's']({selected: true, order:'index'}).data();",
    "var idx = [];",
    "for(var i=0; i<trows.length; i++){",
    "idx.push(trows[i][0]);", # get the index of selected items
    "}",
    "var DT_id = table.table().container().parentNode.id;", # get the output id of DT
    "Shiny.onInputChange(DT_id + '_rows_selected', idx);", # send the index to input$outputid_selected (convert from 0 to 1 based indexing)
    "})"
  ) # .map(function(val){return ++val;})
  ), server=T) # disable the server-side processing so that the returned row index is the real row number of the input dataframe

  ids <- shiny::reactive({
    selected_rows <- input$table_rows_selected
    dataframe()[selected_rows, "id"]
  })
  buttonReactives <- inputReactives(buttons, input)
  count <- shiny::reactive(length(input$table_rows_selected))
  row <- shiny::reactive({
    dataframe()[input$table_rows_selected[[1]], ]
  })
  return(list(count=count, ids=ids, row=row, buttonIDs=buttonReactives))
}
