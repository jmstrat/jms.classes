#' Present an error dialog to the user
#'
#' This function is called to display an error to the user.
#' @param e The error text
#' @param errorid The ID of the error dialog box
#' @param session The session object
#' @export
#' @rdname errors
show_error <- function(e,errorid, session) {
  shinysky::showshinyalert(session, session$ns(errorid), paste0("ERROR: ",e), styleclass = "danger")
  log.error(e)
}

#' @export
#' @rdname errors
hide_error <- function(errorid, session) {
  shinysky::hideshinyalert(session, session$ns(errorid))
}

#' Create a normal push button
#'
#' @param id The ID for the button
#' @param label The label for the button
#' @param ... Further tags to set on the button
#' @export
#' @rdname buttons
normal_button <-function(id,label,...) {
  shiny::actionButton(id, label = label,...)
}

#' Create a push button with the delete icon
#' @export
#' @rdname buttons
delete_button <-function(id,...) {
  shiny::tags$button(id = id, type = "button", class = "btn action-button btn-danger", shiny::icon('trash-o'), '',...)
}

#' Create a push button with the info icon
#' @export
#' @rdname buttons
info_button <- function(id,...) {
  shiny::tags$button(id=id,type="button", class = "btn btn-default",'i',style="font-weight:bold;font-family: \"Times New Roman\", Times, serif;", ...)
}

#' Create a checkbox input with support for extra tags
#' @param id The ID for the input
#' @param ... Further tags to set on the input
#' @export
checkbox <- function(id,...,checked=TRUE) {
  if(checked)
    shiny::tags$input(id=id,type="checkbox",checked="checked",...)
  else
    shiny::tags$input(id=id,type="checkbox",...)
}

#' Make a horizontal rule
#' @param css colour
#' @export
hr <- function(col,...) {
  shiny::tags$hr(style=paste0("width: 30px; height: auto; border: 1px solid ",col,"; margin: 0; margin-top: 8px; padding:0; vertical-align: middle"))
}

#' Make a vector of shiny inputs
#' @param input_id The input that will be called when any input is clicked
#' @param ids Which id will be set for each button
#' @param templateFunction Function to make a button
#' @return The vector of buttons
#' @keywords internal
#' @rdname inputs
make_inputs <- function(input_id,ids,templateFunction,...,session=NULL) {
  input_id2 <- if(!is.null(session)) session$ns(input_id) else input_id
  template<-templateFunction('%s',...,counter=0,onclick = paste0('this.setAttribute(\"counter\",parseInt(this.getAttribute(\"counter\"))+1);Shiny.onInputChange(\"',input_id2,'\",  this.id+\"_\"+this.getAttribute(\"counter\"))'))
  ### -->
  # Slightly faster version of as.character
  html <- htmltools:::paste8("<", template$name, sep = "")
  attribs <- lapply(template$attribs, as.character)
  for (attrib in names(attribs)) {
    attribValue <- attribs[[attrib]]
    text <- htmltools::htmlEscape(attribValue, attribute = TRUE)
    html<-htmltools:::paste8(html," ", attrib, "=\"", text, "\"", sep = "")
  }
  children <- htmltools:::dropNullsOrEmpty(htmltools:::flattenTags(template$children))
  html<-paste0(html,'>')
  if(length(children))
    html<-htmltools:::paste8(html,htmltools:::normalizeText(children[[1]]), "</", template$name, ">", sep = "")
  ### <--
  # Much faster than looping the above, but make sure %s is the only placeholder!
  if(input_id!='')
    sprintf(html,paste0(input_id2, '_',ids))
  else
    sprintf(html,ids)
}

#' @param inputs see details
#' @details
#' One or more columns of inputs may be added to the table via the \code{inputs} parameter. \cr
#' It takes a \code{list} consisting of the following elements: \cr
#' \code{list( \cr
#'     name=character(),   <- column name for the input \cr
#'     id=character(),     <- input called when any input is clicked \cr
#'     template=function(),<- function that returns a input \cr
#'     column=numeric(),   <- column in which to put the input \cr
#'     [ids=data[,id]],    <- ids associated with the inputs (length = nrow(data)) \cr
#'     [... -- additional tags for the input])} \cr
#'  Buttons are generated using \code{make_inputs}.
#' @return \code{list(data=<data with extra columns>, htmlCols=<column #s with HTML>)}
#' @examples
#' inputs=list(
#' list(
#'   name='test',
#'   id='checkbox',
#'   ids=rownames(data),
#'   template=checkbox,
#'   column=1
#' ),
#' list(
#'   name='hr',
#'   id='',
#'   ids=rgb(t(col2rgb(plotrix::color.gradient(c(1,0),0,c(0,1),nslices=nrow(mydata)))/255)),
#'   template=hr,
#'   column=3
#' )
#' )
#' @keywords internal
#' @rdname inputs
addInputsToData <- function(data,inputs,session=NULL) {
  positions=c()
  if(!is.null(inputs)) {
    ncol_before=ncol(data)
    for(input in inputs) {
      id=input$id
      template=input$template
      ids <- if(!is.null(input$ids)) input$ids else data[,'id']
      varArgs=input[!names(input)%in%c('name','id','template','column','ids')]
      inputColumn=do.call(make_inputs,list(input_id=id,templateFunction=template,ids=ids,varArgs,session=session))
      #add the column to the end
      data<-cbind(data,inputColumn)
      colnames(data)[ncol(data)]<-input$name
      positions=c(positions,input$column)
    }
    #Reorder
    data<-as.data.frame(data[,c(1:ncol_before,1:length(positions)+ncol_before)[order(c(1:ncol_before,positions-0.5))]])
  }
  list(data=data,htmlCols=positions)
}

#' @keywords internal
#' @rdname inputs
inputReactives <- function(inputs,input) {
  irs=c()
  for(i in inputs) {
    id=i$id
    irs=c(irs,shiny::eventReactive(input[[id]],{
      components=strsplit(input[[id]],'_')[[1]]
      components[[length(components)-1]]
    }))
  }
  irs
}

#' Disable or enable an input
#'
#' @param id The ID for the input
#' @param session The session object
#' @keywords internal
#' @rdname disableInput
disableInput <- function(id,session) session$sendCustomMessage(type="jsCode",list(code= paste("$('#",session$ns(id),"').prop('disabled',true)",sep="")))
#' @rdname disableInput
enableInput <- function(id,session) session$sendCustomMessage(type="jsCode",list(code= paste("$('#",session$ns(id),"').prop('disabled',false)",sep="")))

#' Disable or enable a select input
#' @keywords internal
#' @rdname disableInput
disableSelect <- function(id,session) session$sendCustomMessage(type="jsCode",list(code= paste("$('#",session$ns(id),"').selectize()[0].selectize.disable()",sep="")))
#' @rdname disableInput
enableSelect <- function(id,session) session$sendCustomMessage(type="jsCode",list(code= paste("$('#",session$ns(id),"').selectize()[0].selectize.enable()",sep="")))

#' Force a reactive value to update (useful for \code{\link[shiny]{reactivePoll}} if the
#' check function indicates the data hasn't changed, but the cached data needs updating anyway)
#'
#' @param r The reactive
#' @export
update_reactive <- function(r) {
  attr(r,'observable')$.updateValue()
  attr(r,'observable')$.dependents$invalidate()

  #if the reactive is a reactive poll, we should also update the cookie to prevent a double update
  ofe=environment(environment(r)$.origFunc)
  if(exists('checkFunc',envir=ofe)) {
    rv<-ofe$rv
    valuesEnv<-get('impl',rv)$.values
    value<-ofe$checkFunc()
    assign('cookie',value,envir=valuesEnv)
  }
}
