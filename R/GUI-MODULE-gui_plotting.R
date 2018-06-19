gui_plottingUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shinysky::shinyalert(ns("error"), FALSE,auto.close.after = 5),
    shiny::fluidRow(shiny::column(9,shiny::textInput(ns("toplot"), label = "Selected Items:",width='100%')),
                    shiny::column(3,normal_button(ns('plot'),"Plot",style="width:100%; margin-top: 25px;"))),
    shiny::uiOutput(ns('components'))
  )
}

#inputList <- <reactive> list(list(plotbutton=reactive(),ids=reactive()),...)
gui_plotting <- function(input, output, session,components,inputList) {
  #Add tabs
  output$components<-shiny::renderUI({
    tabList <- list()
    for(i in 1:length(components())) {
      mod<-components()[[i]]
      tabList[[i]]<-shiny::tabPanel(mod$title,mod$ui(session$ns(paste0('component',mod$index))))
    }
    tabList$id = session$ns('tabs')
    tabList$type = 'pills'
    do.call(shiny::tabsetPanel,tabList)
  })

  #Update on "plot" within page
  buttonCounters<-shiny::reactiveValues()
  shiny::observe({
    for(page in inputList()) {
      if(!identical(buttonCounters[[page$name]],page$plotbutton())) {
        buttonCounters[[page$name]]<-page$plotbutton()
        if(page$plotbutton()>0) {
          plotString<-paste(page$name,paste0(page$ids(),collapse=','),sep=':')
          shiny::updateTextInput(session,'toplot',value=plotString)
          for(comp in components()) {
            if(page$name %in% comp$tokens) {
              shiny::updateTabsetPanel(session, 'tabs', selected = comp$title)
            }
          }
        }
      }
    }
  })

  #Parse the IDS from the input
  userIDS<-shiny::reactive({
    inputString<-input$toplot
    if(!length(inputString) || inputString=='') return(list())
    inputString<-sub('^[[:space:]]*','',inputString)
    components<-strsplit(gsub('([A-Za-z]*):[0-9,:]*','\\1',inputString),' ')[[1]]
    values<-strsplit(gsub('([A-Za-z]*):([0-9,: ]*)','\\2||',inputString),'||',fixed=T)[[1]]
    if(!length(components)) return(list())
    plotList<-list()
    for(i in 1:length(components)) {
      ids<-paste0(values[[i]],collapse=' ')
      ids<-as.numeric(unlist(lapply(strsplit(strsplit(ids,',')[[1]],":"), function(x) {
        if(length(x)==1) return(x)
        seq.int(x[[1]],x[[2]])
      })))
      if(length(ids)) plotList[[components[[i]]]]<-ids
    }
    plotList
  })


  hide_error('error',session)
  errorFunc<-function(e) {
    show_error(e,'error',session)
  }

  loadedModules <- new.env(parent = emptyenv())
  shiny::observe({
    for(i in 1:length(components())) {
      mod<-components()[[i]]
      ids<-shiny::reactive(userIDS()[mod$tokens])
      args=list(module=mod$server,id=paste0('component',mod$index),ids,errorFunc)
      #Load modules / get already loaded
      if(!mod$title %in% names(loadedModules)) {
        log.debug('GUI: loading %s plot module',mod$title)
        loadedModules[[mod$title]]<-do.call(shiny::callModule,args)
      }
    }
  })
}
