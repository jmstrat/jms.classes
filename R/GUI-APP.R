server <- function(input, output, session) {
  jms.logging.setnamespace('GUI')
  #TODO: eventually need register_gui_page and register_gui_plot_component or something...

  config<-as.jms.reactive.database(database2reference(config_db),reactiveSession=session,reactiveUpdateFreq=4000)
  persistent_settings <- config[['persistent_settings']]
  #Load the tables as reactives:

  #TEMP: the initialisation should be done elsewhere...
  pages_table <- tryCatch({
    config[['GUI_Pages']]
  }, error = function(e) {
    config[['GUI_Pages']]<-jms.database.table.id(namespace=character(),ui=character(),server=character(),title=character(),databases=character(),enabled=logical())
    config[['GUI_Pages']]
  })

  plotcomponents_table <- tryCatch({
    config[['GUI_PlotComponents']]
  }, error = function(e) {
    config[['GUI_PlotComponents']]<-jms.database.table.id(namespace=character(),ui=character(),server=character(),title=character(),tokens=character(),enabled=logical())
    config[['GUI_PlotComponents']]
  })

  #Then we extract the data into the following form:
  # list(
  #   list(ui=cell_database_diplayUI,server=cell_database_diplay,title='Cells',databases=c('echem_cell','echem_film')),
  #   list(ui=film_database_diplayUI,server=film_database_diplay,title='Films',databases=c('echem_film','echem_capacity'))
  # )
  databasecomponents<-shiny::reactive({
    comps=list()
    i=0
    for(row in 1:nrow(pages_table())) {
      if(!pages_table()[row,'enabled']) next()
      i=i+1
      newcomp<-list()
      namespace<-pages_table()[row,'namespace']
      if(is.na(namespace)) {
        namespace <- parent.frame()
      } else {
        namespace <- loadNamespace(namespace)
      }
      newcomp$ui<-get(pages_table()[row,'ui'],envir=namespace)
      newcomp$server<-get(pages_table()[row,'server'],envir=namespace)
      newcomp$title<-pages_table()[row,'title']
      newcomp$databases<-strsplit(pages_table()[row,'databases'],',')[[1]]
      newcomp$index=plotcomponents_table()[row,'id']
      comps[[i]]<-newcomp
    }
    comps
  })

  # list(
  #   list(ui=plot_echemUI,server=plot_echem,title='Echem',tokens=c('cells','films'))
  # )
  plotcomponents<-shiny::reactive({
    comps=list()
    i=0
    for(row in 1:nrow(plotcomponents_table())) {
      if(!plotcomponents_table()[row,'enabled']) next()
      i=i+1
      newcomp<-list()
      namespace<-plotcomponents_table()[row,'namespace']
      if(is.na(namespace)) {
        namespace <- parent.frame()
      } else {
        namespace <- loadNamespace(namespace)
      }
      newcomp$ui<-get(plotcomponents_table()[row,'ui'],envir=namespace)
      newcomp$server<-get(plotcomponents_table()[row,'server'],envir=namespace)
      newcomp$title<-plotcomponents_table()[row,'title']
      newcomp$tokens<-strsplit(plotcomponents_table()[row,'tokens'],',')[[1]]
      newcomp$index=plotcomponents_table()[row,'id']
      comps[[i]]<-newcomp
    }
    comps
  })

  #Load the main tab bar UI
  output$modulesUI <- shiny::renderUI({
    tabs<-list()
    len<-length(databasecomponents())
    if(len) {
      for(i in 1:len) {
        mod=databasecomponents()[[i]]
        log.debug('GUI: loading UI for %s module',mod$title)
        tabs[[i]]<-shiny::tabPanel(mod$title,mod$ui(paste0('module',mod$index)))
      }
    } else {
      i=0
    }
    tabs[[i+1]]<-shiny::tabPanel("Plot",gui_plottingUI('plottab'))
    title<-get_key('GUI_Title',persistent_settings())
    if(!is.null(title)) {
      tabs$title=title
    } else {
      tabs$title="Cell Database"
    }
    tabs$id='mainTabs'
    tabs$theme = "www/cerulean.css"
    do.call(shiny::navbarPage,tabs)
  })

  #Load the databases
  projectDB <- as.jms.reactive.database(project_database(),reactiveSession=session,reactiveUpdateFreq=4000)

  loadedDBTables <- new.env(parent = emptyenv())
  loadedModules <- new.env(parent = emptyenv())
  #Call the server functions for each tab
  plotList <- shiny::reactive({
    out<-list()
    len<-length(databasecomponents())
    #Loop over enabled modules
    if(len) {
      for(i in 1:len) {
        mod=databasecomponents()[[i]]
        args=list(module=mod$server,id=paste0('module',mod$index))
        #Load databases / get already loaded
        for(name in mod$databases) {
          if(!name %in% names(loadedDBTables)) {
            log.debug('GUI: loading database table %s',name)
            loadedDBTables[[name]]<-projectDB[[name]]
          }
          args[[name]]<-loadedDBTables[[name]]
        }
        #Load modules / get already loaded
        if(!mod$title %in% names(loadedModules)) {
          log.debug('GUI: loading %s module',mod$title)
          loadedModules[[mod$title]]<-do.call(shiny::callModule,args)
        }
        out[[i]] <- loadedModules[[mod$title]]
      }
    }
    out
  })

  #Switch to plot tab on "plot" within page
  shiny::observe({
    buttons<-lapply(plotList(),function(x) x$plotbutton)
    shiny::observe(for(b in buttons) if(length(b()) && b()>0) shiny::updateNavbarPage(session, 'mainTabs', selected = 'Plot'))
  })

  #Load the plotting interface
  log.debug('GUI: loading plotting module')
  shiny::callModule(gui_plotting,'plottab',plotcomponents,plotList)
}


#' Echem GUI
#'
#' @param port The port for the server to run on
#' Display a modular graphical user interface
#' @export
gui <- function(port=5000) {
  if(!requireNamespace("shiny", quietly=TRUE)) stop('GUI requires the shiny package to be installed')
  if(!requireNamespace("DT", quietly=TRUE)) stop('GUI requires the DT package to be installed')
  if(!requireNamespace("shinyFiles", quietly=TRUE)) stop('GUI requires the shinyFiles package to be installed')
  if(!requireNamespace("shinyBS", quietly=TRUE)) stop('GUI requires the shinyBS package to be installed')
  if(!requireNamespace("shinysky", quietly=TRUE)) stop('GUI requires the shinysky package to be installed:\ndevtools::install_github("AnalytixWare/ShinySky")')

  ui <- shiny::tagList(shiny::tags$head(shiny::tags$script(shiny::HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.code);});')),
                                        shiny::uiOutput('modulesUI')))
  shiny::addResourcePath("sbs", system.file("www", package = "shinyBS"))
  shiny::addResourcePath('www', system.file('www', package='jms.classes'))
  shiny::shinyApp(ui = ui, server = server, options=list(port=port))
}
