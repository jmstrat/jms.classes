#' Register a module to form a tab in the GUI
#'
#' @details
#' Modules have a UI and a server function \cr
#' The UI function takes a single parameter \code{id} as per the shiny module documentation \cr
#' The server function takes 3+ parameters, \code{input}, \code{output} and \code{session} plus reactives
#' corresponding to the databases requested. \cr
#' Server functions should return a list with the following elements:
#' \itemize{
#'  \item{"name"}{The name of the token referencing this module's data (see \code{\link{register_gui_database_module}})}
#'  \item{"plotbutton"}{A \code{\link[shiny]{reactive}} referencing the plot button on the page}
#'  \item{"ids"}{A \code{\link[shiny]{reactive}} list of ids to plot}
#' }
#' This function should usually be used within a package's \code{.onLoad} function.
#'
#' @param moduleUI character string representing the name of the ui function in namespace
#' @param moduleServer character string representing the name of the server function in namespace
#' @param title The (user visible) title of the module
#' @param databases The names of the databases (part of \code{\link{project_database}}) that the module requires access to
#' @param namespace The namespace of the ui and server functions
#' @export
register_gui_database_module <- function(moduleUI, moduleServer, title, databases=c(), namespace=NA) {
  # Fetch the table
  table <- gui_pages_table()

  # Prepare the new row
  newrow <- list(
    namespace=namespace,
    ui=moduleUI,
    server=moduleServer,
    title=title,
    databases=paste0(databases, collapse=",")
  )

  # Has the module alread been regiestered
  matches <- title == table[, "title"]
  matches <- matches[!is.na(matches)]
  if (length(matches) && any(matches)) {
    # We replace the row with the new registration
    id <- table[matches, "id"]
    table[id] <- newrow
  } else {
    # Add the module as a new row
    newrow$enabled <- TRUE
    table[] <- newrow
  }
  return()
}

#' Register a module to form a plot tab in the GUI
#'
#' @details
#' Modules have a UI and a server function \cr
#' The UI function takes a single parameter \code{id} as per the shiny module documentation \cr
#' The server function takes 5 parameters, \code{input}, \code{output} and \code{session} plus
#' an \code{ids} reactive representing the \code{ids} to plot and an errorFunction to be called
#' with a message in case of an error. \cr
#' This function should usually be used within a package's \code{.onLoad} function.
#'
#' @param moduleUI character string representing the name of the ui function in namespace
#' @param moduleServer character string representing the name of the server function in namespace
#' @param title The (user visible) title of the module
#' @param token Names for tokens which represent the data plotted by this module
#' @param namespace The namespace of the ui and server functions
#' @export
register_gui_plot_module <- function(moduleUI, moduleServer, title, tokens=c(), namespace=NA) {
  # Fetch the table
  table <- gui_plot_components_table()

  # Prepare the new row
  newrow <- list(
    namespace=namespace,
    ui=moduleUI,
    server=moduleServer,
    title=title,
    tokens=paste0(tokens, collapse=",")
  )

  # Has the module alread been regiestered
  matches <- title == table[, "title"]
  matches <- matches[!is.na(matches)]
  if (length(matches) && any(matches)) {
    # We replace the row with the new registration
    id <- table[matches, "id"]
    table[id] <- newrow
  } else {
    # Add the module as a new row
    newrow$enabled <- TRUE
    table[] <- newrow
  }
  return()
}


gui_pages_table <- function(database=config_db) {
  # Will raise for any error other than table_not_found
  tryCatch({
    database[["GUI_Pages"]]
  }, table_not_found=function(e) {
    log.info("Initialising GUI pages table")
    database[["GUI_Pages"]] <- jms.database.table.id(
      namespace=character(),
      ui=character(),
      server=character(),
      title=character(),
      databases=character(),
      enabled=logical()
    )
    database[["GUI_Pages"]]
  })
}

gui_plot_components_table <- function(database=config_db) {
  # Will raise for any error other than table_not_found
  tryCatch({
    database[["GUI_PlotComponents"]]
  }, table_not_found=function(e) {
    log.info("Initialising GUI plot components table")
    database[["GUI_PlotComponents"]] <- jms.database.table.id(
      namespace=character(),
      ui=character(),
      server=character(),
      title=character(),
      tokens=character(),
      enabled=logical()
    )
    database[["GUI_PlotComponents"]]
  })
}


.enable_disable <- function(name, tableName, enabled) {
  # Fetch the table
  table <- config_db[[tableName]]
  # Has the module alread been regiestered
  matches <- name == table[, "title"]
  matches <- matches[!is.na(matches)]
  if (length(matches) == 0 || !any(matches)) stop("The module ", name, " does not exist")
  id <- table[matches, "id"]
  table[id, "enabled"] <- enabled
}

#' Enable or disable a GUI module
#'
#' @param name Title of the module
#' @export
#' @rdname enable_disable_gui_module.Rd
enable_gui_database_module <- function(name) {
  .enable_disable(name, "GUI_Pages", TRUE)
}

#' @export
#' @rdname enable_disable_gui_module.Rd
enable_gui_plot_module <- function(name) {
  .enable_disable(name, "GUI_PlotComponents", TRUE)
}

#' @export
#' @rdname enable_disable_gui_module.Rd
disable_gui_database_module <- function(name) {
  .enable_disable(name, "GUI_Pages", FALSE)
}

#' @export
#' @rdname enable_disable_gui_module.Rd
disable_gui_plot_module <- function(name) {
  .enable_disable(name, "GUI_PlotComponents", FALSE)
}


#' Get or set the main title for the GUI interface
#'
#' @param title The new title
#' @export
#' @rdname get_set_gui_title.Rd
gui_title <- function() {
  get_persistent_setting("GUI_Title")
}

#' @export
#' @rdname get_set_gui_title.Rd
set_gui_title <- function(title) {
  set_persistent_setting("GUI_Title", title)
}
