#' Like a checkbox input, but displays as a toggle
#' @export
toggleInput <- function(inputId, label, value=FALSE, onlabel="", offlabel="", style=NULL) {
  value <- shiny::restoreInput(id=inputId, default=value)
  shiny::addResourcePath(prefix="www", directoryPath=system.file("www", package="jms.classes"))

  inputArgs <- list(
    id=inputId, type="checkbox", name="toggleInput", class="toggleInput-checkbox"
  )
  if (value) {
    inputArgs[["checked"]] <- ""
  }

  shiny::tagList(
    htmltools::singleton(
      shiny::tags$head(
        shiny::tags$link(rel="stylesheet", type="text/css", href="www/toggleInput.css")
      )
    ),
    shiny::tags$div(
      class="form-group shiny-input-container",
      style=style,
      shiny::tags$div(
        shiny::tags$label(label, class="control-label"),
        shiny::tags$div(
          class="toggleInput",
          do.call(shiny::tags$input, inputArgs),
          shiny::tags$label(
            class="toggleInput-label", `for`=inputId,
            shiny::tags$style(htmltools::HTML(sprintf('#%s  + .toggleInput-label > .toggleInput-inner::before { content:"%s";}\n#%1$s + .toggleInput-label > .toggleInput-inner::after {content:"%s";}', inputId, onlabel, offlabel))),
            shiny::tags$span(class="toggleInput-inner"),
            shiny::tags$span(class="toggleInput-switch")
          )
        )
      )
    )
  )
}
