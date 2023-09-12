# Loading screen functions for map
# show handler
show_loading <- function(elem) {
  session <- shiny::getDefaultReactiveDomain()
  session$sendCustomMessage("show_loading", elem)
}

# hide handler
hide_loading <- function(elem) {
  session <- shiny::getDefaultReactiveDomain()
  session$sendCustomMessage("hide_loading", elem)
}
