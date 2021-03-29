#' Run \{aRbs\} shiny app
#'
#' Shiny app displaying the functionality of \code{get_arbs()}.
#' @export

# get_arbs_shiny <- function() {
#   appDir <- system.file("shiny-examples", "shiny",
#                         package = "aRbs")
#   if (appDir == "") {
#     stop("Could not find example directory. Try re-installing `aRbs`.", call. = FALSE)
#   }
#
#   shiny::runApp(appDir, display.mode = "normal")
# }

get_arbs_shiny <- function() {
  shiny::shinyApp(ui, server, options = c("launch.browser" = TRUE))
}
