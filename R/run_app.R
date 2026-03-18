#' Launch the Palmer Penguins dashboard
#'
#' @description
#' `run_app()` starts an interactive Shiny application for exploring the
#' Palmer penguins dataset. The app provides tools for visualising
#' relationships between variables, comparing species, and inspecting
#' simple statistical summaries and models.
#'
#' @return
#' This function is called for its side effect of launching a Shiny app.
#' It returns the result of [shiny::shinyApp()].
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function() {
  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
}
