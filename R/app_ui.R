#' @importFrom shiny selectInput plotOutput tableOutput verbatimTextOutput textOutput uiOutput
#' @importFrom htmltools h3
#' @import bslib
#' @import plotly
#' @noRd
app_ui <- function() {
  penguins <- PenguinDive::penguins
  page_navbar(
    title = "Palmer Penguins Dashboard",
    nav_panel(
      "Explorer",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("species", "Species", choices = c("All", unique(penguins$species))),
          selectInput("facet", "Facet by", choices = c("None", "island", "sex")),
          selectInput("xvar", "X variable", choices = names(penguins)[3:6]),
          selectInput("yvar", "Y variable", choices = names(penguins)[3:6])
        ),
        card(
          plotlyOutput("scatter"),
          tableOutput("summary")
        )
      )
    ),
    nav_panel(
      "Species Profiles",
      layout_columns(
        col_widths = c(4, 8),
        card(
          h3("Species"),
          selectInput("species_profile", "Choose species", unique(penguins$species)),
          uiOutput("penguin_image")
        ),
        card(
          h3("Summary statistics"),
          tableOutput("profile_stats")
        )
      )
    ),
    nav_panel(
      "Statistics",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("reg_species", "Species", choices = c("All", unique(penguins$species))),
          selectInput("reg_x", "Predictor (X)", choices = names(penguins)[3:6]),
          selectInput("reg_y", "Response (Y)", choices = names(penguins)[3:6])
        ),
        card(
          h3("Regression results"),
          verbatimTextOutput("regression"),
          h3("Correlation"),
          textOutput("correlation")
        )
      )
    )
  )
}
