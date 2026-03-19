#' @importFrom shiny selectInput plotOutput tableOutput verbatimTextOutput textOutput uiOutput
#' @importFrom htmltools h3
#' @import bslib
#' @import plotly
#' @noRd
app_ui <- function() {
  penguins <- PenguinDive::penguins

  # Pretty labels for numeric variables
  pretty_names <- c(
    bill_length_mm     = "Bill length (mm)",
    bill_depth_mm      = "Bill depth (mm)",
    flipper_length_mm  = "Flipper length (mm)",
    body_mass_g        = "Body mass (g)"
  )

  page_navbar(
    title = "Palmer Penguins Dashboard",

    # ---------------- Explorer ----------------
    nav_panel(
      "Explorer",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("species", "Species", choices = c("All", unique(penguins$species))),
          selectInput("facet", "Facet by", choices = c("None", "island", "sex")),
          selectInput("xvar", "X variable", choices = pretty_names),
          selectInput("yvar", "Y variable", choices = pretty_names)
        ),
        card(
          plotlyOutput("scatter"),
          tableOutput("summary")
        )
      )
    ),

    # ---------------- Species Profiles ----------------
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

    # ---------------- Statistics ----------------
    nav_panel(
      "Statistics",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("reg_species", "Species", choices = c("All", unique(penguins$species))),
          selectInput("reg_x", "Predictor (X)", choices = pretty_names),
          selectInput("reg_y", "Response (Y)", choices = pretty_names)
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
