#' @importFrom shiny selectInput plotOutput tableOutput verbatimTextOutput textOutput uiOutput
#' @importFrom htmltools h3
#' @import bslib
#' @import plotly
#' @noRd
app_ui <- function() {

  penguins <- PenguinDive::penguins

# Pretty labels for numeric variables (UI only)
  pretty_numeric_labels <- c(
    bill_length_mm     = "Bill length (mm)",
    bill_depth_mm      = "Bill depth (mm)",
    flipper_length_mm  = "Flipper length (mm)",
    body_mass_g        = "Body mass (g)"
  )

# Facet labels (pretty → raw)
  facet_choices <- c(
    "None"   = "None",
    "Island" = "island",
    "Sex"    = "sex"
  )

# Species labels (pretty → raw)
  species_raw <- sort(unique(as.character(penguins$species)))
  species_choices <- setNames(
    c("All", species_raw),
    c("All", tools::toTitleCase(species_raw))
  )

  page_navbar(
    title = "Palmer Penguins Dashboard",

# Explorer
    nav_panel(
      "Explorer",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("species", "Species", choices = species_choices),
          selectInput("facet", "Facet by", choices = facet_choices),
          selectInput("xvar", "X variable", choices = pretty_numeric_labels),
          selectInput("yvar", "Y variable", choices = pretty_numeric_labels)
        ),
        card(
          plotlyOutput("scatter"),
          tableOutput("summary")
        )
      )
    ),

# Species Profiles
    nav_panel(
      "Species Profiles",
      layout_columns(
        col_widths = c(4, 8),
        card(
          h3("Species"),
          selectInput("species_profile", "Choose species", choices = species_choices[-1]),
          uiOutput("penguin_image")
        ),
        card(
          h3("Summary statistics"),
          tableOutput("profile_stats")
        )
      )
    ),

# Statistics
    nav_panel(
      "Statistics",
      layout_sidebar(
        sidebar = sidebar(
          selectInput("reg_species", "Species", choices = species_choices),
          selectInput("reg_x", "Predictor (X)", choices = pretty_numeric_labels),
          selectInput("reg_y", "Response (Y)", choices = pretty_numeric_labels)
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
