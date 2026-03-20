#' @importFrom shiny selectInput plotOutput tableOutput verbatimTextOutput textOutput uiOutput
#' @importFrom htmltools h3 h4 div
#' @import bslib
#' @import plotly
#' @noRd
app_ui <- function() {

  penguins <- PenguinDive::penguins

  # Raw numeric vars and pretty labels (UI)
  numeric_vars <- c("bill_length_mm", "bill_depth_mm",
                    "flipper_length_mm", "body_mass_g")
  numeric_labels <- c("Bill length (mm)", "Bill depth (mm)",
                      "Flipper length (mm)", "Body mass (g)")

  # Choices: names = pretty, values = raw
  numeric_choices <- setNames(numeric_vars, numeric_labels)

  # Facet labels (pretty → raw)
  facet_choices <- c(
    "None"   = "None",
    "Island" = "island",
    "Sex"    = "sex"
  )

  # Species labels (pretty → raw)
  species_raw <- sort(unique(as.character(penguins$species)))
  species_choices <- c("All" = "All")
  species_choices <- c(
    species_choices,
    setNames(species_raw, tools::toTitleCase(species_raw))
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
          selectInput("xvar", "X variable", choices = numeric_choices),
          selectInput("yvar", "Y variable", choices = numeric_choices)
        ),
        card(
          plotlyOutput("scatter"),
          div(
            style = "margin-top: -0.5rem; margin-bottom: 0.1rem;",
            h4(
              "Mean measurements by species",
              style = "
                font-size: 1.10rem;
                font-weight: 600;
                margin: 0;
              "
            )
          ),
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
          selectInput("species_profile", "Choose species",
                      choices = species_choices[species_choices != "All"]),
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
          selectInput("reg_x", "Predictor (X)", choices = numeric_choices),
          selectInput("reg_y", "Response (Y)", choices = numeric_choices)
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
