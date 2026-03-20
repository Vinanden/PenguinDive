#' Application server logic
#'
#' @importFrom stats as.formula cor lm
#' @importFrom dplyr filter group_by summarise across where recode select
#' @importFrom shiny renderUI renderTable renderPrint renderText reactive
#' @importFrom htmltools tags
#' @import ggplot2
#' @noRd
app_server <- function(input, output, session) {

  penguins <- PenguinDive::penguins

  theme_set(theme_bw(base_size = 16))

  # Pretty labels for renaming columns and axes (server)
  axis_labels <- c(
    bill_length_mm     = "Bill length (mm)",
    bill_depth_mm      = "Bill depth (mm)",
    flipper_length_mm  = "Flipper length (mm)",
    body_mass_g        = "Body mass (g)"
  )

  pretty_names <- c(
    species            = "Species",
    axis_labels
  )

  penguin_images <- list(
    Adelie    = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
    Gentoo    = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
    Chinstrap = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png"
  )

  # ---- Reactive filtered data ----
  penguins_filtered <- reactive({
    if (input$species == "All") penguins else penguins |> dplyr::filter(species == input$species)
  })

# Scatterplot (interactive)
  output$scatter <- plotly::renderPlotly({
    df <- penguins_filtered()

    df$sex <- dplyr::recode(df$sex, "male" = "Male", "female" = "Female") # Fix sex labels

    xvar <- input$xvar
    yvar <- input$yvar

    df$Species <- df$species

    p <- ggplot(df, aes_string(xvar, yvar, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_manual(
        values = c(
          "Chinstrap" = "#8E5BA6",
          "Gentoo"    = "#3BB6A0",
          "Adelie"    = "#F4A259"
        )
      ) +
      labs(
        x = axis_labels[[xvar]],
        y = axis_labels[[yvar]],
        color = "Species"
      ) +
      theme_bw(base_size = 16)

    if (input$facet != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))
    }

    plotly::ggplotly(
      p,
      tooltip = c("Species", xvar, yvar)
    )
  })

# Summary table
  output$summary <- renderTable({
    df <- penguins_filtered() |>
      dplyr::select(-year) |>
      dplyr::group_by(species) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))

    names(df) <- dplyr::recode(names(df), !!!pretty_names)
    df
  })

# Species profile image
  output$penguin_image <- renderUI({
    species <- input$species_profile
    img_src <- penguin_images[[species]]
    tags$img(src = img_src, width = "100%")
  })

# Species profile stats
  output$profile_stats <- renderTable({
    df <- penguins |>
      dplyr::filter(species == input$species_profile) |>
      dplyr::select(-year) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))

    names(df) <- dplyr::recode(names(df), !!!pretty_names)
    df
  })

# Regression
  output$regression <- renderPrint({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    reg_x <- input$reg_x
    reg_y <- input$reg_y

    lm_formula <- as.formula(paste(reg_y, "~", reg_x))
    model <- lm(lm_formula, data = df)
    summary(model)
  })

# Correlation
  output$correlation <- renderText({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    reg_x <- input$reg_x
    reg_y <- input$reg_y

    cor_val <- cor(df[[reg_x]], df[[reg_y]], use = "complete.obs")
    paste("Correlation:", round(cor_val, 3))
  })
}
