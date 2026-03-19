#' Application server logic
#'
#' @importFrom stats as.formula cor lm
#' @importFrom dplyr filter group_by summarise across where
#' @importFrom ggplot2 ggplot aes_string geom_point facet_wrap theme_bw theme_set labs
#' @importFrom shiny renderUI renderTable renderPrint renderText reactive
#' @importFrom htmltools tags
#' @noRd
app_server <- function(input, output, session) {
  penguins <- PenguinDive::penguins

  theme_set(theme_bw(base_size = 16))

  penguin_images <- list(
    Adelie    = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
    Gentoo    = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png",
    Chinstrap = "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png"
  )

  # ---- Reactive filtered data ----
  penguins_filtered <- reactive({
    if (input$species == "All") penguins else penguins |> dplyr::filter(species == input$species)
  })

  # ---- Scatterplot (interactive) ----
  output$scatter <- plotly::renderPlotly({
    df <- penguins_filtered()

    # Lookup table for pretty labels
    pretty_names <- c(
      bill_length_mm     = "Bill length (mm)",
      bill_depth_mm      = "Bill depth (mm)",
      flipper_length_mm  = "Flipper length (mm)",
      body_mass_g        = "Body mass (g)"
    )

    # Convert pretty label back to raw variable name
    xvar <- names(pretty_names)[pretty_names == input$xvar]
    yvar <- names(pretty_names)[pretty_names == input$yvar]

    p <- ggplot(df, aes_string(xvar, yvar, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        x = input$xvar,   # pretty label
        y = input$yvar    # pretty label
      ) +
      theme_bw(base_size = 16)

    if (input$facet != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))
    }

    plotly::ggplotly(p, tooltip = c("species", xvar, yvar))
  })

  # ---- Summary table ----
  output$summary <- renderTable({
    penguins_filtered() |>
      dplyr::group_by(species) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))
  })

  # ---- Species profile image ----
  output$penguin_image <- renderUI({
    species <- input$species_profile
    img_src <- penguin_images[[species]]
    tags$img(src = img_src, width = "100%")
  })

  # ---- Species profile stats ----
  output$profile_stats <- renderTable({
    penguins |>
      dplyr::filter(species == input$species_profile) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))
  })

  # ---- Regression ----
  output$regression <- renderPrint({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    lm_formula <- as.formula(paste(input$reg_y, "~", input$reg_x))
    model <- lm(lm_formula, data = df)
    summary(model)
  })

  # ---- Correlation ----
  output$correlation <- renderText({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    cor_val <- cor(df[[input$reg_x]], df[[input$reg_y]], use = "complete.obs")
    paste("Correlation:", round(cor_val, 3))
  })
}
