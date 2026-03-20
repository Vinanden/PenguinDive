#' Application server logic
#'
#' @importFrom stats as.formula cor lm
#' @importFrom dplyr filter group_by summarise across where recode select
#' @importFrom ggplot2 ggplot aes_string geom_point facet_wrap theme_bw theme_set labs
#' @importFrom shiny renderUI renderTable renderPrint renderText reactive
#' @importFrom htmltools tags
#' @noRd
app_server <- function(input, output, session) {

  penguins <- PenguinDive::penguins

  theme_set(theme_bw(base_size = 16))

  # Pretty labels for renaming columns and axes
  pretty_names <- c(
    species            = "Species",
    bill_length_mm     = "Bill length (mm)",
    bill_depth_mm      = "Bill depth (mm)",
    flipper_length_mm  = "Flipper length (mm)",
    body_mass_g        = "Body mass (g)"
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

  # ---- Scatterplot (interactive) ----
  output$scatter <- plotly::renderPlotly({
    df <- penguins_filtered()

    # Convert pretty label back to raw variable name
    xvar <- names(pretty_names)[pretty_names == input$xvar]
    yvar <- names(pretty_names)[pretty_names == input$yvar]

    p <- ggplot(df, aes_string(xvar, yvar, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      labs(
        x = input$xvar,
        y = input$yvar,
        color = "Species"
      ) +
      theme_bw(base_size = 16)

    if (input$facet != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facet)))
    }

    plotly::ggplotly(p, tooltip = c("species", xvar, yvar))
  })

  # ---- Summary table ----
  output$summary <- renderTable({
    df <- penguins_filtered() |>
      dplyr::select(-year) |>   # hide year
      dplyr::group_by(species) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))

    names(df) <- dplyr::recode(names(df), !!!pretty_names)
    df
  })

  # ---- Species profile image ----
  output$penguin_image <- renderUI({
    species <- input$species_profile
    img_src <- penguin_images[[species]]
    tags$img(src = img_src, width = "100%")
  })

  # ---- Species profile stats ----
  output$profile_stats <- renderTable({
    df <- penguins |>
      dplyr::filter(species == input$species_profile) |>
      dplyr::select(-year) |>   # hide year
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))

    names(df) <- dplyr::recode(names(df), !!!pretty_names)
    df
  })

  # ---- Regression ----
  output$regression <- renderPrint({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    reg_x <- names(pretty_names)[pretty_names == input$reg_x]
    reg_y <- names(pretty_names)[pretty_names == input$reg_y]

    lm_formula <- as.formula(paste(reg_y, "~", reg_x))
    model <- lm(lm_formula, data = df)
    summary(model)
  })

  # ---- Correlation ----
  output$correlation <- renderText({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    reg_x <- names(pretty_names)[pretty_names == input$reg_x]
    reg_y <- names(pretty_names)[pretty_names == input$reg_y]

    cor_val <- cor(df[[reg_x]], df[[reg_y]], use = "complete.obs")
    paste("Correlation:", round(cor_val, 3))
  })
}
