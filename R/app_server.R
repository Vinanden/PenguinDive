#' Application server logic
#'
#' @importFrom stats as.formula cor lm
#' @importFrom dplyr filter group_by summarise across where recode select
#' @importFrom shiny renderUI renderTable renderPrint renderText reactive
#' @importFrom htmltools tags
#' @importFrom tidyr pivot_longer
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
    species = "Species",
    axis_labels
  )

  # Penguin image
  penguin_image <- "https://allisonhorst.github.io/palmerpenguins/reference/figures/lter_penguins.png"
  output$penguin_image <- renderUI({
    tags$img(src = penguin_image, width = "100%")
  })

  # Reactive filtered data for Explorer
  penguins_filtered <- reactive({
    if (input$species == "All") penguins
    else penguins |> dplyr::filter(species == input$species)
  })

  # Explorer scatterplot
  output$scatter <- plotly::renderPlotly({
    df <- penguins_filtered()

    df$sex <- df$sex |>
      as.character() |>
      dplyr::recode(
        "male"   = "Male",
        "female" = "Female",
        .missing = "Unknown"
      )

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

    plotly::ggplotly(p, tooltip = c("Species", xvar, yvar))
  })

  # Spescies profile table
  output$profile_stats <- renderTable({
    df <- penguins |>
      dplyr::select(-year) |>
      dplyr::group_by(species) |>
      dplyr::summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))

    names(df) <- dplyr::recode(names(df), !!!pretty_names)
    df
  })

  # Regression plot
  output$reg_plot <- plotly::renderPlotly({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    p <- ggplot(df, aes_string(input$reg_x, input$reg_y, color = "species")) +
      geom_point(size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE) +
      scale_color_manual(
        values = c(
          "Chinstrap" = "#8E5BA6",
          "Gentoo"    = "#3BB6A0",
          "Adelie"    = "#F4A259"
        )
      ) +
      labs(
        x = axis_labels[[input$reg_x]],
        y = axis_labels[[input$reg_y]],
        color = "Species"
      ) +
      theme_bw(base_size = 16)

    plotly::ggplotly(p, tooltip = c("species", input$reg_x, input$reg_y))
  })

  # Regression summary (with guard clause)
  output$reg_summary <- renderTable({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    # Guard: need variation
    if (nrow(df) < 3 ||
        length(unique(df[[input$reg_x]])) < 2 ||
        length(unique(df[[input$reg_y]])) < 2) {
      return(data.frame(
        Term = "Model error",
        Value = "Not enough variation to fit regression"
      ))
    }

    model <- lm(as.formula(paste(input$reg_y, "~", input$reg_x)), data = df)
    s <- summary(model)

    # Guard: slope may not exist
    if (nrow(s$coefficients) < 2) {
      return(data.frame(
        Term = "Model error",
        Value = "Regression model has no slope term"
      ))
    }

    data.frame(
      Term = c("Intercept", "Slope", "R-squared", "p-value"),
      Value = c(
        round(s$coefficients[1, 1], 3),
        round(s$coefficients[2, 1], 3),
        round(s$r.squared, 3),
        signif(s$coefficients[2, 4], 3)
      )
    )
  })

  # Correlation text (with guard)
  output$correlation <- renderText({
    df <- penguins
    if (input$reg_species != "All") {
      df <- dplyr::filter(df, species == input$reg_species)
    }

    if (length(unique(df[[input$reg_x]])) < 2 ||
        length(unique(df[[input$reg_y]])) < 2) {
      return("Not enough variation to compute correlation")
    }

    cor_val <- cor(df[[input$reg_x]], df[[input$reg_y]], use = "complete.obs")
    paste("Correlation:", round(cor_val, 3))
  })

  # Correlation page
  output$cor_heatmap <- renderPlot({
    req(input$heatmap_species)

    df <- penguins |>
      dplyr::filter(species == input$heatmap_species) |>
      dplyr::select(where(is.numeric), -year)

    if (nrow(df) < 3) {
      plot.new()
      text(0.5, 0.5, "Not enough data for this species")
      return()
    }

    corr <- cor(df, use = "complete.obs")

    corr_df <- as.data.frame(corr)
    corr_df$Var1 <- rownames(corr_df)

    corr_df <- tidyr::pivot_longer(
      corr_df,
      cols = -Var1,
      names_to = "Var2",
      values_to = "value"
    )

    ggplot(corr_df, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#3BB6A0", mid = "white", high = "#8E5BA6") +
      theme_bw(base_size = 14) +
      labs(
        x = "",
        y = "",
        fill = "Correlation",
        title = paste("Correlations for", input$heatmap_species)
      )
  })
}
