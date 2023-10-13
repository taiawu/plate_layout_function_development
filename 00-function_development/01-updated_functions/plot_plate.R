plot_plate_variable <-
  function(layout,
           fill_col,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           scale_plot_size = 1,
           .well_shape = 22,
           master_text_size = 16,
           well_size = 4,
           .well_outline_color = "#969696",
           show_empty_wells = TRUE,
           .fill_scale = NULL,
           .max_legend_size = 12,
           ...) {
    #### ---- use variable name as title, unless otherwise given
    if (is.null(title)) {
      title <- fill_col
    }

    #### ----  resize geoms and text together
    plot_sizes <- scale_plot_size * c(
      well_size = well_size,
      well_stroke = 0.5,
      master_text_size = master_text_size,
      grid_linewidth = 0.2
    )

    #### ---- add missing wells to layout
    if (show_empty_wells) {
      layout <- tidyplate::add_missing_wells(layout)
    }

    #### ---- set x axis breaks
    first_col <- min(layout$column)
    last_col <- max(layout$column)

    #### ---- set fill scale and guide type
    if (is.numeric(layout[[fill_col]])) {
      fill_scale <- scale_fill_viridis_c(na.value = "transparent")
      guide_type <- "numeric"
      guide_position <- "right"
    } else {
      fill_scale <- scale_fill_viridis_d(option = "turbo")
      guide_type <- "discrete"

      ## only show guide if it would fit well in the screen ...
      # determine if the guides should be shown or not
      n_values <- n_distinct(layout[[fill_col]])
      guide_position <- ifelse(n_values > .max_legend_size,
        yes = "none",
        no = "right"
      )
    }

    #### ---- make plot
    p <-
      ggplot(layout, aes(
        x = .data$column,
        y = .data$row
      )) +
      geom_point(aes(fill = .data[[fill_col]]),
        shape = .well_shape,
        stroke = plot_sizes[["well_stroke"]],
        size = plot_sizes[["well_size"]]
      ) +
      scale_y_discrete(limits = rev) + # start with row A
      scale_x_continuous(breaks = seq(from = first_col, to = last_col, by = 1)) +
      fill_scale +
      plate_theme_light(
        grid_linewidth = plot_sizes[["grid_linewidth"]],
        master_text_size = plot_sizes[["master_text_size"]],
        ...
      ) +

      # determined by the type and number of values in the variable
      theme(legend.position = guide_position) +
      labs(
        title = title,
        subtitle = subtitle,
        caption = caption
      )

    #### ---- don't allow more than ten rows for discrete legends
    if (guide_type == "discrete") {
      p <- p + guides(fill = guide_legend(nrow = 10))
    }

    p
  }

plate_theme_light <-
  function(aspect_ratio = 16 / 24,
           grid_color = "grey",
           grid_linewidth = 0.2,
           master_text_size = 16,
           ...) {
    theme(
      aspect.ratio = aspect_ratio,
      panel.background =
        element_rect(
          color = "transparent",
          fill = "transparent"
        ),
      panel.grid.minor =
        element_blank(),
      panel.grid.major =
        element_line(
          color = grid_color,
          linewidth = grid_linewidth
        ),
      axis.ticks = element_line(
        color = grid_color,
        linewidth = grid_linewidth
      ),
      axis.title =
        element_blank(),
      plot.background =
        element_rect(
          fill = "transparent",
          colour = NA
        ),
      text = element_text(size = master_text_size),
      plot.title = element_text(face = "bold")
    )
  }
