library(ggplot2)

theme_presage <- function(base_theme = "minimal",
                          base_size = 12,
                          base_font = "Libre Franklin",
                          title_size = 15,
                          title_font = "Libre Franklin",
                          title_face = "plain",
                          title_margin = ggplot2::margin(5, 0, 12, 0),
                          subtitle_size = 12,
                          subtitle_font = "Libre Franklin",
                          subtitle_margin = ggplot2::margin(0, 0, 12, 0),
                          show_grid = TRUE,
                          ...) {
  # CUSTOM THEME:

  if (base_theme == "minimal") {
    basic_theme <- ggplot2::theme_minimal(base_size = base_size, base_family = base_font)
  } else if (base_theme %in% c("grey", "gray")) {
    basic_theme <- ggplot2::theme_grey(base_size = base_size, base_family = base_font)
  } else if (base_theme == "bw") {
    basic_theme <- ggplot2::theme_bw(base_size = base_size, base_family = base_font)
  } else if (base_theme == "linedraw") {
    basic_theme <- ggplot2::theme_linedraw(base_size = base_size, base_family = base_font)
  } else if (base_theme == "light") {
    basic_theme <- ggplot2::theme_light(base_size = base_size, base_family = base_font)
  } else if (base_theme == "dark") {
    basic_theme <- ggplot2::theme_dark(base_size = base_size, base_family = base_font)
  } else if (base_theme == "classic") {
    basic_theme <- ggplot2::theme_classic(base_size = base_size, base_family = base_font)
  } else if (base_theme == "void") {
    basic_theme <- ggplot2::theme_void(base_size = base_size, base_family = base_font)
  } else if (base_theme == "test") {
    basic_theme <- ggplot2::theme_test(base_size = base_size, base_family = base_font)
  }

  basic_theme +
    ggplot2::theme(
      # title
      plot.title = ggtext::element_textbox_simple(
        face = title_face,
        size = title_size,
        family = title_font,
        margin = title_margin
      ),
      plot.title.position = "plot",
      # subtitle
      plot.subtitle = ggtext::element_textbox_simple(
        size = subtitle_size,
        vjust = 1,
        family = subtitle_font,
        margin = subtitle_margin
      ),

      # panel
      panel.grid = element_line(color = ifelse(show_grid, "gray92", "transparent")),

      ...
    )
}
