library(ggplot2)
library(showtext)
library(cwi)
library(sysfonts)

sysfonts::font_paths("C:\\Users\\presage\\Documents\\GitHub\\brisk\\www\\")

theme_presage <- function(base_theme = "minimal",
                          base_size = 20,
                          base_font = "",
                          title_size = 22,
                          title_font = "presage",
                          title_face = "plain",
                          title_margin = ggplot2::margin(12, 0, 12, 0),
                          show_grid = TRUE,
                          ...) {
  showtext::showtext_auto()

  # Specify base font
  if (base_font != "") {
    if (base_font == "presage") {
      # sysfonts::font_add_google(name = "Libre Franklin")
      sysfonts::font_add("Libre Franklin",
                         regular = "LibreFranklin-Light.ttf",
                         bold = "LibreFranklin-Bold.ttf",
                         italic = "LibreFranklin-LightItalic.ttf",
                         bolditalic = "LibreFranklin-BoldItalic.ttf")
      # sysfonts::font_add("benton", regular = "BentonSans Medium.otf")
      # base_font <- "benton"
      # title_font <- "benton"
      base_font <- "Libre Franklin"
      title_font <- "Libre Franklin"
    } else if (base_font == "fsi") {
      sysfonts::font_add_google(name = "Orbitron")
      base_font <- "Orbitron"
      title_font <- "Orbitron"
    } else {
      sysfonts::font_add_google(name = base_font)
      title_font <- base_font
    }
  }


  # Specify title font
  if (!title_font %in% c("presage", "fsi")) {
    sysfonts::font_add_google(name = title_font)
  } else if (title_font %in% c("Libre Franklin", "BentonSans", "presage")) {
    # sysfonts::font_add_google(name = "Libre Franklin")
    sysfonts::font_add("Libre Franklin",
                       regular = "LibreFranklin-Light.ttf",
                       bold = "LibreFranklin-Bold.ttf",
                       italic = "LibreFranklin-LightItalic.ttf",
                       bolditalic = "LibreFranklin-BoldItalic.ttf")
    # sysfonts::font_add("benton", regular = "BentonSans Medium.otf")
    # title_font <- "benton"
    title_font <- "Libre Franklin"
  } else if (title_font %in% c("Orbitron", "fsi")) {
    sysfonts::font_add_google(name = "Orbitron")
    title_font <- "Orbitron"
  }


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
      plot.title = ggtext::element_textbox(
        face = title_face,
        size = title_size,
        family = title_font,
        margin = title_margin
        ),
      plot.title.position = "plot",

      # panel
      panel.grid = element_line(color = ifelse(show_grid, "gray92", "transparent")),

      # axis
      # axis.line = element_line(color = ifelse(show_grid, "transparent", "black"), linewidth = 0.5, lineend = "round"),
      # axis.ticks = element_line(color = ifelse(show_grid, "transparent", "grey20"), linewidth = 0.5, lineend = "round"),
      ...
    )
}

presage_logo <- function(logo_name = "presage") {
  if (logo_name == "presage") {
    magick::image_read("C:\\Users\\presage\\Documents\\presagelogo.png")
  } else if (logo_name == "pep") {
    magick::image_read("C:\\Users\\presage\\Documents\\presagepeplogo.png")
  } else {
    magick::image_read("C:\\Users\\presage\\Documents\\presageplogo.png")
  }
}



p <- ggplot(data = mpg) +
  geom_point(aes(x = displ, y = cty)) +
  labs(
    title = "Flight Safety",
    x = "Engine size", y = "City Mileage"
  ) +
  theme_presage(
    title_font = "fsi", base_size = 18, title_size = 22,
    show_grid = TRUE, base_font = ""
  )



