library(monochromeR)
library(colorspace)
library(tidyverse)
library(colorblindr)
library(glue)
library(ggtext)

source("making_theme2.R")

# Define all colours ------------------------------------------------------

presage_colours <- list(
  presage_blue         = "#0072bc",
  presage_light_blue   = "#008fd5",
  presage_blue_green   = "#00abc5",
  light_blue           = "#cce2f1",
  presage_turqoise     = "#00a99d",
  presage_green_blue   = "#00a775",
  presage_green        = "#00a651",
  presage_light_green  = "#39b54a",
  presage_lime_green   = "#8dc63f",
  presage_yellow_green = "#d7df23",
  presage_gray         = "#6d6e71",
  dark_orange          = "#D55E00",
  fsi_blue             = "#041c2c",
  blood_red            = "#991f17",
  light_red            = "#df8879",
  okabeito_magenta     = "#cc79a7",
  okabeito_magenta2    = "#e0aeca",
  okabeito_mustard     = "#e69f00",
  okabeito_mustard2    = "#f0c566",
  okabeito_yellow      = "#f0e442"
)



# Qualitative palette default -----------------------------------------------------

## Purpose: Different colours for representing unordered categories
## This is the default, darker version. It is appropriate for points and lines
qualitative_palette <- c(
  presage_colours$presage_blue,
  presage_colours$presage_light_green,
  presage_colours$okabeito_mustard,
  presage_colours$presage_yellow_green,
  presage_colours$fsi_blue,
  presage_colours$blood_red,
  presage_colours$okabeito_magenta
)


monochromeR::view_palette(qualitative_palette)

## Example:
### POINTS
ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(colour = factor(cyl)), size = 4) +
  scale_color_manual(values = qualitative_palette) +
  theme_minimal()


ggplot(mtcars, aes(wt, mpg)) +
  geom_point(aes(colour = factor(carb)), size = 4) +
  scale_color_manual(values = qualitative_palette) +
  theme_minimal()

#### Check for colour-blind friendliness:
colorblindr::cvd_grid()


### LINES
ggplot(economics_long, aes(date, value01, colour = variable)) +
  geom_line() +
  scale_color_manual(values = qualitative_palette) +
  theme_minimal()

colorblindr::cvd_grid()

















# Qualitative palette light -----------------------------------------------------

## Purpose: Different colours for representing unordered categories
## This is the lighter version. It is appropriate for shading like
### bar charts and area charts

generate_palette(colour = presage_colours$okabeito_magenta,
                 modification = "go_lighter", n = 3, view_palette = TRUE)

qualitative_palette_light <- c(
  presage_colours$presage_blue_green,
  presage_colours$presage_lime_green,
  presage_colours$okabeito_mustard2,
  presage_colours$okabeito_yellow,
  presage_colours$light_blue,
  presage_colours$light_red,
  presage_colours$okabeito_magenta2
)

monochromeR::view_palette(qualitative_palette_light)

ggplot(mpg, aes(y = class)) +
  geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") +
  scale_fill_manual(values = qualitative_palette_light) +
  theme_minimal()

colorblindr::cvd_grid()

ggplot(mpg, aes(y = class)) +
  geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top") +
  scale_fill_manual(values = qualitative_palette) +
  theme_minimal()














# Sequential palettes ------------------------------------------------------

## single hue
blues_palette <- generate_palette(colour = presage_colours$presage_blue, n = 5, modification = "go_lighter")
view_palette(blues_palette)

greens_palette <- generate_palette(colour = presage_colours$presage_light_green, n = 5, modification = "go_lighter")
view_palette(greens_palette)


## multi-hue
sequential_multi_hue_palette <- generate_palette(colour = presage_colours$presage_blue_green,
                 blend_colour = presage_colours$presage_lime_green,
                 n = 5)
view_palette(sequential_multi_hue_palette)








# Diverging palette -------------------------------------------------------

hcl_palettes(type = "diverging", plot = TRUE, n = 6)
diverging_hcl(palette = "Blue-Red 2", n = 6)

diverging_blue_green <- c(blues_palette[c(1, 3, 5)], greens_palette[c(5, 3, 1)])
view_palette(diverging_blue_green)

oranges_palette <- generate_palette(colour = presage_colours$dark_orange,
                 modification = "go_lighter",
                 n = 5)
view_palette(oranges_palette)

diverging_blue_orange <- c(blues_palette[c(1, 3, 5)], oranges_palette[c(5, 3, 1)])
view_palette(diverging_blue_orange)










# Combine all palettes ----------------------------------------------------

presage_palettes <- list(
  default = qualitative_palette,
  qualitative_palette = qualitative_palette,
  qualitative_palette_light = qualitative_palette_light,
  sequential_blue = blues_palette,
  sequential_green = greens_palette,
  sequential_orange = oranges_palette,
  diverging_blue_green = diverging_blue_green,
  diverging_blue_orange = diverging_blue_orange,
  diverging_two = c(presage_colours$presage_blue, presage_colours$dark_orange)
)








# Scales -------------------------------------------------------------------

scale_color_presage <- function(palette = "default",
                                 continuous = FALSE,
                                 .colours = presage_colours,
                                 .palettes = presage_palettes,
                                 .direction = 1,
                                 ...) {

  palette_colours <- .palettes[[palette]]

  if (.direction == -1){
    palette_colours <- rev(palette_colours)
  }

  if(continuous == FALSE) {

    ggplot2::scale_colour_manual(values = palette_colours,
                               na.value = .colours$presage_gray,
                               ...)

  } else {

    ggplot2::scale_colour_gradientn(colours = palette_colours,
                                    na.value = .colours$presage_gray,
                                    ...)
  }

}

scale_colour_presage <- scale_color_presage






scale_fill_presage <- function(palette = "qualitative_palette_light",
                                continuous = FALSE,
                                .colours = presage_colours,
                                .palettes = presage_palettes,
                                .direction = 1,
                                ...) {

  palette_colours <- .palettes[[palette]]

  if (.direction == -1){
    palette_colours <- rev(palette_colours)
  }

  if(continuous == FALSE) {

    ggplot2::scale_fill_manual(values = palette_colours,
                               na.value = .colours$presage_gray,
                               ...)

  } else {

    ggplot2::scale_fill_gradientn(colours = palette_colours,
                                    na.value = .colours$presage_gray,
                                    ...)
  }

}





# Examples ----------------------------------------------------------------


p <- palmerpenguins::penguins %>%
  ggplot() +
  geom_point(aes(x = bill_length_mm,
                 y = flipper_length_mm,
                 fill = species,
                 size = body_mass_g),
             shape = 21) +
  labs(x = "Bill length (mm)",
       y = "Flipper length (mm)",
       title = "Let's try some *italics* in the title",
       subtitle = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
       caption = "Data from {palmerpenguins}") +
  guides(size = "none")

p
p + scale_fill_presage()
p + scale_fill_presage(palette = "qualitative_palette")
p + scale_fill_presage(palette = "qualitative_palette") +
  theme_presage()

colorblindr::cvd_grid()




p2 <- palmerpenguins::penguins %>%
  ggplot() +
  geom_point(aes(x = bill_length_mm,
                 y = flipper_length_mm,
                 color = species,
                 size = body_mass_g),
             alpha = 0.5) +
  labs(x = "Bill length (mm)",
       y = "Flipper length (mm)",
       title = "Let's try some *italics* in the title",
       subtitle = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
       caption = "Data from {palmerpenguins}") +
  guides(size = "none")

p2
p2 + scale_color_presage()
p2 + scale_color_presage() +
  theme_presage()



p2 + scale_color_presage() +
  theme_presage(base_font = "Cascadia Code")

p2 + scale_color_presage() +
  theme_presage(base_font = "Cascadia Code",
                title_font = "Cascadia Code",
                subtitle_font = "Cascadia Code")





p3 <- palmerpenguins::penguins %>%
  ggplot() +
  geom_point(aes(x = bill_length_mm,
                 y = flipper_length_mm,
                 fill = body_mass_g,
                 size = body_mass_g),
             shape = 21,
             colour = "white",
             alpha = 0.8) +
  labs(x = "Bill length (mm)",
       y = "Flipper length (mm)",
       title = "Let's try some **bold** in the title",
       subtitle = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
       caption = "Data from {palmerpenguins}") +
  guides(size = "none")

p3
p3 + scale_fill_presage(continuous = TRUE)
p3 + scale_fill_presage(continuous = TRUE,
                        palette = "sequential_blue",
                        .direction = -1)


p3 + scale_fill_presage(continuous = TRUE,
                        palette = "sequential_blue",
                        .direction = -1) +
  theme_presage()


p3 + scale_fill_presage(continuous = TRUE,
                        palette = "sequential_green",
                        .direction = -1)




p3 + scale_fill_presage(continuous = TRUE,
                        palette = "sequential_orange",
                        .direction = -1)

p3 + scale_fill_presage(continuous = TRUE,
                        palette = "sequential_orange",
                        .direction = -1) +
  theme_presage()


colorblindr::cvd_grid()






p4 <- palmerpenguins::penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = species,
             fill = island),
         stat = "count") +
  geom_bar() +
  labs(title = "Perfectly proportional penguins",
       subtitle = "Where do they all live?",
       caption = "Data from {palmerpenguins}") +
  facet_grid(. ~ sex)

p4
p4 + scale_fill_presage()
p4 + scale_fill_presage(palette = "qualitative_palette")
p4 + scale_fill_presage() +
  theme_presage(
    base_theme = "light", title_face = "bold",
    show_grid = FALSE
  )

colorblindr::cvd_grid()

p4 + scale_fill_presage() +
  theme_presage(
    title_face = "bold",
    show_grid = FALSE
  ) +
  theme(
    strip.background = element_rect(fill = "grey90", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA)
  )









survey_data <- tibble(answer = factor(rep(c("Strongly Disagree", "Moderately Disagree",
                             "Slightly Disagree", "Slightly Agree",
                             "Moderately Agree", "Strongly Agree"), 2),
                       levels = c("Strongly Disagree", "Moderately Disagree",
                                  "Slightly Disagree", "Slightly Agree",
                                  "Moderately Agree", "Strongly Agree")),
       percent = c(5, 10, 10, 10, 20, 45,
                   45, 20, 10, 10, 10, 5),
       group = sort(rep(c("Male", "Female"), 6))) %>%
  mutate(display_percent = case_when(grepl("Dis|Neutral", answer) ~ -percent,
                                     TRUE ~ percent))


p5 <- survey_data %>%
  ggplot() +
  geom_col(aes(x = group,
               fill = answer,
               y = percent),
           width = 0.8) +
  labs(title = "How much do they agree with the statement \"Donuts are delicious\"?",
       caption = "Totally made up data!",
       y = "Percent", x = "Group",
       fill = "Answer") +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
  coord_flip() +
  theme(axis.title = element_blank())

p5

p5 +
  scale_fill_presage(
    palette = "diverging_blue_green")


p5 +
  scale_fill_presage(
    palette = "diverging_blue_orange",
    .direction = -1) +
  theme_presage(
    show_grid = FALSE
  )


p5 +
  scale_fill_presage(
    palette = "diverging_blue_orange",
    .direction = -1) +
  theme_presage(
    base_theme = "light", title_face = "bold",
    show_grid = FALSE
  )


###### ggtext examples -------------------------------

data <- tibble(
  bactname = c("Staphylococcaceae", "Moraxella", "Streptococcus", "Acinetobacter"),
  OTUname = c("OTU 1", "OTU 2", "OTU 3", "OTU 4"),
  value = c(-0.5, 0.5, 2, 3)
)

p7 <- data %>% mutate(
  color = c("#009E73", "#D55E00", "#0072B2", "#000000"),
  name = glue("<i style='color:{color}'>{bactname}</i> ({OTUname})"),
  name = fct_reorder(name, value)
)  %>%
  ggplot(aes(value, name, fill = color)) +
  geom_col(alpha = 0.5) +
  scale_fill_identity() +
  labs(caption = "Example posted on **stackoverflow.com**<br>(using made-up data)")


p7 +
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
  )


p7 +
  theme_presage() +
  theme(
    axis.text.y = element_markdown(),
    plot.caption = element_markdown(lineheight = 1.2)
  )







p8 <- ggplot(mtcars, aes(disp, mpg)) +
  geom_point() +
  labs(
    title = "<b>Fuel economy vs. engine displacement</b><br>
    <span style = 'font-size:10pt'>Lorem ipsum *dolor sit amet,*
    consectetur adipiscing elit, **sed do eiusmod tempor incididunt** ut
    labore et dolore magna aliqua. <span style = 'color:red;'>Ut enim
    ad minim veniam,</span> quis nostrud exercitation ullamco laboris nisi
    ut aliquip ex ea commodo consequat.</span>",
    x = "displacement (in<sup>3</sup>)",
    y = "Miles per gallon (mpg)<br><span style = 'font-size:8pt'>A measure of
    the car's fuel efficiency.</span>"
  )


p8 +
  theme_presage()

p8 +
  theme_presage() +
  theme(
   axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0),
      linetype = 1,
      r = grid::unit(8, "pt"),
      fill = "azure1"
    ),
    axis.title.y = element_textbox_simple(
      hjust = 0,
      orientation = "left-rotated",
      minwidth = unit(1, "in"),
      maxwidth = unit(2, "in"),
      padding = margin(4, 4, 2, 4),
      margin = margin(0, 0, 2, 0),
      fill = "lightsteelblue1"
    )
  )











mtcars %>%
  mutate(
    transmission = ifelse(am == 1, "automatic", "manual")
  ) %>%
  ggplot(aes(hp, mpg, color = transmission)) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(automatic = "#0072B2", manual = "#D55E00"),
    guide = "none"
  ) +
  labs(
    x = "Horse power",
    y = "Miles per gallon (MPG)",
    title = "<span style = 'font-size:14pt; font-family:Helvetica;'>Transmission type impacts fuel efficiency</span><br>
MPG is higher for <span style = 'color:#0072B2;'>automatic</span>
than for <span style = 'color:#D55E00;'>manual</span> transmissions"
  ) +
  theme(
    text = element_text(family = "Times"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 11, lineheight = 1.2)
  )


mtcars %>%
  mutate(
    transmission = ifelse(am == 1, "automatic", "manual")
  ) %>%
  ggplot(aes(hp, mpg, color = transmission)) +
  geom_point(size = 2) +
  scale_color_presage() +
  labs(
    x = "Horse power",
    y = "Miles per gallon (MPG)",
    title = "Transmission type impacts fuel efficiency",
    subtitle = "MPG is higher for <span style = 'color:#0072bc;'>**automatic**</span>
than for <span style = 'color:#39b54a;'>**manual**</span> transmissions"
  ) +
  theme_presage()







mtcars %>%
  mutate(
    transmission = ifelse(am == 1, "automatic", "manual")
  ) %>%
  ggplot(aes(hp, mpg, color = transmission)) +
  geom_point(size = 2) +
  scale_color_presage(palette = "diverging_two") +
  labs(
    x = "Horse power",
    y = "Miles per gallon (MPG)",
    title = "**Transmission type impacts fuel efficiency**",
    subtitle = "MPG is higher for <span style = 'color:#0072B2;'>**automatic**</span>
than for <span style = 'color:#D55E00;'>**manual**</span> transmissions"
  ) +
  theme_presage()


mtcars %>%
  mutate(
    transmission = ifelse(am == 1, "automatic", "manual")
  ) %>%
  ggplot(aes(hp, mpg, color = transmission)) +
  geom_point(size = 2) +
  scale_color_presage(palette = "diverging_two") +
  labs(
    x = "Horse power",
    y = "Miles per gallon (MPG)",
    title = "**<i>Transmission type</i> impacts fuel efficiency**",
    subtitle = "MPG is higher for <span style = 'color:#0072B2;'>**automatic**</span>
than for <span style = 'color:#D55E00;'>**manual**</span> transmissions"
  ) +
  theme_presage()

