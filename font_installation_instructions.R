# Font installation instructions

## Based on: https://www.cararthompson.com/posts/2024-01-12-using-fonts-in-r-for-dataviz/2024-01-12_getting-fonts-to-work

# 1. Fonts are located in fonts folder. Select all fonts, right-click and "Install for all users".
# 2. Run the following command and search for the installed fonts:
systemfonts::system_fonts() |> View()
## Once you find your font, copy the value from the family column.

# 3. In your RStudio Global options, navigate to your graphics options, and set the graphics device to AGG using the dropdown menu.
# 4. Check if a desired font is being used in RStudio Plots pane by providing the copied font family:
library(ggplot2)

p <- trees |>
  ggplot(aes(x = Girth,
             y = Height,
             size = Volume,
             colour = Volume)) +
  geom_point() +
  labs(title = "Did the font work? Let's hope so!",
       subtitle = "If it did, your plot should look the same as this.") +
  theme_minimal()

p +
  theme(text = element_text(family = "Libre Franklin"),
        legend.position = "none")

# 5. Check everything works when you save (note: theme_minimal has transparent background, so we provide bg in ggsave):
ggsave(filename = "plot.png",
       dpi = 400,
       height = 5, width = 8,
       bg = "#FFFFFF")

# 6. Use a font variant:
systemfonts::register_variant(name = "LibreFranklin-Medium", family = "Libre Franklin", weight = "medium")

p + theme(text = element_text(family = "LibreFranklin-Medium"),
        legend.position = "none")


systemfonts::register_variant(name = "LibreFranklin-Bold", family = "Libre Franklin", weight = "bold")

p + theme(text = element_text(family = "LibreFranklin-Bold"),
          legend.position = "none")
