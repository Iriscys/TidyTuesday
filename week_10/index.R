library(cowplot)
library(extrafont)
library(ggpubr)
library(here)
library(imputeTS)
library(png)
library(scales)
library(tidyverse)
library(waffle)

# load data set
df <- read.csv(here("data", "trashwheel.csv")) 

### clean data set
trash_lv <- c("Cigarette Butts", "Glass Bottles", "Plastic Bags", "Plastic Bottles",
              "Polystyrene", "Sports Balls", "Wrappers") 

df1 <- df |>
  filter(Year == 2023) |>
  select(Name, Month, PlasticBottles, Polystyrene, CigaretteButts, GlassBottles,
         PlasticBags, Wrappers, SportsBalls) |> 
  mutate_if(is.numeric, ~replace_na(., 0)) |> 
  pivot_longer( 
    cols = c("PlasticBottles", "Polystyrene", "CigaretteButts", "GlassBottles",
             "PlasticBags", "Wrappers", "SportsBalls"),
    names_to = "trash_type",
    values_to = "trash_amount"
  ) |>
  mutate(trash_type = case_when(
    trash_type == "PlasticBottles" ~ "Plastic Bottles",
    trash_type == "CigaretteButts" ~ "Cigarette Butts",
    trash_type == "GlassBottles" ~ "Glass Bottles",
    trash_type == "PlasticBags" ~ "Plastic Bags",
    trash_type == "SportsBalls" ~ "Sports Balls",
    TRUE ~ trash_type
  )) |>
  group_by(Name, trash_type) |> 
  summarise(trash_amount = sum(trash_amount)) |>
  mutate(trash_type = fct_relevel(trash_type, trash_lv)) 

# calculate the total amount by wheel
wheel_total <- df1 |>
  group_by(Name) |>
  summarise(total = sum(trash_amount))

# calculate the percentage by trash type for each wheel
df1 <- df1 |>
  mutate( 
    total_wheel = case_when(
      Name == "Captain Trash Wheel" ~ wheel_total$total[wheel_total$Name == "Captain Trash Wheel"],
      Name == "Gwynnda Trash Wheel" ~ wheel_total$total[wheel_total$Name == "Gwynnda Trash Wheel"],
      Name == "Mister Trash Wheel" ~ wheel_total$total[wheel_total$Name == "Mister Trash Wheel"],
      Name == "Professor Trash Wheel" ~ wheel_total$total[wheel_total$Name == "Professor Trash Wheel"]),
    trash_pct = floor(trash_amount / total_wheel * 100) 
  )

# prepare icon list for the plot
icon_ls <- c("smoking", "shopping-bag", "wine-bottle",
             "hockey-puck", "box-open", "glass-whiskey", "basketball-ball")

# import trash wheel icon
captain <- png::readPNG(here("images", "captain.png"))
gwynnda <- png::readPNG(here("images", "gwynnda.png"))
mister <- png::readPNG(here("images", "mister.png"))
professor <- png::readPNG(here("images", "professor.png"))

# For first time geom_pictogram icon user may need to operate following steps to import the icon file:
# waffle::install_fa_fonts() # check the location of font in your computer and install them
# font_import() # import font to R
# fa_grep("house") # check icon with keywords, eg. house
# To know more: https://rud.is/rpubs/building-pictograms.html

# create plot
p1 <- ggplot(df1, aes(label = trash_type, values = trash_pct)) +
  geom_pictogram(
    aes(color = trash_type),
    make_proportional = TRUE,
    size = 3,
    flip = TRUE
  ) +
  scale_label_pictogram(
    name = "",
    values = icon_ls
  ) +
  scale_color_manual(name = "",
                     values = c("#9bb996", "#81af9b", "#45ada8", "#547980", "#594f4f")) +
  facet_wrap(~Name, nrow = 2) + 
  labs(
    title = "Trash collection proportion in 2023",
    caption = "Source: Trash collection data in 2023 retreived from TidyTuesday 2024"
  ) +
  coord_equal() +
  theme_minimal() + 
  theme_enhance_waffle() + 
  theme(legend.position = "bottom", 
        legend.text = element_text(hjust = 0, 
                                   vjust = 0.7, 
                                   size = 6),
        plot.title = element_text(hjust = 0.5,
                                  vjust = 1.5,
                                  size = 11),
        plot.caption = element_text(hjust = 2,
                                    size = 5),
        strip.text = element_blank(),
        text = element_text(family = "Comic Sans MS")
  )

# add trash wheel icon
p2 <- ggdraw() +
  draw_image(captain,  x = -0.38, y = 0.2, scale = .21) +
  draw_image(gwynnda,  x = 0.38, y = 0.19, scale = .23) +
  draw_image(mister,  x = -0.385, y = -0.23, scale = .2) +
  draw_image(professor,  x = 0.39, y = -0.23, scale = .2) +
  draw_plot(p1)

# add annotation
p2 +
  annotate(
    "text",
    x = 0.1,
    y = 0.84,
    label = "Captain\nTrash Wheel",
    size = 3,
    color = "#c48d2c",
    fontface = "italic",
    family = "Comic Sans MS"
  ) +
  annotate(
    geom = "curve", 
    x = 0.1, xend = 0.2,
    y = 0.88, yend = 0.91, 
    curvature = -.3, 
    color = "#c48d2c",
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    "text",
    x = 0.1,
    y = 0.39,
    label = "Mister\nTrash Wheel",
    size = 3,
    color = "#25a5d8",
    fontface = "italic",
    family = "Comic Sans MS"
  ) +
  annotate(
    geom = "curve", 
    x = 0.1, xend = 0.2,
    y = 0.43, yend = 0.48, 
    curvature = -.3, 
    color = "#25a5d8",
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    "text",
    x = 0.9,
    y = 0.84,
    label = "Gwynnda\nTrash Wheel",
    size = 3,
    color = "#d19cc7",
    fontface = "italic",
    family = "Comic Sans MS"
  ) +
  annotate(
    geom = "curve", 
    x = 0.9, xend = 0.8,
    y = 0.88, yend = 0.91, 
    curvature = .3, 
    color = "#d19cc7",
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    "text",
    x = 0.9,
    y = 0.39,
    label = "Professor\nTrash Wheel",
    size = 3,
    color = "#7fd39a",
    fontface = "italic",
    family = "Comic Sans MS"
  ) +
  annotate(
    geom = "curve", 
    x = 0.9, xend = 0.8,
    y = 0.43, yend = 0.48, 
    curvature = .3, 
    color = "#7fd39a",
    arrow = arrow(length = unit(2, "mm"))
  )
