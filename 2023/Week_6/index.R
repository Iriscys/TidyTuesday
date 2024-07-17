library(cowplot) 
library(ggpubr)
library(here)
library(png)
library(scales)
library(tidyverse)

# load data set
df <- read.csv(here("data", "big_tech_stock_prices.csv")) 

# data cleaning
df <- df |>
  select(c("stock_symbol", "date", "adj_close")) |>
  filter(stock_symbol %in% c("AAPL", "AMZN", "GOOGL", "META", "MSFT")) |>
  mutate(
    date = as.Date(date, "%d/%m/%Y"),
    stock_symbol = fct_relevel(stock_symbol, c("MSFT","META", "GOOGL","AMZN", "AAPL"))
  ) |>
  filter(date %in% c("2020-01-02", "2020-12-31", "2021-01-04", "2021-12-31", "2022-01-03", "2022-12-29")) |>
  mutate(date = case_when(
    date == "2020-01-02" ~ "start_20",
    date == "2020-12-31" ~ "end_20",
    date == "2021-01-04" ~ "start_21",
    date == "2021-12-31" ~ "end_21",
    date == "2022-01-03" ~ "start_22",
    date == "2022-12-29" ~ "end_22"
  )) |>
  pivot_wider(
    id_cols = stock_symbol,
    names_from = date,
    values_from = adj_close
  ) |>
  rowwise() |>
  mutate(
    "change_2020" = round((end_20 - start_20) / start_20 * 100, 0),
    "change_2021" = round((end_21 - start_21) / start_21 * 100, 0),
    "change_2022" = round((end_22 - start_22) / start_22 * 100, 0)
  ) |>
  select(c("stock_symbol", "change_2020", "change_2021", "change_2022")) |>
  pivot_longer(
    cols = starts_with("change"),
    names_to = "year",
    values_to = "pct",
    names_prefix = "change_"
  )

# plot
p1 <- ggplot(df, aes(x = stock_symbol, y = pct, fill = year)) +
  annotate("rect", 
           xmin = 5.5, xmax = 4.5, 
           ymin = -80, ymax = 90,
           alpha = .7,fill = "#ecf1f7") +
  annotate("rect", 
           xmin = 3.5, xmax = 2.5, 
           ymin = -80, ymax = 90,
           alpha = .7,fill = "#ecf1f7") +
  annotate("rect", 
           xmin = 1.5, xmax = 0.5, 
           ymin = -80, ymax = 90,
           alpha = .7,fill = "#ecf1f7") +
  geom_bar(stat = "identity",
           position = "dodge",
           width = 0.9) +
  geom_text(
    aes(label = ifelse(pct < 0, paste0(pct, "%"), "")), 
    hjust = 1.3,
    position = position_dodge(0.9),
    color = "#91a8ae",
    size = 3,
    family = "Andale Mono") +
  geom_text(
    aes(label = ifelse(pct > 0 & year == "2021", paste0(pct, "%"), "")), 
    hjust = -0.3,
    position = position_dodge(0.8),
    color = "#6acfc9",
    size = 3,
    family = "Andale Mono") +
  geom_text(
    aes(label = ifelse(pct > 0 & year == "2020", paste0(pct, "%"), "")), 
    hjust = -0.3,
    position = position_dodge(1),
    color = "#0a3b7e",
    size = 3,
    family = "Andale Mono") +
  coord_flip() +
  scale_fill_manual(values = c("#0a3b7e", "#6acfc9", "#91a8ae")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(10, "point"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    plot.margin = margin(1.5,0.1,0.5,3, "cm"),
    text = element_text(family = "Andale Mono")
  )

# import company icon
alphabet <- png::readPNG(here("images", "alphabet.png"))
amazon <- png::readPNG(here("images", "amazon.png"))
apple <- png::readPNG(here("images", "apple.png"))
meta <- png::readPNG(here("images", "meta.png"))
microsoft <- png::readPNG(here("images", "microsoft.png"))

p2 <- ggdraw() +
  draw_image(apple,  x = -0.385, y = 0.3, scale = 0.13) +
  draw_image(amazon,  x = -0.385, y = 0.14, scale = 0.17) +
  draw_image(alphabet,  x = -0.385, y = 0, scale = 0.15) +
  draw_image(meta,  x = -0.385, y = -0.14, scale = 0.17) +
  draw_image(microsoft,  x = -0.385, y = -0.28, scale = .21) +
  draw_plot(p1) 

p2 +
  annotate(
    "text",
    x = 0.4,
    y = 0.93,
    label = "Big Tech Performance 2020 - 2022",
    size = 7,
    fontface = "bold",
    family = "Andale Mono"
  )
