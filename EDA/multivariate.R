# Multivariate


# load libraries
library(tidyverse)
library(ggthemes)
library(showtext)
library(here)

# free font from google thats similar to Akkurate Pro
font_add_google("Noto Serif")


# showtext auto and add font
showtext_auto()

# load data
games_cleaned <- read_csv(here("data/games_cleaned.csv")) 


# year vs genre with platformtype
y_vs_genre_plattype <- games_cleaned |>
  ggplot(aes(x = year, y = genre, fill = platform_type)) +
  geom_boxplot(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribution of Year Released vs Genre",
    x = "Year",
    y = "Genre",
    fill = "Platform Type"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# it appears video games were mainly popular from early 2000s to late 2010s
# I'm surprised the spread doesn't extend to 2020 and beyond due to the pandemic
# more people turned to gaming while quarantining at home


main_vs_comp_lp <- games_cleaned |>
  ggplot(aes(x = how_long_to_beat_main_hrs, y = how_long_to_beat_completionist_hrs)) +
  geom_point(aes(color = launch_price_usd)) +
  theme_minimal() +
  labs(
    title = "Distribution of Length to Beat Main Story \nvs Length to Complete the Game",
    x = "Main Story\n(Hours)",
    y = "Complete\n(Hours)",
    color = "Launch Price\n(USD)"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

main_vs_comp_plattype <- games_cleaned |>
  ggplot(aes(x = how_long_to_beat_main_hrs, y = how_long_to_beat_completionist_hrs)) +
  geom_point(aes(color = platform_type)) +
  theme_minimal() +
  labs(
    title = "Distribution of Length to Beat Main Story \nvs Length to Complete the Game",
    x = "Main Story\n(Hours)",
    y = "Complete\n(Hours)",
    color = "Platform Type"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

esti_vs_gs_lp <- games_cleaned |>
  ggplot(aes(x = estimated_revenue_million_usd, y = global_sales_million)) +
  geom_point(aes(color = launch_price_usd)) + 
  theme_minimal() +
  labs(
    title = "Distribution of Estimated Revenue vs Global Sales with launch Price",
    x = "Estimated Revenue\n(by millions in USD)",
    y = "Global Sales\n(by millions in USD)",
    color = "Launch Price"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )

# save out figures
ggsave(here("figures/y_vs_genre_plattype.png"), plot = y_vs_genre_plattype)
ggsave(here("figures/main_vs_comp_lp.png"), plot = main_vs_comp_lp)
ggsave(here("figures/main_vs_comp_plattype.png"), plot = main_vs_comp_plattype)
ggsave(here("figures/esti_vs_gs_lp.png"), plot = esti_vs_gs_lp)