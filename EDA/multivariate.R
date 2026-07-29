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


games_cleaned |>
  ggplot(aes(x = how_long_to_beat_main_hrs, y = how_long_to_beat_completionist_hrs)) +
  geom_point(aes(color = launch_price_usd))

games_cleaned |>
  ggplot(aes(x = how_long_to_beat_main_hrs, y = how_long_to_beat_completionist_hrs)) +
  geom_point(aes(color = platform_type))

games_cleaned |>
  ggplot(aes(x = estimated_revenue_million_usd, y = global_sales_million)) +
  geom_point(aes(color = launch_price_usd))

# save out figures
ggsave(here("figures/y_vs_genre_plattype.png"), plot = y_vs_genre_plattype)
