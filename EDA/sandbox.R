# Sandbox 

# load libraries
library(tidyverse)
library(ggthemes)
library(showtext)

# load data
games <- read_csv("data/games.csv")

# free font from google thats similar to Akkurate Pro
font_add_google("Noto Serif")

# showtext auto and add font
showtext_auto()

# Games Data ----

## Initial Look ----
summary(games)

# missingness check
missing_plot <- games |> 
  naniar::gg_miss_var() +
  labs(
    title = "Distribution of Missingness Per Variable",
    y = "Missingness",
    x = "Variables"
  ) +
  theme(
    axis.title = element_text(family = "Noto Serif"),
    axis.text = element_text(family = "Noto Serif"),
    plot.title = element_text(family = "Noto Serif", hjust = 0.5, size = 20),
    plot.title.position = "panel"
  )


ggsave(("figures/missing_plot.png"), plot = missing_plot)

# some variables are loaded as numeric or character rather than factor
## Cleaning variables ----
games <- games |>
  mutate(
    platform = as.factor(platform),
    platform_type = as.factor(platform_type),
    platform_maker = as.factor(platform_maker),
    platform_generation = as.factor(platform_generation),
    genre = as.factor(genre),
    publisher_region = as.factor(publisher_region),
    publisher_tier = as.factor(publisher_tier),
    launch_price_usd = as.factor(launch_price_usd),
    esrb_rating = as.factor(esrb_rating),
    is_sequel = as.factor(is_sequel),
    is_sequel = fct_recode(is_sequel, "Yes" = "1", "No" = "0"),
    online_multiplayer = factor(online_multiplayer),
    online_multiplayer = fct_recode(online_multiplayer, "Yes" = "1", "No" = "0"),
    dlc_released = as.factor(dlc_released),
    dlc_released = fct_recode(dlc_released, "Yes" = "1", "No" = "0"),
    microtransactions = as.factor(microtransactions),
    microtransactions = fct_recode(microtransactions, "Yes" = "1", "No" = "0"),
    loot_boxes = as.factor(loot_boxes),
    loot_boxes = fct_recode(loot_boxes, "Yes" = "1", "No" = "0"),
    game_pass_available = as.factor(game_pass_available),
    game_pass_available = fct_recode(game_pass_available, "Yes" = "1", "No" = "0"),
    vr_support = as.factor(vr_support),
    vr_support = fct_recode(vr_support, "Yes" = "1", "No" = "0"),
    goty_nominated = as.factor(goty_nominated),
    goty_nominated = fct_recode(goty_nominated, "Yes" = "1", "No" = "0"),
    goty_won = as.factor(goty_won),
    goty_won = fct_recode(goty_won, "Yes" = "1", "No" = "0"),
  )


# PC has the most games followed by mobile
games |>
  count(platform) |>
  arrange(-n) |>
  print(n = 33)


# More finds ---- 
games |>
  select(title, genre, launch_price_usd, global_sales_million) |>
  arrange(-global_sales_million)

# NHL 2025 launch price was $2.99, but global sales was $1495mil

games |>
  select(title, genre, launch_price_usd, global_sales_million) |>
  arrange(global_sales_million)

# Batman: Arkham Horizon, visual novel, $40 for launch price, and only $0.05 mil

games |>
  select(title, genre, launch_price_usd, global_sales_million) |>
  arrange(-launch_price_usd)

games |>
  select(title, genre, launch_price_usd, global_sales_million) |>
  arrange(launch_price_usd)


# !!!
games |>
  select(title, platform, launch_price_usd, global_sales_million, publisher_tier, esrb_rating, year) |>
  filter(publisher_tier == "Indie") |>
  arrange(global_sales_million)


games |>
  select(
    launch_price_usd, genre, how_long_to_beat_main_hrs, 
    how_long_to_beat_completionist_hrs, publisher_tier, platform, 
    global_sales_million, title
    ) |>
  arrange(how_long_to_beat_main_hrs)

# XCOM 6 is an Indie game with the least amount of hours to beat - 6.02 mil, 80 lp, 1 hr, 3.6 to complete


games |>
  select(
    title, genre, how_long_to_beat_main_hrs, 
    how_long_to_beat_completionist_hrs, publisher_tier, platform, 
    global_sales_million, launch_price_usd
  ) |>
  arrange(-how_long_to_beat_main_hrs)

# Final Fantasy XIV:Shadow War is the longest game to beat main hours at 378hrs, $50 lp, $4.02 mil, AA and on PC, 1107 hrs to complete
# and its AA


games |>
  count(genre) |>
  arrange(-n)


games |>
  arrange(year)


# 14 distinct prices - change this to be a factor
games_cleaned |>
  distinct(launch_price_usd)
