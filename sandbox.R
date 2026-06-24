# Sandbox 

# load libraries
library(tidyverse)

# load data
games <- read_csv("data/games.csv")
genre_sum <- read_csv("data/genre_summary.csv")
plat_sum <- read_csv("data/platform_summary.csv")
pub_sum <- read_csv("data/publisher_summary.csv")
vg_sales <- read_csv("data/video_games_sales.csv")
y_trends <- read_csv("data/yearly_trends.csv")


# Games Data ----

## Initial Look ----
summary(games)

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
    esrb_rating = as.factor(esrb_rating),
    is_sequel = as.factor(is_sequel),
    online_multiplayer = as.factor(online_multiplayer),
    dlc_released = as.factor(dlc_released),
    microtransactions = as.factor(microtransactions),
    loot_boxes = as.factor(loot_boxes),
    game_pass_available = as.factor(game_pass_available),
    vr_support = as.factor(vr_support),
    goty_nominated = as.factor(goty_nominated),
    goty_won = as.factor(goty_won)
  )


games |>
  count(platform) |>
  arrange(-n) |>
  print(n = 33)


games |>
  ggplot(aes(x = genre)) +
  geom_bar()

games |>
  ggplot(aes(x = esrb_rating)) +
  geom_bar()

games |>
  ggplot(aes(x = esrb_rating)) +
  geom_bar()

games |>
  ggplot(aes(x = online_multiplayer)) +
  geom_bar()

