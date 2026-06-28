# Exploratory Data Analysis

# The sandbox script includes uni-variate analysis for all the data
# This script is for bivariate and multivariate relations

# load libraries
library(tidyverse)

# load data
games_cleaned <- read_csv("data/games.csv") |>
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


# Genre vs Year
games_cleaned |>
  ggplot(aes(x = year, y = genre)) +
  geom_boxplot()

# it appears video games were mainly popular from early 2000s to late 2010s
# I'm surprised the spread doesn't extend to 2020 and beyond due to the pandemic
# more people turned to gaming while quarantining at home


# Platform vs Year
games_cleaned |>
  ggplot(aes(x = year, y = platform)) +
  geom_boxplot()
