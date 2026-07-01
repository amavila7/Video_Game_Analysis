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


   
# Do certain genres usually sell more than others?


# global sales
games_cleaned |>
  ggplot(aes(x = global_sales_million, y = genre)) +
  geom_boxplot()

# highest sales
# Sports, Shooter, Sandbox, Fighting, & Action have the greatest sales globally

# lowest sales
# visual novel, strategy, rhythm, & Idle/clicker

# launch price vs genre
games_cleaned |>
  ggplot(aes(x = launch_price_usd, y = genre)) +
  geom_boxplot()

# the spread per genre is quite evenly centered around $40
# survival horror, sports, simulation, shooter, puzzle, MMORPG, and Idle/clicker 
# are around $50
# the entire range is $0-$80 without any visual outliers

# launch price vs global sales
games_cleaned |>
  ggplot(aes(x = global_sales_million, y = launch_price_usd)) +
  geom_point()

# launch price seems to be more categorical 
# the best sales are the free games or games under $10
# there is an increase of global sales for games sold at $50+

# 14 distinct prices - changing this to be a factor
games_cleaned |>
  distinct(launch_price_usd) 

games_cleaned <- games_cleaned |>
  mutate(launch_price_usd = as.factor(launch_price_usd))

# boxplot - interesting how it changes to more exact pricing
games_cleaned |>
  ggplot(aes(x = global_sales_million, y = launch_price_usd)) +
  geom_boxplot()

# launched price vs genre
games_cleaned |>
  ggplot(aes(y = genre, fill = launch_price_usd)) +
  geom_bar()

# No genre is cheaper than the other
# all genres have a large range of prices
# there are fewer video games for different genres
# action, sports, shooter, and role-playing are the most popular


games_cleaned |>
  ggplot() +
  geom_violin(aes(x = global_sales_million, y = genre)) +
  geom_boxplot(aes(x = global_sales_million, y = genre, fill = launch_price_usd))



# Do multiplayer games increase sales?
   
games_cleaned |>
  ggplot(aes(x = global_sales_million, y = online_multiplayer)) +
  geom_boxplot()

games_cleaned |>
  ggplot(aes(x = na_sales_million, y = online_multiplayer)) +
  geom_boxplot()

games_cleaned |>
  ggplot(aes(x = jp_sales_million, y = online_multiplayer)) +
  geom_boxplot()

games_cleaned |>
  ggplot(aes(x = eu_sales_million, y = online_multiplayer)) +
  geom_boxplot()

games_cleaned |>
  ggplot(aes(x = other_sales_million, y = online_multiplayer)) +
  geom_boxplot()


# How do microtransactions in a video game impact sales and launch price?

# launched price vs microtransactions
games_cleaned |>
  ggplot(aes(y = launch_price_usd, fill = microtransactions)) +
  geom_bar()

# majority of the games don't have microtransactions
# some free games do have microtransactions which is ironic since the game 
# itself was free

# global sales vs micro
games_cleaned |>
  ggplot(aes(x = global_sales_million, color = microtransactions)) +
  geom_density()

