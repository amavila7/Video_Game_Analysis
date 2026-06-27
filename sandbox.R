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


## Factor Variables ----

# PC has the most games followed by mobile
games |>
  count(platform) |>
  arrange(-n) |>
  print(n = 33)

# large spread
games |>
  ggplot(aes(x = genre)) +
  geom_bar()

# T & M are most common
games |>
  ggplot(aes(x = esrb_rating)) +
  geom_bar()

# even-ish split
games |>
  ggplot(aes(x = online_multiplayer)) +
  geom_bar()


# about 2/3 aren't a sequel
games |>
  ggplot(aes(x = is_sequel)) +
  geom_bar()

# majority do have a dlc
games |>
  ggplot(aes(x = dlc_released)) +
  geom_bar()

# majority don't have microtransactions
games |>
  ggplot(aes(x = microtransactions)) +
  geom_bar()

# very few have loot boxes
games |>
  ggplot(aes(x = loot_boxes)) +
  geom_bar()

# very few are game pass available
games |>
  ggplot(aes(x = game_pass_available)) +
  geom_bar()

# very few support vr
games |>
  ggplot(aes(x = vr_support)) +
  geom_bar()

# very few were nominated for game of the year
games |>
  ggplot(aes(x = goty_nominated)) +
  geom_bar()

# even less won the award 
games |>
  ggplot(aes(x = goty_won)) +
  geom_bar()


## Numeric Variables ----

# year of release - many between early 2000s and late 2010s
games |>
  ggplot(aes(x = year)) +
  geom_boxplot()

# slight skew to the left
games |>
  ggplot(aes(x = metacritic_score)) +
  geom_density()

# user score has a slight left skew
games |>
  ggplot(aes(x = user_score)) +
  geom_histogram(bins = 50) +
  geom_rug()

# national sales has a heavy right skew
games |>
  ggplot(aes(x = na_sales_million)) +
  geom_density()

# similar to na sales
games |>
  ggplot(aes(x = eu_sales_million)) +
  geom_density()

# similar to na sales
games |>
  ggplot(aes(x = jp_sales_million)) +
  geom_density()

# similar to na sales
games |>
  ggplot(aes(x = other_sales_million)) +
  geom_density()

# similar to na sales
games |>
  ggplot(aes(x = global_sales_million)) +
  geom_density()

# similar to na sales
games |>
  ggplot(aes(x = estimated_revenue_million_usd)) +
  geom_density()


# pretty large range of prices $0-$80
games |>
  ggplot(aes(x = launch_price_usd)) +
  geom_density() +
  geom_rug()

# heavy right skew
games |>
  ggplot(aes(x = how_long_to_beat_main_hrs)) +
  geom_density()

# similar to main hours
games |>
  ggplot(aes(x = how_long_to_beat_completionist_hrs)) +
  geom_density()


# Considering the large number of numeric variables with heavy skews, I will 
# use power transformations to standardize the variables more for the potential
# predictive models



### 
# Note: The other datasets have way less observations & will be examined with 
# caution 
###

# Genre Summary ----

## Initial Look ----
summary(genre_sum)

# none of the variables appear to be encoded incorrectly 
# no obvious data cleansing so far
# there are only 20 observations !

## Variables ----

# all variables of interest are numeric

# not exactly sure on how to interpret this variable
# given the context I will proceed under the impression that
# this variable is the number of titles for all genres
genre_sum |>
  ggplot(aes(x = titles)) +
  geom_histogram()

# plot to help understand the relationship of titles and genres
genre_sum |>
  ggplot(aes(x = titles, y = genre)) +
  geom_boxplot()

# mild right skew due to dollar amount
# money tends to have a right skew especially when > $1000
genre_sum |>
  ggplot(aes(x = total_sales_m)) +
  geom_density()

# also mild right skew since it is the avg of a heavy right skew
genre_sum |>
  ggplot(aes(x = avg_sales_m)) +
  geom_density()

# mild left skew with a peak at 74
genre_sum |>
  ggplot(aes(x = avg_metacritic)) +
  geom_density()

# mild left skew with a peak at 7.5
genre_sum |>
  ggplot(aes(x = avg_user_score)) +
  geom_density()


# just under 50% got goty nominated
genre_sum |>
  ggplot(aes(x = pct_goty_nominated)) +
  geom_density()

# heavy right skew - htlb?
genre_sum |>
  ggplot(aes(x = avg_htlb_main)) +
  geom_density()

# peak at 0% and 1%
genre_sum |>
  ggplot(aes(x = pct_online)) +
  geom_density() +
  geom_rug()

# peak at 0.62
genre_sum |>
  ggplot(aes(x = pct_dlc)) +
  geom_density()

# peak at 0.25 and 0.5 
genre_sum |>
  ggplot(aes(x = pct_microtransactions)) +
  geom_histogram()

###
# Note: there are a lot of money related variables that will need a power transformation
# due to the small number of observations, I may not use this dataset
###

# Platform Summary ----

## Initial Look ----

# there are only 33 observations so we will proceed with caution

summary(plat_sum)

## Variables ----

# top genre variable repeats "Action" for all observations
plat_sum |>
  distinct(top_genre)

  



# Publisher Summary ----

## Initial Look ----



# Video Game Sales Summary ----

## Initial Look ----



# Yearly Trends ----

## Initial Look ----