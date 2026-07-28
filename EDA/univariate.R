# Univariate Analysis 

# load libraries
library(tidyverse)


# load data
games <- read_csv("data/games_cleaned.csv")

# Games Data ----

# large spread - Action is the most popular
games |>
  ggplot(aes(y = genre)) +
  geom_bar()

# T & M are most common followed by E
games |>
  ggplot(aes(x = esrb_rating)) +
  geom_bar()

# even-ish split - no cleansing needed
games |>
  ggplot(aes(x = online_multiplayer)) +
  geom_bar()


# about 60% aren't a sequel - slight imbalance but not too bad
games |>
  ggplot(aes(x = is_sequel)) +
  geom_bar()

# about 60% do have a dlc - a bit more of an imbalance considering the yes column 
# goes past 30k
games |>
  ggplot(aes(x = dlc_released)) +
  geom_bar()

# about 70% don't have microtransactions - major imbalance here
games |>
  ggplot(aes(x = microtransactions)) +
  geom_bar()

# very few have loot boxes- about 90% don't have loot boxes
# major imbalance however very few games allow for loot boxes to be a thing
# in context of the 50k games
games |>
  ggplot(aes(x = loot_boxes)) +
  geom_bar()

# very few are game pass available - similar imbalance to loot boxes
# this can be due to game pass availability varying for different platforms & 
# companies
games |>
  ggplot(aes(x = game_pass_available)) +
  geom_bar()

# very few support vr - extreme imbalance 
# likely due to the accessibility of vr acessories 
games |>
  ggplot(aes(x = vr_support)) +
  geom_bar()

# very few were nominated for game of the year
# this makes sense considering how many games there are and how 
# recent this category is for winning an award for
games |>
  ggplot(aes(x = goty_nominated)) +
  geom_bar()

# even less won the award 
# this tracks since even fewer games actually win game of the year
games |>
  ggplot(aes(x = goty_won)) +
  geom_bar()

# pretty large range of prices $0-$80
# middle 50% ranges from $20 to $60
# centered at $40
games |>
  ggplot(aes(y = launch_price_usd)) +
  geom_bar() 

games |>
  ggplot(aes(x = launch_price_usd)) +
  geom_boxplot() +
  geom_rug()

## Numeric Variables ----

# year of release - centered between early 2000s and late 2010s
games |>
  ggplot(aes(x = year)) +
  geom_boxplot() +
  geom_rug()

# there are some peaks throughout the years, but mostly pretty even
games |>
  ggplot(aes(x = year)) +
  geom_histogram(alpha = 0.6) +
  geom_rug()

# slight skew to the left- scores tend to center around 75
games |>
  ggplot(aes(x = metacritic_score)) +
  geom_density()

# user score has a slight left skew - scores tend to center around 7.5
# pretty similar to metacritic score which is to be expected
games |>
  ggplot(aes(x = user_score)) +
  geom_density() +
  geom_rug()

# national sales has a heavy right skew - a power transformation would be helpful
# for standardizing it since the variable is in the millions
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


# heavy right skew - a power transformation similar to the one used for 
# sales would be helpful for standardizing this variable
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

# # Genre Summary ----
# 
# ## Initial Look ----
# summary(genre_sum)
# 
# # none of the variables appear to be encoded incorrectly 
# # no obvious data cleansing so far
# # there are only 20 observations !
# 
# ## Variables ----
# 
# # all variables of interest are numeric
# 
# # not exactly sure on how to interpret this variable
# # given the context I will proceed under the impression that
# # this variable is the number of titles for all genres
# genre_sum |>
#   ggplot(aes(x = titles)) +
#   geom_histogram()
# 
# # plot to help understand the relationship of titles and genres
# genre_sum |>
#   ggplot(aes(x = titles, y = genre)) +
#   geom_boxplot()
# 
# # mild right skew due to dollar amount
# # money tends to have a right skew especially when > $1000
# genre_sum |>
#   ggplot(aes(x = total_sales_m)) +
#   geom_density()
# 
# # also mild right skew since it is the avg of a heavy right skew
# genre_sum |>
#   ggplot(aes(x = avg_sales_m)) +
#   geom_density()
# 
# # mild left skew with a peak at 74
# genre_sum |>
#   ggplot(aes(x = avg_metacritic)) +
#   geom_density()
# 
# # mild left skew with a peak at 7.5
# genre_sum |>
#   ggplot(aes(x = avg_user_score)) +
#   geom_density()
# 
# 
# # just under 50% got goty nominated
# genre_sum |>
#   ggplot(aes(x = pct_goty_nominated)) +
#   geom_density()
# 
# # heavy right skew - htlb?
# genre_sum |>
#   ggplot(aes(x = avg_htlb_main)) +
#   geom_density()
# 
# # peak at 0% and 1%
# genre_sum |>
#   ggplot(aes(x = pct_online)) +
#   geom_density() +
#   geom_rug()
# 
# # peak at 0.62
# genre_sum |>
#   ggplot(aes(x = pct_dlc)) +
#   geom_density()
# 
# # peak at 0.25 and 0.5 
# genre_sum |>
#   ggplot(aes(x = pct_microtransactions)) +
#   geom_histogram()
# 
# ###
# # Note: there are a lot of money related variables that will need a power transformation
# # due to the small number of observations, I may not use this dataset
# ###
# 
# # Platform Summary ----
# 
# ## Initial Look ----
# 
# # there are only 33 observations so we will proceed with caution
# 
# summary(plat_sum)
# 
# ## Variables ----
# 
# # top genre variable repeats "Action" for all observations
# plat_sum |>
#   distinct(top_genre)
# 
# # This dataset shares variables with the genre summary data, publisher data, &
# # yearly trend data however, all the datasets have varying amounts of observations
# # I might disregard these datasets due to the low number of observations and repetition of
# # variables or I might try to combine the data
# 
# # Lets check the video game sales set first
# 
# 
# # Video Game Sales Summary ----
# 
# ## Initial Look ----
# 
# summary(vg_sales)
# 
# # year is a character variable that will make numeric or a factor
# # genre is character which could be fine, but might be more helpful as a factor
# # possibly the same with platform
# 
# ## Data Cleansing ----
# 
# vg_sales <- vg_sales |>
#   mutate(
#     Year = as.numeric(Year),
#     Genre = as.factor(Genre),
#     Platform = as.factor(Platform)
#   )
# 
# ## Variables ----
# 
# 
# # range of years is 1980 to 2020
# vg_sales |>
#   ggplot(aes(x = Year)) +
#   geom_density()
# 
# # 12 genres in this data set
# vg_sales |>
#   ggplot(aes(x = Genre)) +
#   geom_bar()
# 
# # 31 different platforms in this data set
# vg_sales |>
#   ggplot(aes(x = Platform)) +
#   geom_bar()
# 
# # finiding most popular platforms
# # DS, PS2, PS3 are the top 3 
# vg_sales |>
#   count(Platform) |>
#   arrange(desc(n))
# 
# # extreme right skew
# vg_sales |>
#   ggplot(aes(x = NA_Sales)) +
#   geom_density()
# 
# # similar to NA Sales
# vg_sales |>
#   ggplot(aes(x = EU_Sales)) +
#   geom_density()
# 
# # similar to NA Sales
# vg_sales |>
#   ggplot(aes(x = JP_Sales)) +
#   geom_density()
# 
# # similar to NA Sales
# vg_sales |>
#   ggplot(aes(x = Other_Sales)) +
#   geom_density()
# 
# # similar to NA Sales
# vg_sales |>
#   ggplot(aes(x = Global_Sales)) +
#   geom_density()

# Considering the Games Data contained essentially all the same information with 
# more observations, I believe I can disregard these extra data sets for now
