# Classification Problem ----
# Step 1: initial setup
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handling the conflicts
tidymodels_prefer()

# load data
game_data <- read_csv(here("data/games.csv"))

# set seed
set.seed(792158)

# fixing train set ----


# fixing var types
games_cleansed <- game_data |>
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
    online_multiplayer = factor(online_multiplayer),
    dlc_released = as.factor(dlc_released),
    microtransactions = as.factor(microtransactions),
    loot_boxes = as.factor(loot_boxes),
    game_pass_available = as.factor(game_pass_available),
    vr_support = as.factor(vr_support),
    goty_nominated = as.factor(goty_nominated)
  )

# check distribution
goty_won_dist <- games_cleansed |>
  ggplot(aes(x = goty_won)) +
  geom_bar() +
  labs(
    title = "Distribution of Game of the Year Award Won",
    x = "Game of the Year Award Won"
  )

# pretty solid balance of the two categories 

# initial split ----
airbnb_split <- initial_split(airbnb_data, prop = 0.8, strata = host_is_superhost)

airbnb_training <- training(airbnb_split)
airbnb_testing <- testing(airbnb_split)



# building folds
# creating v folds
airbnb_folds <- vfold_cv(airbnb_training, v = 5, repeats = 3, strata = host_is_superhost)


# saving out dist and cleaned data sets ---
save(airbnb_data, file = here("attempt_1/data/airbnb_data.rda"))
save(airbnb_folds, file = here("attempt_1/data/airbnb_folds.rda"))
save(airbnb_training, file = here("attempt_1/data/airbnb_training.rda"))
save(airbnb_testing, file = here("attempt_1/data/airbnb_testing.rda"))
ggsave("plots/super_host_dist.png", plot = super_host_dist)