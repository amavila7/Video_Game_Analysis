# Classification Problem ----
# Step 1: initial setup
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(themis)

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
    goty_nominated = as.factor(goty_nominated),
    goty_won = as.factor(goty_won)
  )

# check distribution
goty_won_dist <- games_cleansed |>
  ggplot(aes(x = goty_won)) +
  geom_bar() +
  labs(
    title = "Distribution of Game of the Year Award Won",
    x = "Game of the Year Award Won"
  )

# large imbalance between the two - needs upsampling

# initial split ----
game_split <- initial_split(games_cleansed, prop = 0.8, strata = goty_won)

game_training <- training(game_split)
game_testing <- testing(game_split)


# now upsampling the training
recipe_upsample <- recipe(goty_won ~., game_training) |>
  # 2:1 ratio
  step_upsample(goty_won, over_ratio = 0.5) |>
  update_role(game_id, title, publisher, developer, new_role = "id_vars") 

# extract modified train from the recipe
train_upsampled <- prep(recipe_upsample) |>
  bake(new_data = NULL)

# verify
train_upsampled |>
  count(goty_won)

# building folds
# creating v folds
game_folds <- vfold_cv(train_upsampled, v = 5, repeats = 3, strata = goty_won)


# saving out dist and cleaned data sets ---
save(games_cleansed, file = here("attempt_1/datasets/game_data.rda"))
save(game_folds, file = here("attempt_1/datasets/game_folds.rda"))
save(train_upsampled, file = here("attempt_1/datasets/train_upsampled.rda"))
save(game_testing, file = here("attempt_1/datasets/game_testing.rda"))
ggsave("figures/goty_won_dist.png", plot = goty_won_dist)
