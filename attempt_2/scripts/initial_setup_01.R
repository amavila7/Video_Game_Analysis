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
set.seed(981235)

# fixing train set ----


# fixing var types
games_cleansed <- game_data |>
  mutate(
    goty_won = as.factor(goty_won)
  )



# large imbalance between the two - needs upsampling
# we know from attempt 1 & EDA

# initial split ----
game_split <- initial_split(games_cleansed, prop = 0.8, strata = goty_won)

game_training <- training(game_split)
game_testing <- testing(game_split)


# now upsampling the training
recipe_upsample <- recipe(goty_won ~., game_training) |>
  # 2:1 ratio
  step_upsample(goty_won, over_ratio = 0.4) |>
  update_role(game_id, title, publisher, developer, new_role = "id_vars") 

# extract modified train from the recipe
train_upsampled <- prep(recipe_upsample) |>
  bake(new_data = NULL)

# verify
train_upsampled |>
  count(goty_won)

# building folds
# creating v folds
game_folds <- vfold_cv(train_upsampled, v = 4, repeats = 4, strata = goty_won)


# saving out dist and cleaned data sets ---
save(games_cleansed, file = here("attempt_2/datasets/game_data.rda"))
save(game_folds, file = here("attempt_2/datasets/game_folds.rda"))
save(train_upsampled, file = here("attempt_2/datasets/train_upsampled.rda"))
save(game_testing, file = here("attempt_2/datasets/game_testing.rda"))

