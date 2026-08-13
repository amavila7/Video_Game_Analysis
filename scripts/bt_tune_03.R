# Classification Problem ----
# Step 3: boosted trees model specification and fitting
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)
library(bonsai)

# handling the conflicts
tidymodels_prefer()

# load data
load(here("attempt_8/recipes/tree_recipe.rda"))
load(here("attempt_8/datasets/game_folds.rda"))

# set seed
set.seed(93815)

# model specification ----
bt_spec <- 
  boost_tree(
    trees = tune(), 
    min_n = tune(),
    mtry = tune(),
    learn_rate = tune()
  ) |> 
  set_engine("lightgbm") |> 
  set_mode("classification")


# define workflows ----
bt_wflow <- workflow() |>
  add_model(bt_spec) |>
  add_recipe(tree_recipe)

# hyperparameter tuning values ----
bt_params <- hardhat::extract_parameter_set_dials(bt_spec) |>
  update(
    trees = trees(),
    mtry = mtry(c(1, 36)),
    min_n = min_n(),
    learn_rate = learn_rate(c(-5, 0))
  )

# building tuning grid
bt_grid <- grid_random(bt_params, size = 20)

# parallel processing
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# fit workflows/models ----
bt_tuned <- bt_wflow |>
  tune_grid(
    resamples = game_folds,
    grid = bt_grid,
    control = control_grid(save_workflow = TRUE)
  )

stopCluster(cl)

# write out results (fitted/trained workflows) ----
save(bt_tuned, file = here("attempt_8/results/bt_tuned.rda"))
