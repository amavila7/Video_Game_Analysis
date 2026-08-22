# Classification Problem ----
# Step 3: random forest model specification and fitting
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)

# handling the conflicts
tidymodels_prefer()

# load data
load(here("attempt_2/recipes/tree_recipe.rda"))
load(here("attempt_2/datasets/game_folds.rda"))

# set seed
set.seed(32846)

# model specification ----
rf_spec <- 
  rand_forest(
    trees = 1000, 
    min_n = tune(),
    mtry = tune()
  ) |> 
  set_engine("ranger") |> 
  set_mode("classification")

# define workflows ----
rf_wflow <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(tree_recipe)

# hyperparameter tuning values ----
rf_params <- hardhat::extract_parameter_set_dials(rf_spec) |>
  update(
    mtry = mtry(c(1, 10)),
    min_n = min_n()
  )

# building tuning grid
rf_grid <- grid_random(rf_params, size = 20)

# parallel processing
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# fit workflows/models ----
rf_tuned <- rf_wflow |>
  tune_grid(
    resamples = game_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE)
  )

stopCluster(cl)

# write out results (fitted/trained workflows) ----
save(rf_tuned, file = here("attempt_2/results/rf_tuned.rda"))