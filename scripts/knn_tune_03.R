# Classification Problem ----
# Step 3: k-nearest neighbors model specification and fitting
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doParallel)

# handling the conflicts
tidymodels_prefer()

# load data
load(here("attempt_1/recipes/basic_recipe.rda"))
load(here("attempt_1/datasets/game_folds.rda"))

# model specification ----
knn_spec <- 
  nearest_neighbor(
    neighbors = tune()
  ) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# define workflows ----
knn_wflow <- workflow() |>
  add_model(knn_spec) |>
  add_recipe(basic_recipe)

# hyperparameter tuning values ----
knn_params <- hardhat::extract_parameter_set_dials(knn_spec) |>
  update(
    neighbors = neighbors()
  )

# set seed for randomness
set.seed(297135)

# defining grid ----
knn_grid <- grid_regular(knn_params, levels = 5)

# parallel processing
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# fit workflows/models ----
knn_tuned <- knn_wflow |>
  tune_grid(
    resamples = game_folds,
    grid = knn_grid,
    control = control_grid(save_workflow = TRUE)
  )

stopCluster(cl)

# write out results (fitted/trained workflows) ----
save(knn_tuned, file = here("attempt_1/results/knn_tuned.rda"))