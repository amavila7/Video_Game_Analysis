# Classification Problem ----
# Step 3: elastic net model specification and fitting
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

# model specifications ----
en_model <- logistic_reg(mixture = tune(), penalty = tune()) |> 
  set_engine("glmnet") |>
  set_mode("classification")

# define workflows ----
en_wflow <- workflow() |>
  add_model(en_model) |>
  add_recipe(basic_recipe)

# set seed for parallel processing
set.seed(728350)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# hyperparameter tuning values ----
en_params <- hardhat::extract_parameter_set_dials(en_model) |>
  update(
    mixture = mixture(range = c(0,1)),
    penalty = penalty()
  )

# building tuning grid
en_grid <- grid_regular(en_params, levels = 5)



# fit folds
en_tuned <- en_wflow |>
  tune_grid(resamples = game_folds,
            grid = en_grid,
            control = control_resamples(save_workflow = TRUE)
  )

stopCluster(cl)

# write out results (fitted/trained workflows) ----
save(en_tuned, file = here("attempt_1/results/en_tuned.rda"))