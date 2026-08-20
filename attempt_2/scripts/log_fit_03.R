# Classification Problem ----
# Step 3: logistic model specification and fitting
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

# rm -f .git/index

# set seed for parallel processing
set.seed(12984)

# model specification ----
log_spec <- logistic_reg() |>
  set_mode("classification") |>
  set_engine("glm")

# workflow ----
log_wflow <-
  workflow() |>
  add_model(log_spec) |>
  add_recipe(basic_recipe)

# Create a cluster object and then register: 
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

log_fit <- log_wflow |> 
  fit_resamples(
    resamples = game_folds, 
    control = control_resamples(save_workflow = TRUE)
  )

stopCluster(cl)


# writing out results
save(log_fit, file = "attempt_1/results/log_fit.rda")