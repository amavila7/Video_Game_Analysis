# Classification Problem ----
# Step 2: recipes
## load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handling the conflicts
tidymodels_prefer()

# load data
load(here("attempt_2/datasets/train_upsampled.rda"))

# building basic recipe ----

basic_recipe <- recipe(goty_won ~ ., data = train_upsampled) |>
  update_role(game_id, title, publisher, developer, new_role = "id_vars") |>
  step_mutate(
    across(where(is.logical), as.factor),
    na_sales_million_log = log10(na_sales_million),
    eu_sales_million_log = log10(eu_sales_million),
    other_sales_million_log = log10(other_sales_million),
    global_sales_million_log = log10(global_sales_million),
    launch_price_usd = as.factor(launch_price_usd)
  ) |>
  step_novel(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.08) |>
  step_unknown(all_nominal_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors(), na_rm = TRUE)

# check recipe
basic_recipe |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()

# building basic tree recipe ----
tree_recipe <- recipe(goty_won ~ ., data = train_upsampled) |>
  update_role(game_id, title, publisher, developer, new_role = "id_vars") |>
  step_mutate(
    across(where(is.logical), as.factor),
    across(where(is.character), as.factor),
    na_sales_million_log = log10(na_sales_million),
    eu_sales_million_log = log10(eu_sales_million),
    other_sales_million_log = log10(other_sales_million),
    global_sales_million_log = log10(global_sales_million),
    launch_price_usd = as.factor(launch_price_usd)
  ) |>
  step_novel(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.05) |>
  step_unknown(all_nominal_predictors()) |>  
  step_impute_mode(all_nominal_predictors()) |>
  step_impute_mean(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors())

# check recipe
tree_recipe |>
  prep() |>
  bake(new_data = NULL) |>
  glimpse()


# write out recipes ----
save(basic_recipe, file = here("attempt_2/recipes/basic_recipe.rda"))
save(tree_recipe, file = here("attempt_2/recipes/tree_recipe.rda"))