# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)
library(bonsai)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_2/results/bt_tuned.rda"))
load(here("attempt_2/datasets/train_upsampled.rda"))
load(here("attempt_2/datasets/game_testing.rda"))

# finalize workflow ----
final_wflow_bt <- bt_tuned |> 
  extract_workflow(bt_tuned) |>  
  finalize_workflow(select_best(bt_tuned, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(5238416)
final_fit_bt <- fit(final_wflow_bt, train_upsampled)

# saving final fit
# save(final_fit_bt, file = here("attempt_2results/final_fit.rda"))

#################################################################
# predicting

game_test <- game_testing |>
  mutate(
    goty_won = as.factor(goty_won)
  )

pred_bt <- game_test |>
  bind_cols(predict(final_fit_bt, new_data = game_test, type = "prob")) |>
  select(game_id, title, goty_won, predicted = .pred_1)


# save results
save(pred_bt, file = "attempt_2/final_results/pred_bt.rda")
