# Train final model

# Load package(s) & set seed ----
library(tidymodels)
library(tidyverse)
library(here)

# Handle conflicts
tidymodels_prefer()

# load required objects
load(here("attempt_2/results/log_fit.rda"))
load(here("attempt_2/datasets/train_upsampled.rda"))
load(here("attempt_2/datasets/game_testing.rda"))


# finalize workflow ----
final_wflow_log <- log_fit |> 
  extract_workflow(log_fit) |>  
  finalize_workflow(select_best(log_fit, metric = "roc_auc"))

# train final model ----
# set seed
set.seed(813705)
final_fit_log <- fit(final_wflow_log, train_upsampled)

# saving final fit
# save(final_fit_log, file = here("attempt_2/results/final_fit.rda"))

#################################################################
# predicting

game_test <- game_testing |>
  mutate(
    goty_won = as.factor(goty_won)
  )

pred_log <- game_test |>
  bind_cols(predict(final_fit_log, new_data = game_test, type = "prob")) |>
  select(game_id, title, goty_won, predicted = .pred_1)

# save results
save(pred_log, file = "attempt_2/final_results/pred_log.rda")
